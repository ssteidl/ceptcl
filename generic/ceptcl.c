/*
 * ceptcl.c --
 *
 *      This file implements a Tcl interface to
 *      Communication EndPoints (CEPs)
 *      and related commands.
 *
 * This was generic/tclIOCmd.c
 *
 * Copyright (c) 1995-1997 Sun Microsystems, Inc.
 * Copyright (c) 2003 Stuart Cassoff
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <unistd.h>
#include <string.h>
#include <netdb.h>
/* This may be needed for ntohs on some platforms */
#include <netinet/in.h>
/* */
/* #include <assert.h> */
#include <tcl.h>
#include "../generic/ceptcl.h"


#define NAGFO(str) Cep_ReportError(interp,"no argument given for ",(str)," option",(char *) NULL)


/*
 * Callback structure for accept callback in a CEP server.
 */

typedef struct AcceptCallback {
	char *script;			/* Script to invoke. */
	Tcl_Interp *interp;		/* Interpreter in which to run it. */
} AcceptCallback;


/*
 * Channel types that can be passed
 */

struct {
	int typeCh;
	const char *typeName;
} Cep_PassableChannelTypes[] = {
	{ 'c' , "cep"    },
	{ 'f' , "file"   },
	{ 't' , "tty"    },
	{ 's' , "socket" },
	{ -1  , NULL     }
};


/*
 * Static functions for this file:
 */

static CepAcceptProc AcceptCallbackProc;
static void RegisterCepServerInterpCleanup (Tcl_Interp *interp, AcceptCallback *acceptCallbackPtr);
static void CepAcceptCallbacksDeleteProc (ClientData clientData, Tcl_Interp *interp);
static void CepServerCloseProc (ClientData callbackData);
static void UnregisterCepServerInterpCleanupProc (Tcl_Interp *interp, AcceptCallback *acceptCallbackPtr);
static int Cep_GetServicePort (Tcl_Interp *interp, const char *service, const char *protocol, int *portPtr);
static int Cep_Cep_Cmd            (ClientData notUsed, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]);
static int Cep_CepPuts_Cmd        (ClientData notUsed, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]);
static int Cep_CepPassChannel_Cmd (ClientData notUsed, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]);
static int Cep_Resolve_Cmd        (ClientData notUsed, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]);


/*
 * Externals
 */

EXTERN int Cep_CepRead_Cmd (ClientData notUsed, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]);



/*
 *---------------------------------------------------------------------------
 *
 * Cep_GetServicePort --
 *
 * This was TclSockGetPort from generic/tclIO.c.
 *
 *	Maps from a string, which could be a service name, to a port.
 *	Used by socket creation code to get port numbers and resolve
 *	registered service names to port numbers.
 *
 * Results:
 *	A standard Tcl result.  On success, the port number is returned
 *	in portPtr. On failure, an error message is left in the interp's
 *	result.
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */

static int
Cep_GetServicePort (
	Tcl_Interp *interp,	/* Interp for error reporting */
	const char *service,	/* Integer or service name */
	const char *protocol,	/* "tcp" or "udp", typically */
	int *portPtr		/* Return port number */
) {
#ifdef USE_GETSERVBYNAME_R
	if (Tcl_GetInt(NULL, service, portPtr) != TCL_OK) {
		/* protocol isn't converted because this function
		 * is only called from within this file
 		 * and always with either "tcp" or "udp".
		 */
		struct servent se;	/* Protocol info for named services */
		struct servent_data sd;	/* Opaque structure used by getserven_r */
		Tcl_DString name;
		Tcl_DStringInit(&name);
		Tcl_UtfToExternalDString(NULL, service, -1, &name);
		/* sd needs to be initialized, se does not */
		(void) memset((void *) &sd, 0, sizeof(sd));
		if (getservbyname_r((const char *) Tcl_DStringValue(&name), protocol, &se, &sd) == 0) {
			*portPtr = ntohs((unsigned short) se.s_port);
			Tcl_DStringFree(&name);
			return TCL_OK;
		}
		Tcl_DStringFree(&name);
	}
#else
	if (Tcl_GetInt(NULL, service, portPtr) != TCL_OK) {
		/* protocol isn't converted because this function
		 * is only called from within this file
 		 * and always with either "tcp" or "udp".
		 */
		struct servent *sep;	/* Protocol info for named services */
		Tcl_DString name;

		Tcl_DStringInit(&name);
		Tcl_UtfToExternalDString(NULL, service, -1, &name);
		sep = getservbyname((const char *) Tcl_DStringValue(&name), protocol);
		Tcl_DStringFree(&name);
		if (sep != NULL) {
			*portPtr = ntohs((unsigned short) sep->s_port);
			return TCL_OK;
		}
	}
#endif /* HAVE_GETSERVBYNAME_R */
	if (Tcl_GetInt(interp, service, portPtr) != TCL_OK) {
		return TCL_ERROR;
	}
	if (*portPtr > 0xFFFF) {
		return rerr("couldn't open cep: port number too high");
	}
	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CepAcceptCallbacksDeleteProc --
 *
 *      Assocdata cleanup routine called when an interpreter is being
 *      deleted to set the interp field of all the accept callback records
 *      registered with the interpreter to NULL. This will prevent the
 *      interpreter from being used in the future to eval accept scripts.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Deallocates memory and sets the interp field of all the accept
 *      callback records to NULL to prevent this interpreter from being
 *      used subsequently to eval accept scripts.
 *
 *----------------------------------------------------------------------
 */

static void
CepAcceptCallbacksDeleteProc (
	ClientData clientData,	/* Data which was passed when the assocdata
				 * was registered. */
	Tcl_Interp *interp	/* Interpreter being deleted - not used. */
) {
	Tcl_HashTable *hTblPtr;
	Tcl_HashEntry *hPtr;
	Tcl_HashSearch hSearch;
	AcceptCallback *acceptCallbackPtr;

	hTblPtr = (Tcl_HashTable *) clientData;
	for (hPtr = Tcl_FirstHashEntry(hTblPtr, &hSearch);
			hPtr != (Tcl_HashEntry *) NULL;
			hPtr = Tcl_NextHashEntry(&hSearch)) {
		acceptCallbackPtr = (AcceptCallback *) Tcl_GetHashValue(hPtr);
		acceptCallbackPtr->interp = (Tcl_Interp *) NULL;
	}
	Tcl_DeleteHashTable(hTblPtr);
	ckfree((char *) hTblPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * RegisterCepServerInterpCleanup --
 *
 *	Registers an accept callback record to have its interp
 *	field set to NULL when the interpreter is deleted.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When, in the future, the interpreter is deleted, the interp
 *	field of the accept callback data structure will be set to
 *	NULL. This will prevent attempts to eval the accept script
 *	in a deleted interpreter.
 *
 *----------------------------------------------------------------------
 */

static void
RegisterCepServerInterpCleanup (
	Tcl_Interp *interp,	/* Interpreter for which we want to be
				 * informed of deletion. */
	AcceptCallback *acceptCallbackPtr
				/* The accept callback record whose
				 * interp field we want set to NULL when
				 * the interpreter is deleted. */
) {
	Tcl_HashTable *hTblPtr;	/* Hash table for accept callback
				 * records to smash when the interpreter
				 * will be deleted. */
	Tcl_HashEntry *hPtr;	/* Entry for this record. */
	int new;		/* Is the entry new? */

	hTblPtr = (Tcl_HashTable *) Tcl_GetAssocData(interp, "CepAcceptCallbacks", NULL);
	if (hTblPtr == (Tcl_HashTable *) NULL) {
		hTblPtr = (Tcl_HashTable *) ckalloc((unsigned) sizeof(Tcl_HashTable));
		Tcl_InitHashTable(hTblPtr, TCL_ONE_WORD_KEYS);
		(void) Tcl_SetAssocData(interp, "CepAcceptCallbacks", CepAcceptCallbacksDeleteProc, (ClientData) hTblPtr);
	}
	hPtr = Tcl_CreateHashEntry(hTblPtr, (char *) acceptCallbackPtr, &new);
	if (!new) {
		Tcl_Panic("RegisterCepServerInterpCleanup: damaged accept record table");
	}
	Tcl_SetHashValue(hPtr, (ClientData) acceptCallbackPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * UnregisterCepServerInterpCleanupProc --
 *
 *	Unregister a previously registered accept callback record. The
 *	interp field of this record will no longer be set to NULL in
 *	the future when the interpreter is deleted.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Prevents the interp field of the accept callback record from
 *	being set to NULL in the future when the interpreter is deleted.
 *
 *----------------------------------------------------------------------
 */

static void
UnregisterCepServerInterpCleanupProc (
	Tcl_Interp *interp,	/* Interpreter in which the accept callback
				 * record was registered. */
	AcceptCallback *acceptCallbackPtr
				/* The record for which to delete the
				 * registration. */
) {
	Tcl_HashTable *hTblPtr;
	Tcl_HashEntry *hPtr;

	hTblPtr = (Tcl_HashTable *) Tcl_GetAssocData(interp, "CepAcceptCallbacks", NULL);
	if (hTblPtr == (Tcl_HashTable *) NULL) {
		return;
	}
	hPtr = Tcl_FindHashEntry(hTblPtr, (char *) acceptCallbackPtr);
	if (hPtr == (Tcl_HashEntry *) NULL) {
		return;
	}
	Tcl_DeleteHashEntry(hPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CepServerCloseProc --
 *
 *	This callback is called when the cep server channel for which it
 *	was registered is being closed. It informs the interpreter in
 *	which the accept script is evaluated (if that interpreter still
 *	exists) that this channel no longer needs to be informed if the
 *	interpreter is deleted.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	In the future, if the interpreter is deleted this channel will
 *	no longer be informed.
 *
 *----------------------------------------------------------------------
 */

static void
CepServerCloseProc (
	ClientData callbackData		/* The data passed in the call to
					 * Tcl_CreateCloseHandler. */
) {
	AcceptCallback *acceptCallbackPtr = (AcceptCallback *) callbackData;

	if (acceptCallbackPtr->interp != (Tcl_Interp *) NULL) {
		UnregisterCepServerInterpCleanupProc(acceptCallbackPtr->interp, acceptCallbackPtr);
	}
	Tcl_EventuallyFree((ClientData) acceptCallbackPtr->script, TCL_DYNAMIC);
	ckfree((char *) acceptCallbackPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * AcceptCallbackProc --
 *
 *	This callback is invoked by the CEP channel driver when it
 *	accepts a new connection from a client on a server socket.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Whatever the script does.
 *
 *----------------------------------------------------------------------
 */

static void
AcceptCallbackProc (
	ClientData callbackData,	/* The data stored when the callback
					 * was created in the call to
					 * OpenCepServer. */
	Tcl_Channel chan,		/* Channel for the newly accepted
					 * connection. */
	const char *addr,		/* Address of client that was
					 * accepted. */
	int port,
	uid_t euid,
	gid_t egid,
	unsigned int flags
) {
	AcceptCallback *acceptCallbackPtr = (AcceptCallback *) callbackData;
	Tcl_Interp *tinterp;
	char *tscript;
	int result;

	/*
	 * Check if the callback is still valid; the interpreter may have gone
	 * away, this is signalled by setting the interp field of the callback
	 * data to NULL.
	 */
    
	if (acceptCallbackPtr->interp == (Tcl_Interp *) NULL) {
		/*
		 * The interpreter has been deleted, so there is no useful
		 * way to utilize the client socket - just close it.
		 */
		Tcl_Close((Tcl_Interp *) NULL, chan);
		return;
	}

	/* Needs to be big enough to hold list
	 * of euid and egid for local ceps.
	 * For other ceps, it holds the port number.
	 * 2 txtints + 2 braces + 1 space + 1 nul.
	 */
	char buf[((TCL_INTEGER_SPACE * 2) + 14)];

	if (MASK2DOMAIN(flags) == CEP_LOCAL) {
		(void) sprintf(buf, "{%d %d}", euid, egid);
	} else {
		(void) sprintf(buf, "%d", port);
	}

	tscript = acceptCallbackPtr->script;
	tinterp = acceptCallbackPtr->interp;

	Tcl_RegisterChannel(tinterp, chan);

	/*
	 * Artificially bump the refcount to protect the channel from
	 * being deleted while the script is being evaluated.
	 */

	Tcl_RegisterChannel((Tcl_Interp *) NULL,  chan);


	Tcl_Preserve((ClientData) tscript);
	Tcl_Preserve((ClientData) tinterp);

	result = TCL_OK;

	if (flags & CEP_CHANRECV) {
		result = Tcl_VarEval(tinterp, "fconfigure ", Tcl_GetChannelName(chan), " ", addr, (char *) NULL);
		if (result != TCL_OK) {
			Tcl_BackgroundError(tinterp);
			Tcl_UnregisterChannel(tinterp, chan);
		}
		addr = Cep_PassableChannelTypeCharToName(port);
	}
	if (result == TCL_OK) {
		result = Tcl_VarEval(tinterp, tscript, " ", Tcl_GetChannelName(chan), " ", addr, " ", buf, (char *) NULL);
		if (result != TCL_OK) {
			Tcl_BackgroundError(tinterp);
			Tcl_UnregisterChannel(tinterp, chan);
		}
	}

	/*
	 * Decrement the artificially bumped refcount.
	 * After this it is not safe anymore to use "chan",
	 * because it may now be deleted.
	 */

	Tcl_UnregisterChannel((Tcl_Interp *) NULL, chan);

	Tcl_Release((ClientData) tinterp);
	Tcl_Release((ClientData) tscript);
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_Cep_Cmd --
 *
 *	This procedure is invoked to process the "cep" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Creates an cep based channel.
 *
 *----------------------------------------------------------------------
 */

static int
Cep_Cep_Cmd (
	ClientData notUsed,		/* Not used. */
	Tcl_Interp *interp,		/* Current interpreter. */
	int objc,			/* Number of arguments. */
	Tcl_Obj *const objv[]		/* Argument objects. */
) {
	static const char *cepOptions[] = {
		"-async",       "-chanrecv", "-domain",
		"-myaddr",      "-myport",   "-noresolve",
		"-noreuseaddr", "-protocol", "-reuseport",
		"-server",      "-type",
		(char *) NULL
	};
	enum cepOptions {
		CEP_OPT_ASYNC,       CEP_OPT_CHANRECV, CEP_OPT_DOMAIN,
		CEP_OPT_MYADDR,      CEP_OPT_MYPORT,   CEP_OPT_NORESOLVE,
		CEP_OPT_NOREUSEADDR, CEP_OPT_PROTOCOL, CEP_OPT_REUSEPORT,
		CEP_OPT_SERVER,      CEP_OPT_TYPE
	};

	Tcl_Channel chan;
	AcceptCallback *acceptCallbackPtr = NULL;
	int optionIndex;
	int a;
	const char *arg;
	int domainSpecified = 0;
	int typeSpecified = 0;
	Tcl_DString tln;
	const char *name = NULL;
	const char *script = NULL;
	const char *myaddr = NULL;
	const char *myportName = NULL;
	const char *protocol = NULL;
	char *copyScript = NULL;
	/* defaults */
	int async = 0;
	int resolve = 1;
	int reuseaddr = 1;
	int reuseport = 0;
	int server = 0;
	int chanrecv = 0;
	int localpair = 0;
	int port = 0;
	int myport = 0;
	int cepDomain = CEP_INET;
	int cepType = CEP_STREAM;
	unsigned int flags;

	for (a = 1; a < objc; a++) {
		arg = Tcl_GetString(objv[a]);
		if (arg[0] != '-') {
			break;
		}
		if (Tcl_GetIndexFromObj(interp, objv[a], cepOptions,
				"option", TCL_EXACT, &optionIndex) != TCL_OK) {
			return TCL_ERROR;
		}
		switch ((enum cepOptions) optionIndex) {
		case CEP_OPT_ASYNC:
			async = 1;		
			break;
		case CEP_OPT_NORESOLVE:
			resolve = 0;
			break;
		case CEP_OPT_NOREUSEADDR:
			reuseaddr = 0;
			break;
		case CEP_OPT_REUSEPORT:
			reuseport = 1;
			break;
		case CEP_OPT_MYADDR:
			if (++a >= objc) { return NAGFO(cepOptions[optionIndex]); }
			myaddr = Tcl_GetString(objv[a]);
			break;
		case CEP_OPT_MYPORT:
			if (++a >= objc) { return NAGFO(cepOptions[optionIndex]); }
			myportName = Tcl_GetString(objv[a]);
			break;
		case CEP_OPT_SERVER:
			if (++a >= objc) { return NAGFO(cepOptions[optionIndex]); }
			script = Tcl_GetString(objv[a]);
			server = 1;
			break;
		case CEP_OPT_CHANRECV:
			if (++a >= objc) { return NAGFO(cepOptions[optionIndex]); }
			script = Tcl_GetString(objv[a]);
			chanrecv = 1;
			break;
		case CEP_OPT_DOMAIN: {
			static const char *domainOptions[] = {
				"inet", "inet6", "local", (char *) NULL
			};
			enum domainOptions {
				DOMAIN_OPT_DOMAIN_INET, DOMAIN_OPT_DOMAIN_INET6, DOMAIN_OPT_DOMAIN_LOCAL
			};
			int domainIndex;

			if (++a >= objc) { return NAGFO(cepOptions[optionIndex]); }
			if (Tcl_GetIndexFromObj(interp, objv[a], domainOptions,
					"option", TCL_EXACT, &domainIndex) != TCL_OK) {
				return TCL_ERROR;
			}
			switch ((enum domainOptions) domainIndex) {
			case DOMAIN_OPT_DOMAIN_INET:
				cepDomain = CEP_INET;
				break;
			case DOMAIN_OPT_DOMAIN_INET6:
				cepDomain = CEP_INET6;
				break;
			case DOMAIN_OPT_DOMAIN_LOCAL:
				cepDomain = CEP_LOCAL;
				break;
			default:
				break;
			}
			domainSpecified = 1;
			break;
		}
		case CEP_OPT_TYPE: {
			static const char *typeOptions[] = {
				"datagram", "raw", "stream", (char *) NULL
			};
			enum typeOptions {
				TYPE_OPT_TYPE_DGRAM, TYPE_OPT_TYPE_RAW, TYPE_OPT_TYPE_STREAM
			};
			int typeIndex;

			if (++a >= objc) { return NAGFO(cepOptions[optionIndex]); }
			if (Tcl_GetIndexFromObj(interp, objv[a], typeOptions,
					"option", TCL_EXACT, &typeIndex) != TCL_OK) {
				return TCL_ERROR;
			}
			switch ((enum typeOptions) typeIndex) {
			case TYPE_OPT_TYPE_DGRAM:
				cepType = CEP_DGRAM;
				break;
			case TYPE_OPT_TYPE_STREAM:
				cepType = CEP_STREAM;
				break;
			case TYPE_OPT_TYPE_RAW:
				cepType = CEP_RAW;
				break;
			default:
				break;
			}
			typeSpecified = 1;
			break;
		}
		case CEP_OPT_PROTOCOL:
			if (++a >= objc) { return NAGFO(cepOptions[optionIndex]); }
			protocol = Tcl_GetString(objv[a]);
			break;
		default:
			Tcl_Panic("Cep_Cmd: bad option index to cepOptions");
		}
	}

	if (server && async) { return rerr("cannot set -async option for server ceps"); }

	if (server && chanrecv) { return rerr("-chanrecv and -server are mutually exclusive"); }

	if (myportName != NULL) {
		if (Cep_GetServicePort(interp, myportName, (cepType == CEP_STREAM ? "tcp" : "udp"), &myport) != TCL_OK) {
			return TCL_ERROR;
		}
	}

	if (server) {
		if (cepDomain == CEP_INET6 || cepDomain == CEP_INET) {
			name = myaddr;          /* NULL implies INADDR_ANY */
		} else {
			name = Tcl_GetString(objv[a]);
			a++;
		}
		if (myport != 0) { return rerr("Option -myport is not valid for servers"); }
	} else if (a < objc) {
		name = Tcl_GetString(objv[a]);
		a++;
	}

	if (a < objc) {
		if (Cep_GetServicePort(interp, (const char *) Tcl_GetString(objv[a]), (cepType == CEP_STREAM ? "tcp" : "udp"), &port) != TCL_OK) {
			return TCL_ERROR;
		}
		a++;
	}

	if ((a == objc) && (cepDomain == CEP_LOCAL || !domainSpecified) && (name == NULL) && !server) {
		cepDomain = CEP_LOCAL;
		localpair = 1;
	} else if (a != objc) {
		return Cep_ReportError(interp, 
			"wrong # args: should be either:\n",
			Tcl_GetString(objv[0]), " ?-domain local? ?-type type?\n",
			Tcl_GetString(objv[0]), " ?-domain domain? ?-type type? ?-protocol protocol? ?-myaddr addr? ?-myport myport? ?-noresolve? ?-noreuseaddr? ?-reuseport? ?-async? host ?port?\n",
			Tcl_GetString(objv[0]), " ?-domain domain? ?-type type? ?-noresolve? ?-noreuseaddr? ?-reuseport? -server command ?port/name?",
			(char *) NULL);
	}

	if (cepDomain == CEP_LOCAL && !localpair) {
		char *r;
		Tcl_DString ds;

		Tcl_DStringInit(&tln);

		Tcl_ResetResult(interp);
		r = Tcl_TranslateFileName(interp, name, &ds);
		if (r == NULL) {
			return TCL_ERROR;
		}
		Tcl_ExternalToUtfDString(NULL, Tcl_DStringValue(&ds), Tcl_DStringLength(&ds), &tln);
		Tcl_DStringFree(&ds);
		name = Tcl_DStringValue(&tln);
	}

	flags = Cep_SetFreshCepFlags(cepDomain, cepType, server, resolve,
			reuseaddr, reuseport, async, chanrecv);

	if (server || chanrecv) {
		acceptCallbackPtr = (AcceptCallback *) ckalloc(sizeof(AcceptCallback));
		acceptCallbackPtr->interp = interp;
		copyScript = ckalloc((unsigned) strlen(script) + 1);
		strcpy(copyScript, script);
		acceptCallbackPtr->script = copyScript;
	}

	if (server) {
		chan = Cep_OpenServer(interp, protocol,
				(const char *) name, port,
				AcceptCallbackProc,
				(ClientData) acceptCallbackPtr,
				flags);

		if (cepDomain == CEP_LOCAL) {
			Tcl_DStringFree(&tln);
		}

		if (chan == (Tcl_Channel) NULL) {
			ckfree(copyScript);
			ckfree((char *) acceptCallbackPtr);
			return TCL_ERROR;
		}

		/*
		 * Register with the interpreter to let us know when the
		 * interpreter is deleted (by having the callback set the
		 * acceptCallbackPtr->interp field to NULL). This is to
		 * avoid trying to eval the script in a deleted interpreter.
		 */
		RegisterCepServerInterpCleanup(interp, acceptCallbackPtr);
       
		/*
		 * Register a close callback. This callback will inform the
		 * interpreter (if it still exists) that this channel does not
		 * need to be informed when the interpreter is deleted.
		 */
		Tcl_CreateCloseHandler(chan, CepServerCloseProc, (ClientData) acceptCallbackPtr);

	} else if (localpair) {
		Tcl_Channel chan2;
		if (async)              { return rerr("cannot set -async option for localpair ceps"); }
		if (myaddr != NULL)     { return rerr("cannot set -myaddr option for localpair ceps"); }
		if (myportName != NULL) { return rerr("cannot set -myport option for localpair ceps"); }
		if (Cep_OpenLocalPair(interp, protocol, &chan, &chan2, flags) != 0) {
			return TCL_ERROR;
		}
		Tcl_RegisterChannel(interp, chan);
		Tcl_RegisterChannel(interp, chan2);
		Cep_ReportError(interp, Tcl_GetChannelName(chan), " ", Tcl_GetChannelName(chan2), (char *) NULL);
		return TCL_OK;
	} else {
		CepAcceptProc *AcceptCallbackProcPtr = (chanrecv ? AcceptCallbackProc : (CepAcceptProc *) NULL);
		chan = Cep_OpenClient(interp, protocol,
			((port == -1 && strlen(name) == 0) ? NULL : name), port,
			myaddr, myport,
			AcceptCallbackProcPtr,
			(ClientData) acceptCallbackPtr,
			flags);

		if (cepDomain == CEP_LOCAL) {
			Tcl_DStringFree(&tln);
		}

		if (chan == (Tcl_Channel) NULL) {
			if (chanrecv) {
				ckfree(copyScript);
				ckfree((char *) acceptCallbackPtr);
			}
			return TCL_ERROR;
		}

		if (chanrecv) {
			RegisterCepServerInterpCleanup(interp, acceptCallbackPtr);
			Tcl_CreateCloseHandler(chan, CepServerCloseProc, (ClientData) acceptCallbackPtr);
		}
	}

	/* localpair is handled above */
	Tcl_RegisterChannel(interp, chan);
	rerr(Tcl_GetChannelName(chan));

	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_CepPuts_Cmd --
 *
 *	This procedure is invoked to process the "cepPuts" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Produces output on a channel.
 *
 *----------------------------------------------------------------------
 */

static int
Cep_CepPuts_Cmd (
	ClientData notUsed,		/* Not used. */
	Tcl_Interp *interp,		/* Current interpreter. */
	int objc,			/* Number of arguments. */
	Tcl_Obj *const objv[]		/* Argument objects. */
) {
	Tcl_Channel chan;		/* The channel to puts on. */
	Tcl_Obj *data;			/* Data to write. */
	int newline = 1;		/* Add a newline at end? */
	const char *channelId;		/* Name of channel for puts. */
	int mode;			/* Mode in which channel is opened. */
	int port;
	int dataLen;
	const char *s;
	int i;
	const unsigned char * dataBytes;

	switch (objc) {
	case 2:
		/*
		 * puts data
		 */
		data = objv[1];
		channelId = "stdout";
		break;
	case 3:
		/*
		 * puts	-nonewline	data
		 * puts	-newline	data
		 * puts	chan		data
		 */ 
		channelId = s = Tcl_GetString(objv[1]);
		if (s[0] == '-') {
			i = ((s[1] == 'n' && s[2] == 'o') ? 3 : 1);
			if (strcmp(s + i, "newline") != 0) {
				goto cmderr;
			}
			newline = (i == 1);
			channelId = "stdout";
		}
		data = objv[2];
		break;
	case 4:
		/*
		 * puts -nonewline	chan	data
		 * puts -newline	chan	data
		 */
		s = Tcl_GetString(objv[1]);
		if (s[0] != '-') {
			goto cmderr;
		}
		i = ((s[1] == 'n' && s[2] == 'o') ? 3 : 1);
		if (strcmp(s + i, "newline") != 0) {
			goto cmderr;
		}
		newline = (i == 1);
		channelId = Tcl_GetString(objv[2]);
		data = objv[3];
		break;
	case 5:
		/*
		 * puts chan	addr	port	data
		 */
		channelId = Tcl_GetString(objv[1]);
		if (Cep_GetServicePort(interp, (const char *) Tcl_GetString(objv[3]), "udp", &port) != TCL_OK) {
			return TCL_ERROR;
		}
		data = objv[4];
		break;
	default:
		/*
		 * puts some bad number of arguments...
		 * other errs; errors above jump here
		 */
		cmderr:
		return Cep_ReportError(interp, "no good; should be \"", Tcl_GetString(objv[0]),
				" blah blah blah\"", (char *) NULL);
	}

	chan = Tcl_GetChannel(interp, channelId, &mode);

	if (chan == (Tcl_Channel) NULL) {
		return TCL_ERROR;
	}

	if ((mode & TCL_WRITABLE) == 0) {
		return Cep_ReportError(interp, "channel \"", channelId,
						"\" wasn't opened for writing", (char *) NULL);
	}

	if ((objc == 5) && (strcmp(Tcl_ChannelName(Tcl_GetChannelType(chan)), "cep") == 0) &&
			(((i = Cep_GetCepType(chan)) == CEP_DGRAM) || (i == CEP_RAW))) {
		dataBytes = Tcl_GetByteArrayFromObj(data, &dataLen);
		dataLen = Cep_Sendto(chan, (const char *) Tcl_GetString(objv[2]), port, dataBytes, dataLen);
		if (dataLen == -1) {
			return TCL_ERROR;
		}
	} else if (objc <= 4) {
		i = 0;
		if (((dataLen = Tcl_WriteObj(chan, data)) < 0) || (newline && ((i = Tcl_WriteChars(chan, "\n", 1)) < 0))) {
			return Cep_ReportError(interp, "error writing \"", channelId, "\": ", Tcl_ErrnoMsg(Tcl_GetErrno()), (char *) NULL);
		}
		dataLen += i;
	} else {
		return rerr("can't do that with a regular Tcl channel");
	}
	Tcl_SetObjResult(interp, Tcl_NewIntObj(dataLen));
	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_CepPassChannel_Cmd --
 *
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *
 *----------------------------------------------------------------------
 */

static int
Cep_CepPassChannel_Cmd (
	ClientData notUsed,		/* Not used. */
	Tcl_Interp *interp,		/* Current interpreter. */
	int objc,			/* Number of arguments. */
	Tcl_Obj *const objv[]		/* Argument objects. */
) {
	Tcl_Channel chans[2];
	int mode;			/* Mode in which a channel is opened. */
	int i;

	if (objc != 3) {
		return Cep_ReportError(interp, "wrong # args: should be \"", Tcl_GetString(objv[0]),
							" channel passChannel\"", (char *) NULL);
	}

	objv++;
	for (i = 1; i >= 0; i--) {
		chans[i] = Tcl_GetChannel(interp, Tcl_GetString(objv[i]), &mode);
		if (chans[i] == (Tcl_Channel) NULL) {
			return TCL_ERROR;
		}
	}
	/* mode will be the mode of chans[0] : the chan that chans[1] will be passed on */
	if ((mode & TCL_WRITABLE) == 0) {
		return Cep_ReportError(interp, "channel \"", Tcl_GetString(objv[0]),
						"\" wasn't opened for writing", (char *) NULL);
	}

	if (strcmp(Tcl_ChannelName(Tcl_GetChannelType(chans[0])), "cep") != 0) {
		return rerr("can't do that with a regular Tcl channel");
	}

	i = Cep_PassChan(chans[0], chans[1]);

	if (i == -1) {
		return rperr("couldn't pass channel: ");
	}

	Tcl_SetObjResult(interp, Tcl_NewIntObj(i));

	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_Resolve_Cmd --
 *
 *	This procedure is invoked to process the "resolve" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *
 *
 * Side effects:
 *
 *
 *----------------------------------------------------------------------
 */

static int
Cep_Resolve_Cmd (
	ClientData notUsed,		/* Not used. */
	Tcl_Interp *interp,		/* Current interpreter. */
	int objc,			/* Number of arguments. */
	Tcl_Obj *const objv[]		/* Argument objects. */
) {
	int cepDomain = CEP_INET;
	int a = 1;
	int nargs;
	Tcl_Obj *const *argPtr;

	if (objc == 1) {
		goto wrongNumArgs;
	}
	if (objc > 2) {
		int domainIndex;
		char *charPtr;
		int len;

		charPtr = Tcl_GetString(objv[a]);
		len = strlen(charPtr);

		if (charPtr[0] == '-') {
			static const char *domainOptions[] = {
				"inet", "inet6", (char *) NULL
			};
			enum domainOptions {
				DOMAIN_INET, DOMAIN_INET6
			};

			if ((len > 1) && (charPtr[1] == 'd') && (strncmp(charPtr, "-domain", len) == 0)) {
				/* Can't happen? */
				if (++a >= objc) { return NAGFO("-domain"); }
				if (Tcl_GetIndexFromObj(interp, objv[a], domainOptions,
						"option", TCL_EXACT, &domainIndex) != TCL_OK) {
					return TCL_ERROR;
				}
				switch ((enum domainOptions) domainIndex) {
				case DOMAIN_INET:
					/* It's already set but this seems more complete */
					cepDomain = CEP_INET;
					break;
				case DOMAIN_INET6:
					cepDomain = CEP_INET6;
					break;
				default:
					break;
				}
			} else {
				return Cep_ReportError(interp, "bad option \"", Tcl_GetString(objv[1]),
								"\": should be -domain", (char *) NULL);
			}
			if (++a >= objc) {
				goto wrongNumArgs;
			}
		}
	}

	nargs = objc - a - 1;
	if (nargs == 0) {
		argPtr = NULL;
	} else {
		argPtr = &objv[a + 1];
	}
	return resolve(interp, Tcl_GetString(objv[a]), cepDomain, nargs, argPtr);

	wrongNumArgs:
	return Cep_ReportError(interp, "wrong # args: should be \"", Tcl_GetString(objv[0]),
						" ?-domain domain? hostname ?optionName ...?\"", (char *) NULL);
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_StrCepDomain --
 *
 *
 * Results:
 *
 *
 * Side effects:
 *
 *
 *
 *----------------------------------------------------------------------
 */

const char *Cep_StrCepDomain (
	int cepDomain
) {
	switch (cepDomain) {
	case CEP_LOCAL:
		return "local";
	case CEP_INET:
		return "inet";
	case CEP_INET6:
		return "inet6";
	default:
		return "?";
	}
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_StrCepType --
 *
 *
 * Results:
 *
 *
 * Side effects:
 *
 *
 *
 *----------------------------------------------------------------------
 */

const char *Cep_StrCepType (
	int cepType
) {
	switch (cepType) {
	case CEP_STREAM:
		return "stream";
	case CEP_DGRAM:
		return "datagram";
	case CEP_RAW:
		return "raw";
	default:
		return "?";
	}
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_SetFreshCepFlags --
 *
 *
 *
 *
 * Results:
 *	flags set according to parms
 *
 * Side effects:
 *
 *
 *----------------------------------------------------------------------
 */

unsigned int
Cep_SetFreshCepFlags (
	int cepDomain,
	int cepType,
	int server,
	int resolve,
	int reuseaddr,
	int reuseport,
	int async,
	int chanrecv
) {
	unsigned int flags = 0;
	flags |= DOMAIN2MASK(cepDomain);
	flags |= TYPE2MASK(cepType);
	if (server)    { flags |= CEP_SERVER; }
	if (resolve)   { flags |= CEP_RESOLVE_NAMES; }
	if (reuseaddr) { flags |= CEP_REUSEADDR; }
	if (reuseport) { flags |= CEP_REUSEPORT; }
	if (async)     { flags |= CEP_ASYNC; }
	if (chanrecv)  { flags |= CEP_CHANRECV; }

	return flags;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_GetPassableChannelTypeChar --
 *
 *
 * Results:
 *
 *
 * Side effects:
 *
 *
 *----------------------------------------------------------------------
 */

int
Cep_GetPassableChannelTypeChar (
	Tcl_Channel channel
) {
	int i;
	const char *typeName;

	typeName = Tcl_ChannelName(Tcl_GetChannelType(channel));

	for (i = 0; Cep_PassableChannelTypes[i].typeCh != -1; i++) {
		if ((*typeName == *Cep_PassableChannelTypes[i].typeName) &&
				(strcmp(typeName, Cep_PassableChannelTypes[i].typeName) == 0)) {
			return Cep_PassableChannelTypes[i].typeCh;
		}
	}

	return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_PassableChannelTypeCharToName --
 *
 *
 * Results:
 *
 *
 * Side effects:
 *
 *
 *----------------------------------------------------------------------
 */

const char *
Cep_PassableChannelTypeCharToName (
	int typeCh
) {
	int i;

	for (i = 0; Cep_PassableChannelTypes[i].typeCh != -1; i++) {
		if (typeCh == Cep_PassableChannelTypes[i].typeCh) {
			return Cep_PassableChannelTypes[i].typeName;
		}
	}

	return (const char *) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_ReportError --
 *
 *
 *
 *
 * Results:
 *	TCL_ERROR
 *
 * Side effects:
 *
 *
 *----------------------------------------------------------------------
 */

int
Cep_ReportError TCL_VARARGS_DEF(Tcl_Interp *,arg1)
{
	Tcl_Interp *interp;
	va_list argList;
	Tcl_Obj *result;

	interp = TCL_VARARGS_START(Tcl_Interp *,arg1,argList);
	if (interp == NULL) {
		return TCL_ERROR;
	}
	result = Tcl_NewObj();
	Tcl_AppendStringsToObjVA(result, argList);
	va_end(argList);
	Tcl_SetObjResult(interp, result);

	return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * npstrlen --
 *
 *	Length of null-padded string.
 *
 *
 * Results:
 *	Length.
 *
 * Side effects:
 *
 *
 *----------------------------------------------------------------------
 */

size_t
npstrlen (
	const char *str,
	size_t max
) {
	size_t l;

	for (l = 0; *str && l < max; l++, str++)
		; /* EMPTY */

	return l;
}

/*
 *----------------------------------------------------------------------
 *
 * ckstrdup --
 *
 *	strdup using ckalloc, so free it with ckfree.
 *
 *
 * Results:
 *	Pointer to new string if successful., otherwise NULL.
 *
 * Side effects:
 * May explode if strlen(str) > 16384 (? - size_t vs int)
 *
 *----------------------------------------------------------------------
 */

char *
ckstrdup (
	const char *str
) {
	char *s;
	int size;

	size = strlen(str);
	s = ckalloc(size + 1);
	if (s != NULL) {
		(void *) memcpy(s, str, size);
		s[size] = '\0';
	}
	return s;
}

/*
 *----------------------------------------------------------------------
 *
 * nagfo --
 *
 *	put a "No argument given for option" message
 *      in the interp's result and return TCL_ERROR
 *
 *
 * Results:
 *	TCL_ERROR
 *
 * Side effects:
 *	Sets the interp's result.
 *
 *----------------------------------------------------------------------
 */

int
nagfo (
	Tcl_Interp *interp,
	const char *optName
) {
	return Cep_ReportError(interp,
			"no argument given for ", optName, " option", (char *) NULL);
}

/*
 *----------------------------------------------------------------------
 *
 * cvl --
 *
 * 	Calculate number of bytes needed to store val as vlq
 *
 * Results:
 *	nbytes
 *
 * Side effects:
 *
 *
 *----------------------------------------------------------------------
 */

int
cvl (
	unsigned long val
) {
	int nbytes = 1;

	while ((val >>= 7) > 0) {
		nbytes++;
	}

	return nbytes;
}

/*
 *----------------------------------------------------------------------
 *
 * wvl --
 *
 *	Write vlq to buf
 *
 * Results:
 *	nbytes		Bytes written
 *
 * Side effects:
 *	buf is written to
 *
 *----------------------------------------------------------------------
 */

int
wvl (
	unsigned char *buf,
	unsigned long val
) {
	int nbytes = 1;
	int i;

	buf[0] = (unsigned char) (val & 0x7f);

	while ((val >>= 7) > 0) {
		for (i = nbytes; i > 0; i--) {
			buf[i] = buf[i-1];
		}
		buf[0] = (unsigned char) ((val & 0x7f) | 0x80);
		nbytes++;
	}

	return nbytes;
}

/*
 *----------------------------------------------------------------------
 *
 * rvl --
 *
 *	Read vlq from buf, return nbytes.
 *	Set *val to vlq value if val != NULL.
 *
 * Results:
 *	nbytes	Size in bytes of vlq
 *
 * Side effects:
 *	Sets *val if val != NULL
 *
 *----------------------------------------------------------------------
 */

int
rvl (
	const unsigned char *buf,
	unsigned long *val
) {
	unsigned long v = 0;
	int nbytes = 0;

	do {
		v <<= 7;
		v += (*buf & 0x7f);
		nbytes++;
	} while (*buf++ >= 0x80);

	if (val != NULL) {
		*val = v;
	}

	return nbytes;
}

/*
 *----------------------------------------------------------------------
 *
 * Ceptcl_Init --
 *
 *      Initialize the new package.  The string "Cep" in the
 *      function name must match the PACKAGE declaration at the top of
 *      configure.in.
 *
 * Results:
 *      A standard Tcl result
 *
 * Side effects:
 *      The cep package is created.
 *      One new command "cep" is added to the Tcl interpreter.
 *
 *----------------------------------------------------------------------
 */

int
Ceptcl_Init (
	Tcl_Interp *interp
) {
	if (Tcl_InitStubs(interp, "8.5", 0) == NULL) {
		return TCL_ERROR;
	}
	if (Tcl_PkgRequire(interp, "Tcl", "8.5", 0) == NULL) {
		return TCL_ERROR;
	}
	if (Tcl_PkgProvide(interp, "ceptcl", PACKAGE_VERSION) != TCL_OK) {
		return TCL_ERROR;
	}

	Tcl_CreateObjCommand(interp, "cep", Cep_Cep_Cmd,
			(ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
	Tcl_CreateObjCommand(interp, "cepPuts", Cep_CepPuts_Cmd,
			(ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
	Tcl_CreateObjCommand(interp, "cepRead", Cep_CepRead_Cmd,
			(ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
	Tcl_CreateObjCommand(interp, "cepPassChannel", Cep_CepPassChannel_Cmd,
			(ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
	Tcl_CreateObjCommand(interp, "resolve", Cep_Resolve_Cmd,
			(ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Ceptcl_SafeInit --
 *
 *      Initialize the new package.  The string "Cep" in the
 *      function name must match the PACKAGE declaration at the top of
 *      configure.in.
 *
 * Results:
 *      A standard Tcl result
 *
 * Side effects:
 *      The cep package is created.
 *      No commands are added
 *
 *----------------------------------------------------------------------
 */

int
Ceptcl_SafeInit (
	Tcl_Interp *interp
) {
	if (Tcl_InitStubs(interp, "8.5", 0) == NULL) {
		return TCL_ERROR;
	}
	if (Tcl_PkgRequire(interp, "Tcl", "8.5", 0) == NULL) {
		return TCL_ERROR;
	}
	if (Tcl_PkgProvide(interp, "ceptcl", PACKAGE_VERSION) != TCL_OK) {
		return TCL_ERROR;
	}

	return TCL_OK;
}


/* EOF */
