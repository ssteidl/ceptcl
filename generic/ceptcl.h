/*
 * ceptcl.h --
 *
 *	This header file contains the function declarations needed for
 *	all of the source files in this package.
 *
 * Copyright (c) 2003 Stuart Cassoff
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _CEPTCL
#define _CEPTCL

#include <tcl.h>

/*
 * Windows needs to know which symbols to export.  Unix does not.
 * BUILD_cep should be undefined for Unix.
 */

#ifdef BUILD_cep
#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLEXPORT
#endif /* BUILD_cep */


/* Domains */
#define CEP_LOCAL	0
#define CEP_INET	1
#define CEP_INET6	2
#define CEP_DOMAIN_ANY	7

/* Types */
#define CEP_RAW		0
#define CEP_DGRAM	1
#define CEP_STREAM	2
#define CEP_TYPE_ANY	7



/*
 * These bits may be ORed together into the "flags" field of a CepState
 * structure.
 */

/*
 *  1111 110 000 000000
 *  5432 109 876 543210
 *  ---- --- --- ------
 *  uuuu ttt ddd 111111
 *  |||| ||| ||| ||||||- Asynchronous cep
 *  |||| ||| ||| |||||-- Async connect in progress
 *  |||| ||| ||| ||||--- Cep is server
 *  |||| ||| ||| |||---- Read is shut down
 *  |||| ||| ||| ||----- Write is shut down
 *  |||| ||| ||| |------ Resolve names
 *  |||| ||| |||-------- Domain
 *  |||| |||------------ Type
 *  ||||---------------- Undefined
 *
 */

#define CEP_ASYNC          (1 << 0)  /* Asynchronous cep. */
#define CEP_ASYNC_CONNECT  (1 << 1)  /* Async connect in progress. */
#define CEP_SERVER         (1 << 2)  /* 1 == cep is a server */
#define CEP_SHUT_READ      (1 << 3)  /* Shutdown for reading */
#define CEP_SHUT_WRITE     (1 << 4)  /* Shutdown for writing */
#define CEP_RESOLVE_NAMES  (1 << 5)  /* Resolve names */
#define CEP_REUSEADDR      (1 << 12)  /* Reuseadd when opening cep */
#define CEP_REUSEPORT      (1 << 13)  /* Reuseport when opening cep */
#define CEP_CHANRECV       (1 << 14)  /* Receive passed channels */
#define DOMAIN_SHIFT       6
#define DOMAIN_MASK        0x07
#define DOMAIN2MASK(D)     ((D & DOMAIN_MASK) << DOMAIN_SHIFT)
#define MASK2DOMAIN(M)     ((M >> DOMAIN_SHIFT) & DOMAIN_MASK)
#define TYPE_SHIFT         9
#define TYPE_MASK          0x07
#define TYPE2MASK(T)       ((T & TYPE_MASK) << TYPE_SHIFT)
#define MASK2TYPE(M)       ((M >> TYPE_SHIFT) & TYPE_MASK)

/*
#define CEP_IS_STREAM(F)   (MASK2TYPE(F) == CEP_STREAM)
#define CEP_IS_DGRAM(F)    (MASK2TYPE(F) == CEP_DGRAM)
#define CEP_IS_RAW(F)      (MASK2TYPE(F) == CEP_RAW)
#define CEP_IS_DGRAM_OR_RAW(F) ((MASK2TYPE(F) == CEP_DGRAM) || (MASK2TYPE(F) == CEP_RAW))
*/


typedef void (CepAcceptProc) (ClientData callbackData,
				Tcl_Channel chan, const char *addr, int port,
				uid_t euid, gid_t egid, unsigned int flags);


EXTERN unsigned int	Cep_SetFreshCepFlags (int cepDomain, int cepType, int server,
						int resolve, int reuseaddr, int reuseport,
						int async, int chanrecv);

EXTERN Tcl_Channel	Cep_OpenClient (Tcl_Interp * interp,
						const char *protocol,
						const char *host, int port,
						const char *myaddr, int myport,
						CepAcceptProc *acceptProc,
						ClientData callbackData,
						unsigned int flags);

EXTERN Tcl_Channel	Cep_OpenServer (Tcl_Interp * interp,
						const char *protocol,
						const char *myName, int port,
						CepAcceptProc *acceptProc,
						ClientData callbackData,
						unsigned int flags);

EXTERN int		Cep_OpenLocalPair (Tcl_Interp * interp,
						const char *protocol,
						Tcl_Channel *chan1, Tcl_Channel *chan2,
						unsigned int flags);

EXTERN Tcl_Channel	MakeCepClientChannel (ClientData sock, int protocol, unsigned int flags);

EXTERN int		Cep_Sendto (Tcl_Channel chan, const char *host, int port, const unsigned char *data, int dataLen);

EXTERN int		Cep_PassChan (Tcl_Channel channel, Tcl_Channel pchannel);

EXTERN int		Ceptcl_Init (Tcl_Interp * interp);

EXTERN int		Ceptcl_SafeInit (Tcl_Interp * interp);

EXTERN int		Cep_ReportError TCL_VARARGS(Tcl_Interp *,arg1);
#define rerr(msg)	Cep_ReportError(interp,msg,(char*)NULL)
#define rperr(msg)	Cep_ReportError(interp,msg,Tcl_ErrnoMsg(Tcl_GetErrno()),(char*)NULL)

EXTERN int		resolve (Tcl_Interp *interp, const char *host, int cepDomain,
					int objc, Tcl_Obj *const objv[]);

EXTERN int		Cep_GetPassableChannelTypeChar (Tcl_Channel channel);

EXTERN const char *	Cep_PassableChannelTypeCharToName (int typeCh);

EXTERN int		Cep_GetCepType (Tcl_Channel channel);

EXTERN const char *	Cep_StrCepDomain (int cepDomain);

EXTERN const char *	Cep_StrCepType (int cepType);

EXTERN size_t		npstrlen (const char *str, size_t max);

EXTERN char *		ckstrdup (const char *str);

EXTERN int		nagfo (Tcl_Interp *interp, const char *optName);

EXTERN int		cvl (unsigned long val);
EXTERN int		wvl (unsigned char *buf, unsigned long val);
EXTERN int		rvl (const unsigned char *buf, unsigned long *val);


#endif /* _CEPTCL */


/* EOF */
