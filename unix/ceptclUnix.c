/*
 * ceptclUnix.c
 *
 *	Channel driver for various socket types,
 *      or Communications EndPoints (CEPs)
 *
 * This was originally unix/tclUnixChan.c
 *
 *
 * Copyright (c) 1995-1997 Sun Microsystems, Inc.
 * Copyright (c) 1998-1999 by Scriptics Corporation.
 * Copyright (c) 2003-2007 by Stuart Cassoff
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

/* #define USE_SELECT */
#define USE_POLL
/* #define USE_KQUEUE */

/* Most of the include/define stuff was taken from unix/tclUnixPort.h */
/* I'm not sure if I really need all of it */
#include <errno.h>
#include <fcntl.h>

#ifdef HAVE_NET_ERRNO_H
#  include <net/errno.h>
#endif

#include <sys/types.h>

#ifdef USE_KQUEUE
#  include <sys/event.h>
#endif

#if TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif

#ifdef USE_POLL
#  include <poll.h>
#endif

/* for iovec */
#include <sys/uio.h>

#include <unistd.h>
#include <stddef.h>

#ifndef HAVE_GETPEEREID
#   include "../compat/openbsd-compat.h"
#endif

#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <net/if.h>
/* #include <assert.h> */
#include <string.h>
#include <tcl.h>

#include "../generic/ceptcl.h"

#ifdef SO_REUSEPORT
#  define CEP_SYS_REUSEPORT SO_REUSEPORT
#else
#  define CEP_SYS_REUSEPORT SO_REUSEADDR
#endif

/* This little bit is from generic/tclIO.h */
/* I'm not sure that it's needed */
/*
 * Make sure that both EAGAIN and EWOULDBLOCK are defined. This does not
 * compile on systems where neither is defined. We want both defined so
 * that we can test safely for both. In the code we still have to test for
 * both because there may be systems on which both are defined and have
 * different values.
 */

#if ((!defined(EWOULDBLOCK)) && (defined(EAGAIN)))
#  define EWOULDBLOCK EAGAIN
#endif

#if ((!defined(EAGAIN)) && (defined(EWOULDBLOCK)))
#  define EAGAIN EWOULDBLOCK
#endif

#if ((!defined(EAGAIN)) && (!defined(EWOULDBLOCK)))
error one of EWOULDBLOCK or EAGAIN must be defined
#endif


/* The rest here was taken from unix/tclUnixPort.h */
/*
 * The following defines the maximum length of the listen queue. This is
 * the number of outstanding yet-to-be-serviced requests for a connection
 * on a server cep, more than this number of outstanding requests and
 * the connection request will fail.
 * Stu: Removed the check for SOMAXCONN.
 * Error if < 100 - Doing this to check what other systems' defaults
 * are and if this check is needed at all.
 */

/*#ifndef SOMAXCONN
#   define SOMAXCONN	100
#endif*/ /* SOMAXCONN */

#if (SOMAXCONN < 100)
#error SOMAXCONN is too low, inform the author
#   undef  SOMAXCONN
#   define SOMAXCONN	100
#endif /* SOMAXCONN < 100 */

/*
 * The following defines how much buffer space the kernel should maintain
 * for a cep (socket).
 */

#define SOCKET_BUFSIZE	4096


/* This bit is from generic/tclInt.h */
/*
 * Macros used to cast between pointers and integers (e.g. when storing an int
 * in ClientData), on 64-bit architectures they avoid gcc warning about "cast
 * to/from pointer from/to integer of different size".
 */

#if !defined(INT2PTR) && !defined(PTR2INT)
#   if defined(HAVE_INTPTR_T) || defined(intptr_t)
#	define INT2PTR(p) ((void*)(intptr_t)(p))
#	define PTR2INT(p) ((int)(intptr_t)(p))
#   else
#	define INT2PTR(p) ((void*)(p))
#	define PTR2INT(p) ((int)(p))
#   endif
#endif
#if !defined(UINT2PTR) && !defined(PTR2UINT)
#   if defined(HAVE_UINTPTR_T) || defined(uintptr_t)
#	define UINT2PTR(p) ((void*)(uintptr_t)(p))
#	define PTR2UINT(p) ((unsigned int)(uintptr_t)(p))
#   else
#	define UINT2PTR(p) ((void*)(p))
#	define PTR2UINT(p) ((unsigned int)(p))
#   endif
#endif


/* The rest of the code was originally unix/tclUnixChan.c or is new code */

#define CEP_CHANNELNAME_MAX (16 + TCL_INTEGER_SPACE)
#define CEP_HOSTNAME_MAX (NI_MAXHOST + 1)

/*
 * This structure describes per-instance state of a cep based channel.
 */

typedef struct CepState {
	Tcl_Channel channel;		/* Channel associated with this cep */
	int fd;				/* The cep itself */
	unsigned int flags;		/* ORed combination of the bitfields defined below */
	Tcl_TimerToken timer;		/* timer */
	int protocol;           	/* Protocol matching system values */
	Tcl_Obj *names;			/* names */
	Tcl_Obj *packetQueue;		/* q */
	CepAcceptProc *acceptProc;	/* Proc to call on accept */
	ClientData acceptProcData;	/* The data for the accept proc */
} CepState;


/*
 * Static routines for this file:
 */

static CepState *	CreateCep (Tcl_Interp *interp,
					const char *protocol,
					const char *host, int port,
					const char *myaddr, int myport,
					unsigned int flags);

static void		CepAccept (ClientData data, int mask);

static void		CepNotify (ClientData data, int mask);

static void		CepUpdateInterest (CepState *statePtr);

static void		CepTimerProc (ClientData data);

static void		CepDeleteTimer (CepState *statePtr);

static int		CepBlockModeProc (ClientData data, int mode);

static int		CepCloseProc (ClientData instanceData,
					Tcl_Interp *interp, int flags);

static int		CepGetHandleProc (ClientData instanceData,
					int direction, ClientData *handlePtr);

static int		CepGetOptionProc (ClientData instanceData,
					Tcl_Interp *interp, const char *optionName,
					Tcl_DString *dsPtr);

static int		CepSetOptionProc (ClientData instanceData,
					Tcl_Interp *interp, const char *optionName,
					const char *value);

static int		CepInputProc (ClientData instanceData,
					char *buf, int toRead, int *errorCode);

static int		CepOutputProc (ClientData instanceData,
					const char *buf, int toWrite, int *errorCode);

static void		CepWatchProc (ClientData instanceData, int mask);

static int		WaitForConnect (CepState *statePtr, int *errorCodePtr);

static Tcl_Channel	MakeCepClientChannelMode (ClientData sock, int protocol,
					unsigned int flags, int mode);

static int		CreateCepAddress (struct sockaddr_storage *sockaddrPtr,
					const char *host, int port, unsigned int flags);

static int		CepDomainToSysDomain (int cepDomain);
static int		SysDomainToCepDomain (int sysDomain);
static int		CepTypeToSysType (int cepType);
static int		SysTypeToCepType (int sysType);
static socklen_t	GetSocketStructSize (int cepDomain);
static int		NameToAddr (const char *host, void *addrPtr, unsigned int flags);

static int		_TCL_SockMinimumBuffers (int sock, int size);

/* Exported */
EXTERN int		Cep_UnixWaitForFile (int fd, int mask, int timeout);

/* Imported */
EXTERN int		Tcl_ReadObjCmd _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));

/* This structure describes the channel type structure for cep based IO */

static Tcl_ChannelType cepChannelType = {
	(char *) "cep"		, /* Type name */
	TCL_CHANNEL_VERSION_2	, /* v2 channel */
	TCL_CLOSE2PROC		, /* Close proc */
	CepInputProc		, /* Input proc */
	CepOutputProc		, /* Output proc */
	NULL			, /* Seek proc */
	CepSetOptionProc	, /* Set option proc */
	CepGetOptionProc	, /* Get option proc */
	CepWatchProc		, /* Initialize notifier */
	CepGetHandleProc	, /* Get OS handles out of channel */
	CepCloseProc		, /* close2proc */
	CepBlockModeProc	, /* Set blocking or non-blocking mode */
	NULL			, /* flush proc */
	NULL			  /* handler proc */
};


/*
 *----------------------------------------------------------------------
 *
 * _TCL_SockMinimumBuffers --
 *
 * This was TclSockMinimumbuffers from generic/tclIO.c
 *
 *      Ensure minimum buffer sizes (non zero).
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      Sets SO_SNDBUF and SO_RCVBUF sizes.
 *
 *----------------------------------------------------------------------
 */

static int
_TCL_SockMinimumBuffers(
	int sock,	/* Socket file descriptor */
	int size	/* Minimum buffer size */
) {
	int current;
	socklen_t len;

	len = sizeof(int);
	getsockopt(sock, SOL_SOCKET, SO_SNDBUF, (void *) &current, &len);
 	if (current < size) {
		len = sizeof(int);
		setsockopt(sock, SOL_SOCKET, SO_SNDBUF, (const void *) &size, len);
	}
	len = sizeof(int);
	getsockopt(sock, SOL_SOCKET, SO_RCVBUF, (void *) &current, &len);
	if (current < size) {
		len = sizeof(int);
		setsockopt(sock, SOL_SOCKET, SO_RCVBUF, (const void *) &size, len);
	}
	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * UnixWaitForFile --
 *
 *	This procedure waits synchronously for a file to become readable
 *	or writable, with an optional timeout.
 *
 * Results:
 *	The return value is an OR'ed combination of TCL_READABLE,
 *	TCL_WRITABLE, and TCL_EXCEPTION, indicating the conditions
 *	that are present on file at the time of the return.  This
 *	procedure will not return until either "timeout" milliseconds
 *	have elapsed or at least one of the conditions given by mask
 *	has occurred for file (a return value of 0 means that a timeout
 *	occurred).  No normal events will be serviced during the
 *	execution of this procedure.
 *
 * Side effects:
 *	Time passes.
 *
 *----------------------------------------------------------------------
 */

#ifdef USE_KQUEUE
EXTERN int
Cep_UnixWaitForFile (
	int fd,			/* Handle for file on which to wait. */
	int mask,		/* What to wait for: OR'ed combination of
				 * TCL_READABLE, TCL_WRITABLE, and
				 * TCL_EXCEPTION. */
	int timeout		/* Maximum amount of time to wait for one
				 * of the conditions in mask to occur, in
				 * milliseconds.  A value of 0 means don't
				 * wait at all, and a value of -1 means
				 * wait forever. */
) {
	int numFound;
	int result = 0;
	int kq;
	struct kevent kevc;
	struct kevent kev;
	struct timespec ts;
	struct timespec *tsp;


 	/*
	 * Setup the timeout
	 * Create a new kqueue
	 * Setup a kevent and add it to the kqueue
	 */

	if (timeout == -1) {
		tsp = NULL;
	} else {
		if (timeout == 0) {
			ts.tv_nsec = 0;
		} else {
			ts.tv_nsec = timeout * 1000000;
		}
		ts.tv_sec = 0;
		tsp = &ts;
	}

	kq = kqueue();

	EV_SET(&kevc, fd, 0, EV_ADD, 0, 0, NULL);

	if (mask & TCL_READABLE)  { kevc.filter |= EVFILT_READ;  }
	if (mask & TCL_WRITABLE)  { kevc.filter |= EVFILT_WRITE;  }
/*	if (mask & TCL_EXCEPTION) { kevc.filter |= 0;  } */

	kevent(kq, (const struct kevent *) &kevc, 1, &kev, 1, tsp);

	/*
	 * Loop in a mini-event loop of our own, waiting for either the
	 * file to become ready or a timeout to occur.
	 */

	while (1) {
		numFound = kevent(kq, (const struct kevent *) &kevc, 1, &kev, 1, tsp);
		if (numFound == 1) {
			if (kev.filter & EVFILT_READ) {
				result |= TCL_READABLE;
			}
			if (kev.filter & EVFILT_WRITE) {
				result |= TCL_WRITABLE;
			}
			if (kev.flags & EV_EOF) {
				result |= TCL_EXCEPTION;
			}
			result &= mask;
			if (result) {
				break;
			}
		}
		if (timeout == 0) {
			break;
		}
	}
	return result;
}
#endif /* USE_KQUEUE */

#ifdef USE_POLL
EXTERN int
Cep_UnixWaitForFile (
	int fd,			/* Handle for file on which to wait. */
	int mask,		/* What to wait for: OR'ed combination of
				 * TCL_READABLE, TCL_WRITABLE, and
				 * TCL_EXCEPTION. */
	int timeout		/* Maximum amount of time to wait for one
				 * of the conditions in mask to occur, in
				 * milliseconds.  A value of 0 means don't
				 * wait at all, and a value of -1 means
				 * wait forever. */
) {
	int numFound;
	int result = 0;
	struct pollfd pfd;


	/*
	 * Set up the pfd
	 */
	pfd.fd = fd;
	pfd.events = 0;

	if (mask & TCL_READABLE)  { pfd.events |= POLLIN;  }
	if (mask & TCL_WRITABLE)  { pfd.events |= POLLOUT; }
	if (mask & TCL_EXCEPTION) { pfd.events |= POLLERR; }

	/*
	 * Loop in a mini-event loop of our own, waiting for either the
	 * file to become ready or a timeout to occur.
	 */

	while (1) {
		/*
		 * Wait for the event or a timeout.
		 */
		numFound = poll(&pfd, 1, timeout);
		if (numFound == 1) {
			if (pfd.revents & POLLIN) {
				result |= TCL_READABLE;
			}
			if (pfd.revents & POLLOUT) {
				result |= TCL_WRITABLE;
			}
			if (pfd.revents & (POLLERR | POLLHUP | POLLNVAL)) {
				result |= TCL_EXCEPTION;
			}
			result &= mask;
			if (result) {
				break;
			}
		}

		if (timeout == 0) {
			break;
		}
	}
	return result;
}
#endif /* USE_POLL */

#ifdef USE_SELECT
EXTERN int
Cep_UnixWaitForFile (
	int fd,			/* Handle for file on which to wait. */
	int mask,		/* What to wait for: OR'ed combination of
				 * TCL_READABLE, TCL_WRITABLE, and
				 * TCL_EXCEPTION. */
	int timeout		/* Maximum amount of time to wait for one
				 * of the conditions in mask to occur, in
				 * milliseconds.  A value of 0 means don't
				 * wait at all, and a value of -1 means
				 * wait forever. */
) {
	Tcl_Time abortTime;
	Tcl_Time now;
	struct timeval blockTime;
	struct timeval *timeoutPtr;
	int numFound;
	int result = 0;
	fd_set readFds;
	fd_set writeFds;
	fd_set exceptFds;


	/* fd sanity check and fd_set init */
	if (fd >= FD_SETSIZE) {
		Tcl_Panic("WaitForFile can't handle file id %d", fd);
	}
	FD_ZERO(&readFds);
	FD_ZERO(&writeFds);
	FD_ZERO(&exceptFds);

	/*
	 * If there is a non-zero finite timeout, compute the time when
	 * we give up.
	 */

	if (timeout > 0) {
		Tcl_GetTime(&now);
		abortTime.sec = now.sec + timeout/1000;
		abortTime.usec = now.usec + (timeout%1000)*1000;
		if (abortTime.usec >= 1000000) {
			abortTime.usec -= 1000000;
			abortTime.sec += 1;
		}
		timeoutPtr = &blockTime;
	} else if (timeout == 0) {
		timeoutPtr = &blockTime;
		blockTime.tv_sec = 0;
		blockTime.tv_usec = 0;
	} else {
		timeoutPtr = NULL;
	}

	/*
	 * Loop in a mini-event loop of our own, waiting for either the
	 * file to become ready or a timeout to occur.
	 */

	while (1) {
		if (timeout > 0) {
			blockTime.tv_sec = abortTime.sec - now.sec;
			blockTime.tv_usec = abortTime.usec - now.usec;
			if (blockTime.tv_usec < 0) {
				blockTime.tv_sec -= 1;
				blockTime.tv_usec += 1000000;
			}
			if (blockTime.tv_sec < 0) {
				blockTime.tv_sec = 0;
				blockTime.tv_usec = 0;
			}
		}

		/*
		 * Prepare fd_sets
		 */
		if (mask & TCL_READABLE)  { FD_SET(fd, &readFds);   }
		if (mask & TCL_WRITABLE)  { FD_SET(fd, &writeFds);  }
		if (mask & TCL_EXCEPTION) { FD_SET(fd, &exceptFds); }

		/*
		 * Wait for the event or a timeout.
		 */

		numFound = select(fd + 1, &readFds, &writeFds, &exceptFds, timeoutPtr);
		if (numFound == 1) {
			if (FD_ISSET(fd, &readFds))   { result |= TCL_READABLE;  }
			if (FD_ISSET(fd, &writeFds))  { result |= TCL_WRITABLE;  }
			if (FD_ISSET(fd, &exceptFds)) { result |= TCL_EXCEPTION; }
			result &= mask;
			if (result) {
				break;
			}
		}

		if (timeout == 0) {
			break;
		}

		/*
		 * The select returned early, so we need to recompute the timeout.
		 */

		Tcl_GetTime(&now);
		if ((abortTime.sec < now.sec)
				|| ((abortTime.sec == now.sec)
				&& (abortTime.usec <= now.usec))) {
			break;
		}
	}
	return result;
}
#endif /* USE_SELECT */

/*
 *----------------------------------------------------------------------
 *
 * WaitForConnect --
 *
 *	Waits for a connection on an asynchronously opened cep to
 *	be completed.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The cep is connected after this function returns.
 *
 *----------------------------------------------------------------------
 */

static int
WaitForConnect (
	CepState *statePtr,	/* State of the cep. */
	int *errorCodePtr	/* Where to store errors? */
) {
	int timeout;		/* How long to wait. */
	int state;			/* Of calling WaitForFile. */
	int flags;			/* fcntl flags for the cep. */

	/*
	 * If an asynchronous connect is in progress, attempt to wait for it
	 * to complete before reading.
	 */

	if (statePtr->flags & CEP_ASYNC_CONNECT) {
		if (statePtr->flags & CEP_ASYNC) {
			timeout = 0;
		} else {
			timeout = -1;
		}
		Tcl_SetErrno(0);
		state = Cep_UnixWaitForFile(statePtr->fd, (TCL_WRITABLE | TCL_EXCEPTION), timeout);
		if (!(statePtr->flags & CEP_ASYNC)) {
			flags = fcntl(statePtr->fd, F_GETFL);
			flags &= (~(O_NONBLOCK));
			(void) fcntl(statePtr->fd, F_SETFL, flags);
		}
		if (state & TCL_EXCEPTION) {
			return -1;
		}
		if (state & TCL_WRITABLE) {
			statePtr->flags &= (~(CEP_ASYNC_CONNECT));
		} else if (timeout == 0) {
			Tcl_SetErrno(EWOULDBLOCK);
			*errorCodePtr = EWOULDBLOCK;
			return -1;
		}
	}
	return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * CepBlockModeProc --
 *
 *	This procedure is invoked by the generic IO level to set blocking
 *	and nonblocking mode on a cep based channel.
 *
 * Results:
 *	0 if successful, errno when failed.
 *
 * Side effects:
 *	Sets the device into blocking or nonblocking mode.
 *
 *----------------------------------------------------------------------
 */

static int
CepBlockModeProc (
	ClientData instanceData,	/* Cep state. */
	int mode			/* The mode to set. Can be one of
					 * TCL_MODE_BLOCKING or
					 * TCL_MODE_NONBLOCKING. */
) {
	CepState *statePtr = (CepState *) instanceData;
	int setting;

	setting = fcntl(statePtr->fd, F_GETFL);
	if (mode == TCL_MODE_BLOCKING) {
		statePtr->flags &= (~(CEP_ASYNC));
		setting &= (~(O_NONBLOCK));
	} else {
		statePtr->flags |= CEP_ASYNC;
		setting |= O_NONBLOCK;
	}
	if (fcntl(statePtr->fd, F_SETFL, setting) < 0) {
		return Tcl_GetErrno();
	}
	return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * CepInputProc --
 *
 *	This procedure is invoked by the generic IO level to read input
 *	from a cep based channel.
 *
 * Results:
 *	The number of bytes read is returned or -1 on error. An output
 *	argument contains the POSIX error code on error, or zero if no
 *	error occurred.
 *
 * Side effects:
 *	Reads input from the input device of the channel.
 *
 *----------------------------------------------------------------------
 */

static int
CepInputProc (
	ClientData instanceData,	/* Cep state. */
	char *buf,			/* Where to store data read. */
	int bufSize,			/* How much space is available
					 * in the buffer? */
	int *errorCodePtr		/* Where to store error code. */
) {
	CepState *statePtr = (CepState *) instanceData;
	int bytesRead;
	int len;

	*errorCodePtr = 0;

	if (((MASK2TYPE(statePtr->flags) == CEP_DGRAM) || (MASK2TYPE(statePtr->flags) == CEP_RAW)) &&
			(Tcl_ListObjLength(NULL,statePtr->packetQueue, &len) == TCL_OK) && (len > 0)) {
		Tcl_Obj *ba;
		unsigned char *b;
		int bl;
		if (Tcl_ListObjIndex(NULL, statePtr->packetQueue, 0, &ba) == TCL_ERROR) {
		}
		b = Tcl_GetByteArrayFromObj(ba, &bl);
		if (bl > bufSize) {
			bl = bufSize;
		}
		memcpy((void *) buf, (const void *) b, (size_t) bl);
		Tcl_ListObjReplace(NULL, statePtr->packetQueue, 0, 1, 0, NULL);
		return bl;

	}

	if (WaitForConnect(statePtr, errorCodePtr) != 0) {
		return -1;
	}

	bytesRead = recvfrom(statePtr->fd, buf, (size_t) bufSize, 0, NULL, 0);

	if (bytesRead > -1) {
		return bytesRead;
	}

	if (Tcl_GetErrno() == ECONNRESET) {
		/*
		 * Turn ECONNRESET into a soft EOF condition.
		 */

		return 0;
	}

	*errorCodePtr = Tcl_GetErrno();

	return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * CepWatchProc --
 *
 *	Initialize the notifier to watch the fd from this channel.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Sets up the notifier so that a future event on the channel will
 *	be seen by Tcl.
 *
 *----------------------------------------------------------------------
 */

static void
CepWatchProc (
	ClientData instanceData,	/* The cep state. */
	int mask			/* Events of interest; an OR-ed
					 * combination of TCL_READABLE,
					 * TCL_WRITABLE and TCL_EXCEPTION. */
) {
	CepState *statePtr = (CepState *) instanceData;

	/*
	 * Make sure we don't mess with server ceps since they will never
	 * be readable or writable at the Tcl level.  This keeps Tcl scripts
	 * from interfering with the -accept behavior.
	 */

	if (!(statePtr->flags & CEP_SERVER)) {
		if (mask != 0) {
			if (	(MASK2TYPE(statePtr->flags) == CEP_DGRAM) ||
				(MASK2TYPE(statePtr->flags) == CEP_RAW)   ||
				(statePtr->flags & CEP_CHANRECV)
			) {
				Tcl_CreateFileHandler(statePtr->fd, mask,
					CepNotify,
					(ClientData) statePtr);
			} else {
				Tcl_CreateFileHandler(statePtr->fd, mask,
					(Tcl_FileProc *) Tcl_NotifyChannel,
					(ClientData) statePtr->channel);
			}
		} else {
			Tcl_DeleteFileHandler(statePtr->fd);
			CepDeleteTimer(statePtr);
		}
	}
}

/*
 *----------------------------------------------------------------------
 *
 * CepGetHandleProc --
 *
 *	Called from Tcl_GetChannelHandle to retrieve OS handles from inside
 *	a cep based channel.
 *
 * Results:
 *	Returns TCL_OK with the fd in handlePtr, or TCL_ERROR if
 *	there is no handle for the specified direction. 
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
CepGetHandleProc (
	ClientData instanceData,	/* The cep state. */
	int direction,			/* Not used. */
	ClientData *handlePtr		/* Where to store the handle.  */
) {
	*handlePtr = (ClientData) INT2PTR(((CepState *) instanceData)->fd);
	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CepOutputProc --
 *
 *	This procedure is invoked by the generic IO level to write output
 *	to a cep based channel.
 *
 * Results:
 *	The number of bytes written is returned. An output argument is
 *	set to a POSIX error code if an error occurred, or zero.
 *
 * Side effects:
 *	Writes output on the output device of the channel.
 *
 *----------------------------------------------------------------------
 */

static int
CepOutputProc (
	ClientData instanceData,	/* Cep state. */
	const char *buf,		/* The data buffer. */
	int toWrite,			/* How many bytes to write? */
	int *errorCodePtr		/* Where to store error code. */
) {
	CepState *statePtr = (CepState *) instanceData;
	int written;

	*errorCodePtr = 0;

	if (WaitForConnect(statePtr, errorCodePtr) != 0) {
		return -1;
	}

	written = sendto(statePtr->fd, buf, (size_t) toWrite, 0, NULL, 0);
/*	written = write(statePtr->fd, buf, (size_t) toWrite);*/
	if (written > -1) {
		return written;
	}

	*errorCodePtr = Tcl_GetErrno();
	return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * CepCloseProc --
 *
 *	This procedure is invoked by the generic IO level to perform
 *	channel-type-specific cleanup when a cep based channel
 *	is closed.
 *
 * Results:
 *	0 if successful, the value of errno if failed.
 *
 * Side effects:
 *	Closes the cep of the channel.
 *
 *----------------------------------------------------------------------
 */

static int
CepCloseProc (
	ClientData instanceData,	/* The cep to close. */
	Tcl_Interp *interp,		/* For error reporting - unused. */
	int flags
) {
	CepState *statePtr = (CepState *) instanceData;
	int errorCode = 0;

	/*
	 * Delete a file handler that may be active for this cep if this
	 * is a server cep - the file handler was created automatically
	 * by Ceptcl as part of the mechanism to accept new client connections.
	 * Channel handlers are already deleted in the generic IO channel
	 * closing code that called this function, so we do not have to
	 * delete them here.
	 */

	if (flags == 0) {
		Tcl_DeleteFileHandler(statePtr->fd);
		CepDeleteTimer(statePtr);
		if ((MASK2DOMAIN(statePtr->flags) == CEP_LOCAL) && (statePtr->names != NULL)) {
			if (statePtr->flags & CEP_SERVER) {
				Tcl_Obj *srv_path;
				Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 2, &srv_path);
				unlink(Tcl_GetByteArrayFromObj(srv_path, (int *) NULL));
			}
			Tcl_DecrRefCount(statePtr->names);
		}
		if (close(statePtr->fd) < 0) {
			errorCode = Tcl_GetErrno();
		}
		if (statePtr->packetQueue != NULL) {
			Tcl_DecrRefCount(statePtr->packetQueue);
			statePtr->packetQueue = NULL;
		}
/*		statePtr->flags |= (CEP_SHUT_READ | CEP_SHUT_WRITE);*/
		Tcl_EventuallyFree((ClientData) statePtr, TCL_DYNAMIC);
/*		ckfree((char *) statePtr);*/
	}
	return errorCode;
}

/*
 *----------------------------------------------------------------------
 *
 * CepSetOptionProc --
 *
 *      Sets an option on a cep.
 *
 * Results:
 *      A standard Tcl result. Also sets the interp's result on error if
 *      interp is not NULL.
 *
 * Side effects:
 *      May modify an option on a device.
 *      Sets Error message if needed (by calling Tcl_BadChannelOption).
 *
 *----------------------------------------------------------------------
 */

static int              
CepSetOptionProc (
	ClientData instanceData,	/* Cep state. */
	Tcl_Interp *interp,		/* For error reporting - can be NULL. */
	const char *optionName,		/* Which option to set? */
	const char *value		/* New value for option. */
) {
	CepState *statePtr = (CepState *) instanceData;
	size_t len;
	int optionInt;
	socklen_t socklen;
	int cepDomain = MASK2DOMAIN(statePtr->flags);
	int cepType = MASK2TYPE(statePtr->flags);
/*	int resolve = (statePtr->flags & CEP_RESOLVE_NAMES);*/

	len = strlen(optionName);

	/*
	 * Option -broadcast boolean
	 */
	if ((len > 1) && (optionName[1] == 'b') &&
			(strncmp(optionName, "-broadcast", len) == 0)) {
		if (Tcl_GetBoolean(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		socklen = sizeof(optionInt);
		if (setsockopt(statePtr->fd, SOL_SOCKET, SO_BROADCAST, (const void *) &optionInt, socklen) < 0) {
			return rperr("can't set broadcast: ");
		}
		return TCL_OK;
	}

	/*
	 * Option -hops n
	 */
	if ((len > 1) && (optionName[1] == 'h') &&
			(strncmp(optionName, "-hops", len) == 0)) {
		int  ret;
		if (Tcl_GetInt(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		if ((cepDomain == CEP_INET) && ((optionInt < 0) || (optionInt > 255))) {
			Tcl_SetErrno(EINVAL);
			return rperr("can't set hops: ");
		}
		socklen = sizeof(optionInt);
		if (cepDomain == CEP_INET6) {
			ret = setsockopt(statePtr->fd, IPPROTO_IPV6, IPV6_UNICAST_HOPS, (const void *) &optionInt, socklen);
		} else {
			ret = setsockopt(statePtr->fd, IPPROTO_IP, IP_TTL, (const void *) &optionInt, socklen);
		}
		if (ret < 0) {
			return rperr("can't set hops: ");
		}
		return TCL_OK;
	}

	/*
	 * Option -shutdown
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 's') &&
			(strncmp(optionName, "-shutdown", len) == 0))) {
		int mask = 0;
		int how;
		int argc;
		const char **argv;
		if (Tcl_SplitList(interp, value, &argc, &argv) == TCL_ERROR) {
			return TCL_ERROR;
		}
		if (argc == 0) {
			ckfree((char *) argv);
			return TCL_OK;
		}
		if (argc > 2) {
			ckfree((char *) argv);
			return rperr("should be read write {read write} {write read} or {}");
		}
		for (argc--; argc >= 0; argc--) {
			if (strcmp(argv[argc], "read") == 0) {
				mask |= CEP_SHUT_READ;
			} else if (strcmp(argv[argc], "write") == 0) {
				mask |= CEP_SHUT_WRITE;
			} else {
				ckfree((char *) argv);
				return rperr("should be read write {read write} {write read} or {}");
			}
		}
		ckfree((char *) argv);

		if ((mask & CEP_SHUT_READ) && (mask & CEP_SHUT_WRITE)) {
			how = SHUT_RDWR;
		} else if (mask & CEP_SHUT_READ) {
			how = SHUT_RD;
		} else {
			how = SHUT_WR;
		}
		if (shutdown(statePtr->fd, how) != 0) {
			return rperr("can't shutdown: ");
		}
		statePtr->flags |= mask;
		return TCL_OK;
	}

	/*
	 * Option -join and -leave
	 */
	if ((len > 1) && ((optionName[1] == 'j') || (optionName[1] == 'l')) &&
			((strncmp(optionName, "-join", len) == 0) || (strncmp(optionName, "-leave", len) == 0))) {
		int op;
		const char *errMsg;
		int ret;
		int argc;
		const char **argv;

		if (Tcl_SplitList(interp, value, &argc, &argv) == TCL_ERROR) {
			return TCL_ERROR;
		}
		if (argc == 0) {
			ckfree((char *) argv);
			return TCL_OK;
		}
		if (strncmp(optionName, "-join", len) == 0) {
			op = 1;
			errMsg = "can't join group: ";
		} else {
			op =  0;
			errMsg = "can't leave group: ";
		}
		if (argc > 2) {
			ckfree((char *) argv);
			return rperr("should be addr, addr interface or {}");
		}
		if (cepDomain == CEP_INET6) {
			struct ipv6_mreq mreq6;

			if (!NameToAddr(argv[0], &mreq6.ipv6mr_multiaddr, statePtr->flags)) {
				ckfree((char *) argv);
				return rperr(errMsg);
			}
			if (argc > 1) {
				if ((mreq6.ipv6mr_interface = if_nametoindex(argv[1])) == 0) {
					ckfree((char *) argv);
					return rperr(errMsg);
				}
			} else {
				mreq6.ipv6mr_interface = 0;
			}
			ret = setsockopt(statePtr->fd, IPPROTO_IPV6, (op ? IPV6_JOIN_GROUP : IPV6_LEAVE_GROUP), (const void *) &mreq6, sizeof(mreq6));
		} else {
			struct ip_mreq mreq;
			if (!NameToAddr(argv[0], &mreq.imr_multiaddr, statePtr->flags)) {
				ckfree((char *) argv);
				return rperr(errMsg);
			}
			if (argc > 1) {
				if (!NameToAddr(argv[1], &mreq.imr_interface, statePtr->flags)) {
					ckfree((char *) argv);
					return rperr(errMsg);
				}
			} else {
				mreq.imr_interface.s_addr = htonl(INADDR_ANY);
			}
			ret = setsockopt(statePtr->fd, IPPROTO_IP, op ? IP_ADD_MEMBERSHIP : IP_DROP_MEMBERSHIP, (const void *) &mreq, sizeof(mreq));
		}
		ckfree((char *) argv);
		if (ret < 0) {
			return rperr(errMsg);
		}
		return TCL_OK;
	}

	/*
	 * Option -loop boolean
	 */
	if ((len > 1) && (optionName[1] == 'l') &&
			(strncmp(optionName, "-loop", len) == 0)) {
		unsigned char optionUChar;

		if (Tcl_GetBoolean(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		optionUChar = optionInt;
		socklen = sizeof(optionUChar);
		if (setsockopt(statePtr->fd,
				((cepDomain == CEP_INET6) ? IPPROTO_IPV6        : IPPROTO_IP),
				((cepDomain == CEP_INET6) ? IPV6_MULTICAST_LOOP : IP_MULTICAST_LOOP),
				(const void *) &optionUChar, socklen) < 0) {
			return rperr("can't set loop: ");
		}
		return TCL_OK;
	}

	/*
	 * Option -mhops n
	 */
	if ((len > 1) && (optionName[1] == 'm') &&
			(strncmp(optionName, "-mhops", len) == 0)) {
		if (Tcl_GetInt(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		socklen = sizeof(unsigned char);
		if (setsockopt(statePtr->fd,
				((cepDomain == CEP_INET6) ? IPPROTO_IPV6        : IPPROTO_IP),
				((cepDomain == CEP_INET6) ? IPV6_MULTICAST_HOPS : IP_MULTICAST_TTL),
				(const void *) &optionInt, socklen) < 0) {
			return rperr("can't set mhops: ");
		}
		return TCL_OK;
	}

	/*
	 * Option -maddr
	 */
	if ((len > 1) && (optionName[1] == 'm') &&
			(strncmp(optionName, "-maddr", len) == 0)) {
		if (cepDomain == CEP_INET6) {
			struct in6_addr addr;
			socklen = sizeof(addr);
			if (!NameToAddr(value, &addr, statePtr->flags)) {
				return rperr("can't set maddr: ");
			}
			if (setsockopt(statePtr->fd, IPPROTO_IPV6, IPV6_MULTICAST_IF, (const void *) &addr, socklen) < 0) {
				return rperr("can't set maddr: ");
			}
		} else {
			struct in_addr addr;
			socklen = sizeof(addr);
			if (!NameToAddr(value, &addr, statePtr->flags)) {
				return rperr("can't set maddr: ");
			}
			if (setsockopt(statePtr->fd, IPPROTO_IP, IP_MULTICAST_IF, (const void *) &addr, socklen) < 0) {
				return rperr("can't set maddr: ");
			}
		}
		return TCL_OK;
	}

	/*
	 * Option -resolve boolean
	 */
	if ((len > 1) && (optionName[1] == 'r') &&
			(strncmp(optionName, "-resolve", len) == 0)) {
		if (Tcl_GetBoolean(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		if (optionInt) {
			statePtr->flags |= CEP_RESOLVE_NAMES;
		} else {
			statePtr->flags &= (~(CEP_RESOLVE_NAMES));
		}
		return TCL_OK;
	}

	/*
	 * Option -header
	 */
	if ((len > 1) && (optionName[1] == 'h') &&
			(strncmp(optionName, "-header", len) == 0)) {
		if ((cepType == CEP_RAW) && (cepDomain == CEP_INET)) {
			if (Tcl_GetBoolean(interp, value, &optionInt) != TCL_OK) {
				return TCL_ERROR;
			}
			socklen = sizeof(optionInt);
			if (setsockopt(statePtr->fd, IPPROTO_IP, IP_HDRINCL, (const void *) &optionInt, socklen) != 0) {
				return rperr("can't set header: ");
			}
		}
		return TCL_OK;
	}

	/*
	 * Option -route
	 */
	if ((len > 1) && (optionName[1] == 'r') &&
			(strncmp(optionName, "-route", len) == 0)) {
		if (Tcl_GetBoolean(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		/* DONTROUTE - so it's the opposite */
		optionInt = !optionInt;
		socklen = sizeof(optionInt);
		if (setsockopt(statePtr->fd, SOL_SOCKET, SO_DONTROUTE, (const void *) &optionInt, socklen) != 0) {
			return rperr("can't set route: ");
		}
		return TCL_OK;
	}

	/*
	 * Option -closeonexec
	 */
	if ((len > 1) && (optionName[1] == 'c') &&
			(strncmp(optionName, "-closeonexec", len) == 0)) {
		if (Tcl_GetBoolean(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		if (fcntl(statePtr->fd, F_SETFD, optionInt ? FD_CLOEXEC : 0) == -1) {
			return rperr("can't set closeonexec: ");
		}
		return TCL_OK;
	}

	/*
	 * Option -sendtimeout
	 */
	/*
	if ((len > 1) && (optionName[1] == 'r') &&
			(strncmp(optionName, "-sendtimeout", len) == 0)) {
		struct timeval t;
		if (Tcl_GetInt(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		t.tv_sec = optionInt;
		t.tv_usec = 0;
		socklen = sizeof(t);
		if (setsockopt(statePtr->fd, SOL_SOCKET, SO_SNDTIMEO, (const void *) &t, socklen) != 0) {
			return rperr("can't set sendtimeout: ");
		}
		return TCL_OK;
	}
	*/

	/*
	 * Option -receivetimeout
	 */
	/*
	if ((len > 1) && (optionName[1] == 'r') &&
			(strncmp(optionName, "-receivetimeout", len) == 0)) {
		struct timeval t;
		if (Tcl_GetInt(interp, value, &optionInt) != TCL_OK) {
			return TCL_ERROR;
		}
		t.tv_sec = optionInt;
		t.tv_usec = 0;
		socklen = sizeof(t);
		if (setsockopt(statePtr->fd, SOL_SOCKET, SO_RCVTIMEO, (const void *) &t, socklen) != 0) {
			return rperr("can't set receivetimeout: ");
		}
		return TCL_OK;
	}
	*/

	/*
	 * Option -peername
	 */
	if ((len > 1) && (optionName[1] == 'p') &&
			(strncmp(optionName, "-peername", len) == 0)) {
		struct sockaddr_storage sockaddr;
		int argc;
		const char **argv;

		if (Tcl_SplitList(interp, value, &argc, &argv) == TCL_ERROR) {
			return TCL_ERROR;
		}
		if (argc != 2) {
			ckfree((char *) argv);
			return rperr("should be addr port or \"{} -1\" to disassociate");
		}
		if (Tcl_GetInt(interp, argv[1], &optionInt) != TCL_OK) {
			ckfree((char *) argv);
			return TCL_ERROR;
		}
		if ((optionInt == -1) && (strlen(argv[0]) == 0)) {
			(void) memset((void *) &sockaddr, 0, sizeof(sockaddr));
			if (cepDomain == CEP_INET6) {
				((struct sockaddr_in6 *) &sockaddr)->sin6_family = AF_UNSPEC;
				/*((struct sockaddr_in6 *) &sockaddr)->sin6_len = GetSocketStructSize(cepDomain);*/
			} else {
				((struct sockaddr_in *) &sockaddr)->sin_family = AF_UNSPEC;
				/*((struct sockaddr_in *) &sockaddr)->sin_len = GetSocketStructSize(cepDomain);*/
			}
		} else {
			if (CreateCepAddress(&sockaddr, argv[0], optionInt, statePtr->flags) == -1) {
				ckfree((char *) argv);
				return rperr("can't set peername: ");
			}
		}
		if (cepDomain == CEP_LOCAL) {
			struct sockaddr_un *slp = (struct sockaddr_un *) &sockaddr;
			socklen = sizeof(*slp) - sizeof(slp->sun_path) + npstrlen(slp->sun_path, sizeof(slp->sun_path));
		} else {
			socklen = GetSocketStructSize(cepDomain);
		}
		if ((connect(statePtr->fd, (struct sockaddr *) &sockaddr, socklen) < 0) && (Tcl_GetErrno() != EAFNOSUPPORT)) {
			ckfree((char *) argv);
			return rperr("can't set peername: ");
		}
		ckfree((char *) argv);
		return TCL_OK;
	}

	return Tcl_BadChannelOption(interp, optionName, "broadcast closeonexec header hops join leave loop maddr mhops peername resolve route shutdown");
}

/*
 *----------------------------------------------------------------------
 *
 * CepGetOptionProc --
 *
 *	Computes an option value for a CEP based channel, or a
 *	list of all options and their values.
 *
 *	Note: This code is based on code contributed by John Haxby.
 *
 * Results:
 *	A standard Tcl result. The value of the specified option or a
 *	list of all options and their values is returned in the
 *	supplied DString. Sets Error message if needed.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
CepGetOptionProc (
	ClientData instanceData,	/* Cep state. */
	Tcl_Interp *interp,		/* For error reporting - can be NULL. */
	const char *optionName,		/* Name of the option to
					 * retrieve the value for, or
					 * NULL to get all options and
					 * their values. */
	Tcl_DString *dsPtr		/* Where to store the computed
					 * value; initialized by caller. */
) {
	CepState *statePtr = (CepState *) instanceData;
	struct sockaddr_storage sockaddr;
	struct sockaddr     *sap = (struct sockaddr     *) &sockaddr;
	struct sockaddr_in6 *s6p = (struct sockaddr_in6 *) &sockaddr;
	struct sockaddr_in  *s4p = (struct sockaddr_in  *) &sockaddr;
/*	struct sockaddr_un  *slp = (struct sockaddr_un  *) &sockaddr;*/
	socklen_t socklen;
	size_t len = 0;
	char optionVal[TCL_INTEGER_SPACE];
	int optionInt;
	char addrBuf[CEP_HOSTNAME_MAX];
	int port;
	int cepDomain = MASK2DOMAIN(statePtr->flags);
	int cepType = MASK2TYPE(statePtr->flags);
	int resolve = (statePtr->flags & CEP_RESOLVE_NAMES);

	if (optionName != (char *) NULL) {
		len = strlen(optionName);
	}

	/*
	 * Option -error
	 */
	if ((len > 1) && (optionName[1] == 'e') &&
			(strncmp(optionName, "-error", len) == 0)) {
		int err;
		int ret;

		socklen = sizeof(int);
		ret = getsockopt(statePtr->fd, SOL_SOCKET, SO_ERROR, (void *) &err, &socklen);
		if (ret != 0) {
			err = Tcl_GetErrno();
		}
		if (err != 0) {
			Tcl_DStringAppend(dsPtr, Tcl_ErrnoMsg(err), -1);
		}
		return TCL_OK;
	}

	/*
	 * Option -peername
	 */
	 if ((len == 0) ||
			((len > 1) && (optionName[1] == 'p') &&
			(strncmp(optionName, "-peername", len) == 0))) {

		socklen = sizeof(sockaddr);
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-peername");
			Tcl_DStringStartSublist(dsPtr);
		}
		if (cepDomain == CEP_LOCAL) {
			Tcl_Obj *cep_path;
			uid_t euid;
			gid_t egid; 

			if (statePtr->names != (Tcl_Obj *) NULL) {
				Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 0, &cep_path);
				Tcl_DStringAppendElement(dsPtr, Tcl_GetString(cep_path));
				Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 1, &cep_path);
				Tcl_DStringAppendElement(dsPtr, Tcl_GetString(cep_path));
			} else {
				addrBuf[0] = '-';
				addrBuf[1] = '\0';
				Tcl_DStringAppendElement(dsPtr, addrBuf);
				Tcl_DStringAppendElement(dsPtr, addrBuf);
			}
			Tcl_DStringStartSublist(dsPtr);
			if (getpeereid(statePtr->fd, &euid, &egid) == 0) {
				(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%u", euid);
				Tcl_DStringAppendElement(dsPtr, optionVal);
				(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%u", egid);
				Tcl_DStringAppendElement(dsPtr, optionVal);
			} else {
				Tcl_DStringAppendElement(dsPtr, "-1");
				Tcl_DStringAppendElement(dsPtr, "-1");
			}
			Tcl_DStringEndSublist(dsPtr);
		} else if (getpeername(statePtr->fd, sap, &socklen) == 0) {
			if (SysDomainToCepDomain(sap->sa_family) != cepDomain) {
				Tcl_Panic("ceptcl: sa_family != cepDomain", "huh? (1)");
			}
			if (getnameinfo(sap, socklen, addrBuf, sizeof(addrBuf), NULL, 0, NI_NUMERICHOST) != 0) {
				addrBuf[0] = '?';
				addrBuf[1] = '\0';
			}
			Tcl_DStringAppendElement(dsPtr, addrBuf);
			if (getnameinfo(sap, socklen, addrBuf, sizeof(addrBuf), NULL, 0, (resolve ? 0 : NI_NUMERICHOST)) != 0) {
				addrBuf[0] = '?';
				addrBuf[1] = '\0';
			} else if (resolve) {
				Tcl_DString ds;
				Tcl_DStringInit(&ds);
				Tcl_ExternalToUtfDString(NULL, addrBuf, -1, &ds);
				Tcl_DStringAppendElement(dsPtr, (const char *) Tcl_DStringValue(&ds));
				Tcl_DStringFree(&ds);
			} else {
				Tcl_DStringAppendElement(dsPtr, addrBuf);
			}
			if (cepDomain == CEP_INET6) {
				port = ntohs((unsigned short) s6p->sin6_port);
			} else {
				port = ntohs((unsigned short) s4p->sin_port);
			}
			(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%d", port);
			Tcl_DStringAppendElement(dsPtr, optionVal);
		} else if ((len == 0) || ((Tcl_GetErrno() == ENOTCONN) && ((cepType == CEP_DGRAM) || (cepType == CEP_RAW)))) {
			/*
			 * getpeername failed - but if we were asked for all the options
			 * (len==0) or the cep is an unconnected datagram or raw cep,
			 * don't flag an error at that point because it could
			 * be an fconfigure request on a server socket. (which have
			 * no peer). same must be done on win&mac.
			 */
			Tcl_DStringAppendElement(dsPtr, "");
			Tcl_DStringAppendElement(dsPtr, "");
			Tcl_DStringAppendElement(dsPtr, "-1");
		} else {
			return rperr("can't get peername: ");
		}
		if (len == 0) {
			Tcl_DStringEndSublist(dsPtr);
		} else {
			return TCL_OK;
		}
	}

	/*
	 * Option -sockname
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 's') &&
			(strncmp(optionName, "-sockname", len) == 0))) {
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-sockname");
			Tcl_DStringStartSublist(dsPtr);
		}
		if (cepDomain == CEP_LOCAL) {
			Tcl_Obj *cep_path;
			uid_t euid;
			gid_t egid; 

			if (statePtr->names != (Tcl_Obj *) NULL) {
				Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 0, &cep_path);
				Tcl_DStringAppendElement(dsPtr, Tcl_GetString(cep_path));
				Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 1, &cep_path);
				Tcl_DStringAppendElement(dsPtr, Tcl_GetString(cep_path));
			} else {
				addrBuf[0] = '-';
				addrBuf[1] = '\0';
				Tcl_DStringAppendElement(dsPtr, addrBuf);
				Tcl_DStringAppendElement(dsPtr, addrBuf);
			}
			Tcl_DStringStartSublist(dsPtr);
			if (getpeereid(statePtr->fd, &euid, &egid) == 0) {
				(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%u", euid);
				Tcl_DStringAppendElement(dsPtr, optionVal);
				(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%u", egid);
				Tcl_DStringAppendElement(dsPtr, optionVal);
			} else {
				Tcl_DStringAppendElement(dsPtr, "-1");
				Tcl_DStringAppendElement(dsPtr, "-1");
			}
			Tcl_DStringEndSublist(dsPtr);
		} else {
			socklen = sizeof(sockaddr);
			if (getsockname(statePtr->fd, sap, &socklen) != 0) {
				return rperr("can't get sockname: ");
			}
			if (SysDomainToCepDomain(sap->sa_family) != cepDomain) {
				Tcl_Panic("ceptcl: sa_family != cepDomain", "huh? (2)");
			}
			if (getnameinfo(sap, socklen, addrBuf, sizeof(addrBuf), NULL, 0, NI_NUMERICHOST) != 0) {
				addrBuf[0] = '?';
				addrBuf[1] = '\0';
			}
			Tcl_DStringAppendElement(dsPtr, addrBuf);
			if (getnameinfo(sap, socklen, addrBuf, sizeof(addrBuf), NULL, 0, (resolve ? 0 : NI_NUMERICHOST)) != 0) {
				addrBuf[0] = '?';
				addrBuf[1] = '\0';
			} else if (resolve) {
				Tcl_DString ds;
				Tcl_DStringInit(&ds);
				Tcl_ExternalToUtfDString(NULL, addrBuf, -1, &ds);
				Tcl_DStringAppendElement(dsPtr, (const char *) Tcl_DStringValue(&ds));
				Tcl_DStringFree(&ds);
			} else {
				Tcl_DStringAppendElement(dsPtr, addrBuf);
			}
			if (cepDomain == CEP_INET6) {
				port = ntohs((unsigned short) s6p->sin6_port);
			} else {
				port = ntohs((unsigned short) s4p->sin_port);
			}
			(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%d", port);
			Tcl_DStringAppendElement(dsPtr, optionVal);
		}
		if (len == 0) {
			Tcl_DStringEndSublist(dsPtr);
		} else {
			return TCL_OK;
		}
	}


	/*
	 * Option -hops
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'h') &&
			(strncmp(optionName, "-hops", len) == 0))) {
		optionInt = 0;
		if ((cepDomain == CEP_INET6) || (cepDomain == CEP_INET)) {
			int ret = 0;
			socklen = sizeof(optionInt);
			if (cepDomain == CEP_INET6) {
				ret = getsockopt(statePtr->fd, IPPROTO_IPV6, IPV6_UNICAST_HOPS, (void *) &optionInt, &socklen);
			} else {
				ret = getsockopt(statePtr->fd, IPPROTO_IP, IP_TTL, (void *) &optionInt, &socklen);
			}
			if (ret != 0) {
				return rperr("can't get hops: ");
			}
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-hops");
		}
		(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%d", optionInt);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -broadcast
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'b') &&
			(strncmp(optionName, "-broadcast", len) == 0))) {
		optionInt = -1;
		socklen = sizeof(optionInt);
		if (getsockopt(statePtr->fd, SOL_SOCKET, SO_BROADCAST, (void *) &optionInt, &socklen) != 0) {
			return rperr("can't get broadcast: ");
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-broadcast");
		}
		(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%d", (optionInt > 0));
		Tcl_DStringAppendElement(dsPtr, optionVal);
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -domain
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'd') &&
			(strncmp(optionName, "-domain", len) == 0))) {
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-domain");
		}
		Tcl_DStringAppendElement(dsPtr, Cep_StrCepDomain(cepDomain));
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -type
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 't') &&
			(strncmp(optionName, "-type", len) == 0))) {
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-type");
		}
		Tcl_DStringAppendElement(dsPtr, Cep_StrCepType(cepType));
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -join
	 */
	/*
	if ((len == 0) || ((len > 1) && (optionName[1] == 'j') &&
			(strncmp(optionName, "-join", len) == 0))) {
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-join");
		}
		Tcl_DStringAppendElement(dsPtr, "");
		if (len > 0) {
			return TCL_OK;
		}
	}
	*/

	/*
	 * Option -leave
	 */
	/*
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'l') &&
			(strncmp(optionName, "-leave", len) == 0))) {
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-leave");
		}
		Tcl_DStringAppendElement(dsPtr, "");
		if (len > 0) {
			return TCL_OK;
		}
	}
	*/

	/*
	 * Option -shutdown
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 's') &&
			(strncmp(optionName, "-shutdown", len) == 0))) {
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-shutdown");
			if (((statePtr->flags & CEP_SHUT_READ) && (statePtr->flags & CEP_SHUT_WRITE)) ||
					!((statePtr->flags & CEP_SHUT_READ) || (statePtr->flags & CEP_SHUT_WRITE))) {
				Tcl_DStringStartSublist(dsPtr);
			}
		}
		if (statePtr->flags & CEP_SHUT_READ) {
			Tcl_DStringAppendElement(dsPtr, "read");
		}
		if (statePtr->flags & CEP_SHUT_WRITE) {
			Tcl_DStringAppendElement(dsPtr, "write");
		}
		if (len == 0) {
			if (((statePtr->flags & CEP_SHUT_READ) && (statePtr->flags & CEP_SHUT_WRITE)) ||
					!((statePtr->flags & CEP_SHUT_READ) || (statePtr->flags & CEP_SHUT_WRITE))) {
				Tcl_DStringEndSublist(dsPtr);
			}
		} else {
			return TCL_OK;
		}
	}

	/*
	 * Option -loop
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'l') &&
			(strncmp(optionName, "-loop", len) == 0))) {
		unsigned char optionUChar = 0;
		if ((cepDomain == CEP_INET6) || (cepDomain == CEP_INET)) {
			int ret = 0;
			socklen = sizeof(optionUChar);
			if (cepDomain == CEP_INET6) {
				ret = getsockopt(statePtr->fd, IPPROTO_IPV6, IPV6_MULTICAST_LOOP, (void *) &optionUChar, &socklen);
			} else {
				ret = getsockopt(statePtr->fd, IPPROTO_IP, IP_MULTICAST_LOOP, (void *) &optionUChar, &socklen);
			}
			if (ret != 0) {
				return rperr("can't get loop: ");
			}
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-loop");
		}
		(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%u", optionUChar);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -mhops
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'm') &&
			(strncmp(optionName, "-mhops", len) == 0))) {
		optionInt = 0;
		if ((cepDomain == CEP_INET6) || (cepDomain == CEP_INET)) {
			socklen = sizeof(unsigned char);
			if (getsockopt(statePtr->fd,
					((cepDomain == CEP_INET6) ? IPPROTO_IPV6 : IPPROTO_IP),
					((cepDomain == CEP_INET6) ? IPV6_MULTICAST_HOPS : IP_MULTICAST_TTL),
					(void *) &optionInt, &socklen) != 0) {
				return rperr("can't get mhops: ");
			}
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-mhops");
		}
		(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%u", (unsigned char) optionInt);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -maddr
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'm') &&
			(strncmp(optionName, "-maddr", len) == 0))) {
		addrBuf[0] = '*';
		addrBuf[1] = '\0';
		if (cepDomain == CEP_INET6) {
			struct in6_addr addr;
			socklen = sizeof(addr);
			if (getsockopt(statePtr->fd, IPPROTO_IPV6, IPV6_MULTICAST_IF, (void *) &addr, &socklen) != 0) {
				return rperr("can't get maddr: ");
			}
			if (inet_ntop(CepDomainToSysDomain(cepDomain), (const void *) &addr, addrBuf, sizeof(addrBuf)) == NULL) {
				addrBuf[0] = '?';
				addrBuf[1] = '\0';
			}
		} else if (cepDomain == CEP_INET) {
			struct in_addr addr;
			socklen = sizeof(addr);
			if (getsockopt(statePtr->fd, IPPROTO_IP, IP_MULTICAST_IF, (void *) &addr, &socklen) != 0) {
				return rperr("can't get maddr: ");
			}
			if (inet_ntop(CepDomainToSysDomain(cepDomain), (const void *) &addr, addrBuf, sizeof(addrBuf)) == NULL) {
				addrBuf[0] = '?';
				addrBuf[1] = '\0';
			}
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-maddr");
		}
		Tcl_DStringAppendElement(dsPtr, addrBuf);
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -protocol
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'p') &&
			(strncmp(optionName, "-protocol", len) == 0))) {
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-protocol");
		}
		if (statePtr->protocol == 0) {
			Tcl_DStringAppendElement(dsPtr, "default");
		} else {
			struct protoent *pe = getprotobynumber(statePtr->protocol);
			if (pe == NULL) {
				return rperr("can't get protocol: ");
			} else {
				Tcl_DString ds;
				Tcl_DStringInit(&ds);
				Tcl_ExternalToUtfDString(NULL, pe->p_name, -1, &ds);
				Tcl_DStringAppendElement(dsPtr, (const char *) Tcl_DStringValue(&ds));
				Tcl_DStringFree(&ds);
			}
		}
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -resolve
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'r') &&
			(strncmp(optionName, "-resolve", len) == 0))) {
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-resolve");
		}
		Tcl_DStringAppendElement(dsPtr, (resolve ? "1" : "0"));
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -header
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'h') &&
			(strncmp(optionName, "-header", len) == 0))) {
		optionInt = 0;
		if ((cepType == CEP_RAW) && (cepDomain == CEP_INET)) {
			socklen = sizeof(optionInt);
			if (getsockopt(statePtr->fd, IPPROTO_IP, IP_HDRINCL, (void *) &optionInt, &socklen) != 0) {
				return rperr("can't get header: ");
			}
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-header");
		}
		Tcl_DStringAppendElement(dsPtr, (optionInt == 0) ? "0" : "1");
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -route
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'r') &&
			(strncmp(optionName, "-route", len) == 0))) {
		optionInt = 0;
		socklen = sizeof(optionInt);
		if (getsockopt(statePtr->fd, SOL_SOCKET, SO_DONTROUTE, (void *) &optionInt, &socklen) != 0) {
			return rperr("can't get route: ");
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-route");
		}
		/* DONTROUTE - so it's backwards */
		Tcl_DStringAppendElement(dsPtr, (optionInt == 0) ? "1" : "0");
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -closeonexec
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'c') &&
			(strncmp(optionName, "-closeonexec", len) == 0))) {
		optionInt = 0;
		optionInt = fcntl(statePtr->fd, F_GETFD, FD_CLOEXEC);
		optionInt &= FD_CLOEXEC;
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-closeonexec");
		}
		(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%d", optionInt);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -qlen
	 */
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'q') &&
			(strncmp(optionName, "-qlen", len) == 0))) {
		int qlen = 0;
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-qlen");
		}
		if ((cepType == CEP_DGRAM) || (cepType == CEP_RAW)) {
			if (Tcl_ListObjLength(NULL, statePtr->packetQueue, &qlen) != TCL_OK) {
				/* something? */
			}
		}
		(void) snprintf(optionVal, TCL_INTEGER_SPACE, "%d", qlen);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		if (len > 0) {
			return TCL_OK;
		}
	}

	/*
	 * Option -sendtimeout
	 */
	/*
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 's') &&
			(strncmp(optionName, "-sendtimeout", len) == 0))) {
		struct timeval t;
		socklen = sizeof(t);
		if (getsockopt(statePtr->fd, SOL_SOCKET, SO_SNDTIMEO, (void *) &t, &socklen) != 0) {
			return rperr("can't get sendtimeout: ");
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-sendtimeout");
			Tcl_DStringStartSublist(dsPtr);
		}
		snprintf(optionVal, TCL_INTEGER_SPACE, "%ld", t.tv_sec);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		snprintf(optionVal, TCL_INTEGER_SPACE, "%ld", t.tv_usec);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		if (len == 0) {
			Tcl_DStringEndSublist(dsPtr);
		} else {
			return TCL_OK;
		}
	}
	*/

	/*
	 * Option -receivetimeout
	 */
	/*
	if ((len == 0) ||
			((len > 1) && (optionName[1] == 'r') &&
			(strncmp(optionName, "-receivetimeout", len) == 0))) {
		struct timeval t;
		socklen = sizeof(t);
		if (getsockopt(statePtr->fd, SOL_SOCKET, SO_RCVTIMEO, (void *) &t, &socklen) != 0) {
			return rperr("can't get receivetimeout: ");
		}
		if (len == 0) {
			Tcl_DStringAppendElement(dsPtr, "-receivetimeout");
			Tcl_DStringStartSublist(dsPtr);
		}
		snprintf(optionVal, TCL_INTEGER_SPACE, "%ld", t.tv_sec);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		snprintf(optionVal, TCL_INTEGER_SPACE, "%ld", t.tv_usec);
		Tcl_DStringAppendElement(dsPtr, optionVal);
		if (len == 0) {
			Tcl_DStringEndSublist(dsPtr);
		} else {
			return TCL_OK;
		}
	}
	*/

	if (len > 0) {
		return Tcl_BadChannelOption(interp, optionName, "broadcast closeonexec domain header hops maddr mhops loop peername protocol qlen resolve route shutdown sockname type");
	}

	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateCepAddress --
 *
 *	This function initializes a sockaddr structure for a host and port.
 *
 * Results:
 *	sizeof the sockaddr on success, -1 on error
 *
 *
 * Side effects:
 *	Fills in the *sockaddrPtr structure.
 *
 *----------------------------------------------------------------------
 */

static int
CreateCepAddress (
	struct sockaddr_storage *sockaddrPtr,	/* Socket address */
	const char *host,			/* Host.  NULL implies INADDR_ANY */
	int port,				/* Port number */
	unsigned int flags
) {
	struct sockaddr     *sap = (struct sockaddr     *) sockaddrPtr;
	struct sockaddr_in6 *s6p = (struct sockaddr_in6 *) sockaddrPtr;
	struct sockaddr_in  *s4p = (struct sockaddr_in  *) sockaddrPtr;
	struct sockaddr_un  *slp = (struct sockaddr_un  *) sockaddrPtr;
	struct in6_addr addr6;
	struct in_addr  addr4;
	void *addrPtr;
	int size;
	int cepDomain;


	cepDomain = MASK2DOMAIN(flags);

	(void) memset((void *) sockaddrPtr, 0, sizeof(struct sockaddr_storage));

	port = htons((unsigned short) (port & 0xFFFF));
	sap->sa_family = CepDomainToSysDomain(cepDomain);

	if (cepDomain == CEP_LOCAL) {
		if (host != NULL) {
			Tcl_DString ds;
			Tcl_DStringInit(&ds);
			Tcl_UtfToExternalDString(NULL, host, -1, &ds);
			size = Tcl_DStringLength(&ds);
			if (size > sizeof(slp->sun_path)) {
				Tcl_DStringFree(&ds);
				Tcl_SetErrno(ENAMETOOLONG);
				return -1;
			}
			memcpy((void *) slp->sun_path, (const void *) Tcl_DStringValue(&ds), size);
			Tcl_DStringFree(&ds);
			return (sizeof(*slp) - sizeof(slp->sun_path) + size);
		}
		return -1;
	}


	size = GetSocketStructSize(cepDomain);

	if (cepDomain == CEP_INET6) {
		s6p->sin6_port = port;
		addrPtr = &addr6;
	} else {
		s4p->sin_port = port;
		addrPtr = &addr4;
	}
	(void) memset(addrPtr, 0, size);

	if (host == NULL) {
		if (cepDomain == CEP_INET6) {    
			addr6 = in6addr_any;
		} else {
			addr4.s_addr = htonl(INADDR_ANY);
		}
	} else {
		if (!NameToAddr(host, addrPtr, flags)) {
			return -1;
		}
	}

	/*
	 * NOTE: On 64 bit machines the assignment below is rumored to not
	 * do the right thing. Please report errors related to this if you
	 * observe incorrect behavior on 64 bit machines such as DEC Alphas.
	 * Should we modify this code to do an explicit memcpy?
	 * Stu is not sure?
	 */

	if (cepDomain == CEP_INET6) {
		s6p->sin6_addr = addr6;
	} else {
		s4p->sin_addr = addr4;
	}

	return size;	/* Success. */
}

/*
 *----------------------------------------------------------------------
 *
 * CreateCep --
 *
 *	This function opens a new cep in client or server mode
 *	and initializes the CepState structure.
 *
 * Results:
 *	Returns a new CepState, or NULL with an error in the interp's
 *	result, if interp is not NULL.
 *
 * Side effects:
 *	Opens a cep.
 *
 *----------------------------------------------------------------------
 */

static CepState *
CreateCep (
	Tcl_Interp *interp,		/* For error reporting; can be NULL. */
	const char *protocol,
	const char *host,		/* Name of host on which to open port, NULL means INADDR_ANY */
	int port,			/* Port number to open. */
	const char *myaddr,		/* Client-side address */
	int myport,			/* Client-side port */
	unsigned int flags
) {
	CepState *statePtr;
	struct sockaddr_storage sockaddr;
	struct sockaddr_storage mysockaddr;
	int size = -1;
	int sock = -1;
	int asyncConnect;
	int status;
	int curState;
	int origState = 0;
	int domain;
	int type;
	int proto = 0;
	int cepDomain = MASK2DOMAIN(flags);
	int cepType   = MASK2TYPE(flags);
	int server    = (flags & CEP_SERVER);
	int reuseaddr = (flags & CEP_REUSEADDR);
	int reuseport = (flags & CEP_REUSEPORT);
	int async     = (flags & CEP_ASYNC);
	int resolve   = (flags & CEP_RESOLVE_NAMES);
	int chanrecv  = (flags & CEP_CHANRECV);


	domain = CepDomainToSysDomain(cepDomain);
	type = CepTypeToSysType(cepType);


	if (!((host == NULL) && (port == -1))) {
		if ((size = CreateCepAddress(&sockaddr, host, port, flags)) == -1) {
			goto addressError;
		}
	}

	if (cepDomain != CEP_LOCAL && (myaddr != NULL || myport != 0)) {
		if ((size = CreateCepAddress(&mysockaddr, myaddr, myport, flags)) == -1) {
			goto addressError;
		}
	}

	if ((protocol != NULL) && (strlen(protocol) > 0) && (strcmp(protocol, "default") != 0)) {
		if (Tcl_GetInt(interp, protocol, &proto) != TCL_OK) {
			struct protoent *pe;
			Tcl_DString ds;
			Tcl_DStringInit(&ds);
			Tcl_UtfToExternalDString(NULL, protocol, -1, &ds);
			pe = getprotobyname((const char *) Tcl_DStringValue(&ds));
			Tcl_DStringFree(&ds);
			if (pe == NULL) {
				goto addressError;
			}
			proto = pe->p_proto;
		}
	}

	if ((sock = socket(domain, type, proto)) == -1) {
		goto addressError;
	}

	/*
	 * Set the close-on-exec flag so that the cep will not get
	 * inherited by child processes.
	 */

	(void) fcntl(sock, F_SETFD, FD_CLOEXEC);

	/*
	 * Set kernel space buffering
	 */
	if (cepType == CEP_STREAM) {
		_TCL_SockMinimumBuffers(sock, SOCKET_BUFSIZE);
	}

	asyncConnect = 0;
	status = 0;

	if (server) {
		if (cepDomain == CEP_INET || cepDomain == CEP_INET6) {
			if (reuseaddr) {
				status = 1;
				(void) setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const void *) &status, sizeof(status));
			}
			if (reuseport) {
				status = 1;
				(void) setsockopt(sock, SOL_SOCKET, CEP_SYS_REUSEPORT, (const void *) &status, sizeof(status));
			}
		}

		status = bind(sock, (struct sockaddr *) &sockaddr, size);

		if (cepType == CEP_STREAM) {
			if (status != -1) {
				status = listen(sock, SOMAXCONN);
			}
		}
	} else {
		if (cepDomain == CEP_INET || cepDomain == CEP_INET6) {
			if (myaddr != NULL || myport != 0) {
				if (reuseaddr) {
					status = 1;
					(void) setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const void *) &status, sizeof(status));
				}
				if (reuseport) {
					status = 1;
					(void) setsockopt(sock, SOL_SOCKET, CEP_SYS_REUSEPORT, (const void *) &status, sizeof(status));
				}
	
				status = bind(sock, (struct sockaddr *) &mysockaddr, size);

				if (status < 0) {
					goto bindError;
				}
			}
		}

		if ((host == NULL) && (port == -1)) {
			status = 0;
		} else {
			/*
			 * Attempt to connect. The connect may fail at present with an
			 * EINPROGRESS but at a later time it will complete. The caller
			 * will set up a file handler on the cep if she is interested in
			 * being informed when the connect completes.
			 */

			if (async) {
				origState = fcntl(sock, F_GETFL);
				curState = origState | O_NONBLOCK;
				status = fcntl(sock, F_SETFL, curState);
			} else {
				status = 0;
			}

			if (status >= 0) {
				status = connect(sock, (struct sockaddr *) &sockaddr, size);
				if (status < 0) {
					if (Tcl_GetErrno() == EINPROGRESS) {
						asyncConnect = 1;
						status = 0;
					}
				} else {
				/*
				 * Here we are if the connect succeeds. In case of an
				 * asynchronous connect we have to reset the channel to
				 * blocking mode.  This appears to happen not very often,
				 * but e.g. on a HP 9000/800 under HP-UX B.11.00 we enter
				 * this stage. [Bug: 4388]
				 */
					if (async) {
						origState = fcntl(sock, F_GETFL);
						curState = origState & ~(O_NONBLOCK);
						status = fcntl(sock, F_SETFL, curState);
					}
				}
			}
		}
	}

	bindError:
	if (status < 0) {
		/* Setup msg, close cep. */
		rperr("couldn't open cep: ");
		if (sock != -1) {
			close(sock);
		}
		return NULL;
	}

	/* Allocate and setup a new CepState for this cep. */

	statePtr = (CepState *) ckalloc((unsigned) sizeof(CepState));
	statePtr->fd = sock;


	statePtr->flags = Cep_SetFreshCepFlags(cepDomain, cepType, server, resolve,
				reuseaddr, reuseport, async, chanrecv);

/*
	statePtr->flags = 0;
	statePtr->flags |= DOMAIN2MASK(cepDomain);
	statePtr->flags |= TYPE2MASK(cepType);
*/

	if (asyncConnect) {
		statePtr->flags |= CEP_ASYNC_CONNECT;
	}

/*
	if (server) {
		statePtr->flags |= CEP_SERVER;
	}
	if (resolve) {
		statePtr->flags |= CEP_RESOLVE_NAMES;
	}
*/
	statePtr->timer = (Tcl_TimerToken) NULL;
	statePtr->protocol = proto;

	if (cepDomain == CEP_LOCAL && host != NULL) {
		Tcl_Obj *ho;
		Tcl_DString ds;
		const char *nat;

		statePtr->names = Tcl_NewObj();
		Tcl_IncrRefCount(statePtr->names);

		ho = Tcl_NewStringObj(host, -1);
		Tcl_IncrRefCount(ho);

		Tcl_DStringInit(&ds);

		nat = Tcl_FSGetNativePath(ho);
		Tcl_ExternalToUtfDString(NULL, nat, -1, &ds);

		Tcl_ListObjAppendElement(interp, statePtr->names, ho);
		Tcl_ListObjAppendElement(interp, statePtr->names, Tcl_NewStringObj(Tcl_DStringValue(&ds), Tcl_DStringLength(&ds)));

		Tcl_DecrRefCount(ho);

		if (server) {
			Tcl_ListObjAppendElement(interp, statePtr->names, Tcl_NewByteArrayObj(nat, strlen(nat) + 1));
		}

		Tcl_DStringFree(&ds);
	} else {
		statePtr->names = NULL;
	}

	if ((cepType == CEP_DGRAM) || (cepType == CEP_RAW)) {
		statePtr->packetQueue = Tcl_NewObj();
		Tcl_IncrRefCount(statePtr->packetQueue);
	} else {
		statePtr->packetQueue = NULL;
	}

	return statePtr;

	addressError:
	/* Close cep, setup msg. */
	/*
	if (sock != -1) {
		close(sock);
	}
	*/
	rperr("couldn't open cep: ");
	return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * MakeCepClientChannel --
 *
 *	Creates a Tcl_Channel from an existing client cep.
 *
 * Results:
 *	The Tcl_Channel wrapped around the preexisting cep.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
MakeCepClientChannel (
	ClientData sock,	/* The cep to wrap up into a channel. */
	int protocol,
	unsigned int flags
) {
	return MakeCepClientChannelMode(sock, protocol, flags, (TCL_READABLE | TCL_WRITABLE));
}

/*
 *----------------------------------------------------------------------
 *
 * MakeCepClientChannelMode --
 *
 *	Creates a Tcl_Channel from an existing client cep
 *	with given mode.
 *
 * Results:
 *	The Tcl_Channel wrapped around the preexisting cep.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static Tcl_Channel
MakeCepClientChannelMode (
	ClientData sock,	/* The socket to wrap up into a channel. */
	int protocol,
	unsigned int flags,
	int mode
) {
	CepState *statePtr;
	char channelName[CEP_CHANNELNAME_MAX];
	struct sockaddr_storage sockaddr;
	socklen_t size = sizeof(struct sockaddr);
	int cepDomain = MASK2DOMAIN(flags);
	int cepType = MASK2TYPE(flags);


	if (getsockname(PTR2INT(sock), (struct sockaddr *) &sockaddr, &size) != 0) {
		return NULL;
	}

	if (cepDomain == CEP_DOMAIN_ANY) {
		cepDomain = SysDomainToCepDomain(sockaddr.ss_family);
	}

	if (cepType == CEP_TYPE_ANY) {
		int optionInt = -1;
		size = sizeof(optionInt);
		if (getsockopt(PTR2INT(sock), SOL_SOCKET, SO_TYPE, (void *) &optionInt, &size) != 0) {
			return NULL;
		}
		cepType = SysTypeToCepType(optionInt);
	}

	statePtr = (CepState *) ckalloc((unsigned) sizeof(CepState));
	statePtr->fd = PTR2INT(sock);
	statePtr->flags = 0;
	statePtr->flags |= DOMAIN2MASK(cepDomain);
	statePtr->flags |= TYPE2MASK(cepType);
	if (flags & CEP_RESOLVE_NAMES) {
		statePtr->flags |= CEP_RESOLVE_NAMES;
	}
	statePtr->protocol = protocol;
	statePtr->acceptProc = NULL;
	statePtr->acceptProcData = (ClientData) NULL;
	statePtr->names = NULL;
	statePtr->packetQueue = NULL;

	(void) snprintf(channelName, CEP_CHANNELNAME_MAX, "cep%d", statePtr->fd);

	statePtr->channel = Tcl_CreateChannel(&cepChannelType, channelName, (ClientData) statePtr, mode);

	if (Tcl_SetChannelOption((Tcl_Interp *) NULL, statePtr->channel,
		 	"-translation", "auto crlf") == TCL_ERROR) {
		Tcl_Close((Tcl_Interp *) NULL, statePtr->channel);
		return NULL;
	}

	return statePtr->channel;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_OpenLocalPair --
 *
 *
 * Results:
 *
 * Side effects:
 *	End of the universe.
 *
 *----------------------------------------------------------------------
 */

int
Cep_OpenLocalPair (
	Tcl_Interp *interp,
	const char *protocol,
	Tcl_Channel *chan1,
	Tcl_Channel *chan2,
	unsigned int flags
) {
	int result;
	int sv[2];
	int proto = 0;


	if ((protocol != NULL) && (strcmp(protocol, "default") != 0)) {
		if (Tcl_GetInt(interp, protocol, &proto) != TCL_OK) {
			struct protoent *pe;
			Tcl_DString ds;
			Tcl_DStringInit(&ds);
			Tcl_UtfToExternalDString(NULL, protocol, -1, &ds);
			pe = getprotobyname((const char *) Tcl_DStringValue(&ds));
			Tcl_DStringFree(&ds);
			if (pe == NULL) {
				rperr("couldn't create localpair: ");
				return -1;
			} 
			proto = pe->p_proto;
		}
	}


	result = socketpair(CepDomainToSysDomain(MASK2DOMAIN(flags)), CepTypeToSysType(MASK2TYPE(flags)), proto, sv);

	if (result != 0) {
		rperr("couldn't create localpair: ");
		return -1;
	}

	(void) fcntl(sv[0], F_SETFD, FD_CLOEXEC);
	(void) fcntl(sv[1], F_SETFD, FD_CLOEXEC);

	*chan1 = MakeCepClientChannel((ClientData) INT2PTR(sv[0]), proto, flags);
	if (*chan1 == NULL) {
		close(sv[0]);
		close(sv[1]);
		return -1;
	}

	*chan2 = MakeCepClientChannel((ClientData) INT2PTR(sv[1]), proto, flags);
	if (*chan1 == NULL) {
		Tcl_Close(interp, *chan1);
		close(sv[0]);
		close(sv[1]);
		return -1;
	}

	return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_OpenClient --
 *
 *	Opens a client cep and creates a channel around it.
 *
 * Results:
 *	The channel or NULL if failed.	An error message is returned
 *	in the interpreter on failure.
 *
 * Side effects:
 *	Opens a client cep and creates a new channel.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
Cep_OpenClient (
	Tcl_Interp *interp,		/* For error reporting; can be NULL. */
	const char *protocol,
	const char *host,		/*  */
	int port,			/* Port number to open. */
	const char *myaddr,		/* Client-side address */
	int myport,			/* Client-side port */
	CepAcceptProc *acceptProc,	/* Callback */
	ClientData acceptProcData,	/* Data for the callback. */
	unsigned int flags
) {
	CepState *statePtr;
	char channelName[CEP_CHANNELNAME_MAX];

	/*
	 * Create a new client cep and wrap it in a channel.
	 */

	statePtr = CreateCep(interp, protocol, host, port, myaddr, myport, flags);
	if (statePtr == NULL) {
		return NULL;
	}

	statePtr->acceptProc = acceptProc;
	statePtr->acceptProcData = acceptProcData;

	(void) snprintf(channelName, CEP_CHANNELNAME_MAX, "cep%d", statePtr->fd);

	statePtr->channel = Tcl_CreateChannel(&cepChannelType, channelName, (ClientData) statePtr, (TCL_READABLE | TCL_WRITABLE));

	/* Is this a good default setting? Anything else to set? */
	if (Tcl_SetChannelOption(interp, statePtr->channel, "-translation", "auto crlf") == TCL_ERROR) {
		Tcl_Close((Tcl_Interp *) NULL, statePtr->channel);
		return NULL;
	}

	return statePtr->channel;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_OpenServer --
 *
 *	Opens a server cep and creates a channel around it.
 *
 * Results:
 *	The channel or NULL if failed. If an error occurred, an
 *	error message is left in the interp's result if interp is
 *	not NULL.
 *
 * Side effects:
 *	Opens a server socket cep and creates a new channel.
 *
 *----------------------------------------------------------------------
 */

Tcl_Channel
Cep_OpenServer (
	Tcl_Interp *interp,		/* For error reporting - may be NULL. ? */
	const char *protocol,
	const char *myAddr,		/* */
	int port,
	CepAcceptProc *acceptProc,	/* Callback for accepting connections
					 * from new clients. */
	ClientData acceptProcData,	/* Data for the callback. */
	unsigned int flags
) {
	CepState *statePtr;
	char channelName[CEP_CHANNELNAME_MAX];

	/*
	 * Create a new server cep and wrap it in a channel.
	 */
	statePtr = CreateCep(interp, protocol, myAddr, port, NULL, NULL, flags);
	if (statePtr == NULL) {
		return NULL;
	}

	statePtr->acceptProc = acceptProc;
	statePtr->acceptProcData = acceptProcData;

	/*
	 * Set up the callback mechanism for accepting connections
	 * from new clients.
	 */

	Tcl_CreateFileHandler(statePtr->fd, TCL_READABLE, CepAccept, (ClientData) statePtr);

	(void) snprintf(channelName, CEP_CHANNELNAME_MAX, "cep%d", statePtr->fd);

	statePtr->channel = Tcl_CreateChannel(&cepChannelType, channelName, (ClientData) statePtr, 0);

	return statePtr->channel;
}

/*
 *----------------------------------------------------------------------
 *
 * CepAccept --
 *	Accept a CEP socket connection.	 This is called by the event loop.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Creates a new connection socket. Calls the registered callback
 *	for the connection acceptance mechanism.
 *
 *----------------------------------------------------------------------
 */

static void
CepAccept (
	ClientData data,	/* Callback token. */
	int mask		/* Not used. */
) {
	CepState *statePtr = (CepState *) data;	/* Client data of server socket. */
	CepState *newCepState;			/* State for new socket. */
	struct sockaddr_storage sockaddr;	/* The remote address */
	socklen_t size = sizeof(struct sockaddr_storage);
	int newsock;				/* The new client socket */
	char channelName[CEP_CHANNELNAME_MAX];
	int cepDomain;

	newsock = accept(statePtr->fd, (struct sockaddr *) &sockaddr, &size);
	if (newsock < 0) {
		return;
	}

	/*
	 * Set close-on-exec flag to prevent the newly accepted socket from
	 * being inherited by child processes.
	 */

	(void) fcntl(newsock, F_SETFD, FD_CLOEXEC);

	newCepState = (CepState *) ckalloc((unsigned) sizeof(CepState));

	cepDomain = MASK2DOMAIN(statePtr->flags);

	newCepState->flags = 0;
	newCepState->flags |= DOMAIN2MASK(cepDomain);
	newCepState->flags |= TYPE2MASK(MASK2TYPE(statePtr->flags));
	if (statePtr->flags & CEP_RESOLVE_NAMES) {
		newCepState->flags |= CEP_RESOLVE_NAMES;
	}
	newCepState->protocol = statePtr->protocol;
	newCepState->fd = newsock;
	newCepState->acceptProc = NULL;
	newCepState->acceptProcData = NULL;

	if (cepDomain == CEP_LOCAL) {
		newCepState->names = statePtr->names;
		Tcl_IncrRefCount(newCepState->names);
	} else {
		newCepState->names = NULL;
	}

	newCepState->packetQueue = NULL;

	(void) snprintf(channelName, CEP_CHANNELNAME_MAX, "cep%d", newsock);

	newCepState->channel = Tcl_CreateChannel(&cepChannelType, channelName,
					(ClientData) newCepState, (TCL_READABLE | TCL_WRITABLE));

	Tcl_SetChannelOption(NULL, newCepState->channel, "-translation", "auto crlf");

	if (statePtr->acceptProc != NULL) {
		struct sockaddr_in6 *s6p = (struct sockaddr_in6 *) &sockaddr;
		struct sockaddr_in  *s4p = (struct sockaddr_in  *) &sockaddr;
/*		struct sockaddr_un  *slp = (struct sockaddr_un  *) &sockaddr;*/
		char addrBuf[CEP_HOSTNAME_MAX];
		int port = -1;
		uid_t euid = (unsigned) -1;
		gid_t egid = (unsigned) -1; 
		char *addrPtr = addrBuf;
		Tcl_DString ds;

		Tcl_DStringInit(&ds);

		if (cepDomain == CEP_LOCAL) {
			Tcl_Obj *cep_path;
			Tcl_DStringStartSublist(&ds);
			Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 0, &cep_path);
			Tcl_DStringAppendElement(&ds, Tcl_GetString(cep_path));
			Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 1, &cep_path);
			Tcl_DStringAppendElement(&ds, Tcl_GetString(cep_path));
			Tcl_DStringEndSublist(&ds);
			addrPtr = Tcl_DStringValue(&ds);
			if (getpeereid(newsock, &euid, &egid) != 0) {
				Tcl_Panic("CepAccept getpeereid error (%s)", Tcl_ErrnoMsg(Tcl_GetErrno()));
			}
		} else {
			if (getnameinfo((struct sockaddr *) &sockaddr, size, addrBuf, sizeof(addrBuf), NULL, 0, NI_NUMERICHOST) != 0) {
				addrBuf[0] = '?';
				addrBuf[1] = '\0';
			}
			if (cepDomain == CEP_INET6) {
				port = ntohs((unsigned short) s6p->sin6_port);
			} else {
				port = ntohs((unsigned short) s4p->sin_port);
			}
		}

		(*statePtr->acceptProc)(statePtr->acceptProcData,
				newCepState->channel, (const char *) addrPtr, port,
				euid, egid, DOMAIN2MASK(cepDomain));

		Tcl_DStringFree(&ds);
	}
}

/*
 *----------------------------------------------------------------------
 *
 * CepNotify --
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

static void
CepNotify (
	ClientData data,	/* Callback token. */
	int mask
) {
	CepState *statePtr = (CepState *) data;		/* Client data of cep */
	struct sockaddr_storage sockaddr;		/* The remote address */
	struct sockaddr     *sap = (struct sockaddr     *) &sockaddr;
	struct sockaddr_in6 *s6p = (struct sockaddr_in6 *) &sockaddr;
	struct sockaddr_in  *s4p = (struct sockaddr_in  *) &sockaddr;

	socklen_t socklen;
	ssize_t bytesRead;
	int blobsize = SOCKET_BUFSIZE;
	Tcl_Obj *blob;
	Tcl_Obj *packet;
	Tcl_Obj *addrInfo;
	Tcl_Obj *boing;

	int cepDomain = MASK2DOMAIN(statePtr->flags);
	int cepType = MASK2TYPE(statePtr->flags);
/*

	int resolve = (statePtr->flags & CEP_RESOLVE_NAMES);
*/

	char addrBuf[CEP_HOSTNAME_MAX];
/*	char *addrPtr = addrBuf;*/
	int port = -1;

	int passfd = -1;
	struct msghdr msg;
	struct iovec vec;
	struct cmsghdr *cmsg;
	union {
		struct cmsghdr hdr;
		unsigned char buf[CMSG_SPACE(sizeof(int))];
	} cmsgbuf;


	if ((mask & TCL_WRITABLE) || (mask & TCL_EXCEPTION)) {
		Tcl_NotifyChannel(statePtr->channel, mask);
		return;
	}

	CepDeleteTimer(statePtr);

	/* rcv gives the number of bytes + header */

	blob = Tcl_NewObj();
	socklen = sizeof(sockaddr);

	msg.msg_name       = (void *) sap;
	msg.msg_namelen    = socklen;
	msg.msg_iov        = &vec;
	msg.msg_iovlen     = 1;
	msg.msg_control    = &cmsgbuf.buf;
	msg.msg_controllen = sizeof(cmsgbuf.buf);
	msg.msg_flags      = 0;

	vec.iov_base = Tcl_SetByteArrayLength(blob, blobsize);
	vec.iov_len  = blobsize;

	bytesRead = recvmsg(statePtr->fd, &msg, 0);
	Tcl_SetByteArrayLength(blob, bytesRead);
	socklen = msg.msg_namelen;

	if (statePtr->flags & CEP_CHANRECV) {
		for (cmsg = CMSG_FIRSTHDR(&msg); cmsg != NULL; cmsg = CMSG_NXTHDR(&msg, cmsg)) {
			if (cmsg->cmsg_len == CMSG_LEN(sizeof(int)) && cmsg->cmsg_level == SOL_SOCKET && cmsg->cmsg_type == SCM_RIGHTS) {
				passfd = *(int *)CMSG_DATA(cmsg);
				break;
			}
		}
		if (passfd != -1) {
			int mode = 0;
			Tcl_Channel passChannel;
			uid_t euid;
			gid_t egid;
			const char *trout;
			int typeCh;
			int modeCh;


			if (getpeereid(statePtr->fd, &euid, &egid) != 0) {
				Tcl_Panic("CepAccept getpeereid error (%s)", Tcl_ErrnoMsg(Tcl_GetErrno()));
			}

			trout = Tcl_GetByteArrayFromObj(blob, (int *) NULL);

			typeCh = (int) *trout++;
			modeCh = (int) *trout++;

			switch (modeCh) {
			case 'b':
				mode |= (TCL_READABLE | TCL_WRITABLE);
				break;
			case 'r':
				mode |= TCL_READABLE;
				break;
			case 'w':
				mode |= TCL_WRITABLE;
				break;
			case ' ':
			default:
				/* Huh? */
				break;
			}

			switch (typeCh) {
			case 'c': {
				unsigned int flags = 0;
				int protocol = 0;
				trout += rvl(trout, (unsigned long *) &protocol);
				trout += rvl(trout, (unsigned long *) &flags);
				passChannel = MakeCepClientChannelMode((ClientData) INT2PTR(passfd), protocol, flags, mode);
				break;
			}
			case 'f':
			case 't':
			case 's':
				passChannel = Tcl_MakeFileChannel((ClientData) INT2PTR(passfd), mode);
				break;
			default:
				/* Huh? */
				passChannel = NULL;
				break;
			}
			if (passChannel == NULL) {
				close(passfd);
				return;
			}

			(*statePtr->acceptProc)(statePtr->acceptProcData,
					passChannel, trout, typeCh,
					euid, egid, (DOMAIN2MASK(cepDomain) | CEP_CHANRECV));

			return;
		}
	}

	if (cepDomain == CEP_LOCAL && cepType == CEP_STREAM) {
		Tcl_NotifyChannel(statePtr->channel, mask);
		return;
	}


	packet = Tcl_NewObj();
	addrInfo = Tcl_NewObj();

	if (cepDomain == CEP_LOCAL) {
/*
		Tcl_DString ds;
		uid_t euid;
		gid_t egid; 

		Tcl_DStringInit(&ds);

		Tcl_DStringStartSublist(&ds);
		Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 0, &cep_path);
		Tcl_DStringAppendElement(&ds, Tcl_GetString(cep_path));
		Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 1, &cep_path);
		Tcl_DStringAppendElement(&ds, Tcl_GetString(cep_path));
		Tcl_DStringEndSublist(&ds);
		addrPtr = Tcl_DStringValue(&ds);
		if (getpeereid(newsock, &euid, &egid) != 0) {
			Tcl_Panic("CepAccept getpeereid error (%s)", Tcl_ErrnoMsg(Tcl_GetErrno()));
		}
*/
		Tcl_Obj *cep_path;
		Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 0, &cep_path);
		Tcl_ListObjAppendElement(NULL, addrInfo, cep_path);
		Tcl_ListObjIndex((Tcl_Interp *) NULL, statePtr->names, 1, &cep_path);
		Tcl_ListObjAppendElement(NULL, addrInfo, cep_path);

		Tcl_ListObjAppendElement(NULL, addrInfo, Tcl_NewIntObj(port));
	} else {
		if (getnameinfo(sap, socklen, addrBuf, sizeof(addrBuf), NULL, 0, NI_NUMERICHOST) != 0) {
			addrBuf[0] = '?';
			addrBuf[1] = '\0';
		}

		boing = Tcl_NewStringObj(addrBuf, -1);
		Tcl_ListObjAppendElement(NULL, addrInfo, boing);

		if (statePtr->flags & CEP_RESOLVE_NAMES) {
			if (getnameinfo(sap, socklen, addrBuf, sizeof(addrBuf), NULL, 0, 0) != 0) {
				Tcl_ListObjAppendElement(NULL, addrInfo, boing);
			} else {
				Tcl_DString ds;
				Tcl_DStringInit(&ds);
				Tcl_ExternalToUtfDString(NULL, addrBuf, -1, &ds);
				Tcl_ListObjAppendElement(NULL, addrInfo, Tcl_NewStringObj((const char *) Tcl_DStringValue(&ds), -1));
				Tcl_DStringFree(&ds);
			}
		} else {
			Tcl_ListObjAppendElement(NULL, addrInfo, boing);
		}

		if (cepDomain == CEP_INET6) {
			port = ntohs((unsigned short) s6p->sin6_port);
		} else {
			port = ntohs((unsigned short) s4p->sin_port);
		}
		Tcl_ListObjAppendElement(NULL, addrInfo, Tcl_NewIntObj(port));
	}

	Tcl_ListObjAppendElement(NULL, addrInfo, Tcl_NewIntObj(bytesRead));

	Tcl_ListObjAppendElement(NULL, packet, addrInfo);
	Tcl_ListObjAppendElement(NULL, packet, blob);

	Tcl_ListObjAppendElement(NULL, statePtr->packetQueue, packet);

	Tcl_Preserve((ClientData) statePtr);
	Tcl_Preserve((ClientData) statePtr->channel);

	Tcl_NotifyChannel(statePtr->channel, mask);

	if (statePtr->packetQueue != NULL) {
		CepUpdateInterest(statePtr);
	}

	Tcl_Release((ClientData) statePtr->channel);
	Tcl_Release((ClientData) statePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CepUpdateInterest --
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
static void
CepUpdateInterest (
	CepState *statePtr
) {
	int len = 0;

	if ((Tcl_ListObjLength(NULL, statePtr->packetQueue, &len) == TCL_OK) && (len > 0)) {
		if (statePtr->timer == (Tcl_TimerToken) NULL) {
			statePtr->timer = Tcl_CreateTimerHandler(0, CepTimerProc, (ClientData) statePtr);
		}
	} 
}

/*
 *----------------------------------------------------------------------
 *
 * CepTimerProc --
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
static void
CepTimerProc (
	ClientData data
) {
	CepState *statePtr = (CepState *) data;		/* Client data of cep */
	int len = 0;

	if ((Tcl_ListObjLength(NULL, statePtr->packetQueue, &len) == TCL_OK) && (len > 0)) {
        /*
	 * Restart the timer in case a channel handler reenters the
         * event loop before UpdateInterest gets called by Tcl_NotifyChannel.
         */
		statePtr->timer = Tcl_CreateTimerHandler(0, CepTimerProc, (ClientData) statePtr);
		Tcl_Preserve((ClientData) statePtr);
		Tcl_Preserve((ClientData) statePtr->channel);
		Tcl_NotifyChannel(statePtr->channel, TCL_READABLE);
		Tcl_Release((ClientData) statePtr->channel);
		Tcl_Release((ClientData) statePtr);
	} else {
		statePtr->timer = (Tcl_TimerToken) NULL;
		CepUpdateInterest(statePtr);
	}
}

/*
 *----------------------------------------------------------------------
 *
 * CepDeleteTimer --
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
static void
CepDeleteTimer (
	CepState *statePtr
) {
	Tcl_DeleteTimerHandler(statePtr->timer);
	statePtr->timer = (Tcl_TimerToken) NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_Sendto --
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

int
Cep_Sendto (
	Tcl_Channel chan,
	const char *host,
	int port,
	const unsigned char *data,
	int dataLen
) {
	CepState *statePtr;
	struct sockaddr_storage sockaddr;
	int cepDomain;
	int size;

	statePtr = (CepState *) Tcl_GetChannelInstanceData(chan);
	cepDomain = MASK2DOMAIN(statePtr->flags);
	if ((size = CreateCepAddress(&sockaddr, host, port,  statePtr->flags)) == -1) {
		return -1;
	}
	return sendto(statePtr->fd, data, (size_t) dataLen, 0, (struct sockaddr *) &sockaddr, size);
}


/*
 *----------------------------------------------------------------------
 *
 * CepDomainToSysDomain --
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

static int
CepDomainToSysDomain (
	int cepDomain
) {
	switch (cepDomain) {
	case CEP_LOCAL:
		return AF_LOCAL;
	case CEP_INET:
		return AF_INET;
	case CEP_INET6:
		return AF_INET6;
	default:
		return -1;
	}
}

/*
 *----------------------------------------------------------------------
 *
 * SysDomainToCepDomain --
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

static int
SysDomainToCepDomain (
	int sysDomain
) {
	switch (sysDomain) {
	case AF_LOCAL:
		return CEP_LOCAL;
	case AF_INET:
		return CEP_INET;
	case AF_INET6:
		return CEP_INET6;
	default:
		return -1;
	}
}

/*
 *----------------------------------------------------------------------
 *
 * CepTypeToSysType --
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

static int
CepTypeToSysType (
	int cepType
) {
	switch (cepType) {
	case CEP_STREAM:
		return SOCK_STREAM;
	case CEP_DGRAM:
		return SOCK_DGRAM;
	case CEP_RAW:
		return SOCK_RAW;
	default:
		return -1;
	}
}

/*
 *----------------------------------------------------------------------
 *
 * SysTypeToCepType --
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

static int
SysTypeToCepType (
	int sysType
) {
	switch (sysType) {
	case SOCK_STREAM:
		return CEP_STREAM;
	case SOCK_DGRAM:
		return CEP_DGRAM;
	case SOCK_RAW:
		return CEP_RAW;
	default:
		return -1;
	}
}

/*
 *----------------------------------------------------------------------
 *
 * GetSocketStructSize --
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

static socklen_t
GetSocketStructSize (
	int cepDomain
) {
	switch (cepDomain) {
	case CEP_LOCAL:
		return sizeof(struct sockaddr_un);
	case CEP_INET:
		return sizeof(struct sockaddr_in);
	case CEP_INET6:
		return sizeof(struct sockaddr_in6);
	default:
		return 0;
	}
}

/*
 *----------------------------------------------------------------------
 *
 * NameToAddr --
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

static int
NameToAddr (
	const char *host,
	void *addrPtr,
	unsigned int flags
) {
	struct hostent *hostent;		/* Host database entry */
	Tcl_DString ds;

	Tcl_DStringInit(&ds);
	Tcl_UtfToExternalDString(NULL, host, -1, &ds);

	if (inet_pton(CepDomainToSysDomain(MASK2DOMAIN(flags)), (const char *) Tcl_DStringValue(&ds), addrPtr) != 1) {
		if (flags & CEP_RESOLVE_NAMES) {
			hostent = gethostbyname2((const char *) Tcl_DStringValue(&ds), CepDomainToSysDomain(MASK2DOMAIN(flags)));
		} else {
			hostent = NULL;
		}
		if (hostent != NULL) {
			memcpy(addrPtr, (void *) hostent->h_addr_list[0], (size_t) hostent->h_length);
		} else {
			Tcl_SetErrno(EHOSTUNREACH);
			Tcl_DStringFree(&ds);
			return 0;	/* error */
		}
	}

	Tcl_DStringFree(&ds);

	return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * resolve --
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

int
resolve (
	Tcl_Interp *interp,
	const char *host,
	int cepDomain,
	int objc,			/* Number of arguments. */
	Tcl_Obj *const objv[]		/* Argument objects. */
) {
	struct hostent *hostent;		/* Host database entry */
	Tcl_DString ds;
	char *charPtr;
	int sysDomain;
	Tcl_Obj *resultList;
	int i;
	char addrBuf[CEP_HOSTNAME_MAX];
	int optName = 0;
	int optAliases = 0;
	int optDomain = 0;
	int optLength = 0;
	int optAddresses = 0;

	if (objc > 0) {
		static const char *resolveOptions[] = {
			"-name", "-aliases", "-domain", "-length",
			"-addresses", "-all", (char *) NULL
		};
		enum resolveOptions {
			RESOPT_NAME, RESOPT_ALIASES, RESOPT_DOMAIN, RESOPT_LENGTH,
			RESOPT_ADDRESSES, RESOPT_ALL
		};
		int optionIndex;

		for (i = 0; i < objc; i++) {
			if (Tcl_GetIndexFromObj(interp, objv[i], resolveOptions,
					"option", 0, &optionIndex) != TCL_OK) {
				return TCL_ERROR;
			}
			switch ((enum resolveOptions) optionIndex) {
			case RESOPT_NAME:
				optName = 1;
				break;
			case RESOPT_ALIASES:
				optAliases = 1;
				break;
			case RESOPT_DOMAIN:
				optDomain = 1;
				break;
			case RESOPT_LENGTH:
				optLength = 1;
				break;
			case RESOPT_ADDRESSES:
				optAddresses = 1;
				break;
			case RESOPT_ALL:
				objc = 0;
				break;
			default:
				break;
			}
		}
	} else {
		objc = -1;
		optAddresses = 1;
	}

	sysDomain = CepDomainToSysDomain(cepDomain);
	Tcl_DStringInit(&ds);
	charPtr = Tcl_UtfToExternalDString(NULL, host, -1, &ds);
	hostent = gethostbyname2(charPtr, sysDomain);
	Tcl_DStringFree(&ds);

	if (hostent == NULL) {
		const char *h_errnoMsg;
		struct {
		        int _h_errno;
			const char *name;
		} h_errnoNames[] = {
			{ HOST_NOT_FOUND , "HOST_NOT_FOUND" },
			{ TRY_AGAIN      , "TRY_AGAIN"      },
			{ NO_RECOVERY    , "NO_RECOVERY"    },
			{ NO_DATA        , "NO_DATA"        },
			{ NETDB_INTERNAL , "NETDB_INTERNAL" },
			{ NETDB_SUCCESS  , "NETDB_SUCCESS"  },
			{ -1             , NULL             },
			{ -1             , "unknown error"  }
		};


        h_errnoMsg = hstrerror(h_errno);
	
		for (i = 0; h_errnoNames[i].name != NULL; i++) {
            if (h_errno == h_errnoNames[i]._h_errno) {
				break;
			}
		}
		if (h_errnoNames[i].name == NULL) {
			char *buf;
			int len;
			len = strlen(h_errnoMsg) + TCL_INTEGER_SPACE + 3;
			buf = (char *) ckalloc(len);
			i++;
            (void) snprintf(buf, len, "%s: %d", h_errnoMsg, h_errno);
			Tcl_SetErrorCode(interp, "POSIX", h_errnoNames[i].name, buf, (char *) NULL);
			rerr(buf);
			ckfree(buf);
		} else {
			Tcl_SetErrorCode(interp, "POSIX", h_errnoNames[i].name, h_errnoMsg, (char *) NULL);
			rerr(h_errnoMsg);
		}
		return TCL_ERROR;
	}


	resultList = Tcl_NewListObj(0, (Tcl_Obj *const *) NULL);

	if (objc == 0 || (objc > 1 && optName)) { if (Tcl_ListObjAppendElement(interp, resultList, Tcl_NewStringObj("-name", -1)) == TCL_ERROR) { return TCL_ERROR; } }
	if (objc == 0 || optName) {
		if (Tcl_ListObjAppendElement(interp, resultList, Tcl_NewStringObj(hostent->h_name, -1)) == TCL_ERROR) { return TCL_ERROR; }
	}

	if (objc == 0 || (objc > 1 && optAliases)) { if (Tcl_ListObjAppendElement(interp, resultList, Tcl_NewStringObj("-aliases", -1)) == TCL_ERROR) { return TCL_ERROR; } }
	if (objc == 0 || optAliases) {
		Tcl_Obj *tmpList;
		if (objc != 1) {
			tmpList = Tcl_NewListObj(0, (Tcl_Obj *const *) NULL);
		} else {
			tmpList = resultList;
		}
		for (i = 0; hostent->h_aliases[i] != NULL; i++) {
			if (Tcl_ListObjAppendElement(interp, tmpList, Tcl_NewStringObj(hostent->h_aliases[i], -1)) == TCL_ERROR) {
				return TCL_ERROR;
			}
		}
		if (objc != 1) {
			if (Tcl_ListObjAppendElement(interp, resultList, tmpList) == TCL_ERROR) { return TCL_ERROR; }
		}
	}

	if (objc == 0 || (objc > 1 && optDomain)) { if (Tcl_ListObjAppendElement(interp, resultList, Tcl_NewStringObj("-domain", -1)) == TCL_ERROR) { return TCL_ERROR; } }
	if (objc == 0 || optDomain) {
		if (Tcl_ListObjAppendElement(interp, resultList, Tcl_NewStringObj(Cep_StrCepDomain(SysDomainToCepDomain(hostent->h_addrtype)), -1)) == TCL_ERROR) {	return TCL_ERROR; }
	}

	if (objc == 0 || (objc > 1 && optLength)) { if (Tcl_ListObjAppendElement(interp, resultList, Tcl_NewStringObj("-length", -1)) == TCL_ERROR) { return TCL_ERROR; } }
	if (objc == 0 || optLength) {
		if (Tcl_ListObjAppendElement(interp, resultList, Tcl_NewIntObj(hostent->h_length)) == TCL_ERROR) { return TCL_ERROR; }
	}

	if (objc == 0 || (objc > 1 && optAddresses)) { if (Tcl_ListObjAppendElement(interp, resultList, Tcl_NewStringObj("-addresses", -1)) == TCL_ERROR) { return TCL_ERROR; } }
	if (objc == 0 || optAddresses) {
		Tcl_Obj *tmpList;
		if (objc != 1) {
			tmpList = Tcl_NewListObj(0, (Tcl_Obj *const *) NULL);
		} else {
			tmpList = resultList;
		}
		for (i = 0; hostent->h_addr_list[i] != NULL; i++) {
			if (inet_ntop(sysDomain, (const void *) hostent->h_addr_list[i], addrBuf, sizeof(addrBuf)) == NULL) {
				charPtr = "?";
			} else {
				charPtr = addrBuf;
			}
			if (Tcl_ListObjAppendElement(interp, tmpList, Tcl_NewStringObj(charPtr, -1)) == TCL_ERROR) { return TCL_ERROR; }
		}
		if (objc != 1) {
			if (Tcl_ListObjAppendElement(interp, resultList, tmpList) == TCL_ERROR) { return TCL_ERROR; }
		}
	}

	Tcl_SetObjResult(interp, resultList);

	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_CepRead_Cmd --
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
Cep_CepRead_Cmd (
	ClientData notUsed,		/* Not used. */
	Tcl_Interp *interp,		/* Current interpreter. */
	int objc,			/* Number of arguments. */
	Tcl_Obj *const objv[]		/* Argument objects. */
) {
	CepState *statePtr;
	Tcl_Channel chan;
	int len;


	if ((objc != 2) && (objc != 3)) {
		Tcl_WrongNumArgs(interp, 1, objv, "channelId ?varName?");
		return TCL_ERROR;
	}

	chan = Tcl_GetChannel(interp, Tcl_GetString(objv[1]), NULL);
	if (chan == (Tcl_Channel) NULL) {
		return TCL_ERROR;
	}

	if (strcmp(Tcl_ChannelName(Tcl_GetChannelType(chan)), "cep") != 0) {
		return Tcl_ReadObjCmd(notUsed, interp, objc, objv);
	}

	statePtr = (CepState *) Tcl_GetChannelInstanceData(chan);

	if (MASK2TYPE(statePtr->flags) == CEP_STREAM) {
		return Tcl_ReadObjCmd(notUsed, interp, objc, objv);
	}

	Tcl_ResetResult(interp);

	if ((MASK2TYPE(statePtr->flags) == CEP_DGRAM) || (MASK2TYPE(statePtr->flags) == CEP_RAW)) {
		Tcl_Obj *packetAndInfoListObj;
		Tcl_Obj *packetInfoListObj;
		Tcl_Obj *blob;


		if (Tcl_ListObjLength(NULL, statePtr->packetQueue, &len) == TCL_ERROR) {
			Tcl_Panic("ceptcl: impossible situation #0", "");
		}

		if (len == 0) {
			return TCL_OK;
		}

		if (Tcl_ListObjIndex(NULL, statePtr->packetQueue, 0, &packetAndInfoListObj) == TCL_ERROR) {
			Tcl_Panic("ceptcl: impossible situation #1", "");
		}
		if (Tcl_ListObjIndex(NULL, packetAndInfoListObj, 1, &blob) == TCL_ERROR) {
			Tcl_Panic("ceptcl: impossible situation #2", "");
		}
		if (objc == 3) {
			if (Tcl_ListObjIndex(NULL, packetAndInfoListObj, 0, &packetInfoListObj) == TCL_ERROR) {
				Tcl_Panic("ceptcl: impossible situation #3", "");
			}
			if (Tcl_ObjSetVar2(interp, objv[2], NULL, packetInfoListObj, TCL_LEAVE_ERR_MSG) == NULL) {
				return TCL_ERROR;
			}
		}
		Tcl_SetObjResult(interp, blob);
		Tcl_ListObjReplace(NULL, statePtr->packetQueue, 0, 1, 0, NULL);
	}

	return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Cep_GetCepType --
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
Cep_GetCepType (
	Tcl_Channel channel
) {
	return MASK2TYPE(((CepState *) Tcl_GetChannelInstanceData(channel))->flags);
}


/*
 *----------------------------------------------------------------------
 *
 * Cep_PassChan --
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
Cep_PassChan (
	Tcl_Channel channel,
	Tcl_Channel pchannel
) {
	CepState *statePtr;
	int passfd;
	int bytesSent;
	struct msghdr msg;
	struct iovec vec;
	struct cmsghdr *cmsg;
	union {
		struct cmsghdr hdr;
		unsigned char buf[CMSG_SPACE(sizeof(int))];
	} cmsgbuf;
	Tcl_DString chanInfo;
	Tcl_DString ds;
	int typeCh;
	int mode;
	int modeCh;
	int i;
	int j;
	int nfcopts = 1;

	const char *fconfigureStd[] = {"-blocking", "-buffering", "-buffersize", "-encoding", "-eofchar", "-translation", (char *) NULL};
	const char *fconfigureTty[] = {"-mode", "-xchar", (char *) NULL};
	const char **fconfigureOpts[2] = { fconfigureStd };

	statePtr = (CepState *) Tcl_GetChannelInstanceData(channel);

	if (Tcl_GetChannelHandle(pchannel, (TCL_READABLE | TCL_WRITABLE), (ClientData) &passfd) != TCL_OK) {
		return -1;
	}

	if ((typeCh = Cep_GetPassableChannelTypeChar(pchannel)) == -1) {
		return -1;
	}
	switch (typeCh) {
	case 't':
		fconfigureOpts[nfcopts++] = fconfigureTty;
		break;
	default:
		break;
	}

	mode = Tcl_GetChannelMode(pchannel);
	if ((mode & TCL_READABLE) && (mode & TCL_WRITABLE)) {
		modeCh = 'b';
	} else if (mode & TCL_READABLE) {
		modeCh = 'r';
	} else if (mode & TCL_WRITABLE) {
		modeCh = 'w';
	} else {
		modeCh = ' ';
	}

	Tcl_DStringInit(&chanInfo);
	Tcl_DStringAppend(&chanInfo, (unsigned char *)&typeCh, 1);
	Tcl_DStringAppend(&chanInfo, (unsigned char *)&modeCh, 1);

	if (typeCh == 'c') {
		int i;
		unsigned char buf[20]; /* 20 is a safe guess? */
		i = wvl(buf, statePtr->protocol);
		if (i > 20) { Tcl_Panic("Cep_PassChan: vlq too big (> 20) %d", i); }
		Tcl_DStringAppend(&chanInfo, buf, i);
		i = wvl(buf, statePtr->flags);
		if (i > 20) { Tcl_Panic("Cep_PassChan: vlq too big (> 20) %d", i); }
		Tcl_DStringAppend(&chanInfo, buf, i);
	}

	for (j = 0; j < nfcopts; j++) {
		for (i = 0; fconfigureOpts[j][i] != (const char *) NULL; i++) {
			Tcl_DStringInit(&ds);
			if (Tcl_GetChannelOption((Tcl_Interp *) NULL, pchannel, fconfigureOpts[j][i], &ds) == TCL_ERROR) {
				Tcl_DStringFree(&chanInfo);
				Tcl_DStringFree(&ds);
				return -1;
			}
			Tcl_DStringAppendElement(&chanInfo, fconfigureOpts[j][i]);
			Tcl_DStringAppendElement(&chanInfo, Tcl_DStringValue(&ds));
			Tcl_DStringFree(&ds);
		}
	}

	msg.msg_name       = NULL;
	msg.msg_namelen    = 0;
	msg.msg_iov        = &vec;
	msg.msg_iovlen     = 1;
	msg.msg_control    = &cmsgbuf.buf;
	msg.msg_controllen = sizeof(cmsgbuf.buf);
	msg.msg_flags      = 0;

	cmsg = CMSG_FIRSTHDR(&msg);
	cmsg->cmsg_len          = CMSG_LEN(sizeof(int));
	cmsg->cmsg_level        = SOL_SOCKET;
	cmsg->cmsg_type         = SCM_RIGHTS;
	*(int *)CMSG_DATA(cmsg) = passfd;

	vec.iov_base = (void *) Tcl_DStringValue(&chanInfo);
	vec.iov_len  = Tcl_DStringLength(&chanInfo)+1;  /* +1 for '\0' */

	bytesSent = sendmsg(statePtr->fd, &msg, 0);

	Tcl_DStringFree(&chanInfo);

	return bytesSent;
}


/* EOF */
