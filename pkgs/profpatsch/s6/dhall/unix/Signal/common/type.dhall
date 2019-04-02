let POSIX1990 =
	  { SIGHUP :
		  Natural
	  , SIGINT :
		  Natural
	  , SIGQUIT :
		  Natural
	  , SIGILL :
		  Natural
	  , SIGABRT :
		  Natural
	  , SIGFPE :
		  Natural
	  , SIGKILL :
		  Natural
	  , SIGSEGV :
		  Natural
	  , SIGPIPE :
		  Natural
	  , SIGALRM :
		  Natural
	  , SIGTERM :
		  Natural
	  , SIGUSR1 :
		  Natural
	  , SIGUSR2 :
		  Natural
	  , SIGCHLD :
		  Natural
	  , SIGCONT :
		  Natural
	  , SIGSTOP :
		  Natural
	  , SIGTSTP :
		  Natural
	  , SIGTTIN :
		  Natural
	  , SIGTTOU :
		  Natural
	  }

let POSIX2001 =
	  { SIGBUS :
		  Natural
	  , SIGPOLL :
		  Natural
	  , SIGPROF :
		  Natural
	  , SIGSYS :
		  Natural
	  , SIGTRAP :
		  Natural
	  , SIGURG :
		  Natural
	  , SIGVTALRM :
		  Natural
	  , SIGXCPU :
		  Natural
	  , SIGXFSZ :
		  Natural
	  }

in  POSIX1990 ⩓ POSIX2001