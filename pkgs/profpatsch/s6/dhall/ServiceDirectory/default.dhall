let Signal = ./../imports/Signal.dhall

in    { finish =
		  None Text
	  , up =
		  True
	  , setid =
		  True
	  , notification-fd =
		  None Natural
	  , timeout-kill =
		  0
	  , timeout-finish =
		  5000
	  , max-death-tally =
		  100
	  , down-signal =
		  Signal.SIGTERM
	  }
	: ./default-type.dhall