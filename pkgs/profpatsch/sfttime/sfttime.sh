#!/usr/bin/env bash
# this script was created in 3BBE.81[sft].
# usage:
# to convert to sfttime:
#	$0 [c date] [digitcount] [nodate]
#	if date is given, converts the given date to sfttime.
#	if date is not given, converts the current date to sfttime.
#	digitcount specifies the accuracy for the time part.
#	nodate hides the date part.
# to convert from sfttime:
#	$0 r sfttime [unix]
#	converts the given sfttime to 'standard' time.
#	if 'unix' is provided, the output will be in unix time.
# to show info about sfttime units
#	$0 i [[sft]]$num
#	displays name of unit [sft]$num, as well as it's value
#	in both days and 'standard' units.

case $1 in
	"c")
		unixtime=$(date --date="$2" +%s.%N)
		shift
		shift
		mode=fw
		;;
	"r")
		shift
		sfttime=$1
		if [[ $sfttime =~ ^([0-9A-F]*(.[0-9A-F]+)?)(\[[sS][fF][tT]\])?$ ]] && [[ $sfttime ]]; then
			sfttime=${BASH_REMATCH[1]}
		else
			echo "error" 2>&1
			exit 1
		fi
		shift
		mode=bw
		;;
	"i")
		shift
		inforeq=$1
		if [[ $inforeq =~ ^(\[[sS][fF][tT]\])?(-?[0-9]+)$ ]]; then
			inforeq=${BASH_REMATCH[2]}
			let inforeq=$inforeq
			mode=in
		elif [[ $inforeq =~ ^(\[[sS][fF][tT]\])?[eE][pP][oO][cC][hH]$ ]]; then
			echo "[sft]epoch:"
			echo "unix time 49020"
			echo "1970-01-01 13:37:00 UTC"
			exit 0
		else
			echo "error" 2>&1
			exit 1
		fi
		shift
		mode=in
		;;
	*)
		unixtime=$(date +%s.%N)
		mode=fw
		;;
esac

case $mode in
	"fw")
		sfttime=$(echo "obase=16; ($unixtime-49020)/86400" | bc -l)
		if [[ $1 -ge 1 ]]; then
			digits=$1
			shift
		elif [[ ! $1 ]] || [[ $1 == nodate ]]; then
			digits=3
		else
			digits=0
		fi

		if [[ $sfttime =~ ^([0-9A-F]+)[.]([0-9A-F]{$digits}).*$ ]]; then
			date=${BASH_REMATCH[1]}
			time=${BASH_REMATCH[2]}
		else
			echo "Error" &1>2
			exit 1
		fi

		if [[ $digits -eq 0 ]]; then
			echo "$date[sft]"
		else
			if [[ $1 == nodate ]]; then
				echo ".$time[sft]"
				shift
			else
				echo "$date.$time[sft]"
			fi
		fi
		;;
	"bw")
		unixtime=$(echo "ibase=16; $sfttime*15180+BF7C" | bc -l)
		case $1 in
			unix)
				shift
				echo $unixtime
				;;
			*)
				date --date="1970-01-01 $unixtime sec"
				;;
		esac
		;;
	"in")
		name="[sft]$inforeq"
		case $inforeq in
      -4) newname="[sft]tick";;
			-3)	newname="[sft]tentacle";;
			-2)	newname="[sft]schinken";;
			-1)	newname="[sft]major";;
			0)	newname="day";;
			1)	newname="[sft]vergil";;
			2)	newname="[sft]stallman";;
			3)	newname="[sft]odin";;
		esac
		if [[ $newname ]]; then
			echo "alternative name for $name: $newname"
			name="$newname"
		fi
		one="1 $name"
		echo "$one after [sft]epoch:"
		sfttime=$(echo "obase=16; 16^$inforeq" | bc -l)[sft]
		echo $sfttime
		echo "time equivalent of $one:"
		echo "the duration of $(echo 794243384928000*16^$inforeq | bc -l) periods of the radiation corresponding to the transition between the two hyperfine levels of the ground state of the caesium 133 atom"

		echo "standard time units equivalent:"
		seconds=$(echo "86400*16^$inforeq" | bc -l)
		if [[ $(echo "$seconds < 60" | bc -l) == 1 ]]; then
			echo "$seconds seconds"
		elif [[ $(echo "$seconds < 3600" | bc -l) == 1 ]]; then
			echo "$(echo $seconds/60 | bc -l) minutes"
		elif [[ $(echo "$seconds < 86400" | bc -l) == 1 ]]; then
			echo "$(echo $seconds/3600 | bc -l) hours"
		elif [[ $(echo "$seconds < 86400*365.2425" | bc -l) == 1 ]]; then
			echo "$(echo $seconds/86400 | bc -l) days"
		else
			echo "$(echo $seconds/86400/365.2425 | bc -l) years"
		fi
		;;
esac
