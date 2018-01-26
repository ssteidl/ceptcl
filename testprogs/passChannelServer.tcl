# Pass channel server

package require ceptcl


proc srv {cep addr id} {
	fconfigure $cep -blocking 0 -buffering none
	fileevent $cep r [list feh $cep]
}

proc feh {cep} {
	set d [cepRead $cep]
	if {[eof $cep]} {
		close $cep
#		set ::vwait 1
		return
	}
	if {0 && $d eq {c}} {
		#flush stdout
		set q stdout
		fconfigure $q -encoding utf-8 -blocking 0 -buffering none -translation binary -mode 9600,n,8,1
		puts [fconfigure $q]
		flush stdout
		cepPassChannel $cep $q
	}	
	if {1 && $d eq {c}} {
		lassign [cep] here there
		fconfigure $here -blocking 0 -buffering none
		fconfigure $there -blocking 0 -buffering none
		fileevent $here r [list cow $here]
		puts [fconfigure $there]
		cepPassChannel $cep $there
		close $there
	}	
}


proc cow {cep} {
	set d [cepRead $cep]
	if {[eof $cep]} {
		close $cep
		set ::vwait 1
		return
	}
	puts $d
}

cep -domain local -server srv localcep
vwait vwait


# EOF
