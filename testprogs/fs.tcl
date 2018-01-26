package require ceptcl

proc moo {cep addr id} {
puts !!$addr
	fconfigure $cep -blocking 0 -buffering none
	fileevent $cep r [list boing $cep]
}
proc boing {cep} {
	puts [set d [cepRead $cep]]
	if {[eof $cep]} {close $cep; return }
	if {$d eq {c}} {
#set q  [lindex [cep] 0]
puts passing
flush stdout
#set q [open jdjdjd w+]
set q stdout
fconfigure $q -encoding utf-8 -blocking 0 -buffering none -translation binary
puts [fconfigure $q]
flush stdout
		cepPassChannel $cep $q
	}	
}
set c [cep -domain local -server moo herring]
puts $c
puts [fconfigure $c]
#fileevent $c r boing
vwait t
#fconfigure $c -buffering none

#cepPuts $c localhost 12345 moo

