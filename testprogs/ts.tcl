package require ceptcl

proc moo {args} {
	puts **$args

#	puts [readpacket [lindex $args 0]]
#	puts ![read [lindex $args 0]]!
#set ::plop 1
}


proc oom {cep add port} {
#	fconfigure $cep -blocking 0
	fileevent $cep readable [list moo $cep]

}

set c [cep -server oom 12345]
#fconfigure $c -blocking 0
#fconfigure $c -header 1
#puts [fconfigure $c]
#fileevent $c readable [list moo $c]


vwait plop
