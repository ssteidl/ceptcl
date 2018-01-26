package require ceptcl

proc moo {args} {
	puts $args
	#puts [read [lindex $args 0]]
#puts [fconfigure [lindex $args 0]]
		puts [readpacket [lindex $args 0]]
set ::plop 1
}

set c [cep -type datagram 127.0.0.1 12345]
#fconfigure $c -join 224.0.0.250
#fconfigure $c -blocking 0
#fconfigure $c -header 1
#puts [fconfigure $c]
fileevent $c readable [list moo $c]


vwait plop

