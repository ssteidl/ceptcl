package require ceptcl

proc moo {args} {
	puts $args
	#puts [read [lindex $args 0]]
#	puts [readpacket [lindex $args 0]]
#set ::plop 1
}


set c [cep -type datagram -myport 12345 {} -1]
fconfigure $c -blocking 0
fconfigure $c -header 1
puts [fconfigure $c]
fileevent $c readable [list moo $c]


vwait plop
