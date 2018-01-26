package require ceptcl

proc moo {args} {
global count
	puts $args
	#puts [read [lindex $args 0]]
puts [fconfigure [lindex $args 0]]
#close [lindex $args 0]
	if {[incr count] > 3} {
#		puts [readpacket [lindex $args 0]]
fileevent [lindex $args 0] r {}
		set count 0
	}
#set ::plop 1
}

set count 0
set c [cep -type datagram -myport 12345 {} -1]
#fconfigure $c -blocking 0
#fconfigure $c -header 1
#puts [fconfigure $c]
fileevent $c readable [list moo $c]


vwait plop

