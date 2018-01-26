

package require ceptcl 0.4

proc r {cep} {
	puts $cep
	puts [read $cep]
}

set c [cep -type datagram -myport 12345 {} -1]
fconfigure $c -blocking 0
puts [fconfigure $c]
fileevent $c r [list r $c]
vwait v
