# Pass channel client

package require ceptcl


proc feh {cep} {
	if {[eof $cep]} {
		exit
	}
	puts ![cepRead $cep]!
}

proc recvchan {chan type ids} {
	puts "received chan: $chan  type: $type"
	puts [fconfigure $chan]
	puts $chan hello
	flush $chan
}

set c [cep -domain local -chanrecv recvchan localcep]
fconfigure $c -blocking 0 -buffering none
fileevent $c r [list feh $c]
puts [fconfigure $c]
puts $c moo
after 3000 [list puts $c c]
vwait vwait


# EOF
