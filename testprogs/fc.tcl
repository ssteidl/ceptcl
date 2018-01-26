package require ceptcl

proc moo {cep} {
	if {[eof $cep]} { exit }
	puts [cepRead $cep]
}

proc plop {args} {

puts [lindex $args 0] moo!
#	puts [fconfigure [lindex $args 0]]
}

set c [cep -domain local -chanrecv plop herring]
fconfigure $c -blocking 0 -buffering none
fileevent $c r [list moo $c]
puts [fconfigure $c]
puts $c moo
after 3000 [list puts $c c]
vwait t


