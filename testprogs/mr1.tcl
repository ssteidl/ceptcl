# Multicast receiver 1

package require ceptcl

set maddr 232.3.2.3
set mport 23232

proc reader {cep} {
	set d [cepRead $cep pip]
	puts $d
puts $pip
	if {$d eq {end}} {
		set ::plop 1
	}
}

set c [cep -type datagram -myaddr $maddr -myport $mport {} -1]
fconfigure $c -blocking 0
#fconfigure $c -join [list]
fconfigure $c -join [list $maddr 127.0.0.1]


puts {Receiver (1):}
puts [fconfigure $c]

fileevent $c readable [list reader $c]

vwait plop


# EOF
