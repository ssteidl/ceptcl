# Multicast sender 1

package require ceptcl

set maddr 232.3.2.3
set mport 23232

set c [cep -type datagram {} -1]

#set c [cep -type datagram $maddr $mport 127.0.0.1 12345]

#fconfigure $c -route 0
#fconfigure $c -maddr 192.168.2.2
fconfigure $c -maddr 127.0.0.1
#fconfigure $c -mhops 42
puts {Sender (1):}
puts [fconfigure $c]
	
for {set i 0} {$i < 10} {incr i} {
	cepPuts $c $maddr $mport $i
#	puts -nonewline $c $i
#	flush $c
	puts -nonewline .
	flush stdout
	after 1500
}

cepPuts $c $maddr $mport end


# EOF
