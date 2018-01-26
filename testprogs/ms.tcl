package require ceptcl

set c [cep -type datagram {} -1]
	
while {1} {
	cepPuts $c 127.0.0.1 12345 moo
	puts -nonewline .
	flush stdout
	after 1500
}

