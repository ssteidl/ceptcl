package require ceptcl

set c [cep -type datagram localhost 12345]

puts $c boing
flush $c

