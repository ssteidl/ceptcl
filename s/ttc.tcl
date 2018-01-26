#package require ceptcl

set c [socket localhost 12345]

puts $c boing
flush $c
