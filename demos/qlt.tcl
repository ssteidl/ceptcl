

package require ceptcl

proc q {sock host port} {set ::s0 $sock}

set s0 {}
set s1 [cep -domain local -server q herring]
set s2 [cep -domain local herring]

vwait ::s0


puts "server $s1: [fconfigure $s1]"
puts "server accept $s0: [fconfigure $s0]"
puts "client $s2: [fconfigure $s2]"

close $s2
close $s1
