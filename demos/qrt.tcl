package require ceptcl
proc q {args} {puts $args ; set ::h 1}
if {1} {
    set r [cep -domain local -receiver q qx]
    set c [cep -domain local -type datagram qx]
} else {
    set r [cep -domain inet6 -receiver q 12345]
    set c [cep -domain inet6  -type datagram localhost 12345]
}
puts -nonewline $c "hello"
flush $c
vwait h