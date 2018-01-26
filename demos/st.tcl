


package require ceptcl


proc r {args} {
    puts $args
}


cep -type datagram -receiver r -myaddr 192.168.2.255

vwait forever

