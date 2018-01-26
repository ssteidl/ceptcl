package require ceptcl

proc z {args} {puts $args}
set srvs(-) {}
proc recvcmd {cep addr port datalen data} {
    global srvs
puts moo
    if {![info exists srvs($addr,$port)]} {
	foreach {c1 c2} [cep] { break }
	set srvs($addr,$port) [list $c1 $c2 $cep]
	fconfigure $c1 -buffering none -blocking 0 -translation binary -encoding binary
	fileevent $c1 readable [list writer $c1 $cep $addr $port]
	srvcmd $c2 $addr $port
    }
    puts -nonewline [lindex $srvs($addr,$port) 0] $data
}




proc writer {cep1 out addr port} {
    sendto $out $addr $port [read $cep1]
}

proc reader {cep} {
    puts $cep

    puts [read $cep]
    if {[eof $cep]} {
	close $cep
    }
}


proc srvcmd {chan addr port} {
    fconfigure $chan -buffering none -blocking 0 -translation binary -encoding binary
    fileevent $chan readable [list reader $chan]
}

cep -receiver recvcmd 11253

vwait plop
