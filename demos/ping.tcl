#!/bin/sh
# \
exec tclsh8.4 "$0" ${1+"$@"}

#
# ICMP ECHO - 'ping' implementation using ceptcl
#
# 2004 Stuart Cassoff
#

if {$argc < 1 || $argc > 3} {
    puts "Usage: $argv0 \[-t\] host \[port\]"
    exit
}

package require ceptcl
package require packet
package require netutil


namespace eval ping {    
    variable done 0
    variable tr 0
    variable peer {}
    variable port 0
    variable afterId {}

    proc rcvpkt {c} {
	variable done
	variable tr
	variable afterId
	variable peer
	variable port
	variable ippkt
	variable in_ippkt
	variable in_icmppkt

	after cancel $afterId

	set pkt [read $c]
	::packet::ip_pkt2arr [namespace which -variable in_ippkt] $pkt

	if {[::packet::inet_sum32 $pkt] == 0} {
	    #puts "ip sum ok"
	} else {
	    puts "ip sum nogood"
	}
	if {[::packet::inet_sum32 $in_ippkt(data)] == 0} {
	    #puts "icmp sum ok"
	} else {
	    puts "icmp sum nogood"
	}

	::packet::icmp_pkt2arr [namespace which -variable in_icmppkt] $in_ippkt(data)

	if ${tr} {
	    puts "$in_ippkt(sourceaddress)"
	    if {$in_icmppkt(type) == 11} {
		incr ippkt(ttl)
		set ip [::packet::ip_arr2pkt [namespace which -variable ippkt]]
		set afterId [after 20000 [list set [namespace which -variable done] 2]]
		after 1 [list sendto $c $peer $port $ip]
		return
	    }
	} else {
	    puts "Got reply from $in_ippkt(sourceaddress)"
	}
	set done 1
    }

    set port 24368
    set peername [lindex $argv 0]
    if {[string index $peername 0] eq "-"} {
	if {$peername eq "-t"} {
	    set tr 1
	} else {
	    puts "Usage: $argv0 \[-t\] host \[port\]"
	    exit
	}
	set peername [lindex $argv 1]
    }
    if {$tr} {
	if {$argc == 3} {
	    set port [lindex $argv 2]
	}
    } else {
	if {$argc == 2} {
	    set port [lindex $argv 1]
	}
    }

    if {1} {
	# cheat and let the system resolve the name
	set c [cep -type raw -protocol icmp $peername $port]
	set ippkt(sourceaddress) [lindex [fconfigure $c -sockname] 0]
	set peer [set ippkt(destaddress) [lindex [fconfigure $c -peername] 0]]
	close $c
    }

    if {$tr} {
	puts "traceroute to $peername ($peer)"
    } else {
	puts "pinging $peername ($peer), port $port"
    }


    set icmppkt(type) 8
    set icmppkt(code) 0
    set icmppkt(identifier) 42
    set icmppkt(sequence) 12
    set icmppkt(data) "Hi! This is a test of some sockety code I'm working on. Sorry to bother you."
    set pkt [::packet::icmp_arr2pkt [namespace which -variable icmppkt]]

    set ippkt(flags)          0
    set ippkt(id)             1234
    set ippkt(offset)         0
    set ippkt(options)        ""
    set ippkt(protocol)       1
    set ippkt(tos)            0
    set ippkt(version)        4
    if {$tr} {
	set ippkt(ttl) 1
    } else {
	set ippkt(ttl) 255
    }
    set ippkt(data) $pkt
    set ip [::packet::ip_arr2pkt [namespace which -variable ippkt]]

    if {1} {
	set c [cep -type raw -protocol icmp {} -1]
	fconfigure $c -blocking 0 -buffering none -translation binary -encoding binary
	fconfigure $c -header 1
	fileevent $c readable [list [namespace which -command rcvpkt] $c]

	set afterId [after 20000 [list set [namespace which -variable done] 2]]
	after 1 [list sendto $c $peer 1 $ip]
	#after 1 [list puts -nonewline $c $ip]
	vwait [namespace which -variable done]
	if {$done == 2} {
	    puts "timeout"
	}
    }
}

# EOF
