#package require udp
package require ceptcl

array set ::db {0 ""}
set ::db(linuxsys.net,soa) {dns2.sr-tech.com}
set ::db(linuxsys.net,email) {hostmaster.sr-tech.com}
set ::db(linuxsys.net,serial) 2002032103
set ::db(linuxsys.net,refresh) 1800
set ::db(linuxsys.net,retry) 3600
set ::db(linuxsys.net,expire) 2400
set ::db(linuxsys.net,ttl) 3600
set ::db(linuxsys.net,NS) {dns.sr-tech.com dns3.sr-tech.com}
set ::db(linuxsys.net,MX) {{5 mail.sr-tech.com} {10 mail2.sr-tech.com}}
set ::db(linuxsys.net,A,) {69.3.36.102}
set ::db(linuxsys.net,A,www) {69.3.36.98}
set ::db(linuxsys.net,A,mail) {69.3.36.99}
set ::db(linuxsys.net,A,test) {69.3.36.100}
set ::db(linuxsys.net,CNAME,dns) {ns.sr-tech.com}
set ::db(linuxsys.net,CNAME,dns2) {dns2.sr-tech.com}
set ::db(linuxsys.net,PTR) ""
set ::db(linuxsys.net,HINFO,www) {{1.1ghz Intel P4} {SR Tech Linux}}

proc cvtname {dnsname} {
    #convert from binary dns hostname format to readable string
    set result ""
    set chr 0

    while {$chr < [string length $dnsname]} {
	binary scan [string index $dnsname $chr] c* groupcnt
	if {$chr > 0} {set dnsname [string replace $dnsname $chr $chr "."]}
	incr chr +$groupcnt
	incr chr
    }
    return [string range $dnsname 1 end]
}
proc cvthostnamedns {hostname} {
    #convert from text hostname format to dns binary format
    set result ""
    set hostname [split $hostname "."]
    foreach part $hostname {
	append result "[format %c [string length $part]]$part"
    }
    return $result
}

proc cvtaddressdns {address} {
    #convert from text dotted address format to dns binary format
    set result ""
    set address [split $address "."]
    foreach octet $address {
	append result "[format %c $octet]"
    }
    return $result
}

proc process_dns {host port pkt} {
    #[binary format H* $reply]
    binary scan $pkt H* cvt_data
    #puts $cvt_data
    set transid [string range $cvt_data 0 3]
    set params  [string range $cvt_data 4 7]
    set quests  [string range $cvt_data 8 11]
    set answers [string range $cvt_data 12 15]
    set authors [string range $cvt_data 16 19]
    set addits  [string range $cvt_data 20 23]
    set queryname  [string range $cvt_data 24 end-8]
    set querytype  [string range $cvt_data end-7 end-4]
    set queryclass [string range $cvt_data end-3 end]

    switch $querytype {
	0001  {set qtype A}
	0002  {set qtype NS}
	0005  {set qtype CNAME}
	000c  {set qtype PTR}
	000d  {set qtype HINFO}
	000f  {set qtype MX}
	default {set qtype error}
    }

    set qname [split [cvtname [string range $pkt 12 end-4]] "."]
    set llen [llength $qname]
    set qhost   [string map {" " .} [lrange $qname 0 end-3]]
    set qdomain [string map {" " .} [lrange $qname end-2 end-1]]

    puts "Rec query from $host on port $port for host $qhost at $qdomain type $qtype"

    set answer "0001"
    set author "0000"
    set params "8180" ; #this indicates a successful query

    set class $queryclass
    set type $querytype

    #heres all the brains of the lookup
    set error 0
    if {! [info exists ::db($qdomain,soa)]} {
	#domain doesnt exist, so resolve elsewhere if
	set params "8181"; #this indicates an error
	set error 1
	set answer "0000"
	puts "error: no domain entry found"
    }

    if {! [info exists ::db($qdomain,$qtype,$qhost)] && $qtype != "NS" && $qtype != "MX"} {
	#host doesnt exist, so return an error reply in bits 13-16=0001
	set params "8181"; #this indicates an error
	set error 1
	set answer "0000"
	puts "error: no host entry found"
    }

    set resp_answer ""
    set recindex 0
    while {! $error} {
	#return a response for each record found

	switch $qtype {
	    A     {set lookup [cvtaddressdns  [lindex $::db($qdomain,$qtype,$qhost) $recindex]]}
	    MX    {set mxprefix [lindex [lindex $::db($qdomain,$qtype) $recindex] 0]
		set lookup [cvthostnamedns [lindex [lindex $::db($qdomain,$qtype) $recindex] 1]]
	    }
	    NS    {set lookup [cvthostnamedns [lindex $::db($qdomain,$qtype) $recindex]]}
	    CNAME {set lookup [cvthostnamedns [lindex $::db($qdomain,$qtype,$qhost) $recindex]]}
	    HINFO {set lookup [lindex $::db($qdomain,$qtype,$qhost) $recindex]}
	}
	#test for no more records, then break to sent accumulated response
	if {$lookup == ""} {break}

	set len [string length $lookup]
	binary scan $lookup H* hexlookup

	switch $qtype {
	    A     {set data $hexlookup}
	    NS    {set data "${hexlookup}00"
		incr len
	    }
	    CNAME {set data "${hexlookup}00"
		incr len
	    }
	    MX    {set data "[format %04x $mxprefix]${hexlookup}00"
		incr len +3
	    }
	    HINFO {incr recindex
		set data "[format %02x $len]${hexlookup}"
		set lookup [lindex $::db($qdomain,$qtype,$qhost) $recindex]
		set len [string length $lookup]
		binary scan $lookup H* hexlookup
		append data "[format %02x $len]${hexlookup}00"
		set len [expr [string length $data] / 2 -1]
	    }
	}

	set len [format %04x $len]
	set ttl [format %08x $::db($qdomain,ttl)]
	#puts "${type} ${class} ${ttl} $len $data"
	append resp_answer "C00C${type}${class}${ttl}${len}${data}"
	incr recindex
	set answer [format %04x $recindex]
    }

    set resp_author ""
    set resp_addit  ""
    set response "[string range $cvt_data 0 3]${params}[string range $cvt_data 8 11]${answer}${author}[string range $cvt_data 20 end]"
    udp_puts $host $port [binary format H* ${response}${resp_answer}${resp_author}${resp_addit}]

}

# Send data to a remote UDP socket
proc udp_puts {host port data} {
    udp_conf $::sock $host $port
    puts -nonewline $::sock $data
}

proc udpEventHandler {sock} {
    set pkt [read $sock]
    set peer [udp_conf $sock -peer]
    process_dns [lindex $peer 0] [lindex $peer 1] $pkt
    return
}

proc udp_listen {port} {
    set srv [udp_open $port]
    fconfigure $srv -buffering none -translation binary
    fileevent $srv readable [list ::udpEventHandler $srv]
    puts "Listening on udp port: [udp_conf $srv -myport]"
    return $srv
}

set ::sock [udp_listen 53]
vwait forever
close $::sock
