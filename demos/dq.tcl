#! /bin/sh
# \
    exec tclsh8.4 "$0" ${1+"$@"}

# dq - quick & dirty dns query using resolv from tcllib
# and a slightly modified dns from tcllib
# and ceptcl!
#
# 2004 Stuart Cassoff
#

if {$argc != 1} {
    puts "Usage: $argv0 hostname"
    return 1
}
source dns.tcl
source /usr/local/lib/tcllib/dns/resolv.tcl
catch {resolv::resolve [lindex $argv 0]} msg
puts $msg

# EOF
