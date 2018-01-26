package require ceptcl
close stdin
foreach {newstdin tonewstdin} [cep] {break}
puts $tonewstdin moo
flush $tonewstdin
puts [gets stdin]
