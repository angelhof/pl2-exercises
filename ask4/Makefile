
enable_prof:
	cabal configure --enable-executable-profiling ; echo "Add this to ask1.cabal \"-with-rtsopts=-p -s -h -i0.1\" "

disable_prof:
	cabal configure --disable-executable-profiling

profile:
	cat tests/test_g2.in | cabal run && hp2ps -e8in -c typeInf.hp
