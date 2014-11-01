CABAL=/home/mgaut72/.cabal/bin/cabal --config-file=/home/mgaut72/.cabal/config

compile: sandbox
	${CABAL} install --bindir=${PWD}

sandbox:
	${CABAL} sandbox init


clean:
	${CABAL} clean; rm -f compile
