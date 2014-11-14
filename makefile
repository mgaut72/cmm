CABAL=/home/mgaut72/.cabal/bin/cabal --config-file=/home/mgaut72/.cabal/config

compile:
	${CABAL} install --bindir=${PWD}

clean:
	${CABAL} clean; rm -f compile
