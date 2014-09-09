CABAL=/home/mgaut72/.cabal/bin/cabal --config-file=/home/mgaut72/.cabal/config

compile:
	${CABAL} sandbox init && ${CABAL} install

clean:
	${CABAL} clean
