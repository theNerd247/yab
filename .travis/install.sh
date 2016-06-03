#!/bin/sh

#copied from .travis.yml on the stack website:
# http://docs.haskellstack.org/en/stable/GUIDE/#travis-with-caching

echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
if [ -f configure.ac ]; then autoreconf -i; fi

set -ex
case "$BUILD" in
	stack)
		stack --no-terminal --install-ghc $ARGS test --bench --only-dependencies
		;;
	cabal)
		cabal --version
		travis_retry cabal update
		cabal install --only-dependencies --enable-tests --enable-benchmarks --force-reinstalls --ghc-options=-O0 --reorder-goals --max-backjumps=-1 $CABALARGS $PACKAGES
		;;
esac
set +ex


