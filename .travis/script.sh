#!/bin/sh

#copied from .travis.yml on the stack website:
# http://docs.haskellstack.org/en/stable/GUIDE/#travis-with-caching
set -ex
case "$BUILD" in
	stack)
		stack --no-terminal $ARGS test --bench --no-run-benchmarks --haddock --no-haddock-deps
		;;
	cabal)
		cabal install --enable-tests --enable-benchmarks --force-reinstalls --ghc-options=-O0 --reorder-goals --max-backjumps=-1 $CABALARGS $PACKAGES

		ORIGDIR=$(pwd)
		for dir in $PACKAGES
		do
			cd $dir
			cabal check || [ "$CABALVER" == "1.16" ]
			cabal sdist
			SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz && \
				(cd dist && cabal install --force-reinstalls "$SRC_TGZ")
			cd $ORIGDIR
		done
		;;
esac
set +ex
