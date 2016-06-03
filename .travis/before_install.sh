#!/bin/sh

#copied from .travis.yml on the stack website:
# http://docs.haskellstack.org/en/stable/GUIDE/#travis-with-caching

# Using compiler above sets CC to an invalid value, so unset it
unset CC

# We want to always allow newer versions of packages when building on GHC HEAD
CABALARGS=""
if [ "x$GHCVER" = "xhead" ]; then CABALARGS=--allow-newer; fi

# Download and unpack the stack executable
export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$HOME/.local/bin:/opt/alex/$ALEXVER/bin:/opt/happy/$HAPPYVER/bin:$HOME/.cabal/bin:$PATH
mkdir -p ~/.local/bin 

if [ `uname` = "Darwin" ]
then
	travis_retry curl --insecure -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
else
	travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
fi

# Use the more reliable S3 mirror of Hackage
mkdir -p $HOME/.cabal
echo 'remote-repo: hackage.haskell.org:http://hackage.fpcomplete.com/' > $HOME/.cabal/config
echo 'remote-repo-cache: $HOME/.cabal/packages' >> $HOME/.cabal/config

if [ "$CABALVER" != "1.16" ]
then
	echo 'jobs: $ncpus' >> $HOME/.cabal/config
fi

# Get the list of packages from the stack.yaml file
export PACKAGES=$(stack --install-ghc query locals | grep '^ *path' | sed 's@^ *path:@@')
