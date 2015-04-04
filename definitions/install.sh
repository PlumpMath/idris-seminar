o=git://github.com/idris-lang/Idris-dev
git clone $o
cd Idris-dev
cabal sandbox init
cabal update
cabal install --only-dependencies
make
