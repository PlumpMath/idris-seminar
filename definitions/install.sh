git clone git://github.com/idris-lang/Idris-dev
cd Idris-dev
cabal sandbox init
cabal update
cabal install --only-dependencies
make
