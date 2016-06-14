
# make install with user-defined PREFIX is broken ...

set -x
#D=/tmp/local
D=$HOME/x/local

mkdir -p $D/bin
mkdir -p $D/lib
mkdir -p $D/lib/perl5

cp -f bin/cilly* $D/bin
cp -fRv lib/App $D/lib/perl5/
