mkdir -p /tmp/build/purescript/

strip ../dist/build/psc/psc
strip ../dist/build/psci/psci
strip ../dist/build/psc-make/psc-make
strip ../dist/build/docgen/docgen

cp ../dist/build/psc/psc /tmp/build/purescript/
cp ../dist/build/psci/psci /tmp/build/purescript/
cp ../dist/build/psc-make/psc-make /tmp/build/purescript/
cp ../dist/build/docgen/docgen /tmp/build/purescript/

cp ../prelude/prelude.purs /tmp/build/purescript/

cp install.sh /tmp/build/purescript/

pushd /tmp/build

tar -zcvf latest.tar.gz purescript

popd

cp /tmp/build/latest.tar.gz .

shasum latest.tar.gz > latest.sha

rm -rf /tmp/build/
