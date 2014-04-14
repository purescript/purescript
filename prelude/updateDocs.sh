../dist/build/docgen/docgen prelude.purs > README.md
mkdir -p images
../dist/build/hierarchy/hierarchy prelude.purs | dot -Tpng -o images/Prelude.png
pandoc -o ../docs/source/prelude.rst README.md
