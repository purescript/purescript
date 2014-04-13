../dist/build/docgen/docgen prelude.purs > README.md
../dist/build/hierarchy/hierarchy prelude.purs | dot -Tpng -o typeclasses.png
pandoc -o ../docs/source/prelude.rst README.md
