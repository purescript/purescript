../dist/build/docgen/docgen prelude.purs > README.md
pandoc -o ../docs/source/prelude.rst README.md
