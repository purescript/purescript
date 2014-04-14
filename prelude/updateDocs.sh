../dist/build/docgen/docgen prelude.purs > README.md
../dist/build/hierarchy/hierarchy prelude.purs -o prelude
mkdir -p images
for f in prelude/*; do
    BASE=$(basename "$f")
    dot -Tpng -o images/"$BASE".png "$f"
done
rm -r prelude
pandoc -o ../docs/source/prelude.rst README.md
