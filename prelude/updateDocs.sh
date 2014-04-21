../dist/build/docgen/docgen --hierarchy-images prelude.purs > README.md
../dist/build/hierarchy/hierarchy prelude.purs -o prelude
mkdir -p images
mkdir -p ../docs/source/images
for f in prelude/*; do
    BASE=$(basename "$f")
    dot -Tpng -o images/"$BASE".png "$f"
    cp images/"$BASE".png ../docs/source/images/"$BASE".png
done
rm -r prelude
pandoc -o ../docs/source/prelude.rst README.md
