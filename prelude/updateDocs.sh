../dist/build/psc-docs/psc-docs --hierarchy-images prelude.purs > README.md
../dist/build/psc-hierarchy/psc-hierarchy prelude.purs -o prelude

mkdir -p images

for f in prelude/*; do
    BASE=$(basename "$f")
    dot -Tpng -o images/"$BASE".png "$f"
done

rm -r prelude
