../dist/build/psc-docs/psc-docs --hierarchy-images \
  modules/Prelude.purs \
  modules/Data.Function.purs \
  modules/Prelude.Unsafe.purs \
  modules/Control.Monad.Eff.Unsafe.purs \
  modules/Debug.Trace.purs \
  modules/Control.Monad.ST.purs \
  > README.md
../dist/build/hierarchy/hierarchy modules/Prelude.purs -o prelude

mkdir -p images

for f in prelude/*; do
    BASE=$(basename "$f")
    dot -Tpng -o images/"$BASE".png "$f"
done

rm -r prelude
