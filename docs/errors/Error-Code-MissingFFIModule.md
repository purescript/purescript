This error means that a foreign module could not be found. Make sure you have specified the required foreign files (`--ffi`) correctly.

For example, say `psc` can't find the Prelude module (in versions 0.7.0 and later) but you have already installed it with `bower install purescript-prelude`. Then you could call `psc` with something like

    $(npm bin)/psc --ffi 'bower_components/purescript-*/src/**/*.js' \
      'bower_components/purescript-*/src/**/*.purs' mymodule.purs

in order to let `psc` find the installed module's `.js` files.