Getting Started
===============

Compiler Installation
---------------------

If you have the Haskell Plaform installed, then you can install the latest released version from Hackage::

  cabal update
  cabal install purescript

If you would like to build the latest version of the code, clone this repository and build::

  git clone git://github.com:paf31/purescript.git
  cabal configure --enable-tests
  cabal build
  cabal test
  cabal install

Alternatively, consider installing PureScript in a Cabal sandbox using ``cabal sandbox init``.

Creating a new PureScript project
---------------------------------

You can create a new PureScript project using ``grunt-init`` and `this PureScript project template <https://github.com/purescript-contrib/grunt-init-purescript>`_. This will give you a project that uses `Grunt <http://gruntjs.com>`_ as its build tool and `Bower <http://bower.io>`_ for dependency management. See the `README <https://github.com/purescript-contrib/grunt-init-purescript>`_ for step-by-step instructions.

Compiler usage
--------------

The `psc` executable takes a list of PureScript source files as arguments and by default writes out its errors or output to the console.

The following options are supported:

--stdin                Read input from standard input instead of from files.
--output               Write the generated Javascript to the specified file.
--externs              Write a list of foreign imports declarations to the specified file in addition to generating Javascript output.
--runtime-type-checks  Generate simple runtime type checks for function arguments with simple types.
--no-tco               Turn off tail-call elimination.
--no-prelude           Do not include the Prelude in the generated Javascript.
--no-magic-do          Turn off optimizations which inline calls to ``>>=`` for the ``Eff`` monad.
--no-opts              Disable all optimizations.
--main                 Generate a call to ``main`` in the specified module after all other generated Javascript. Defaults to ``Main`` if the option is used but no value is provided.
--module               If specified, any code which is not referenced transitively from this module will be removed. This argument can be used multiple times.
--codegen              A list of modules for which Javascript and externs should be generated. This argument can be used multiple times.
--browser-namespace    Specify the namespace that PureScript modules will be exported to when running in the browser.
--verbose-errors       Generate verbose error messages.

psc-make
--------

The ``psc-make`` executable makes CommonJS modules and supports incremental compilation. Unlike ``psc``, it does not do dead code elimination.

The following command line options are supported:

--output=VAL           The output directory. Default: ``output``.
--runtime-type-checks  Generate runtime type checks
--verbose-errors       Display verbose error messages.
--help=FMT             Show this help in format FMT (``pager``, ``plain``, or ``groff``). Default: ``pager``.
--no-magic-do          Disable the optimization that overloads the ``do`` keyword to generate efficient code specifically for the ``Eff`` monad.
--no-opts              Skip the optimization phase.
--no-prelude           Omit the Prelude.
--no-tco               Disable tail call optimizations
