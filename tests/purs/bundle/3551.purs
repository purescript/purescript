module Main where

import Prelude
import Effect (Effect)
import Effect.Console (error, log)

import ModuleWithDeadCode (class FooBar, exportThatUsesBar, results)

main :: Effect Unit
main = do
  when results.barIsExported $ error "bar is exported"
  when results.fooIsNotEliminated $ error "foo is not eliminated"

  -- These are brittleness canaries; if they fail, then the compiler output has
  -- probably changed such that the above checks are not doing their job.
  unless results.exportThatUsesBarIsExported $
    error "likely test error: check that barIsExported is working"
  unless results.barIsNotEliminated $
    error "likely test error: check that fooIsNotEliminated is working"

  when (exportThatUsesBar 0) $ log "Done"
