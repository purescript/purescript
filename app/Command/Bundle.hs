-- | Bundles compiled PureScript modules for the browser.
module Command.Bundle (command) where

import Prelude

import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import Options.Applicative qualified as Opts

app :: IO ()
app = do
  hPutStrLn stderr $ unlines
    [ "'purs bundle' was removed in the v0.15.0 release."
    , "See https://github.com/purescript/documentation/blob/master/migration-guides/0.15-Migration-Guide.md"
    , "for more information and bundler alternatives."
    ]
  exitFailure

-- | Make it go.
command :: Opts.Parser (IO ())
command = run <$> (Opts.helper <*> pure ()) where
  run :: () -> IO ()
  run _ = app
