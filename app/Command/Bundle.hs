-- | Bundles compiled PureScript modules for the browser.
module Command.Bundle (command) where

import Prelude

import           System.Exit (exitFailure)
import           System.IO (stderr, hPutStrLn)
import qualified Options.Applicative as Opts

app :: IO ()
app = do
  hPutStrLn stderr $ unlines
    [ "'purs bundle' was removed in the v0.15.0 release."
    , "Use 'esbuild' or another bundler. For info on migrating to a bundler, see"
    , "https://github.com/purescript/documentation/blob/master/migration-guides/0.15-Migration-Guide.md#how-can-i-bundle-my-library-or-application"
    ]
  exitFailure

-- | Make it go.
command :: Opts.Parser (IO ())
command = run <$> (Opts.helper <*> pure ()) where
  run :: () -> IO ()
  run _ = app
