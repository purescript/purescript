-- | Bundles compiled PureScript modules for the browser.
module Command.Bundle (command) where

import Prelude

import           System.Exit (exitFailure)
import           System.IO (stderr, hPutStrLn)
import qualified Options.Applicative as Opts

app :: IO ()
app = do
  hPutStrLn stderr "'purs bundle' was removed in the v0.15.0 release. Use 'esbuild' or another bundler."
  exitFailure

-- | Make it go.
command :: Opts.Parser (IO ())
command = run <$> (Opts.helper <*> pure ()) where
  run :: () -> IO ()
  run _ = app
