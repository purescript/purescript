module Main where

import           Prelude             ()
import           Prelude.Compat

import           Control.Exception
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.IO          as T
import           Data.Version          (showVersion)
import           Data.Monoid           ((<>))
import           Network
import           Options.Applicative   (ParseError (..))
import qualified Options.Applicative   as Opts
import           System.Exit
import           System.IO

import qualified Paths_purescript      as Paths

data Options = Options
  { optionsPort :: PortID
  }

main :: IO ()
main = do
    Options port <- Opts.execParser opts
    client port
  where
    parser =
        Options <$>
        (PortNumber . fromIntegral <$>
         Opts.option Opts.auto (Opts.long "port" <> Opts.short 'p' <> Opts.value (4242 :: Integer)))
    opts = Opts.info (version <*> Opts.helper <*> parser) mempty
    version = Opts.abortOption (InfoMsg (showVersion Paths.version)) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

client :: PortID -> IO ()
client port = do
    h <-
        connectTo "127.0.0.1" port `catch`
        (\(SomeException e) ->
              putStrLn
                  ("Couldn't connect to psc-ide-server on port: " ++
                   show port ++ " Error: " ++ show e) >>
              exitFailure)
    T.hPutStrLn h =<< T.getLine
    BS8.putStrLn =<< BS8.hGetLine h
    hFlush stdout
    hClose h
