{-# LANGUAGE RecordWildCards #-}

module Command.IdeClient where

import           Prelude             ()
import           Prelude.Compat

import           Control.Exception
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.IO          as T
import           Data.Monoid           ((<>))
import           Network
import qualified Options.Applicative   as Opts
import           System.Exit
import           System.IO

data Options = Options
  { optionsPort :: PortID
  }

command :: Opts.Parser (IO ())
command = client <$> (Opts.helper <*> options) where
  options :: Opts.Parser Options
  options = Options . PortNumber . fromIntegral <$>
    Opts.option Opts.auto (Opts.long "port" <> Opts.short 'p' <> Opts.value (4242 :: Integer))

client :: Options -> IO ()
client Options{..} = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  let handler (SomeException e) = do
        putStrLn ("Couldn't connect to psc-ide-server on port " ++ show optionsPort ++ ":")
        print e
        exitFailure
  h <- connectTo "127.0.0.1" optionsPort `catch` handler
  T.hPutStrLn h =<< T.getLine
  BS8.putStrLn =<< BS8.hGetLine h
  hFlush stdout
  hClose h
