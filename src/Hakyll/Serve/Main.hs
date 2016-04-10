{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Hakyll.Serve.Main where

import Control.Concurrent (forkIO)
import Hakyll (hakyllWith)
import Hakyll.Core.Configuration (Configuration)
import Hakyll.Core.Rules (Rules)
import Hakyll.Serve.Middleware (MiddlewareStack, wrap)
import Hakyll.Serve.Listeners (TLSSettings, listenTLS, listen)
import Hakyll.Serve.Applications (staticSite)
import Options.Applicative
import Options.Applicative.Arrows
import System.Environment (withArgs, getArgs)
import Safe (tailDef)

data HakyllServeCommand
  = HakyllCommand HakyllCommandOptions
  | ServeCommand
  deriving (Show)

data HakyllCommandOptions = HakyllCommandOptions
  { hspSubcommand :: String
  , hspVerbose :: Bool
  , hspInternalLinks :: Bool
  , hspNoServer :: Bool
  } deriving (Show)

data HakyllServeOptions = HakyllServeOptions
  { hsCommand :: HakyllServeCommand
  } deriving (Show)

hakyllServeOptions :: Parser HakyllServeOptions
hakyllServeOptions = runA $ proc () -> do
  cmd <- (asA . hsubparser) hakyllServeCommand -< ()
  A helper -< HakyllServeOptions cmd

hakyllServeCommand :: Mod CommandFields HakyllServeCommand
hakyllServeCommand =
  command "hakyll"
    (info (hakyllCommandOptions) (progDesc "Access hakyll commands"))
  <> command "serve"
    (info (pure ServeCommand) (progDesc "Start a server hosting the site"))

hakyllCommandOptions :: Parser HakyllServeCommand
hakyllCommandOptions = HakyllCommand
  <$> (HakyllCommandOptions
  <$> (strArgument (metavar "SUBCOMMAND"))
  <*> (switch (long "verbose"))
  <*> (switch (long "internal-links"))
  <*> (switch (long "no-server")))

pinfo :: ParserInfo HakyllServeOptions
pinfo = info hakyllServeOptions
  (progDesc "Hakyll static site compiler and server")

data ServeConfiguration = ServeConfiguration
  { hakyllConfiguration :: Configuration
  , middleware :: MiddlewareStack
  , tlsConfiguration :: Maybe TLSConfiguration
  , port :: Int
  }

data TLSConfiguration = TLSConfiguration
  { tlsMiddleware :: MiddlewareStack
  , tlsSettings :: TLSSettings
  , tlsPort :: Int
  }

hakyllServeWith :: ServeConfiguration -> Rules a -> IO ()
hakyllServeWith conf rules = do
  r <- execParser pinfo

  case hsCommand r of
    HakyllCommand _ -> do
      args <- getArgs
      withArgs (tailDef [] args) $ hakyllWith (hakyllConfiguration conf) rules
    ServeCommand -> do
      _ <- forkIO $ case tlsConfiguration conf of
        Just tlsConfig -> listenTLS
          (tlsSettings tlsConfig)
          (tlsPort tlsConfig)
          ((tlsMiddleware tlsConfig) `wrap` staticSite Nothing)
        Nothing -> return ()
      listen (port conf) ((middleware conf) `wrap` staticSite Nothing)
  print r
