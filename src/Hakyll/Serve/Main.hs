{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Hakyll.Serve.Main
  ( -- * Main wrappers
    hakyllServe
  , hakyllServeWith
    -- * Configuration
  , ServeConfiguration(..)
  , TLSConfiguration(..)
  , Stage(..)
  , defaultServeConfiguration
    -- * Lenses
  , hakyllConfiguration
  , middleware
  , devTransform
  , stagingTransform
  , prodTransform
  , tlsConfiguration
  , port
  , stage
  , path
  , tlsMiddleware
  , tlsSettings
  , tlsPort
    -- * Programmatic access
  , serve
  ) where

import Control.Concurrent (forkIO)
import Control.Lens (makeLenses, (^.))
import Hakyll (hakyllWith, defaultConfiguration)
import Hakyll.Core.Configuration (Configuration)
import Hakyll.Core.Rules (Rules)
import Hakyll.Serve.Middleware (MiddlewareStack, wrap)
import Hakyll.Serve.Listeners (TLSSettings, listenTLS, listen)
import Hakyll.Serve.Applications (staticSite)
import Options.Applicative
import Options.Applicative.Arrows
import System.Environment (withArgs, getArgs)
import Safe (tailDef)

data Stage = Development | Staging | Production deriving (Show)

data HakyllServeCommand
  = HakyllCommand HakyllCommandOptions
  | ServeCommand
  deriving (Show)

data HakyllCommandOptions = HakyllCommandOptions String Bool Bool Bool
  deriving (Show)

data HakyllServeOptions = HakyllServeOptions
  { hsCommand :: HakyllServeCommand
  } deriving (Show)

data ServeConfiguration = ServeConfiguration
  { _hakyllConfiguration :: Configuration
    -- | Base middleware stack.
  , _middleware :: MiddlewareStack
    -- | @Developer@ stage configuration transformer.
  , _devTransform :: ServeConfiguration -> ServeConfiguration
    -- | @Staging@ stage configuration transfomer.
  , _stagingTransform :: ServeConfiguration -> ServeConfiguration
    -- | @Production@ stage configuration transfomer.
  , _prodTransform :: ServeConfiguration -> ServeConfiguration
    -- | TLS/SSL configuration.
  , _tlsConfiguration :: Maybe TLSConfiguration
    -- | Port to start the non-TLS server.
  , _port :: Int
    -- | The current stage. This selects which transformer is used before
    -- starting the server.
  , _stage :: Stage
    -- | Path where the source of built site is located. Defaults to `_site`.
  , _path :: Maybe FilePath
  }

data TLSConfiguration = TLSConfiguration
  { -- | A function for modifying or completely replacing the middleware stack
    -- for the TLS server.
    _tlsMiddleware :: MiddlewareStack -> MiddlewareStack
    -- | Warp @TLSSettings@, which can be used to provide certificates.
  , _tlsSettings :: TLSSettings
    -- | Port to start the TLS server.
  , _tlsPort :: Int
  }

makeLenses ''ServeConfiguration
makeLenses ''TLSConfiguration

hakyllServeOptions :: Parser HakyllServeOptions
hakyllServeOptions = runA $ proc () -> do
  cmd <- (asA . hsubparser) hakyllServeCommand -< ()
  A helper -< HakyllServeOptions cmd

hakyllServeCommand :: Mod CommandFields HakyllServeCommand
hakyllServeCommand =
  command "hakyll"
    (info hakyllCommandOptions (progDesc "Access hakyll commands"))
  <> command "serve"
    (info (pure ServeCommand) (progDesc "Start a server hosting the site"))

hakyllCommandOptions :: Parser HakyllServeCommand
hakyllCommandOptions = HakyllCommand
  <$> (HakyllCommandOptions
  <$> strArgument (metavar "SUBCOMMAND")
  <*> switch (long "verbose")
  <*> switch (long "internal-links")
  <*> switch (long "no-server"))

pinfo :: ParserInfo HakyllServeOptions
pinfo = info hakyllServeOptions
  (progDesc "Hakyll static site compiler and server")

defaultServeConfiguration :: ServeConfiguration
defaultServeConfiguration = ServeConfiguration
  { _hakyllConfiguration = defaultConfiguration
  , _middleware = mempty
  , _devTransform = id
  , _stagingTransform = id
  , _prodTransform = id
  , _tlsConfiguration = Nothing
  , _port = 9119
  , _stage = Development
  , _path = Nothing
  }

-- | The __serve__ cousin of @Hakyll.hakyllWith@. It provides a wrapper of the
-- usual Hakyll commands and a command for serving the built site.
hakyllServeWith :: ServeConfiguration -> Rules a -> IO ()
hakyllServeWith conf rules = do
  r <- execParser pinfo

  case hsCommand r of
    HakyllCommand _ -> do
      args <- getArgs
      withArgs (tailDef [] args) $ hakyllWith (_hakyllConfiguration conf) rules
    ServeCommand -> serve conf

-- | The __serve__ cousin of `Hakyll.hakyll`. It provides a wrapper of the
-- usual Hakyll commands and a command for serving the built site.
hakyllServe :: Rules a -> IO ()
hakyllServe = hakyllServeWith defaultServeConfiguration

-- | Starts a server with the provided @ServeConfiguration@. If TLS settings
-- are provided, an additional server is started for handling secure requests.
serve :: ServeConfiguration -> IO ()
serve conf = case _stage conf of
  Development -> serve' (_devTransform conf conf)
  Staging -> serve' (_stagingTransform conf conf)
  Production -> serve' (_prodTransform conf conf)

serve' :: ServeConfiguration -> IO ()
serve' conf = do
  let mw = conf ^. middleware
  print $ conf ^. port
  _ <- forkIO $ case conf ^. tlsConfiguration of
    Just tlsConfig -> listenTLS
      (tlsConfig ^. tlsSettings)
      (tlsConfig ^. tlsPort)
      ((tlsConfig ^. tlsMiddleware) mw `wrap` staticSite (conf ^. path))
    Nothing -> return ()
  listen (conf ^. port) (mw `wrap` staticSite (conf ^. path))

