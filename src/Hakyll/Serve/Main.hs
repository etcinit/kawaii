{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Main module contains a wrapper function that can be used as your
-- program's main function. It encapsulates both the Hakyll commands and the
-- server functionality into a single binary.
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

-- | The development stage an application is executing in.
--
-- This is used to alter the behavior of the server depending on the stage of
-- development. For example, a local development server might only have an
-- HTTP listener and a smaller middleware stack, while a production server
-- also listens for TLS connections and has additional middlewares that only
-- make sense on a public server.
data Stage = Development | Staging | Production deriving (Show)

-- | The configuration for the server process.
--
-- The different transforms can be used to specify how the configuration
-- changes depending on the stage. While starting up, the server process will
-- take the base configuration, check the stage and pass the configuration
-- through the appropriate transformer.
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
    -- | TLS/SSL configuration. If it's not provided, a TLS listener will not
    -- be started.
  , _tlsConfiguration :: Maybe TLSConfiguration
    -- | Port to start the non-TLS server.
  , _port :: Int
    -- | The current stage. This selects which transformer is used before
    -- starting the server.
  , _stage :: Stage
    -- | Path where the source of built site is located. Defaults to `_site`.
  , _path :: Maybe FilePath
  }

-- | The configuration for the TLS/HTTPS server.
data TLSConfiguration = TLSConfiguration
  { -- | A function for modifying or completely replacing the middleware stack
    -- for the TLS server.
    _tlsMiddleware :: MiddlewareStack -> MiddlewareStack
    -- | Warp @TLSSettings@, which can be used to provide certificates.
  , _tlsSettings :: TLSSettings
    -- | Port to start the TLS server.
  , _tlsPort :: Int
  }

data HakyllServeCommand
  = HakyllCommand HakyllCommandOptions
  | ServeCommand
  deriving (Show)

data HakyllCommandOptions = HakyllCommandOptions String Bool Bool Bool
  deriving (Show)

data HakyllServeOptions = HakyllServeOptions
  { hsCommand :: HakyllServeCommand }
  deriving (Show)

hakyllServeOptions :: Parser HakyllServeOptions
hakyllServeOptions = runA $ proc () -> do
  cmd <- (asA . hsubparser) hakyllServeCommand -< ()
  A helper -< HakyllServeOptions cmd

hakyllServeCommand :: Mod CommandFields HakyllServeCommand
hakyllServeCommand
  = command "hakyll"
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

-- | A default configuration set which can be used as a starting point.
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

makeLenses ''ServeConfiguration
makeLenses ''TLSConfiguration

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

  _ <- forkIO $ case conf ^. tlsConfiguration of
    Just tlsConfig -> listenTLS
      (tlsConfig ^. tlsSettings)
      (tlsConfig ^. tlsPort)
      ((tlsConfig ^. tlsMiddleware) mw `wrap` staticSite (conf ^. path))
    Nothing -> return ()

  listen (conf ^. port) (mw `wrap` staticSite (conf ^. path))

