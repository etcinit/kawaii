{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-- |
--
-- Copyright: (c) Eduardo Trujillo, 2016
-- License: Apache
-- Stability: experimental
--
-- The Main module contains an implementation of a configurable static web
-- server with support for a middleware stack and different environments
-- (development, staging, production).
--
-- Take a look at 'ServeConfiguration' for all the possible configuration
-- options or simply use it's 'Data.Default.Default' instance for a basic
-- server.
--
module Network.Wai.Serve.Main
  ( serve
  , serve'
  ) where

import Control.Concurrent.Lifted   (fork)
import Control.Monad.Logger        (runStdoutLoggingT)
import Control.Monad.Trans         (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Network.Wai.Serve.Applications (staticSite)
import Network.Wai.Serve.Listeners    (listen, listenTLS)
import Network.Wai.Serve.Middleware   (wrap)
import Network.Wai.Serve.Types        (ServeConfiguration (..), Stage (..),
                                       TLSConfiguration (..))

-- | Starts a server with the provided @ServeConfiguration@. If TLS settings
-- are provided, an additional server is started for handling secure requests.
serve :: (MonadIO m, MonadBaseControl IO m) => ServeConfiguration -> m ()
serve = serve' . transform

-- | Starts a server with the provided @ServeConfiguration@. If TLS settings
-- are provided, an additional server is started for handling secure requests.
-- Unlike $serve'$, stage transforms are not applied on the provided
-- configuration.
serve' :: (MonadIO m, MonadBaseControl IO m) => ServeConfiguration -> m ()
serve' ServeConfiguration{..} = runStdoutLoggingT $ do
  let site = staticSite _scPath

  _ <- fork $ case _scTlsConfiguration of
    Just TLSConfiguration{..} -> listenTLS
      _tlsSettings
      _tlsPort
      (_tlsMiddleware _scMiddleware `wrap` site)
    Nothing -> pure ()

  listen _scPort (_scMiddleware `wrap` site)

-- | Applies the respective configuration transform to the current stage.
transform :: ServeConfiguration -> ServeConfiguration
transform conf@ServeConfiguration{..} = transformer conf
  where
    transformer = case _scStage of
      Development -> _scDevTransform
      Staging -> _scStagingTransform
      Production -> _scProdTransform
