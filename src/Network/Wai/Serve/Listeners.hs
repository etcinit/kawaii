{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The Listeners module includes wrappers for starting TLS and non-TLS Warp
-- servers, with support for IPv4 and IPv6.
module Network.Wai.Serve.Listeners
  ( -- * Listeners
    listen
  , listenTLS
    -- * Re-exports
  , TLSSettings
  , tlsSettingsChain
  ) where

import           Control.Monad.Logger        (MonadLogger, logDebug)
import           Control.Monad.Trans         (MonadIO, liftIO)
import qualified Data.Text                   as T
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (defaultSettings, runSettings,
                                              setHost, setPort)
import           Network.Wai.Handler.WarpTLS (TLSSettings, runTLS,
                                              tlsSettingsChain)

-- | Serves a WAI Application on the specified port.
-- The target port is printed to stdout before hand, which can be useful for
-- debugging purposes.
listen :: (MonadLogger m, MonadIO m) => Int -> Application -> m ()
listen port app = do
  let settings = setHost "*6" (setPort port defaultSettings)

  -- Inform which port we will be listening on.
  $(logDebug) $ mconcat ["Listening on port ", T.pack $ show port, "..."]

  -- Serve the WAI app using Warp
  liftIO $ runSettings settings app

-- | Serves a WAI Application on the specified port.
-- The target port is printed to stdout before hand, which can be useful for
-- debugging purposes.
listenTLS
  :: (MonadLogger m, MonadIO m)
  => TLSSettings
  -> Int
  -> Application
  -> m ()
listenTLS tlsSettings port app = do
  let settings = setHost "*6" (setPort port defaultSettings)

  -- Inform which port we will be listening on.
  $(logDebug) $ mconcat ["Listening on port ", T.pack $ show port, " (TLS)..."]

  -- Serve the WAI app using Warp
  liftIO $ runTLS tlsSettings settings app
