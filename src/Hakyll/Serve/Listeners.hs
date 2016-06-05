{-# LANGUAGE OverloadedStrings #-}

-- | The Listeners module includes wrappers for starting TLS and non-TLS Warp
-- servers, with support for IPv4 and IPv6.
module Hakyll.Serve.Listeners
  ( -- * Listeners
    listen
  , listenTLS
    -- * Re-exports
  , TLSSettings
  , tlsSettingsChain
  ) where

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost,
  setPort)
import Network.Wai (Application)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS, tlsSettingsChain)

-- | Serves a WAI Application on the specified port.
-- The target port is printed to stdout before hand, which can be useful for
-- debugging purposes.
listen :: Int -> Application -> IO ()
listen port app = do
  let settings = setHost "*6" (setPort port defaultSettings)

  -- Inform which port we will be listening on.
  putStrLn $ "Listening on port " ++ show port ++ "..."
  -- Serve the WAI app using Warp
  runSettings settings app

-- | Serves a WAI Application on the specified port.
-- The target port is printed to stdout before hand, which can be useful for
-- debugging purposes.
listenTLS :: TLSSettings -> Int -> Application -> IO ()
listenTLS tlsSettings port app = do
  let settings = setHost "*6" (setPort port defaultSettings)

  -- Inform which port we will be listening on.
  putStrLn $ "Listening on port " ++ show port ++ " (TLS)..."
  -- Serve the WAI app using Warp
  runTLS tlsSettings settings app
