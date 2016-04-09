{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Serve.Listeners where

import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost,
  setPort)
import Network.Wai (Application)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain)
import System.Environment (lookupEnv)

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
listenTLS :: Int -> Application -> IO ()
listenTLS port app = do
  certPath <- lookupEnv "BLOG_TLS_CERT"
  chainPath <- lookupEnv "BLOG_TLS_CHAIN"
  keyPath <- lookupEnv "BLOG_TLS_KEY"

  let tlsSettings = tlsSettingsChain
                      (fromMaybe "cert.pem" certPath)
                      [fromMaybe "fullchain.pem" chainPath]
                      (fromMaybe "privkey.pem" keyPath)
  let settings = setHost "*6" (setPort port defaultSettings)

  -- Inform which port we will be listening on.
  putStrLn $ "Listening on port " ++ show port ++ " (TLS)..."
  -- Serve the WAI app using Warp
  runTLS tlsSettings settings app
