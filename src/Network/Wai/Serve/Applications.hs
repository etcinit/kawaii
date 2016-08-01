{-# LANGUAGE OverloadedStrings #-}

-- | Utility Wai applications.
module Network.Wai.Serve.Applications where

import Data.Maybe  (fromMaybe, mapMaybe)
import Data.String (fromString)

import Data.Text                      (pack)
import Network.Wai                    (Application)
import Network.Wai.Application.Static (defaultWebAppSettings, ss404Handler,
                                       ssAddTrailingSlash, ssIndices, ssMaxAge,
                                       ssRedirectToIndex, staticApp)
import Network.Wai.Middleware.Vhost   (redirectTo)
import WaiAppStatic.Types             (MaxAge (MaxAgeSeconds), toPiece)

-- | An application for static websites.
--
-- It serves files from the provided path. It defaults to `_site` which is the
-- default location for Hakyll websites.
staticSite :: Maybe FilePath -> Application
staticSite path = staticApp
  (defaultWebAppSettings . fromString $ fromMaybe "_site" path)
  { ssIndices = mapMaybe (toPiece . pack) ["index.html"]
  , ssRedirectToIndex = False
  , ssAddTrailingSlash = True
  , ss404Handler = Just redirectHome
  , ssMaxAge = MaxAgeSeconds 604801
  }

-- | Handler for redirecting requests to the root of the site.
--
-- Useful for 404 handlers.
redirectHome :: Application
redirectHome _ sendResponse = sendResponse $ redirectTo "/"
