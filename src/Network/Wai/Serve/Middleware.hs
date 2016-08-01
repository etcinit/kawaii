{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Serve.Middleware
  ( -- * Middleware Stacks
    MiddlewareStack(..)
  , (<#>)
  , flatten
  , wrap
    -- * Middlewares
  , loggerMiddleware
  , forceSSLMiddleware
  , gzipMiddleware
  , domainMiddleware
  , securityHeadersMiddleware
  , stsHeadersMiddleware
  , cspHeadersMiddleware
  , deindexifyMiddleware
  ) where

import Data.ByteString (ByteString)
import Data.Monoid     ((<>))
import Prelude         hiding (unwords)

import           Data.Text                            (Text, intercalate,
                                                       unwords)
import qualified Data.Text                            as T (concat)
import           Data.Text.Encoding                   (encodeUtf8)
import           Network.Wai                          (Application, Middleware,
                                                       pathInfo)
import           Network.Wai.Middleware.AddHeaders    (addHeaders)
import           Network.Wai.Middleware.ForceDomain   (forceDomain)
import           Network.Wai.Middleware.ForceSSL      (forceSSL)
import           Network.Wai.Middleware.Gzip          (GzipFiles (GzipCompress),
                                                       def, gzip, gzipFiles)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Vhost         (redirectTo)
import           Safe                                 (lastMay)

import Network.Wai.Serve.Types

infixl 5 <#>
-- | A combinator for adding a middleware to the bottom of a stack.
(<#>) :: MiddlewareStack -> Middleware -> MiddlewareStack
(<#>) (MiddlewareStack s) m = MiddlewareStack (s ++ [m])

-- | Combines the entire stack into a single middleware.
flatten :: MiddlewareStack -> Middleware
flatten (MiddlewareStack s) = foldr (.) id s

-- | Wraps a WAI application in the stack. (Alias of $flatten$)
wrap :: MiddlewareStack -> Application -> Application
wrap = flatten

dt :: Text -> [Text] -> Text
dt prefix [] = prefix
dt prefix xs = unwords (prefix : xs)

showDirective :: Directive -> Text
showDirective (BaseURI xs) = dt "base-uri" xs
showDirective (ChildSrc xs) = dt "child-src" xs
showDirective (ConnectSrc xs) = dt "connect-src" xs
showDirective (DefaultSrc xs) = dt "default-src" xs
showDirective (FontSrc xs) = dt "font-src" xs
showDirective (FormAction xs) = dt "form-action" xs
showDirective (FrameAncestors xs) = dt "frame-ancestors" xs
showDirective (FrameSrc xs) = dt "frame-src" xs
showDirective (ImgSrc xs) = dt "img-src" xs
showDirective (ManifestSrc xs) = dt "manifest-src" xs
showDirective (MediaSrc xs) = dt "media-src" xs
showDirective (ObjectSrc xs) = dt "object-src" xs
showDirective (PluginTypes xs) = dt "plugin-types" xs
showDirective (Referrer x) = dt "referrer" [x]
showDirective (ReflectedXSS x) = dt "reflected-xss" [x]
showDirective (ReportURI x) = dt "report-uri" [x]
showDirective (Sandbox x) = dt "sandbox" [x]
showDirective (ScriptSrc xs) = dt "script-src" xs
showDirective (StyleSrc xs) = dt "style-src" xs
showDirective UpgradeInsecureRequests = dt "upgrade-insecure-requests" []

-- | Logger middleware.
loggerMiddleware :: Middleware
loggerMiddleware = logStdout

-- | Middleware for forcing requests to be done through SSL.
forceSSLMiddleware :: Middleware
forceSSLMiddleware = forceSSL

-- | Gzip compression middleware.
gzipMiddleware :: Middleware
gzipMiddleware = gzip $ def {gzipFiles = GzipCompress}

-- | Domain redirection middleware.
-- When the site is live, we want to redirect users to the right domain name
-- regarles of whether they arrive from a www. domain, the server's IP address
-- or a spoof domain which is pointing to this server.
domainMiddleware :: Domain -> Middleware
domainMiddleware target = forceDomain
  $ \domain -> if domain `elem` [target, "localhost"]
    then Nothing
    else Just target

-- | Common security headers middleware.
securityHeadersMiddleware :: Middleware
securityHeadersMiddleware = addHeaders
  [ ("X-Frame-Options", "SAMEORIGIN")
  , ("X-XSS-Protection", "1; mode=block")
  , ("X-Content-Type-Options", "nosniff")
  ]

-- | Strict Transport Security middleware.
stsHeadersMiddleware :: Middleware
stsHeadersMiddleware = addHeaders
  [("Strict-Transport-Security", "max-age=31536000; includeSubdomains")]

-- | Content Security Policy middleware.
-- Here we add the CSP header which includes the policies for this blog.
cspHeadersMiddleware :: [Directive] -> Middleware
cspHeadersMiddleware directives = addHeaders
  [("Content-Security-Policy", encodeUtf8 $ glue directives)]
  where
    glue :: [Directive] -> Text
    glue [] = showDirective $ DefaultSrc ["'none'"]
    glue xs = intercalate "; " (map showDirective xs)

-- | De-indexify middleware.
-- Redirects any path ending in `/index.html` to just `/`.
deindexifyMiddleware :: Middleware
deindexifyMiddleware app req sendResponse
  = if lastMay (pathInfo req) == Just "index.html"
    then sendResponse $ redirectTo newPath
    else app req sendResponse

    where
      newPath :: ByteString
      newPath = encodeUtf8 $ processPath oldPath

      processPath :: [Text] -> Text
      processPath xs = case xs of
        [] -> "/"
        _ -> T.concat $ map prefixSlash xs

      oldPath :: [Text]
      oldPath = init $ pathInfo req

      prefixSlash :: Text -> Text
      prefixSlash = (<>) "/"
