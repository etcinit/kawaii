{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Serve.Middleware
  ( -- * Types
    Domain
  , SourceList
  , TypeList
  , Directive(..)
    -- * Middleware Stacks
  , MiddlewareStack(..)
  , (<#>)
  , flatten
  , wrap
    -- * Middlewares
  , gzipMiddleware
  , domainMiddleware
  , securityHeadersMiddleware
  , stsHeadersMiddleware
  , cspHeadersMiddleware
  , deindexifyMiddleware
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text, intercalate)
import qualified Data.Text as T (concat)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid ((<>))
import Safe (lastMay)
import Network.Wai (Application, Middleware, pathInfo)
import Network.Wai.Middleware.AddHeaders    (addHeaders)
import Network.Wai.Middleware.ForceDomain   (forceDomain)
import Network.Wai.Middleware.Gzip          (def, gzip)
import Network.Wai.Middleware.Vhost         (redirectTo)

type Domain = ByteString

type SourceList = [Text]
type TypeList = [Text]

data Directive
  = BaseURI SourceList
  | ChildSrc SourceList
  | ConnectSrc SourceList
  | DefaultSrc SourceList
  | FontSrc SourceList
  | FormAction SourceList
  | FrameAncestors SourceList
  | FrameSrc SourceList
  | ImgSrc SourceList
  | ManifestSrc SourceList
  | MediaSrc SourceList
  | ObjectSrc SourceList
  | PluginTypes TypeList
  | Referrer Text
  | ReflectedXSS Text
  | ReportURI Text
  | Sandbox Text
  | ScriptSrc SourceList
  | StyleSrc SourceList
  | UpgradeInsecureRequests

data MiddlewareStack = MiddlewareStack [Middleware]

instance Monoid MiddlewareStack where
  mempty = MiddlewareStack []
  mappend (MiddlewareStack x) (MiddlewareStack y) = MiddlewareStack (x ++ y)

infixl 5 <#>
(<#>) :: MiddlewareStack -> Middleware -> MiddlewareStack
(<#>) (MiddlewareStack s) m = MiddlewareStack (s ++ [m])

flatten :: MiddlewareStack -> Middleware
flatten (MiddlewareStack s) = foldr (.) id s

wrap :: MiddlewareStack -> Application -> Application
wrap s app = (flatten s) app

dt :: Text -> [Text] -> Text
dt prefix [] = prefix
dt prefix xs = intercalate " " ([prefix] ++ xs)

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
showDirective (UpgradeInsecureRequests) = dt "upgrade-insecure-requests" []

-- | Gzip compression middleware.
gzipMiddleware :: Middleware
gzipMiddleware = gzip def

-- | Domain redirection middleware.
-- When the site is live, we want to redirect users to the right domain name
-- regarles of whether they arrive from a www. domain, the server's IP address
-- or a spoof domain which is pointing to this server.
domainMiddleware :: Domain -> Middleware
domainMiddleware target = forceDomain $ 
  \domain -> if domain `elem` [target, "localhost"]
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
deindexifyMiddleware app req sendResponse =
  if lastMay (pathInfo req) == Just "index.html"
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