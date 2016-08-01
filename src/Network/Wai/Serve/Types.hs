{-# LANGUAGE TemplateHaskell #-}

-- |
--
-- Copyright: (c) Eduardo Trujillo, 2016
-- License: Apache
-- Stability: experimental
--
-- Types used by the Kawaii package.
module Network.Wai.Serve.Types
  ( -- * Middleware Stack
    MiddlewareStack(..)
  , -- * Server configuration
    ServeConfiguration(..)
  , TLSConfiguration(..)
  , -- * CSP
    SourceList
  , TypeList
  , Directive(..)
  , -- * Miscellaneous types
    Domain
  , Stage(..)
  , -- * Lenses
    scMiddleware
  , scDevTransform
  , scStagingTransform
  , scProdTransform
  , scTlsConfiguration
  , scPort
  , scStage
  , scPath
  , tlsMiddleware
  , tlsSettings
  , tlsPort
  ) where

import Data.ByteString (ByteString)

import Control.Lens                (makeLenses)
import Data.Default                (Default, def)
import Data.Text                   (Text)
import Network.Wai                 (Middleware)
import Network.Wai.Handler.WarpTLS (TLSSettings)

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

-- | A middleware stack is a simple container of Wai middleware. The top of the
-- stack is considered to be the outermost layer of middleware while the bottom
-- of the stack is the innermost layer of middleware.
--
-- The stack is a 'Monoid', so it can be combined with other stacks using
-- 'Data.Monoid.<>', and an empty one can be obtained using 'mempty'.
--
data MiddlewareStack
  -- | The constructor takes a list of middleware, where the left-most item in
  -- the list will end up at the top of the stack, and the right-most at the
  -- bottom.
  = MiddlewareStack [Middleware]

instance Monoid MiddlewareStack where
  mempty = MiddlewareStack []
  mappend (MiddlewareStack x) (MiddlewareStack y) = MiddlewareStack (x ++ y)

-- | The development stage an application is executing in.
--
-- This is used to alter the behavior of the server depending on the stage of
-- development. For example, a local development server might only have an
-- HTTP listener and a smaller middleware stack, while a production server
-- also listens for TLS connections and has additional middlewares that only
-- make sense on a public server.
--
data Stage = Development | Staging | Production deriving (Show)

-- | The configuration for the server process.
--
-- The different transforms can be used to specify how the configuration
-- changes depending on the stage. While starting up, the server process will
-- take the base configuration, check the stage and pass the configuration
-- through the appropriate transformer.
--
data ServeConfiguration = ServeConfiguration
  { -- | Base middleware stack.
    _scMiddleware       :: MiddlewareStack
    -- | @Developer@ stage configuration transformer.
  , _scDevTransform     :: ServeConfiguration -> ServeConfiguration
    -- | @Staging@ stage configuration transfomer.
  , _scStagingTransform :: ServeConfiguration -> ServeConfiguration
    -- | @Production@ stage configuration transfomer.
  , _scProdTransform    :: ServeConfiguration -> ServeConfiguration
    -- | TLS/SSL configuration. If it's not provided, a TLS listener will not
    -- be started.
  , _scTlsConfiguration :: Maybe TLSConfiguration
    -- | Port to start the non-TLS server.
  , _scPort             :: Int
    -- | The current stage. This selects which transformer is used before
    -- starting the server.
  , _scStage            :: Stage
    -- | Path where the source of built site is located. Defaults to `_site`.
  , _scPath             :: Maybe FilePath
  }

instance Default ServeConfiguration where
  def = ServeConfiguration
    { _scMiddleware = mempty
    , _scDevTransform = id
    , _scStagingTransform = id
    , _scProdTransform = id
    , _scTlsConfiguration = Nothing
    , _scPort = 9119
    , _scStage = Development
    , _scPath = Nothing
    }

-- | The configuration for the TLS/HTTPS server.
data TLSConfiguration = TLSConfiguration
  { -- | A function for modifying or completely replacing the middleware stack
    -- for the TLS server.
    _tlsMiddleware :: MiddlewareStack -> MiddlewareStack
    -- | Warp @TLSSettings@, which can be used to provide certificates.
  , _tlsSettings   :: TLSSettings
    -- | Port to start the TLS server.
  , _tlsPort       :: Int
  }

makeLenses ''ServeConfiguration
makeLenses ''TLSConfiguration
