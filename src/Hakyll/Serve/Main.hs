{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
--
-- Copyright: (c) Eduardo Trujillo, 2016
-- License: Apache
-- Stability: experimental
--
-- The Main module contains a wrapper function that can be used as your
-- program's main function. It encapsulates both the Hakyll commands and the
-- server functionality into a single application.
--
-- Since this module mainly provides some glue for putting this two
-- applications together, the actual documentation for the server part is
-- available on 'Network.Wai.Serve'.
--
module Hakyll.Serve.Main
  ( -- * Main wrappers
    hakyllServe
  , hakyllServeWith
    -- * Configuration
  , HakyllServeConfiguration(..)
  , -- * Lenses
    hscHakyllConfiguration
  , hscServeConfiguration
  ) where

import Options.Applicative
import System.Environment  (getArgs, withArgs)

import Control.Lens               (makeLenses)
import Data.Default               (Default, def)
import Hakyll                     (defaultConfiguration, hakyllWith)
import Hakyll.Core.Configuration  (Configuration)
import Hakyll.Core.Rules          (Rules)
import Options.Applicative.Arrows
import Safe                       (tailDef)

import Network.Wai.Serve.Main  (serve)
import Network.Wai.Serve.Types (ServeConfiguration (..))

-- | The configuration for the server process.
--
-- The different transforms can be used to specify how the configuration
-- changes depending on the stage. While starting up, the server process will
-- take the base configuration, check the stage and pass the configuration
-- through the appropriate transformer.
--
data HakyllServeConfiguration = HakyllServeConfiguration
  { _hscHakyllConfiguration :: Configuration
  , _hscServeConfiguration  :: ServeConfiguration
  }

instance Default HakyllServeConfiguration where
  def = HakyllServeConfiguration
    { _hscHakyllConfiguration = defaultConfiguration
    , _hscServeConfiguration = def
    }

data HakyllServeCommand
  = HakyllCommand HakyllCommandOptions
  | ServeCommand
  deriving (Show)

data HakyllCommandOptions = HakyllCommandOptions String Bool Bool Bool
  deriving (Show)

data HakyllServeOptions = HakyllServeOptions
  { hsCommand :: HakyllServeCommand
  } deriving (Show)

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
  $ progDesc "Hakyll static site compiler and server"

-- | The __serve__ cousin of @Hakyll.hakyllWith@. It provides a wrapper of the
-- usual Hakyll commands and a command for serving the built site.
hakyllServeWith :: HakyllServeConfiguration -> Rules a -> IO ()
hakyllServeWith conf rules = do
  r <- execParser pinfo

  case hsCommand r of
    HakyllCommand _ -> do
      args <- getArgs

      withArgs (tailDef [] args)
        $ hakyllWith (_hscHakyllConfiguration conf) rules
    ServeCommand -> serve (_hscServeConfiguration conf)

-- | The __serve__ cousin of `Hakyll.hakyll`. It provides a wrapper of the
-- usual Hakyll commands and a command for serving the built site.
hakyllServe :: Rules a -> IO ()
hakyllServe = hakyllServeWith def

makeLenses ''HakyllServeConfiguration
