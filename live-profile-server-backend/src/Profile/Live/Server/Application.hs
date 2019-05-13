{-|
Module      : Profile.Live.Server.Application
Description : Creation of WAI application
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application(
  -- * WAI application
    liveProfileApp
  -- * Subservers
  , coreServer
  , documentedServer
  , liveProfileServer
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Network.Wai (Application)
import Servant
import Control.Natural
import Servant.Server.Auth.Token
import Servant.Server.Auth.Token.Persistent

import Profile.Live.Server.API
import Profile.Live.Server.Application.Bined
import Profile.Live.Server.Application.Connection
import Profile.Live.Server.Application.EventLog
import Profile.Live.Server.Application.Session
import Profile.Live.Server.Application.Upload
import Profile.Live.Server.Config
import Profile.Live.Server.Monad

import Control.Monad.Morph

type MyApi = ConnectionAPI :<|> SessionAPI :<|> EventLogAPI :<|> BinedAPI :<|> UploadAPI

-- | Handlers for core API that the server implements
coreServer :: AppState -> Server CoreLiveProfileAPI
coreServer app = hoistServer (Proxy :: Proxy MyApi) (convertApp app) server
  where
  server =
         connectionServer
    :<|> sessionServer
    :<|> eventLogServer
    :<|> binedServer
    :<|> uploadServer

-- | Implementation of documented server
documentedServer :: AppState -> Server DocumentedLiveProfileAPI
documentedServer app =
  (hoistServer (Proxy :: Proxy AuthAPI) (convertApp app) authServer )
  :<|> coreServer app

-- | Implmenetation of full profile API
liveProfileServer :: AppState -> Server LiveProfileAPI
liveProfileServer app = documentedServer app :<|> files app

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: AppState -> App a -> Handler a
convertApp app = Handler . h . flip evalStateT app . runApp
  where
    h :: ExceptT ServerError (PersistentBackendT IO) a
      -> ExceptT ServerError IO a
    h et = do
      ExceptT $ fmap join $ runPersistentBackendT cfg pool $ runExceptT et


    pool = appPool app
    cfg  = appAuth app



-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: AppState -> ServerT Raw m
files app = serveDirectory (configStatic . appConfig $ app)

-- | Create WAI application for live profiler
liveProfileApp :: AppState -> Application
liveProfileApp = serve liveProfileAPI . liveProfileServer
