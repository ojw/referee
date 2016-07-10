{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Routes

  ( AllRoutes
  , allRoutes
  , module Referee.User.Routes
  , module Referee.Matchmaking.Routes
  , allApplication
  )

where

import Servant
import Network.Wai
import qualified Data.ByteString as B

import Referee.UuidMap

import Referee.User.Routes
import Referee.User.Api
import Referee.Matchmaking.Routes
import Referee.Matchmaking.Api
import Referee.Login.Routes
import Referee.Login.Api
import Referee.Common.Types

type AllRoutes =
       "user" :> UserRoutes
  :<|> "matchmaking" :> MatchmakingRoutes
  :<|> "login" :> LoginRoutes

allRoutes :: Proxy AllRoutes
allRoutes = Proxy

allServer
  :: (Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter MatchmakingF m2
  -> Interpreter LoginF m1
  -> B.ByteString
  -> Server AllRoutes
allServer userI matchmakingI loginI secret =
       userServer userI loginI
  :<|> matchmakingServer matchmakingI
  :<|> loginServer loginI secret

allApplication
  :: (Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter MatchmakingF m2
  -> Interpreter LoginF m1
  -> B.ByteString
  -> Application
allApplication userI matchmakingI loginI secret = serve allRoutes (allServer userI matchmakingI loginI secret)
