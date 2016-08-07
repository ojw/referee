{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Referee.Client where

import Servant.API
import Data.Proxy
import Servant.Reflex
import Reflex.Dom

import Referee.User.Widgets
import Referee.Login.Widgets
import Referee.User.Routes

import Referee.Routes

runGUI
  :: MonadWidget t m
  => String -- host
  -> Int -- port
  -> m ()
runGUI host port = do
  let (register :<|> getUsers :<|> getUser :<|> checkName)
        :<|> mm :<|> lgn :<|> clnt =
        client
        allRoutes
        Proxy
        (constDyn (BaseFullUrl Http host port ""))

  registerWidget register
  loginWidget
