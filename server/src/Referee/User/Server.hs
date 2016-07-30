{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Referee.User.Server where

import Servant
import Network.Wai
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T

import qualified Data.Text.Encoding as Encoding
import Control.Error
import Data.ByteString.Lazy.Char8 as Char8

import Referee.User.Types
import Referee.User.Routes
import Referee.User.Types
import Referee.Common.Types
import Referee.User.Api
import Referee.Login.Api
import Referee.Login.Types
import qualified Referee.Authentication as Auth


-- | Register a user and create a login record.
-- Ideally both creations would be in a single transaction, and indeed
-- the interpreters have to be in the same `m`.  However, whether or not
-- they'll both run in one transaction will depend on the interpreters
-- themselves.
fancyRegisterUser
  :: (Monad m, Translates m IO)
  => Interpreter UserF m
  -> Interpreter LoginF m
  -> Int -- bcrypt cost
  -> UserRegistration T.Text
  -> Handler (Maybe UserId)
fancyRegisterUser interpretUser interpretLogin bcryptCost registration =
  liftIO $ do
  hashedPass <- (hashPassword bcryptCost . Encoding.encodeUtf8 . registrationPassword) registration
  translate $ do
    muserId <- interpretUser $ registerUser registration
    case muserId of
      Nothing -> return Nothing
      Just userId ->
        interpretLogin $ carefulCreateLogin (Email (registrationEmail registration)) hashedPass userId

-- alas, creating a user requires creating a login,
-- so the userServer needs access to an interpreter for login...
userServer
  :: (Monad m, Translates m IO)
  => Interpreter UserF m
  -> Interpreter LoginF m
  -> Int -- bcrypt cost
  -> Server UserRoutes
userServer interpretUser interpretLogin bcryptCost =
       fancyRegisterUser interpretUser interpretLogin bcryptCost
  :<|> (liftIO . translate) (interpretUser getUsers)
  :<|> liftIO . translate . interpretUser . getUser
  :<|> liftIO . translate . interpretUser . checkName

userApplication
  :: (Monad m, Translates m IO)
  => Interpreter UserF m
  -> Interpreter LoginF m
  -> Int -- bcrypt cost
  -> Application
userApplication interpretUser interpretLogin bcryptCost = serve userRoutes (userServer interpretUser interpretLogin bcryptCost)
