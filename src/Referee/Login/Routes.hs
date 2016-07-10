{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Login.Routes where

import Servant
import Network.Wai
import Jose.Jwt
import Jose.Jws
import Jose.Jwa
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.Maybe
import Control.Error ((??)) -- best package ever
import Data.Aeson

import Referee.Common.Types
import Referee.Login.Types
import Referee.Login.Api

-- will return a servant error on auth failure
type LoginRoutes =
     Header "email" T.Text
  :> Header "password" T.Text
  :> "login"
  :> Get '[JSON] Jwt

loginRoutes :: Proxy LoginRoutes
loginRoutes = Proxy

loginServer
  :: Translates m IO
  => Interpreter LoginF m
  -> B.ByteString -- the secret, for signing the jwt
  -> Server LoginRoutes
loginServer interpretLogin secret =
  -- apparently headers being missing is a problem the handler has to deal with
  \memail mpassword -> do

    email <- fmap Email memail ?? err400 { errBody = Char8.pack "Missing username, bub." }
    password <- mpassword ?? err400 { errBody = Char8.pack "Gotta include a password dawg." }

    -- initially I didn't like having to call translate here, but I'm okay with it now
    -- although I think translate might be the wrong word...
    -- the intention is more like "do the transactional thing now"
    maybeLogin <- liftIO . translate . interpretLogin $ verifyCredentials email (T.encodeUtf8 password)

    -- not gonna lie, I'm in love with '??'

    login <- maybeLogin ?? err401 { errBody = Char8.pack "Nah those credentials didn't work." }

    let ejwt = hmacEncode HS512 (Char8.toStrict (Data.Aeson.encode (userId login))) secret

    case ejwt of
      Left jwtError -> throwError err500 { errBody = Char8.pack "Welp something went wrong on our end trying to create your auth token." }
      Right jwt -> return jwt
