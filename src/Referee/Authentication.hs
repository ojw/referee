{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Authentication where

import Servant
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import Servant.Client (AuthClientData, mkAuthenticateReq)
import Servant.Client.Experimental.Auth (AuthenticateReq)
import qualified Servant.Common.Req as SCR
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Network.Wai (Request, requestHeaders)
import qualified Jose.Jws as JWS
import qualified Jose.Jwt as JWT
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Referee.User.Types as User
import Referee.Common.Types

type Auth = Header "user-auth" T.Text -- BC8.ByteString
type ClientAuth = Maybe T.Text

withAuthHelper secret token =
    let jwt = JWS.hmacDecode secret . TE.encodeUtf8 <$> token
    in case jwt of
    -- should probably do something w/ the date the token was issued... we're already in IO
        Just (Right (_, claim)) ->
            let userId = Aeson.decode (BL.fromStrict claim) :: Maybe User.UserId
            in maybe (Left err401) Right userId
        _ -> Left err401 -- should be descriptive

withAuth secret f token =
  case withAuthHelper secret token of
    Left err -> throwError err
    Right userId -> f userId

withAuth2 secret f token =
  case withAuthHelper secret token of
    Left err -> \_ -> throwError err
    Right userId -> f userId

withAuth3 secret f token =
  case withAuthHelper secret token of
    Left err -> \_ _ -> throwError err
    Right userId -> f userId
