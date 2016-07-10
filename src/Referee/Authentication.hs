{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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

import qualified Referee.User.Types as User
import Referee.Common.Types

-- The header under which the JWT lives
jwtUserAuthHeader = "jwt-user-auth"

{-- Hooking it into Servant --}
authHandler :: Secret -> AuthHandler Request User.UserId
authHandler secret =
  let handler req =
       let jwt = JWS.hmacDecode secret <$> lookup jwtUserAuthHeader (requestHeaders req)
        in case jwt of
         -- should probably do something w/ the date the token was issued... we're already in IO
             Just (Right (_, claim)) ->
               let userId = Aeson.decode (BL.fromStrict claim) :: Maybe User.UserId
               in maybe (throwError err401) return userId
             _ -> throwError err401 -- should be descriptive
  in mkAuthHandler handler

type instance AuthServerData (AuthProtect jwtUserAuthHeader) = User.UserId

-- Prefix each Servant route in `t` with `f`; this allows us to define
-- Auth as if it were server-level, but have it actually be route-level, which
-- makes the client functions more natural (you provide a JWT with each fn, rather
-- than inputting a JWT to get a collection of fns).
type family Distribute f t where
  Distribute f (a :<|> b) = Distribute f a :<|> Distribute f b
  Distribute f (a :> b) = f :> a :> b

-- Distribute JWT auth over `t`
type WithAuthentication (t :: *) = Distribute (AuthProtect "jwt-user-auth") t

-- To be used with Servant's serveWithContext
getAuthContext :: Secret -> Context (AuthHandler Request User.UserId ': '[])
getAuthContext secret = authHandler secret :. EmptyContext

-- More servant stuff...
type instance AuthClientData (AuthProtect "jwt-user-auth") = JWT.Jwt

authenticateReq :: JWT.Jwt -> SCR.Req -> SCR.Req
authenticateReq jwt = SCR.addHeader "jwt-user-auth" (BC8.unpack . JWT.unJwt $ jwt)

-- Transform a JWT into something that can be passed to a client function
toAuthToken :: JWT.Jwt -> AuthenticateReq  (AuthProtect "jwt-user-auth")
toAuthToken tok = mkAuthenticateReq tok authenticateReq
