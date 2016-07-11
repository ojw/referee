{-# Language OverloadedStrings #-}

module TestMatchmaking where

import Test.Hspec

import Servant.Client (BaseUrl(..), Scheme(..))
import Servant.Client (AuthClientData, mkAuthenticateReq)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Jose.Jws as JWS
import qualified Jose.Jwt as JWT
import qualified Jose.Jwa as JWA
import qualified Data.UUID as UUID
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Control.Concurrent(forkIO, ThreadId, killThread)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.Either as Either

import Referee.Matchmaking.Routes
import qualified Referee.Authentication as Auth
import qualified Referee.Matchmaking.Handler.InMemory as InMem
import qualified Referee.Matchmaking.Client as MMC
import Referee.Common.Types (Secret, translate)

-- Matchmaking is now tied to Games, and I was too cowardly to figure out these tests given that they need a bunch of Game code.

-- mkToken :: Secret -> UUID.UUID -> JWT.Jwt
-- mkToken secret uuid =
--   let Right tok = JWS.hmacEncode JWA.HS512 secret (BL.toStrict (Aeson.encode uuid))
--   in tok

-- withMMServer secret body = do
--     tvar <- runIO InMem.newMatchmakingMap
--     let app = matchmakingApplication (translate . InMem.inMemoryMatchmakingHandler tvar) secret
--     serverThread <- runIO . forkIO $ run 8080 app
--     body
--     runIO . killThread $ serverThread

-- testRouteAuthentication = do
--   let secret = "d-don't even trip, dawg!" :: Secret
--       goodToken = Auth.toAuthToken $ mkToken secret UUID.nil
--       badToken  = Auth.toAuthToken $ mkToken "bad secret" UUID.nil
--       base = BaseUrl Http "localhost" 8080 ""
--   withMMServer secret $ do
--     manager <- runIO . newManager $ defaultManagerSettings
--     goodResult <- runIO . runExceptT $ MMC.joinRandom goodToken manager base
--     badResult <- runIO . runExceptT $ MMC.joinRandom badToken manager base
--     describe "Matchmaking routes have authentication handling." $ do
--       specify "If a good auth token is used, the request will succeed." $ do
--         goodResult `shouldSatisfy` Either.isRight
--       specify "If a bad auth token is used, the request will fail." $ do
--         badResult `shouldSatisfy` Either.isLeft

-- main = do
--   testRouteAuthentication
