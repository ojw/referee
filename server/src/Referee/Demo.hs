module Referee.Demo where

import Network.Wai.Handler.Warp
import Network.HTTP.Client
import Servant.Client
import Control.Monad.Trans.Except
import qualified Data.Text as T
import Control.Concurrent (forkIO, killThread, ThreadId)

import Referee
import Referee.User
import Referee.User.Client

setup :: IO ()
setup = do
  userMap <- newUserMap
  mmMap <- newMatchmakingMap
  let userHandler = inMemoryUserHandler userMap
      matchmakingHandler = inMemoryMatchmakingHandler mmMap
      allApp = allApplication userHandler matchmakingHandler
  run 8081 allApp

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081 "/user"

registerDemo :: Manager -> ExceptT ServantError IO (Maybe UserId)
registerDemo manager = register (UserRegistration (T.pack "james") (T.pack "email")) manager baseUrl

startServer :: IO ThreadId
startServer = forkIO setup

demo :: IO ()
demo = do
  serverThread <- startServer
  manager <- newManager defaultManagerSettings
  users <- runExceptT (Referee.User.Client.getUsers manager baseUrl)
  print users
  foo <- runExceptT (registerDemo manager)
  print foo
  users' <- runExceptT (Referee.User.Client.getUsers manager baseUrl)
  print users'
  killThread serverThread
