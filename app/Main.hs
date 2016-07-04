module Main where

import Referee.User.Handler.InMemory
import Referee.User.Server
import Referee.Matchmaking.Server
import Referee.Matchmaking.Handler.InMemory
import Network.Wai.Handler.Warp
import Servant.JS

main :: IO ()
main = do
  writeJSForAPI userApi vanillaJS "api.js"
  userMap <- newUserMap
  let userHandler = inMemoryUserHandler userMap
      userApp = userApplication userHandler
  run 8081 userApp
