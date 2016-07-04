module Main where

import Network.Wai.Handler.Warp
import Servant.JS

import Referee.Routes
import Referee.User.Handler.InMemory
import Referee.Matchmaking.Handler.InMemory

main :: IO ()
main = do
  writeJSForAPI allRoutes vanillaJS "api.js"
  userMap <- newUserMap
  mmMap <- newMatchmakingMap
  let userHandler = inMemoryUserHandler userMap
      userApp = userApplication userHandler
      matchmakingHandler = inMemoryMatchmakingHandler mmMap
      matchmakingApp = matchmakingApplication matchmakingHandler
  run 8081 userApp
