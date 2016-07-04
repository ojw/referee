module Main where

import Network.Wai.Handler.Warp
import Servant.JS

import Referee.Routes
import Referee.User
import Referee.Matchmaking

main :: IO ()
main = do
  writeJSForAPI allRoutes vanillaJS "api.js"
  userMap <- newUserMap
  mmMap <- newMatchmakingMap
  let userHandler = inMemoryUserHandler userMap
      matchmakingHandler = inMemoryMatchmakingHandler mmMap
      allApp = allApplication userHandler matchmakingHandler
  run 8081 allApp
