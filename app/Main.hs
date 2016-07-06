module Main where

import Network.Wai.Handler.Warp
import Servant.JS

import Referee

main :: IO ()
main = do
  writeJSForAPI allRoutes vanillaJS "api.js"
  userMap <- newUserMap
  mmMap <- newMatchmakingMap
  let userHandler = translate . inMemoryUserHandler userMap
      matchmakingHandler = translate . inMemoryMatchmakingHandler mmMap
      allApp = allApplication userHandler matchmakingHandler
  run 8081 allApp
