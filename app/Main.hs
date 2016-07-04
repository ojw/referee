module Main where

import Referee.User.Handler.InMemory
import Referee.User.Server
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  userMap <- newUserMap
  let userHandler = handleUserApi userMap
      userApp = userApplication userHandler
  run 8081 userApp
