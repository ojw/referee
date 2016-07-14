module Main where

import Network.Wai.Handler.Warp
import Servant.JS
import qualified Data.ByteString.Char8 as B

import Referee
import Referee.Examples.RockPaperScissors
import Referee.Config (Config)
import qualified Referee.Config as Config

runApp :: Config -> IO ()
runApp config = do
  userMap <- newUserMap
  mmMap <- newMatchmakingMap
  loginMap <- newLoginMap
  gameMap <- newGameMap
  let userHandler = inMemoryUserHandler userMap
      matchmakingHandler = inMemoryMatchmakingHandler mmMap
      loginHandler = inMemoryLoginHandler loginMap
      gameHandler = inMemoryGameHandler rpsRules gameMap
      secret = Config._jwtSecret config
      cost = Config._bcryptCost config
      allApp = allApplication userHandler matchmakingHandler loginHandler gameHandler secret cost
  run 8081 allApp

main :: IO ()
main = do
  -- load the environment from a .env file, if one exists
  Config.seedEnv
  -- read configuration from environment variables
  errorOrConfig <- Config.getConfig
  -- run the app, if possible
  either putStrLn runApp errorOrConfig
