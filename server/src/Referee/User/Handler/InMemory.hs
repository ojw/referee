module Referee.User.Handler.InMemory where

import Control.Monad.Free
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Monad.IO.Class

import Referee.User.Types
import Referee.User.Api
import Referee.UuidMap

type UserMap = UuidMap User

updateUserId :: UserId -> User -> User
updateUserId uuid user = user { userId = uuid }

-- | I think it'll be more useful to have an IO (TVar UserMap) than an IO UserMap,
-- given handleUserApi.
newUserMap :: IO (TVar UserMap)
newUserMap = do
  newMap <- emptyIO updateUserId
  newTVarIO newMap

-- gotta change this to bytestring
-- after hashing the provided pw immediately after receipt
handleAddUser :: UserMap ->  UserRegistration T.Text -> (UserId, UserMap)
handleAddUser userMap registration = insert user userMap
  where user = User (registrationName registration) (registrationEmail registration) nilId

handleCheckName :: T.Text -> UserMap -> Bool
handleCheckName name userMap = null $ filter (\(userId, user) -> (userName user == name)) (toList userMap)

handleUserF :: TVar UserMap -> UserF a -> STM a
handleUserF tMap userF = do
  userMap <- readTVar tMap
  case userF of
    AddUser reg cont -> do
      let (userId, userMap') = handleAddUser userMap reg
      writeTVar tMap userMap'
      return (cont userId)
    GetUsers cont -> return . cont $ map snd (toList userMap)
    GetUser userId cont -> return . cont $ Referee.UuidMap.lookup userId userMap
    CheckName name cont -> return . cont $ handleCheckName name userMap

inMemoryUserHandler :: TVar UserMap -> Free UserF a -> STM a
inMemoryUserHandler tvar = foldFree (handleUserF tvar)
