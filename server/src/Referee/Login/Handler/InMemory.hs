module Referee.Login.Handler.InMemory where

import Control.Monad.Free
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)

import Referee.Common.Types as Types
import Referee.User.Types
import Referee.Login.Types
import Referee.Login.Api
import qualified Referee.UuidMap as UuidMap

type LoginMap = UuidMap.UuidMap Login

updateLoginId :: LoginId -> Login -> Login
updateLoginId uuid login = login { loginId = uuid }

newLoginMap :: IO (TVar LoginMap)
newLoginMap = do
  newMap <- UuidMap.emptyIO updateLoginId
  newTVarIO newMap

handleLookupLogin :: LoginMap -> Types.Email -> Maybe Login
handleLookupLogin loginMap targetEmail =
  (listToMaybe . filter (\login -> email login  == targetEmail)
  . map snd . UuidMap.toList) loginMap

handleCreateLogin :: LoginMap -> Types.Email -> HashedPassword -> UserId -> (LoginId, LoginMap)
handleCreateLogin loginMap email hashedPass userId = UuidMap.insert (Login email hashedPass userId UuidMap.nilId) loginMap

handleLoginF :: TVar LoginMap -> LoginF a -> STM a
handleLoginF tMap loginF = do
  loginMap <- readTVar tMap
  case loginF of
    LookupLogin email cont -> return . cont $ handleLookupLogin loginMap email
    CreateLogin email hashedPass userId cont -> do
      let (loginId, loginMap') = handleCreateLogin loginMap email hashedPass userId
      writeTVar tMap loginMap'
      return (cont (Just loginId))

inMemoryLoginHandler :: TVar LoginMap -> Free LoginF a -> STM a
inMemoryLoginHandler tvar = foldFree (handleLoginF tvar)
