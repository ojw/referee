module Referee.Login.Handler.InMemory where

import Control.Monad.Free
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)

import Referee.User.Types
import Referee.Login.Types
import Referee.Login.Api
import Referee.UuidMap

type LoginMap = UuidMap Login

updateLoginId :: LoginId -> Login -> Login
updateLoginId uuid login = login { loginId = uuid }

newLoginMap :: IO (TVar LoginMap)
newLoginMap = do
  newMap <- emptyIO updateLoginId
  newTVarIO newMap

handleLookupLogin :: LoginMap -> T.Text -> Maybe Login
handleLookupLogin loginMap targetUsername =
  (listToMaybe . filter (\login -> username login  == targetUsername)
  . map snd . toList) loginMap

-- I think we should switch username to email in login
-- and have to confirm that an email is unregistered when creating login.
createLogin :: LoginMap -> T.Text -> HashedPassword -> UserId -> (Bool, LoginMap)
createLogin = undefined
