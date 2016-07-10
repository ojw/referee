module Referee.Login.Types

  ( Login (..)
  -- don't want to export the constructor on this guy
  -- so it's impossible to make a HashedPassword that isn't hashed
  , HashedPassword
  , hashPassword
  , validatePassword
  )

where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Crypto.KDF.BCrypt as BCrypt

import Referee.User.Types

-- username is the field we need on the login route to lookup the login record
-- so it would be nice for it to live on the login record
-- yuck, should it live in both places?
-- probably username should live w/ user, and be used to look up login info
-- buuut I'm gonna leave it here for now

-- this is a lesson in the drawbacks to low-level, fine-granularity,
-- free-monad based APIs

-- although... maybe not.
-- as a higher-level api with a different backend could use the lower-level
-- api differently.

-- for now, this will just take whatever form is short-term convenient

data Login = Login
  { username :: T.Text
  , hashedPassword :: HashedPassword
  , userId :: UserId
  }

newtype HashedPassword = HashedPassword { hashedBytes :: B.ByteString }
  deriving (Eq)

-- this really needs to live in a config file or something
bCryptCost = 10

hashPassword :: B.ByteString -> IO HashedPassword
hashPassword password = HashedPassword <$> BCrypt.hashPassword bCryptCost password

validatePassword :: B.ByteString -> Login -> Bool
validatePassword password login = BCrypt.validatePassword password (hashedBytes (hashedPassword login))
