{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Referee.UuidMap where

import qualified Data.Map as Map
import qualified Data.UUID as UUID
import qualified System.Random as Random

-- It may be better to paramaterize w/ g instead of requiring Random.StdGen.
-- This version gives an easier-to-use API and should be okay for now.
-- Maybe newtype it to hide implementation details?  Or nah.
data UuidMap a = UuidMap
  { gen :: Random.StdGen
  , updateId :: UUID.UUID -> a -> a
  , members :: Map.Map UUID.UUID a
  }

nilId :: UUID.UUID
nilId = UUID.nil

emptyIO :: (UUID.UUID -> a -> a) -> IO (UuidMap a)
emptyIO updateId = do
  g <- Random.getStdGen
  return (UuidMap g updateId Map.empty)

empty :: Random.StdGen -> (UUID.UUID -> a -> a) -> UuidMap a
empty g updateId = UuidMap g updateId Map.empty

lookup :: UUID.UUID -> UuidMap a -> Maybe a
lookup uuid uuidMap = Map.lookup uuid (members uuidMap)

findWithDefault :: a -> UUID.UUID -> UuidMap a -> a
findWithDefault def uuid uuidMap = Map.findWithDefault def uuid (members uuidMap)

adjust :: (a -> a) -> UUID.UUID -> UuidMap a -> UuidMap a
adjust f uuid = translate (Map.adjust f uuid)

update :: (a -> Maybe a) -> UUID.UUID -> UuidMap a -> UuidMap a
update f uuid = translate (Map.update f uuid)

delete :: UUID.UUID -> UuidMap a -> UuidMap a
delete uuid = translate (Map.delete uuid)

-- intended for internal use implementing a map API
-- probably shouldn't expose to users, as it spills details
translate :: (Map.Map UUID.UUID a -> Map.Map UUID.UUID a) -> UuidMap a -> UuidMap a
translate f uuidMap = uuidMap { members = members' }
  where members' = f (members uuidMap)

insert :: a -> UuidMap a -> (UUID.UUID, UuidMap a)
insert val uuidMap = (uuid, UuidMap gen' (updateId uuidMap) members')
  where (uuid, gen') = Random.random (gen uuidMap)
        val' = updateId uuidMap uuid val
        members' = Map.insert uuid val' (members uuidMap)

toList :: UuidMap a -> [(UUID.UUID, a)]
toList uuidMap = Map.toList (members uuidMap)

member :: UUID.UUID -> UuidMap a -> Bool
member uuid uuidMap = Map.member uuid (members uuidMap)

keys :: UuidMap a -> [UUID.UUID]
keys uuidMap = Map.keys (members uuidMap)
