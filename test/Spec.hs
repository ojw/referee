import Test.Hspec

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Set as Set

import Referee.User
import Referee.Matchmaking

main :: IO ()
main = hspec $ do
  userApiTest
  matchmakingApiTest

-- Yuck addUser collides w/ too many names at the moment
userApiTest = do
  let sharedName = T.pack "some name"
      registration1 = UserRegistration sharedName (T.pack "an email")
      registration2 = UserRegistration sharedName (T.pack "another email")
  userMap <- runIO newUserMap
  let interpret = runIO . inMemoryUserHandler userMap
  describe "Adding a user works." $ do
    users <- interpret getUsers
    specify "The initial user list is empty." $ do
      users `shouldBe` []
    mUserId <- interpret . registerUser $ registration1
    specify "Adding a user returns a userid." $ do
      mUserId `shouldSatisfy` Maybe.isJust
    users' <- interpret getUsers
    specify "Now there should be one user." $ do
      users' `shouldNotSatisfy` null
      length users' `shouldBe` 1
    mUser <- interpret . getUser . Maybe.fromJust $ mUserId
    let user = Maybe.fromJust mUser
    specify "Now that a user exists, it can be looked up my id." $ do
      mUser `shouldSatisfy` Maybe.isJust
    specify "Also, the user's info should match what was in the registration." $ do
      userName user `shouldBe` registrationName registration1
      userEmail user `shouldBe` registrationEmail registration1
    available <- interpret $ checkName (T.pack "some name")
    specify "The first user's name is no longer available." $ do
      available `shouldBe` False
    mUserId' <- interpret . registerUser $ registration2
    specify "Adding another user with the same name will fail." $ do
      mUserId' `shouldBe` Nothing

matchmakingApiTest = do
  matchmakingMap <- runIO newMatchmakingMap
  let interpret = runIO . inMemoryMatchmakingHandler matchmakingMap
      player1 = 1
      player2 = 2
      player3 = 3
  describe "Matchmaking creation works." $ do
    matches <- interpret publicMatches
    specify "Initially there are no public matches." $ do
      matches `shouldBe` []
    mmId <- interpret $ createMatchmaking Public
    matches' <- interpret publicMatches
    specify "After adding a public matchmaking thing, there should be one." $ do
      matches' `shouldNotSatisfy` null
    itWorked <- interpret $ tryJoin player1 mmId
    specify "Should be possible to join the new matchmaking." $ do
      itWorked `shouldBe` True
    secondPlayer <- interpret $ tryJoin player2 mmId
    specify "A second player can join the matchmaking." $ do
      secondPlayer `shouldBe` True
    thirdPlayer <- interpret $ tryJoin player3 mmId
    specify "...but there's no room for more players." $ do
      thirdPlayer `shouldBe` False
