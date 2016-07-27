import Test.Hspec

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.UUID as UUID

import Data.UUID.V4

import Referee.User
import Referee.Matchmaking
import Referee.Common.Types
import Referee.Game
import Referee.Examples.RockPaperScissors.Rules
-- import qualified TestMatchmaking as MatchmakingTests

main :: IO ()
main = hspec $ do
  userApiTest
  matchmakingApiTest
  endToEndTest
  -- MatchmakingTests.main

userApiTest = do
  let sharedName = T.pack "some name"
      registration1 = UserRegistration sharedName (T.pack "an email") (T.pack "pw")
      registration2 = UserRegistration sharedName (T.pack "another email") (T.pack "pw")
  userMap <- runIO newUserMap
  let interpret = runIO . translate . inMemoryUserHandler userMap
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
  let interpret = runIO . translate . inMemoryMatchmakingHandler matchmakingMap
      Just player1 = UUID.fromText . T.pack $ "1499467f-456f-438a-ad37-f4b1ea88326c"
      Just player2 = UUID.fromText . T.pack $ "b90bde1c-3388-42ff-b4b5-fbbaf376624f"
      Just player3 = UUID.fromText . T.pack $ "66860ac0-b1a2-4d3e-84aa-eb2deb4d642d"
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
  describe "Joining random matches also works." $ do
    mmId <- interpret $ joinRandom player1
    mmm <- interpret $ getMatchmaking mmId
    specify "After joining a random match, that match exists." $ do
      mmm `shouldSatisfy` Maybe.isJust

endToEndTest = do
  userMap <- runIO newUserMap
  mmMap <- runIO newMatchmakingMap
  gameMap <- runIO newGameMap
  let userHandler = inMemoryUserHandler userMap
      matchmakingHandler = inMemoryMatchmakingHandler mmMap
      gameHandler = inMemoryGameHandler rpsRules gameMap
      user1Registration = UserRegistration (T.pack "user1") (T.pack "email1@somewhere.com") (T.pack "password1")
      user2Registration = UserRegistration (T.pack "user2") (T.pack "email2@somewhere.com") (T.pack "password2")
  describe "Should be possible to register some users and then play some games." $ do
    mUserId1 <- runIO . translate . userHandler $ registerUser user1Registration
    mUserId2 <- runIO . translate . userHandler $ registerUser user2Registration
    specify "User registration should work." $ do
      mUserId1 `shouldSatisfy` Maybe.isJust
      mUserId2 `shouldSatisfy` Maybe.isJust
    let userId1 = Maybe.fromJust mUserId1
        userId2 = Maybe.fromJust mUserId2
    mmId <- runIO . translate . matchmakingHandler $ createMatchmaking Private
    joined1 <- runIO . translate . matchmakingHandler $ join userId1 mmId
    joined2 <- runIO . translate . matchmakingHandler $ join userId2 mmId
    specify "Matchmaking was created and joined successfully." $ do
      joined1 `shouldBe` True
      joined2 `shouldBe` True
    -- this bit of code would be nicer if I also provided a higher-level api
    -- corresponding to the routes rather than just low-level functionality
    Just mm <- runIO . translate . matchmakingHandler $ getMatchmaking mmId
    mGameId <- runIO . translate . gameHandler $ create mm
    specify "Can create a game from matchmaking." $ do
      mGameId `shouldSatisfy` Maybe.isJust
    let gameId = Maybe.fromJust mGameId
    -- gotta awkwardly figure out which user is which player...
    -- in the future this needs to be more deterministic
    Just view <- runIO . translate . gameHandler $ view gameId userId1
    let p1 = fst (player1 view)
    let p2 = fst (player2 view)
    addedCommand1 <- runIO . translate . gameHandler $ addCommand gameId p1 (Player1, Rock)
    specify "Player 1's command worked." (addedCommand1 `shouldBe` True)
    outcome1 <- runIO . translate . gameHandler $ outcome gameId
    specify "Game isn't over yet..." (outcome1 `shouldSatisfy` Maybe.isNothing)
    addedCommand2 <- runIO . translate . gameHandler $ addCommand gameId p2 (Player2, Paper)
    specify "Player 2's command worked." (addedCommand2 `shouldBe` True)
    outcome2 <- runIO . translate . gameHandler $ outcome gameId
    specify "Player 1 should have won." (outcome2 `shouldBe` Just (WinnerIs p2))
