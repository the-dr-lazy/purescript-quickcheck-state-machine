module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Gen as Gen
import Data.Array (fromFoldable)
import Data.Debug (class Debug, debug, genericDebug)
import Data.Either (Either(..))
import Data.Functor.Barbie as Barbie
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:), unsnoc, length, toUnfoldable, mapWithIndex, deleteAt, filter)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Debug (trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Queue (einit, elng, epop, epush, esetup, eteardown)
import Test.QuickCheck (class Arbitrary, Result(..), arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf)
import Test.QuickCheck.MBT (Outcome(..), testModel)
import Test.QuickCheck.StateMachine.Sequential (Concrete(..), Failure(..), Mock(..), StateMachine, Symbolic(..), forAll, runActions)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- First, we create a model that represents the system under test
newtype Model r = Model (List Int)

instance Debug (Model r) where
    debug (Model xs) = debug (List.toUnfoldable xs :: Array Int)

-- This represents a command in stateful testing
-- Here, because we are simulating a FIFO queue
-- we use the three commands push, pop, and length
data Command r = Push Int | Pop | GetLength

instance Barbie.Functor Command where
    map _ = case _ of
        Push x -> Push x
        Pop -> Pop
        GetLength -> GetLength

instance Barbie.Traversable Command where
    traverse _ = case _ of
        Push x -> pure $ Push x
        Pop -> pure Pop
        GetLength -> pure GetLength

derive instance Generic (Command r) _

instance Debug (Command r) where
    debug = genericDebug

-- This represents the Response of a command
-- Note that while Responses map 1-to-1 to commands, this is
-- not necessary for the general algorithm to work.
-- However, IMO, a 1-to-1 mapping makes things more readable.
data Response r = Pushed | Popped (Maybe Int) | Length Int

instance Barbie.Functor Response where
    map _ = case _ of
        Pushed -> Pushed
        Popped mx -> Popped mx
        Length l -> Length l

instance Barbie.Traversable Response where
    traverse _ = case _ of
        Pushed -> pure $ Pushed
        Popped mx -> pure $ Popped mx
        Length l -> pure $ Length l

derive instance Generic (Response r) _

instance Debug (Response r) where
    debug = genericDebug

transition :: forall r. Command r -> Response r -> Model r -> Model r
transition command response model = case command, response, model of
    Push x, Pushed, (Model xs) -> Model (x List.: xs)
    Pop, (Popped _), (Model xs) -> Model (List.unsnoc xs # map _.init # fromMaybe List.Nil)
    GetLength, (Length _), model -> model
    _, _, model -> model

mock :: Model Symbolic -> Command Symbolic -> Mock (Response Symbolic)
mock model command = case command, model of
    Push x, _ -> pure Pushed
    Pop, (Model xs) ->
        pure $ Popped (List.unsnoc xs # map _.last)
    GetLength, (Model xs) -> pure $ Length (List.length xs)

precondition :: Model Symbolic -> Command Symbolic -> Boolean
precondition (Model (List.Nil)) _ = true
precondition (Model (List.Cons _ _)) _ = false

generator :: Model Symbolic -> Maybe (Gen (Command Symbolic))
generator model = Just $ (Push <$> arbitrary) `Gen.choose` (pure Pop) `Gen.choose` (pure GetLength)

-- This is the system under test
-- It uses the Queue.purs implementation, which uses Queue.py under the hood
semantics :: Command Concrete -> Aff (Response Concrete)
semantics command = case command of
    Push x -> do
        epush x
        pure Pushed
    Pop -> Popped <$> epop
    GetLength -> Length <$> elng

-- The postcondition here is simple - we just verify that the mocked Response
-- is equal to the real Response. This won't always work, ie if a server generates
-- UUIDs. In that case, we'll need some other comparison, but for here, simple
-- equality comparison works
postcondition :: Model Concrete -> Command Concrete -> Response Concrete -> Boolean
postcondition (Model model) command response = case command, response of
    (Push _), Pushed -> true
    Pop, (Popped _) -> true
    GetLength, (Length l) -> List.length model == l
    _, _ -> false

-- This is the main function that does the PBT and shows the results
-- It should show { failures: Nil, successes: 100, total: 100 }
-- To run more than 100 tests, change the number 100 below
main :: Effect Unit
main =
    launchAff_
        $ runSpec [ consoleReporter ] do
              describe "testModel" do
                  it "works" do
                      let
                          stateMachine :: StateMachine Model Command Response Aff
                          stateMachine =
                              { initialModel: Model mempty
                              , mock
                              , generator
                              , precondition
                              , transition
                              , semantics
                              , postcondition
                              }

                      result <- forAll stateMachine \actions -> do
                          esetup 0
                          einit []
                          { failure } <- runActions stateMachine actions
                          pure $ case failure of
                              Nothing -> Success
                              Just FailedPostcondition -> Failed "PostCondition"
                              Just MockSemanticsMismatch -> Failed "Mock semantic mismatch"
                              _ -> Failed "Other!"

                      case result of
                          Success -> pure unit
                          Failed message -> throwError $ error message

-- res <-
--     testModel
--         { seed: 0
--         , nres: 100
--         , setup: esetup
--         , teardown: eteardown
--         , sutInitializer: initializer
--         , initialModelGenerator: arbitrary
--         , commandListGenerator: arbitrary
--         , commandShrinker: shrinker
--         , mock
--         , sut
--         , postcondition
--         }
-- 100 `shouldEqual` (length $ filter (\r -> isSuccess r.outcome) res)
