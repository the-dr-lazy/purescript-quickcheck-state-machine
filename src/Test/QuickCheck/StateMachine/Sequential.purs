{-|
Module     : Test.QuickCheck.StateMachine.Sequential
Maintainer : Mohammad Hasani (the-dr-lazy.github.io) <the-dr-lazy@pm.me>
Copyright  : (c) 2020-2022 Purescript QuickCheck State Machine
License    : MPL 2.0

This Source Code Form is subject to the terms of the Mozilla Public
License, version 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

module Test.QuickCheck.StateMachine.Sequential where

import Data.Dynamic
import Data.Either
import Data.Maybe
import Data.Ord
import Debug
import Effect
import Prelude
import Test.QuickCheck.Gen

import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec, tailRec, tailRecM, tailRecM2)
import Control.Monad.Rec.Class as Recursion
import Control.Monad.State (State, StateT(..), evalStateT, runState)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Debug (class Debug, debug, genericDebug)
import Data.Debug as Debug
import Data.Foldable (fold)
import Data.Functor.Barbie (traverse)
import Data.Functor.Barbie as Barbie
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, TypeRep, typeRep)
import Effect.Class (class MonadEffect, liftEffect)
import Partial (crashWith)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Random.LCG as LCG
import Test.QuickCheck (class Testable, Result(..), arbitrary, test)
import Test.QuickCheck.Gen (listOf) as Gen

type Counterexamples = List

suchThatEither :: forall a. Gen a -> (a -> Boolean) -> Gen (Either (Counterexamples a) a)
suchThatEither generator prediction = sized (try mempty 0 <<< max 100)
    where
    try :: List a -> Int -> Int -> Gen (Either (List a) a)
    try counterexamples _ 0 = pure (Left (List.reverse counterexamples))
    try counterexamples k n = do
        x <- resize (2 * k + n) generator
        if prediction x then pure (Right x)
        else try (x : counterexamples) (k + 1) (n - 1)

newtype Variable = Variable Int

instance Debug Variable where
    debug (Variable n) = Debug.constructor "Variable" [ Debug.int n ]

derive newtype instance Ord Variable

newtype Symbolic a = Symbolic (forall r. (Typeable a => Variable -> r) -> r)

newtype Concrete a = Concrete (forall r. (Typeable a => a -> r) -> r)

newtype Reference a r = Reference (r a)

newtype Action command response =
    Action
        { command :: command Symbolic
        , response :: response Symbolic
        , variables :: List Variable
        }

derive instance Generic (Action command response) _

instance (Debug (command Symbolic), Debug (response Symbolic)) => Debug (Action command response) where
    debug = genericDebug

newtype Actions command response = Actions (List (Action command response))

-- type StateMachine model command response m =
--   { transition     :: forall r. Ord1 r => model r -> command r -> response r -> model r
--   , precondition   :: model Symbolic -> command Symbolic -> Logic
--   , postcondition  :: model Concrete -> command Concrete -> response Concrete -> Logic
--   , invariant      :: Maybe (model Concrete -> Logic)
--   , generator      :: model Symbolic -> Maybe (Gen (command Symbolic))
--   , shrinker       :: model Symbolic -> command Symbolic -> List (command Symbolic)
--   , semantics      :: command Concrete -> m (response Concrete)
--   , mock           :: model Symbolic -> command Symbolic -> Mock (response Symbolic)
--   , cleanup        :: model Concrete -> m Unit
--   }

newtype Counter = Counter Int

newtype Mock a = Mock (State Counter a)

derive newtype instance Functor Mock
derive newtype instance Applicative Mock
derive newtype instance Monad Mock

symbolic :: forall a. Typeable a => Mock (Reference a Symbolic)
symbolic = Mock do
    Counter n <- State.get
    State.put <<< Counter $ n + 1
    pure <<< Reference $ Symbolic \f -> f (Variable n)

runMock :: forall a. Counter -> Mock a -> { response :: a, counter :: Counter }
runMock counter (Mock state) = let Tuple response counter = runState state counter in { response, counter }

-------------------------------------------------------
-- Sequential

type StateMachine model command response m =
    { initialModel :: forall r. model r
    , generator :: model Symbolic -> Maybe (Gen (command Symbolic))
    , precondition :: model Symbolic -> command Symbolic -> Boolean
    , transition :: forall r. command r -> response r -> model r -> model r
    , mock :: model Symbolic -> command Symbolic -> Mock (response Symbolic)
    , semantics :: command Concrete -> m (response Concrete)
    , postcondition :: model Concrete -> command Concrete -> response Concrete -> Boolean
    }

data Deadlock model command response =
    Deadlock
        { model :: model Symbolic
        , actions :: Actions command response
        , counterexamples :: Counterexamples (command Symbolic)
        }

generateActions :: forall model command response m. Barbie.Traversable response => StateMachine model command response m -> Gen (Either (Deadlock model command response) (Actions command response))
generateActions stateMachine = do
    size <- Gen.sized \n -> Gen.chooseInt 100 n
    tailRecM go { size, actions: mempty, model: stateMachine.initialModel, counter: Counter 0 }
    where
    go :: _ -> Gen (Recursion.Step _ (Either (Deadlock model command response) (Actions command response)))
    go { size: 0, actions } = pure <<< Recursion.Done <<< Right <<< Actions <<< List.reverse $ actions
    go { size, actions, model: inputModel, counter } =
        case stateMachine.generator inputModel of
            Nothing -> pure $ Recursion.Loop { size: 0, actions, model: inputModel, counter }
            Just generateCommand -> do
                eCommand <- generateCommand `suchThatEither` stateMachine.precondition inputModel
                case eCommand of
                    Left counterexamples -> pure <<< Recursion.Done <<< Left $ Deadlock { model: inputModel, actions: Actions $ List.reverse actions, counterexamples }
                    Right command -> do
                        let { response, counter: counter' } = runMock counter (stateMachine.mock inputModel command)
                        let outputModel = stateMachine.transition command response inputModel
                        let variables = getUsedVariables response
                        pure $ Recursion.Loop { size: (size - 1), actions: (Action { command, response, variables } List.: actions), model: outputModel, counter: counter' }

getUsedVariables :: forall barbie. Barbie.Traversable barbie => barbie Symbolic -> List Variable
getUsedVariables = Barbie.foldMap \(Symbolic variable) -> List.singleton (variable identity)

getUsedConcretes :: forall barbie. Barbie.Traversable barbie => barbie Concrete -> List (Dynamic Concrete)
getUsedConcretes = Barbie.foldMap \(Concrete f) -> List.singleton (f \x -> dynamic (Concrete \f' -> f' x))

data Failure
    = FailedPostcondition
    | BrokenInvariant
    | MockSemanticsMismatch
    | Exception

newtype Context = Context (Map Variable (Dynamic Concrete))

emptyContext :: Context
emptyContext = Context Map.empty

reifyByContext :: forall barbie. Barbie.Traversable barbie => Context -> barbie Symbolic -> barbie Concrete
reifyByContext (Context context) = Barbie.map \(Symbolic variable) -> case Map.lookup (variable identity) context of
    Nothing -> unsafeCrashWith "Context variable not found..."
    Just dyn -> case unwrapDynamic (variable \_ -> typeRep) dyn of
        Nothing -> unsafeCrashWith "Can't reify type..."
        Just x -> x

insertConcretes :: List Variable -> List (Dynamic Concrete) -> Context -> Context
insertConcretes variables dyns (Context context) = case variables, dyns of
    List.Nil, List.Nil -> Context context
    List.Cons variable variables, List.Cons dyn dyns -> insertConcretes variables dyns (Context $ Map.insert variable dyn context)
    _, _ -> unsafeCrashWith "impossible"

runActions
    :: forall model command response m
     . MonadRec m
    => Barbie.Traversable command
    => Barbie.Traversable response
    => StateMachine model command response m
    -> Actions command response
    -> m { failure :: Maybe Failure }
runActions stateMachine (Actions actions) = do
    let
        go :: { actions :: List (Action command response), context :: Context, inputModel :: model Concrete } -> m (Recursion.Step _ _)
        go { actions: List.Nil } = pure $ Recursion.Done Nothing
        go { actions: List.Cons (Action action) actions, context, inputModel } = do
            let command = reifyByContext context action.command
            response <- stateMachine.semantics command
            let variables = getUsedConcretes response
            if List.length variables /= List.length action.variables then pure $ Recursion.Done (Just MockSemanticsMismatch)
            else do
                let outputModel = stateMachine.transition command response inputModel
                if not $ stateMachine.postcondition outputModel command response then pure $ Recursion.Done (Just FailedPostcondition)
                else go { actions, context: insertConcretes action.variables variables context, inputModel: outputModel }

    failure <- tailRecM go { actions, inputModel: stateMachine.initialModel, context: emptyContext }
    pure { failure }

forAll
    :: forall model command response m property
     . Testable property
    => Debug (model Symbolic)
    => Debug (command Symbolic)
    => Debug (response Symbolic)
    => MonadRec m
    => MonadEffect m
    => Barbie.Traversable response
    => StateMachine model command response m
    -> (Actions command response -> m property)
    -> m Result
forAll stateMachine mkProperty = do
    seed <- liftEffect $ LCG.randomSeed
    let eActions = sequence $ evalGen (Gen.listOf 100 $ generateActions stateMachine) { newSeed: LCG.mkSeed 100, size: 1000 }
    let
        go :: List (Actions command response) -> m (Recursion.Step _ Result)
        go = case _ of
            List.Nil -> pure $ Recursion.Done Success
            List.Cons action actions -> do
                property <- mkProperty action
                case evalGen (test property) { newSeed: seed, size: 1 } of
                    failure@(Failed _) -> pure $ Recursion.Done failure
                    Success -> pure $ Recursion.Loop actions
    case eActions of
        Left deadlock -> pure <<< Failed <<< prettyPrintDeadlock $ deadlock
        Right actions -> tailRecM go actions

prettyPrintDeadlock
    :: forall model command response
     . Debug (model Symbolic)
    => Debug (command Symbolic)
    => Debug (response Symbolic)
    => Deadlock model command response
    -> String
prettyPrintDeadlock (Deadlock { model, actions: Actions actions, counterexamples }) = fold
    [ """
    A deadlock occured while generating actions.
    No pre-condition holds in the following model:


    """
    , "\t" <> (replaceAll (Pattern "\n") (Replacement "\n\t") <<< Debug.prettyPrint <<< debug $ model)
    , """


    The following actions have been generated so far:


    """
    , "\t" <> (replaceAll (Pattern "\n") (Replacement "\n\t") <<< Debug.prettyPrint <<< debug $ (List.toUnfoldable actions :: Array _))
    , """

    Example actions generated whose pre-condition doesn't hold:


    """
    , "\t" <> (replaceAll (Pattern "\n") (Replacement "\n\t") <<< Debug.prettyPrint <<< debug $ (List.toUnfoldable counterexamples :: Array _))
    ]
