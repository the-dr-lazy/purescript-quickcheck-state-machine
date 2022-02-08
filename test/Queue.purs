module Queue (epush, epop, elng, esetup, eteardown, einit) where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)

foreign import push :: Int -> Effect Unit

foreign import pop :: Effect (Array Int)

foreign import lng :: Effect Int

foreign import setup :: Int -> Effect Unit

foreign import teardown :: Int -> Effect Unit

foreign import init :: Array Int -> Effect Unit

epush :: Int -> Aff Unit
epush = liftEffect <<< push

epop :: Aff (Maybe Int)
epop = do
    l <- liftEffect pop
    liftEffect $ pure (if (Array.length l == 0) then Nothing else (Array.(!!) l 0))

elng :: Aff Int
elng = liftEffect lng

esetup :: Int -> Aff Unit
esetup = liftEffect <<< setup

eteardown :: Int -> Aff Unit
eteardown = liftEffect <<< teardown

einit :: Array Int -> Aff Unit
einit = liftEffect <<< init
