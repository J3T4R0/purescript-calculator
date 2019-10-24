module App.Update
  ( State
  , AppEffects  
  , foldp
  ) where
import Prelude
import Pux (EffModel)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
-- LOCAL
import App.Events (Event(..))
type State = {
  currentColor :: String, 
  Int
}
type AppEffects = (console :: CONSOLE)
foldp :: Event -> State -> EffModel State Event AppEffects
foldp Increment state =
  { state: state + 1
  , effects:
    [ log (show $ state + 1) *> pure Nothing
    ]
  }
foldp Decrement state =
  { state: state - 1
  , effects:
    [ log (show $ state - 1) *> pure Nothing
    ]
  }
foldp (UserClick color) state =
  { state: state { currentColor = color }
  , effects: []
  }

foldp Reset state = noEffects state { currentColor = "", Int = 0}