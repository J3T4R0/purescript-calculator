module App.View
  ( view
  ) where
import Prelude hiding (div)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, button)
import App.Styles (styledButton)
import Text.Smolder.Markup (text, (#!), (!)) -- added (!)

-- LOCAL
import App.Update (State)
import App.Events (Event(..))
view :: State -> HTML Event
view state =
  div do
    button #! onClick (const Increment) $ text "Increment"
    div $ text "Count " <> show $ state
    button #! onClick (const Decrement) $ text "Decrement"
  	div ! styledButton "red" $ text ""
    div ! styledButton "green" $ text ""
    div ! styledButton "blue" $ text ""
    div ! styledButton "yellow" $ text ""
    div ! styledButton "red" #! onClick (const $ UserClick "red") $ text
    ""