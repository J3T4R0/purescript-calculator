module Main where
import Prelude
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
-- LOCAL
import App.View (view)
import App.Update (foldp, AppEffects)

init :: State
init = 
	{currentColor: "",
	Int: 0}
main :: Eff (CoreEffects AppEffects) Unit
main = do
  app <- start
    { initialState: 0
    , view
    , foldp
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input