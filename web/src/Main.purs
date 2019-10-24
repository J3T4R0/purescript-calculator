module Main where

import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)

main :: forall fx. Eff (CoreEffects fx) Unit
main = do
	app <- start
		{ initialState: 0
		, view
		, foldp
		, inputs: []
		}
	renderToDOM "#app" app.markup app.input