module Main where

import Prelude
import Control.Monad.Eff (Eff)

class ITransformable a
class IDrawable a
class IShape a

foreign import data CHRONO :: !
foreign import data SF :: !

foreign import data Window :: *
foreign import data Shape :: *
foreign import data Transformable :: *
foreign import data Drawable :: *
foreign import data RectangleShape :: *
foreign import data Event :: *
foreign import data Color :: *
foreign import data Texture :: *

instance iTransformableRectangleShape :: ITransformable RectangleShape
instance iDrawableRectangleShape :: IDrawable RectangleShape
instance iShapeRectangleShape :: IShape RectangleShape

type ChronoEffect a = forall e. Eff (chrono::CHRONO | e) a
type SfEffect a = forall e. Eff (sf::SF | e) a
type AllEffects a = forall e. Eff (sf::SF, chrono::CHRONO | e) a

type IterationData =
  {
      window :: Window
    , pastTime :: Number
    , backgroundColor :: Color
    , startingRotation :: Number
    , startingScale :: Number
    , rotation :: Number
    , scale :: Number
    , bgShape :: RectangleShape
    , fgShape :: RectangleShape
    , continue :: Boolean
  }

foreign import toNumber :: Int -> Number

foreign import easeIn :: Number -> Number

foreign import timeNow :: ChronoEffect Number

foreign import makeWindow :: Int -> Int -> String -> SfEffect Window
foreign import makeColor :: Int -> Int -> Int -> Int -> SfEffect Color
foreign import makeTextureFromFile :: String -> SfEffect Texture
foreign import makeRectangleShape :: Number -> Number -> SfEffect RectangleShape

foreign import shapeSetTexture :: forall s. (IShape s) => s -> Texture -> SfEffect Unit

foreign import transformableSetRotation :: forall t. (ITransformable t) => t -> Number -> SfEffect Unit
foreign import transformableSetScale :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit
foreign import transformableSetOrigin :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit
foreign import transformableMove :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit

foreign import eventIsClosed :: Event -> SfEffect Boolean

foreign import windowIsOpen :: Window -> SfEffect Boolean
foreign import windowPollEvent :: Window -> (Event -> SfEffect Unit) -> SfEffect Unit
foreign import windowClear :: Window -> Color -> SfEffect Unit
foreign import windowDisplay :: Window -> SfEffect Unit
foreign import windowDraw :: forall d. (IDrawable d) => Window -> d -> SfEffect Unit
foreign import windowClose :: Window -> SfEffect Unit

foreign import loop ::
      forall r.
      ({continue :: Boolean | r} -> AllEffects {continue :: Boolean | r})
  ->  {continue :: Boolean | r}
  ->  AllEffects Unit

main :: AllEffects Unit
main = do
  window <- makeWindow windowWidth windowHeight "PureScript Pure11 C++11 Animation - Lettier.com"
  startAppLoop window

windowWidth :: Int
windowWidth = 600

windowHeight :: Int
windowHeight = 600

windowWidthN :: Number
windowWidthN = toNumber windowWidth

windowHeightN :: Number
windowHeightN = toNumber windowHeight

startAppLoop :: Window -> AllEffects Unit
startAppLoop window = do
  let textureWidth = 500.0
  let textureHeight = textureWidth

  let startingRotation = 0.0
  let startingScale = 0.0
  let rotation = startingRotation
  let scale = startingScale
  let scaleTemp = easeIn scale

  texture0 <- makeTextureFromFile "static/purescript-logo.png"
  texture1 <- makeTextureFromFile "static/purescript-logo-inverted.png"

  shape0 <- makeRectangleShape textureWidth textureHeight
  shape1 <- makeRectangleShape textureWidth textureHeight

  shapeSetTexture shape0 texture0
  shapeSetTexture shape1 texture1

  transformableSetOrigin shape0 (textureWidth / 2.0) (textureHeight / 2.0)
  transformableSetOrigin shape1 (textureWidth / 2.0) (textureHeight / 2.0)

  transformableMove shape0 (windowWidthN / 2.0) (windowHeightN / 2.0)
  transformableMove shape1 (windowWidthN / 2.0) (windowHeightN / 2.0)

  transformableSetRotation shape0 rotation
  transformableSetRotation shape1 rotation

  transformableSetScale shape0 scaleTemp scaleTemp
  transformableSetScale shape1 scaleTemp scaleTemp

  let bgShape = shape1
  let fgShape = shape0

  backgroundColor <- makeColor 29 34 45 255
  presentTime <- timeNow

  loop
    iteration
    {
        window: window
      , pastTime: presentTime
      , backgroundColor: backgroundColor
      , startingRotation: startingRotation
      , startingScale: startingScale
      , rotation: rotation
      , scale: scale
      , bgShape: bgShape
      , fgShape: fgShape
      , continue: true
    }

iteration ::
      IterationData
  ->  AllEffects IterationData
iteration
  {
      window: window
    , pastTime: pastTime
    , backgroundColor: backgroundColor
    , startingRotation: startingRotation
    , startingScale: startingScale
    , rotation: rotation
    , scale: scale
    , bgShape: bgShape
    , fgShape: fgShape
    , continue: continue
  }
  = do
  presentTime <- timeNow
  let diff = presentTime - pastTime
  let delta = diff / 1000.0

  let rotation' = ((60.0 * delta) + (if rotation >= 360.0 || scale > 34.0 then startingRotation else rotation))
  let scale' = ((8.0 * delta) + (if scale > 34.0 then startingScale else scale))
  let scaleTemp = easeIn scale'

  let bgShape' = if scale > 34.0 then fgShape else bgShape
  let fgShape' = if scale > 34.0 then bgShape else fgShape

  transformableSetRotation fgShape' rotation'
  transformableSetScale    fgShape' scaleTemp scaleTemp

  windowPollEvent window (\ event -> do
      isClosed <- eventIsClosed event
      if isClosed
        then do
          windowClose window
          pure unit
        else pure unit
    )

  let output =
        {
            window: window
          , pastTime: presentTime
          , backgroundColor: backgroundColor
          , startingRotation: startingRotation
          , startingScale: startingScale
          , rotation: rotation'
          , scale: scale'
          , bgShape: bgShape'
          , fgShape: fgShape'
          , continue: true
        }

  isOpen <- windowIsOpen window
  if not isOpen
    then pure (output { continue = false })
    else do
      render
        {
            window: window
          , backgroundColor: backgroundColor
          , bgShape: bgShape'
          , fgShape: fgShape'
        }

      pure output

render ::
      forall r.
      {
          window :: Window
        , backgroundColor :: Color
        , bgShape :: RectangleShape
        , fgShape :: RectangleShape
        | r
      }
  ->  SfEffect Unit
render
  {
      window: window
    , backgroundColor: backgroundColor
    , bgShape: bgShape
    , fgShape: fgShape
  }
  = do
  windowClear window backgroundColor
  windowDraw window bgShape
  windowDraw window fgShape
  windowDisplay window