module Main where

import SDL hiding (Event)
import SDL.Video.Renderer (Rectangle(..), drawRect, fillRect)
import SDL.Vect (Point(..))
import qualified SDL.Event as SDLEvent

import Linear (V4(..), V2(..))

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Data.Maybe (mapMaybe, fromMaybe)
import Control.Lens
import Control.Monad (unless)

import qualified Control.Wire as Wire

$(makeLensesFor [
    ("keyboardEventKeysym", "keysym"),
    ("keyboardEventKeyMotion", "keyMotion")
  ] ''KeyboardEventData)

$(makeLensesFor [
    ("keysymKeycode", "keycode")
  ] ''Keysym)

data Direction = MoveLeft | MoveRight | MoveUp | MoveDown
  deriving (Eq, Ord)

data SceneEvent = Move Direction
  deriving (Eq, Ord)

data Event = SceneEvent SceneEvent | Quit
  deriving (Eq, Ord)

$(makePrisms ''Event)

type Events = Set.Set Event

data Scene = Scene { _position :: (Int, Int) }

$(makeLenses ''Scene)

type Draw = Renderer -> IO ()

height = 100
width = 100

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Lonely Rectangle" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer $ appWire $ Scene (50, 50)

createRect :: (Integral i, Num r) => (i, i) -> (i, i) -> Rectangle r
createRect (x, y) (w, h) =
  let leftCorner = P (V2 (fromIntegral x) (fromIntegral y)) in
  let dimensions = V2 (fromIntegral w) (fromIntegral h) in
  Rectangle leftCorner dimensions

renderScene :: Scene -> Renderer -> IO ()
renderScene (Scene position) renderer = do
  rendererDrawColor renderer $= V4 0 255 0 255
  fillRect renderer $ Just $ createRect position (height, width)

parseEvents :: [SDLEvent.Event] -> Events
parseEvents = foldl parseEvent Set.empty
  where
    parseEvent s (SDLEvent.Event _ (KeyboardEvent keyboardEvent)) =
      parseKeyboardEvent keyboardEvent s
    parseEvent s (SDLEvent.Event _ (WindowClosedEvent _)) =
      Set.insert Quit s
    parseEvent s _ = s

    eventsMap = Map.fromList [
        (KeycodeQ, Quit),
        (KeycodeLeft, SceneEvent $ Move MoveLeft),
        (KeycodeRight, SceneEvent $ Move MoveRight),
        (KeycodeUp, SceneEvent $ Move MoveUp),
        (KeycodeDown, SceneEvent $ Move MoveDown),
        (KeycodeA, SceneEvent $ Move MoveLeft),
        (KeycodeD, SceneEvent $ Move MoveRight),
        (KeycodeW, SceneEvent $ Move MoveUp),
        (KeycodeS, SceneEvent $ Move MoveDown)
      ]

    parseKeyboardEvent :: KeyboardEventData -> Events -> Events
    parseKeyboardEvent keyboardEvent s =
      if keyboardEvent ^. keyMotion == Pressed
      then fromMaybe s $
        flip Set.insert s <$>
        Map.lookup (keyboardEvent ^. keysym ^. keycode) eventsMap
      else s

updateScene :: Scene -> Events -> Scene
updateScene scene = foldr applyEvent scene . onlySceneEvents
  where
    onlySceneEvents = mapMaybe (^? _SceneEvent) . Set.toList

    applyEvent (Move MoveLeft) = (position . _1) `over` subtract 10
    applyEvent (Move MoveRight) = (position . _1) `over` (+10)
    applyEvent (Move MoveUp) = (position . _2) `over` subtract 10
    applyEvent (Move MoveDown) = (position . _2) `over` (+10)

appWire :: Scene -> Wire.Wire Int e IO Events Draw
appWire init = proc events -> do
  rec
    scene <- Wire.delay init -< scene'
    let scene' = flip updateScene events scene
  Wire.returnA -< renderScene scene

appLoop :: Renderer -> Wire.Wire Int e IO Events Draw -> IO ()
appLoop renderer w = do
  events <- parseEvents <$> pollEvents
  (Right draw, w') <- Wire.stepWire w 0 (Right events)
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  draw renderer
  present renderer
  unless (Set.member Quit events) $ appLoop renderer w'
