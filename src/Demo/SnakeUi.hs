module Demo.SnakeUi (startApp)
where

import Demo.Snake 
    (initialState
    , SnakeState(..)
    , SnakeMove(..)
    , Pos
    , addMove
    , newSnakeState
    , CounterEvent(..))

import Brick
import Brick.BChan
import Brick.Widgets.Border
import qualified Graphics.Vty as GV

import qualified Linear.V2 as LV (V2(..))
import Control.Concurrent
import Control.Monad


data GRID_NAME = GRID_NAME deriving (Show, Ord, Eq)

data Cell = SnakeCell | AppleCell | GridCell deriving (Show)

ui :: SnakeState -> [Widget GRID_NAME]
ui s = [borderWithLabel (str "snake") $ snakeUi s <+> scoreUi s ]

snakeUi :: SnakeState -> Widget GRID_NAME
snakeUi s = pad $ vBox $ map hBox wids
  where
    pad w = padLeft (Pad 1) $ padTopBottom 1 $ border w

    rs = [ 0 .. rows s - 1]
    wids = map (\y -> [ draw (LV.V2 x y)  | x <- [0 .. cols s - 1] ]) rs
    draw = wid . isCell

    isCell :: Pos -> Cell
    isCell p
      | p `elem` snake s = SnakeCell
      | p == apple s = AppleCell
      | otherwise = GridCell

    wid :: Cell -> Widget GRID_NAME
    wid c = case c of
      SnakeCell -> withAttr (attrName "snake") $ str "  "
      AppleCell -> withAttr (attrName "apple") $ str "  "
      GridCell -> withAttr (attrName "grid") $ str "  "

scoreUi :: SnakeState -> Widget GRID_NAME
scoreUi s = pad $ score <=> fill ' '
    where 
      pad w = padLeft (Pad 10) $ padAll 1 $ vLimit 10 $ borderWithLabel (str "score") $ padAll 1 w
      score = str $ "score: " ++ show (counter s)

handleEvent :: SnakeState -> BrickEvent GRID_NAME CounterEvent -> Brick.EventM GRID_NAME (Next SnakeState)
handleEvent s (VtyEvent (GV.EvKey GV.KEsc [])) = Brick.halt s
handleEvent s (VtyEvent (GV.EvKey GV.KUp [])) = Brick.continue (addMove s SnakeUp)
handleEvent s (VtyEvent (GV.EvKey GV.KDown [])) = Brick.continue (addMove s SnakeDown)
handleEvent s (VtyEvent (GV.EvKey GV.KLeft [])) = Brick.continue (addMove s SnakeLeft)
handleEvent s (VtyEvent (GV.EvKey GV.KRight [])) = Brick.continue (addMove s SnakeRight)
handleEvent s (AppEvent (Counter i)) = Brick.continue (newSnakeState s)
handleEvent s _ = continue s

aMap :: AttrMap
aMap = attrMap GV.defAttr [
  (attrName "grid", bg GV.brightBlack)
  , (attrName "apple", bg GV.brightGreen)
  , (attrName "snake", bg GV.red)
  ]

theApp :: App SnakeState CounterEvent GRID_NAME
theApp = App {
    appDraw = ui,
    appStartEvent = return,
    appHandleEvent = handleEvent,
    appAttrMap = const aMap,
    appChooseCursor = neverShowCursor
}

startApp :: IO SnakeState
startApp = do 
    eventChannel <- Brick.BChan.newBChan 10

    _ <- forkIO $ forever $ do 
        Brick.BChan.writeBChan eventChannel (Counter 1)
        threadDelay 1000000

    let buildVty = GV.mkVty GV.defaultConfig
    initialVty <- buildVty

    customMain initialVty buildVty (Just eventChannel) theApp initialState
