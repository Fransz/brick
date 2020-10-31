module Demo.Grid (startApp)
where

import Brick
import Brick.Widgets.Border

import qualified Graphics.Vty as GV

data GRID_NAME = GRID_NAME deriving (Show, Ord, Eq)

data State = State {
  rows :: Int,
  cols :: Int,
  snake :: [(Int, Int)],
  apple :: (Int, Int),
  moves :: [(Int, Int)]
} deriving (Show)

data Cell = SnakeCell | AppleCell | GridCell deriving (Show)
          
ui :: State -> [Widget GRID_NAME]
ui s = [ borderWithLabel (str "snake") grid ]
  where
    rs = [ 0 .. rows s - 1]
    ws = map (\y -> [ draw (x, y) | x <- [0 .. cols s -1] ]) rs
    draw = wid . isCell
    grid = vBox $ map hBox ws

    isCell :: (Int, Int) -> Cell
    isCell (x, y) 
      | (x, y) `elem` snake s = SnakeCell
      | (x, y) == apple s = AppleCell
      | otherwise = GridCell

    wid :: Cell -> Widget GRID_NAME
    wid c = case c of
      SnakeCell -> withAttr (attrName "snake") $ str "  "
      AppleCell -> withAttr (attrName "apple") $ str "  "
      GridCell -> withAttr (attrName "grid") $ str "  "

initialState :: State
initialState = State 50 50 [(2,6), (3,6), (4,6), (5,6), (6,6)] (40,6) []

handleEvent :: State -> BrickEvent GRID_NAME ev -> Brick.EventM GRID_NAME (Next State)
handleEvent s (VtyEvent (GV.EvKey GV.KEsc [])) = Brick.halt s
handleEvent s (VtyEvent (GV.EvKey GV.KUp [])) = Brick.continue (movApple s (0, -1))
handleEvent s (VtyEvent (GV.EvKey GV.KDown [])) = Brick.continue (movApple s (0, 1))
handleEvent s (VtyEvent (GV.EvKey GV.KLeft [])) = Brick.continue (movApple s (-1, 0))
handleEvent s (VtyEvent (GV.EvKey GV.KRight [])) = Brick.continue (movApple s (1, 0))
handleEvent s _ = continue s

movApple :: State -> (Int, Int) -> State
movApple s (x, y) = let a = apple s
                        x' = (fst a + x) `mod` cols s
                        y' = (snd a + y) `mod` rows s
                    in s { apple = (x', y') }

aMap :: AttrMap
aMap = attrMap GV.defAttr [
  (attrName "grid", bg GV.brightBlack)
  , (attrName "apple", bg GV.brightGreen)
  , (attrName "snake", bg GV.red)
  ]

theApp :: App State e GRID_NAME
theApp = App {
    appDraw = ui,
    appStartEvent = return,
    appHandleEvent = handleEvent,
    appAttrMap = const aMap,
    appChooseCursor = neverShowCursor
}

startApp :: IO State
startApp = defaultMain theApp initialState
