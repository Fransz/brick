module Demo.Grid (startApp)
where

import Brick
import Brick.BChan
import Brick.Widgets.Border

import Linear.V2
import Lens.Micro
import qualified Graphics.Vty as GV
import Control.Concurrent
import Control.Monad
import Brick.Widgets.Center


newtype CounterEvent = Counter Int

data GRID_NAME = GRID_NAME deriving (Show, Ord, Eq)

type Pos = V2 Int

data State = State {
  rows :: Int,
  cols :: Int,
  snake :: [Pos],
  apple :: Pos,
  moves :: [(Int, Int)],
  counter :: Int
} deriving (Show)

data Cell = SnakeCell | AppleCell | GridCell deriving (Show)


ui :: State -> [Widget GRID_NAME]
ui s = [ borderWithLabel (str "snake") grid ]
  where
    rs = [ 0 .. rows s - 1]
    ws = map (\y -> [ draw (V2 x y)  | x <- [0 .. cols s -1] ]) rs
    draw = wid . isCell
    grid = vBox $ map hBox ws

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

initialState :: State
initialState = State 50 50 [V2 2 6, V2 3 6, V2 4 6, V2 4 7, V2 4 8] (V2 40 6) [] 0

handleEvent :: State -> BrickEvent GRID_NAME CounterEvent -> Brick.EventM GRID_NAME (Next State)
handleEvent s (VtyEvent (GV.EvKey GV.KEsc [])) = Brick.halt s
handleEvent s (VtyEvent (GV.EvKey GV.KUp [])) = Brick.continue (movApple s (0, -1))
handleEvent s (VtyEvent (GV.EvKey GV.KDown [])) = Brick.continue (movApple s (0, 1))
handleEvent s (VtyEvent (GV.EvKey GV.KLeft [])) = Brick.continue (movApple s (-1, 0))
handleEvent s (VtyEvent (GV.EvKey GV.KRight [])) = Brick.continue (movApple s (1, 0))
handleEvent s (AppEvent (Counter i)) = Brick.continue (incCounter s i)
handleEvent s _ = continue s

incCounter :: State -> Int -> State
incCounter s i = s { counter = i + 1 }

movApple :: State -> (Int, Int) -> State
movApple s (x, y) = let a = apple s
                        a' = a & _x %~ (\x' -> (x'+ x) `mod` cols s)
                        a'' = a' & _y %~ (\y' -> (y'+ y) `mod` rows s)
                    in s { apple = a'' }

aMap :: AttrMap
aMap = attrMap GV.defAttr [
  (attrName "grid", bg GV.brightBlack)
  , (attrName "apple", bg GV.brightGreen)
  , (attrName "snake", bg GV.red)
  ]

theApp :: App State CounterEvent GRID_NAME
theApp = App {
    appDraw = ui,
    appStartEvent = return,
    appHandleEvent = handleEvent,
    appAttrMap = const aMap,
    appChooseCursor = neverShowCursor
}

startApp :: IO State
startApp = do 
    eventChannel <- Brick.BChan.newBChan 10

    forkIO $ forever $ do 
        Brick.BChan.writeBChan eventChannel (Counter 1)
        threadDelay 1000000

    let buildVty = GV.mkVty GV.defaultConfig
    initialVty <- buildVty

    customMain initialVty buildVty (Just eventChannel) theApp initialState
