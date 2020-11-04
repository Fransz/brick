{-# LANGUAGE TupleSections #-}

module Demo.TetrisUi (startApp)
where

import Demo.Tetris
    (
    initialState
    , newTetrisState
    , TetrisState (..)
    , Block(..)
    )

import Brick
import Brick.Widgets.Border
import Brick.BChan
import Brick.Widgets.Center

import qualified Graphics.Vty as GV

import qualified Linear.V2 as LV (V2(..))

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified Data.Map as Map (Map, findWithDefault, fromList)

type Pos = LV.V2 Int

data TetrisEvent = TetrisEvent

data TETRISNAME = TETRISNAME deriving (Show, Ord, Eq)

data Cell = TetrisCell | AppleCell | GridCell deriving (Show)

ui :: TetrisState -> [Widget TETRISNAME]
ui s = [borderWithLabel (str "tetris") $ tetrisUi s <+> scoreUi s ]

tetrisUi :: TetrisState -> Widget TETRISNAME
tetrisUi s = pad $ vBox $ map hBox wids
  where
    pad w = padLeft (Pad 1) $ padTopBottom 1 $ border w

    -- Map of all blocks, all positions with the blocks attr.
    ps :: Map.Map Pos AttrName                      
    ps = Map.fromList (concatMap ats (blocks s))

    -- List off absolute positions of a block, in a tuple with the blocks attrName
    ats :: Block -> [(Pos, AttrName)]               
    ats b = map ((,attrName (name b)) . (+ c b)) (bps b)

    -- All positions on the grid as widget with the correct attrname
    rs = [ 0 .. rows s - 1]
    wids = map (\y -> [ draw (LV.V2 x y)  | x <- [0 .. cols s - 1] ]) rs
    draw = wid . cellAttr

    cellAttr :: Pos -> AttrName
    cellAttr p = Map.findWithDefault (attrName "grid") p ps

    wid :: AttrName -> Widget TETRISNAME
    wid a = withAttr a $ str "  "

scoreUi :: TetrisState -> Widget TETRISNAME
scoreUi s = pad $ if gameover s then msg else score <=> fill ' '
    where
      pad w = padLeft (Pad 10) $ padAll 1 $ vLimit 8 $ borderWithLabel (str "score") $ padAll 1 w
      score = hCenter (str ("score: " ++ show (counter s)))
      msg = center $ str "GAME OVER"

handleEvent :: TetrisState -> BrickEvent TETRISNAME TetrisEvent -> EventM TETRISNAME (Next TetrisState)
handleEvent s (VtyEvent (GV.EvKey GV.KEsc [])) = halt s
handleEvent s (AppEvent TetrisEvent) = continue (newTetrisState s)
handleEvent s _ = continue s

aMap :: AttrMap
aMap = attrMap GV.defAttr [
  (attrName "grid", bg GV.white)
  , (attrName "iblock", bg GV.brightRed)
  , (attrName "oblock", bg GV.brightGreen)
  , (attrName "tblock", bg GV.brightYellow)
  , (attrName "sblock", bg GV.brightBlue)
  , (attrName "zblock", bg GV.brightCyan)
  , (attrName "jblock", bg GV.brightMagenta)
  , (attrName "lblock", bg GV.brightBlack)
  ]

theApp :: App TetrisState TetrisEvent TETRISNAME
theApp = App {
    appDraw = ui,
    appStartEvent = return,
    appHandleEvent = handleEvent,
    appAttrMap = const aMap,
    appChooseCursor = neverShowCursor
}

startApp :: IO TetrisState
startApp = do
    eventChannel <- newBChan 10

    _ <- forkIO $ forever $ do
        writeBChan eventChannel TetrisEvent
        threadDelay 100000

    let buildVty = GV.mkVty GV.defaultConfig
    initialVty <- buildVty

    customMain initialVty buildVty (Just eventChannel) theApp initialState
