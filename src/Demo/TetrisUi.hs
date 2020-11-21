module Demo.TetrisUi (startApp) where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (execState)
import qualified Data.Map as Map (findWithDefault)
import Demo.Tetris
  ( Tetris (..),
    TetrisDirection (..),
    changeSpeed,
    freeFallM,
    initTetris,
    moveTetrisM,
    posNameMap,
    tickTetrisM,
  )
import qualified Graphics.Vty as GV
import qualified Linear.V2 as LV (V2 (..))

type Pos = LV.V2 Int

data TetrisEvent = TetrisEvent

data TETRISNAME = TETRISNAME deriving (Show, Ord, Eq)

data Cell = TetrisCell | AppleCell | GridCell deriving (Show)

ui :: Tetris -> [Widget TETRISNAME]
ui s = [borderWithLabel (str "tetris") $ tetrisUi s <+> scoreUi s]

tetrisUi :: Tetris -> Widget TETRISNAME
tetrisUi s = hLimit 100 $ center $ pad $ vBox $ map hBox wids
  where
    pad w = padLeft (Pad 1) $ padTopBottom 1 $ border w

    rs = [0 .. rows s - 1]
    wids = map (\y -> [draw (LV.V2 x y) | x <- [0 .. cols s - 1]]) rs
    draw = wid . cellAttr

    cellAttr :: Pos -> AttrName
    cellAttr p = attrName $ Map.findWithDefault "grid" p $ posNameMap s

    wid :: AttrName -> Widget TETRISNAME
    wid a = withAttr a $ str "  "

scoreUi :: Tetris -> Widget TETRISNAME
scoreUi s = pad $ if gameover s then msg else scoreboard <=> fill ' '
  where
    pad w = padLeft (Pad 10) $ padAll 1 $ vLimit 8 $ borderWithLabel (str "score") $ padAll 1 w
    scoreboard =
      hCenter $
        vBox
          [ str ("count: " ++ show (counter s)),
            str ("score: " ++ show (score s)),
            str ("speed: " ++ show (speed s))
          ]
    msg = center $ str "GAME OVER"

handleEvent :: Tetris -> BrickEvent TETRISNAME TetrisEvent -> EventM TETRISNAME (Next Tetris)
handleEvent s (VtyEvent (GV.EvKey GV.KEsc [])) = halt s
handleEvent s (VtyEvent (GV.EvKey GV.KLeft [])) = continue $ execState (moveTetrisM TetrisLeft) s
handleEvent s (VtyEvent (GV.EvKey GV.KRight [])) = continue $ execState (moveTetrisM TetrisRight) s
handleEvent s (VtyEvent (GV.EvKey GV.KUp [])) = continue $ execState (moveTetrisM TetrisUp) s
handleEvent s (VtyEvent (GV.EvKey GV.KDown [])) = continue $ execState freeFallM s
handleEvent s (VtyEvent (GV.EvKey (GV.KChar '+') [])) = handleSpeed s (+)
handleEvent s (VtyEvent (GV.EvKey (GV.KChar '-') [])) = handleSpeed s (-)
handleEvent s (AppEvent TetrisEvent) = continue $ execState tickTetrisM s
handleEvent s _ = continue s

{-
 - handleTick :: Tetris -> EventM TETRISNAME (Next Tetris)
 - handleTick g = do
 -   g' <- liftIO $ tickTetris g
 -   continue g'
 -}

handleSpeed :: Tetris -> (Int -> Int -> Int) -> EventM TETRISNAME (Next Tetris)
handleSpeed g (+/-) = do
  g' <- liftIO $ changeSpeed g (+/-)
  continue g'

aMap :: AttrMap
aMap =
  attrMap
    GV.defAttr
    [ (attrName "grid", bg GV.white),
      (attrName "iblock", bg GV.brightRed),
      (attrName "oblock", bg GV.brightGreen),
      (attrName "tblock", bg GV.brightYellow),
      (attrName "sblock", bg GV.brightBlue),
      (attrName "zblock", bg GV.brightCyan),
      (attrName "jblock", bg GV.brightMagenta),
      (attrName "lblock", bg GV.brightBlack),
      (attrName "wallblock", bg GV.yellow)
    ]

theApp :: App Tetris TetrisEvent TETRISNAME
theApp =
  App
    { appDraw = ui,
      appStartEvent = return,
      appHandleEvent = handleEvent,
      appAttrMap = const aMap,
      appChooseCursor = neverShowCursor
    }

startApp :: IO Tetris
startApp = do
  eventChannel <- newBChan 10
  delay <- newTVarIO 1000000

  forkIO $ sleepApp eventChannel delay

  let buildVty = GV.mkVty GV.defaultConfig
  initialVty <- buildVty

  state <- initTetris delay

  customMain initialVty buildVty (Just eventChannel) theApp state

sleepApp :: BChan TetrisEvent -> TVar Int -> IO ()
sleepApp chan delay = forever $ do
  writeBChan chan TetrisEvent
  d <- readTVarIO delay
  threadDelay d
