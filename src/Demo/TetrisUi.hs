module Demo.TetrisUi (startApp) where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map (findWithDefault)
import Demo.Tetris
  ( Game (..),
    TetrisDirection (..),
    changeSpeed,
    freeFall,
    initGame,
    moveGame,
    posNameMap,
    tickGame,
  )
import qualified Graphics.Vty as GV
import qualified Linear.V2 as LV (V2 (..))

type Pos = LV.V2 Int

data TetrisEvent = TetrisEvent

data TETRISNAME = TETRISNAME deriving (Show, Ord, Eq)

data Cell = TetrisCell | AppleCell | GridCell deriving (Show)

ui :: Game -> [Widget TETRISNAME]
ui s = [borderWithLabel (str "tetris") $ tetrisUi s <+> scoreUi s]

tetrisUi :: Game -> Widget TETRISNAME
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

scoreUi :: Game -> Widget TETRISNAME
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

handleEvent :: Game -> BrickEvent TETRISNAME TetrisEvent -> EventM TETRISNAME (Next Game)
handleEvent s (VtyEvent (GV.EvKey GV.KEsc [])) = halt s
handleEvent s (VtyEvent (GV.EvKey GV.KLeft [])) = continue $ moveGame TetrisLeft s
handleEvent s (VtyEvent (GV.EvKey GV.KRight [])) = continue $ moveGame TetrisRight s
handleEvent s (VtyEvent (GV.EvKey GV.KUp [])) = continue $ moveGame TetrisUp s
handleEvent s (VtyEvent (GV.EvKey GV.KDown [])) = continue $ freeFall s
handleEvent s (VtyEvent (GV.EvKey (GV.KChar '+') [])) = handleSpeed s (+)
handleEvent s (VtyEvent (GV.EvKey (GV.KChar '-') [])) = handleSpeed s (-)
handleEvent s (AppEvent TetrisEvent) = handleTick s
handleEvent s _ = continue s

handleTick :: Game -> EventM TETRISNAME (Next Game)
handleTick g = do
  g' <- liftIO $ tickGame g
  continue g'

handleSpeed :: Game -> (Int -> Int -> Int) -> EventM TETRISNAME (Next Game)
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

theApp :: App Game TetrisEvent TETRISNAME
theApp =
  App
    { appDraw = ui,
      appStartEvent = return,
      appHandleEvent = handleEvent,
      appAttrMap = const aMap,
      appChooseCursor = neverShowCursor
    }

startApp :: IO Game
startApp = do
  eventChannel <- newBChan 10
  delay <- newTVarIO 1000000

  forkIO $ sleepApp eventChannel delay

  let buildVty = GV.mkVty GV.defaultConfig
  initialVty <- buildVty

  state <- initGame delay

  customMain initialVty buildVty (Just eventChannel) theApp state

sleepApp :: BChan TetrisEvent -> TVar Int -> IO ()
sleepApp chan delay = forever $ do
  writeBChan chan TetrisEvent
  d <- readTVarIO delay
  threadDelay d
