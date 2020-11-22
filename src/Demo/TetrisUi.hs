module Demo.TetrisUi (startApp) where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (execState)
import qualified Data.Map as Map (findWithDefault)
import Demo.Tetris
  ( Tetris (..),
    TetrisDirection (..),
    TetrisS,
    freeFallM,
    initialTetris,
    moveTetrisM,
    posNameMap,
    tickTetrisM,
  )
import qualified Graphics.Vty as GV
import qualified Linear.V2 as LV (V2 (..))
import qualified System.Random as Random (newStdGen)

type Pos = LV.V2 Int

data TetrisEvent = TetrisEvent

data TETRISNAME = TETRISNAME deriving (Show, Ord, Eq)

data Game = Game
  { pause :: Bool,
    gameDone :: Bool,
    delay :: TVar Int,
    game :: Tetris
  }

ui :: Game -> [Widget TETRISNAME]
ui s = [borderWithLabel (str "tetris") $ tetrisUi s <+> scoreUi s]

tetrisUi :: Game -> Widget TETRISNAME
tetrisUi s = hLimit 100 $ center $ pad $ vBox $ map hBox wids
  where
    tetris = game s
    pad w = padLeft (Pad 1) $ padTopBottom 1 $ border w

    rs = [0 .. rows tetris - 1]
    wids = map (\y -> [draw (LV.V2 x y) | x <- [0 .. cols tetris - 1]]) rs
    draw = wid . cellAttr

    cellAttr :: Pos -> AttrName
    cellAttr p = attrName $ Map.findWithDefault "grid" p $ posNameMap tetris

    wid :: AttrName -> Widget TETRISNAME
    wid a = withAttr a $ str "  "

scoreUi :: Game -> Widget TETRISNAME
scoreUi s
  | pause s = pad pauseMsg
  | gameDone s = pad gameDoneMsg
  | otherwise = pad $ scoreboard <=> fill ' '
  where
    tetris = game s
    pad w = padLeft (Pad 10) $ padRight (Pad 10) $ padAll 1 $ vLimit 8 $ borderWithLabel (str "score") $ padAll 1 w
    scoreboard =
      hCenter $
        vBox
          [ str ("count: " ++ show (counter tetris)),
            str ("score: " ++ show (score tetris)),
            str ("delay: " ++ show (speed tetris))
          ]
    gameDoneMsg = center $ str "GAME OVER"
    pauseMsg = center $ str "PAUSE"

handleEvent :: Game -> BrickEvent TETRISNAME TetrisEvent -> EventM TETRISNAME (Next Game)
handleEvent s (VtyEvent (GV.EvKey (GV.KChar 'p') [])) = continue $ s {pause = not $ pause s}
handleEvent s (VtyEvent (GV.EvKey GV.KEsc [])) = halt s
handleEvent s (VtyEvent (GV.EvKey GV.KLeft [])) = handleTetrisEvent s (moveTetrisM TetrisLeft)
handleEvent s (VtyEvent (GV.EvKey GV.KRight [])) = handleTetrisEvent s (moveTetrisM TetrisRight)
handleEvent s (VtyEvent (GV.EvKey GV.KUp [])) = handleTetrisEvent s (moveTetrisM TetrisUp)
handleEvent s (VtyEvent (GV.EvKey GV.KDown [])) = handleTetrisEvent s freeFallM
handleEvent s (AppEvent TetrisEvent) = handleTetrisEvent s tickTetrisM
handleEvent s (VtyEvent (GV.EvKey (GV.KChar '+') [])) = handleDelay s (+)
handleEvent s (VtyEvent (GV.EvKey (GV.KChar '-') [])) = handleDelay s (-)
handleEvent s _ = continue s

handleTetrisEvent :: Game -> TetrisS () -> EventM TETRISNAME (Next Game)
handleTetrisEvent s stateAct
  | pause s || gameDone s = continue s
  | otherwise = do
    let tetris' = execState stateAct $ game s
    s' <- liftIO $ setDelay s $ speed tetris'
    continue $ s' {game = tetris', gameDone = gameOver tetris'}

handleDelay :: Game -> (Int -> Int -> Int) -> EventM TETRISNAME (Next Game)
handleDelay g (+/-) = do
  g' <- liftIO $ changeDelay g (+/-)
  continue g'

changeDelay :: Game -> (Int -> Int -> Int) -> IO Game
changeDelay g (+/-) = do
  d <- readTVarIO (delay g)
  let s = (+/-) d 10000
  atomically $ writeTVar (delay g) s
  return g {game = (game g) {speed = s}}

setDelay :: Game -> Int -> IO Game
setDelay s d = do
  atomically $ writeTVar (delay s) d
  return s

{-
 - handleTick :: Tetris -> EventM TETRISNAME (Next Tetris)
 - handleTick g = do
 -   g' <- liftIO $ tickTetris g
 -   continue g'
--
-- Change the speed and the delay of the game

--
-- Set the delay of the game with the speed.

 -}

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

--
-- Initialize the game. I.e. create a stdGen
initGame :: TVar Int -> IO Game
initGame delay = do
  gen <- Random.newStdGen
  speed <- readTVarIO delay
  return $ Game {pause = False, gameDone = False, delay = delay, game = initialTetris {gen = gen, speed = speed}}

--
-- Main
startApp :: IO Game
startApp = do
  eventChannel <- newBChan 10
  delay <- newTVarIO 1000000

  forkIO $ sleepApp eventChannel delay

  let buildVty = GV.mkVty GV.defaultConfig
  initialVty <- buildVty

  state <- initGame delay

  customMain initialVty buildVty (Just eventChannel) theApp state

--
-- sleap thread
sleepApp :: BChan TetrisEvent -> TVar Int -> IO ()
sleepApp chan delay = forever $ do
  writeBChan chan TetrisEvent
  d <- readTVarIO delay
  threadDelay d
