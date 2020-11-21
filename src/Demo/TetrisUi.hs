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
    gameOver :: Bool,
    {- delay :: Int, -}
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
scoreUi s = pad $ if gameOver s then msg else scoreboard <=> fill ' '
  where
    tetris = game s
    pad w = padLeft (Pad 10) $ padAll 1 $ vLimit 8 $ borderWithLabel (str "score") $ padAll 1 w
    scoreboard =
      hCenter $
        vBox
          [ str ("count: " ++ show (counter tetris)),
            str ("score: " ++ show (score tetris)),
            str ("speed: " ++ show (speed tetris))
          ]
    msg = center $ str "GAME OVER"

handleEvent :: Game -> BrickEvent TETRISNAME TetrisEvent -> EventM TETRISNAME (Next Game)
handleEvent s (VtyEvent (GV.EvKey GV.KEsc [])) = halt s
handleEvent s (VtyEvent (GV.EvKey GV.KLeft [])) = continue $ handleTetrisEvent s (moveTetrisM TetrisLeft)
handleEvent s (VtyEvent (GV.EvKey GV.KRight [])) = continue $ handleTetrisEvent s (moveTetrisM TetrisRight)
handleEvent s (VtyEvent (GV.EvKey GV.KUp [])) = continue $ handleTetrisEvent s (moveTetrisM TetrisUp)
handleEvent s (VtyEvent (GV.EvKey GV.KDown [])) = continue $ handleTetrisEvent s freeFallM
handleEvent s (AppEvent TetrisEvent) = continue $ handleTetrisEvent s tickTetrisM
handleEvent s _ = continue s

handleTetrisEvent :: Game -> TetrisS () -> Game
handleTetrisEvent s act = s {game = execState act $ game s}

{-
 - handleTick :: Tetris -> EventM TETRISNAME (Next Tetris)
 - handleTick g = do
 -   g' <- liftIO $ tickTetris g
 -   continue g'
handleEvent s (VtyEvent (GV.EvKey (GV.KChar '+') [])) = handleSpeed s (+)
handleEvent s (VtyEvent (GV.EvKey (GV.KChar '-') [])) = handleSpeed s (-)
--
-- Change the speed and the delay of the game
changeSpeed :: Tetris -> (Int -> Int -> Int) -> IO Tetris
changeSpeed g (+/-) = do
  d <- readTVarIO (delay g)
  atomically $ writeTVar (delay g) $ (+/-) d 10000
  return g {speed = (+/-) d 10000}

--
-- Set the delay of the game with the speed.
setDelay :: Tetris -> IO Tetris
setDelay g = do
  atomically $ writeTVar (delay g) (speed g)
  return g

handleSpeed :: Tetris -> (Int -> Int -> Int) -> EventM TETRISNAME (Next Tetris)
handleSpeed g (+/-) = do
  g' <- liftIO $ changeSpeed g (+/-)
  continue g'
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

--
-- Initialize the game. I.e. create a stdGen
initGame :: TVar Int -> IO Game
initGame delay = do
  g <- Random.newStdGen
  speed <- readTVarIO delay
  return $ Game {pause = False, gameOver = False, game = initialTetris {gen = g, speed = speed}}
