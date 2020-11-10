module Main where

import qualified Brick.AttrMap as BA
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Util as BU
import qualified Brick.Widgets.Border as BWB
import qualified Brick.Widgets.Border.Style as BWBS
import qualified Brick.Widgets.Center as BWC
import qualified Brick.Widgets.Core as BW
import qualified Brick.Widgets.List as BL
import Control.Monad (void)
import qualified Data.Vector as DV (fromList)
import qualified Demo.CustomEvent as DC
import qualified Demo.HelloWorld as HW
import qualified Demo.SnakeUi as SU
import qualified Demo.TetrisUi as DT
import qualified Graphics.Vty as GV

--
-- MenuState  a vector of strings
type MenuState = BL.List () String

drawUi :: MenuState -> [BT.Widget ()]
drawUi s = [ui]
  where
    widget = BL.renderList renderEntry True s
    renderEntry isSelected = entryAttribute isSelected . BWC.hCenter . BW.str . show
    entryAttribute isSelected =
      BW.withAttr aName . (if isSelected then BW.withAttr BL.listSelectedAttr . BW.withAttr cName else id)
    aName = BA.attrName "mainmenu"
    cName :: BA.AttrName
    cName = BL.listSelectedAttr <> BA.attrName "custom"

    ui =
      BW.withBorderStyle BWBS.unicode $
        BWC.vCenter $
          BWC.hCenter $
            BWB.borderWithLabel (BW.str "label") $
              BW.hLimit 25 $ BW.vLimit 15 widget

drawCursor :: MenuState -> [BT.CursorLocation ()] -> Maybe (BT.CursorLocation ())
drawCursor = undefined

handleEvent :: MenuState -> BT.BrickEvent () ev -> BT.EventM () (BT.Next MenuState)
handleEvent s (BT.VtyEvent (GV.EvKey GV.KEsc [])) = BM.halt $ BL.listFindBy (== "quit") s
handleEvent s (BT.VtyEvent (GV.EvKey GV.KEnter [])) = BM.halt s
handleEvent s (BT.VtyEvent e) = BM.continue =<< BL.handleListEvent e s
handleEvent s _ = BM.continue s

startEvent :: MenuState -> BT.EventM () MenuState
startEvent = undefined

aMap :: BA.AttrMap
aMap = BA.attrMap GV.defAttr attributes
  where
    attributes =
      [ (BA.attrName "mainmenu", BU.fg GV.brightGreen),
        (BL.listSelectedAttr <> BA.attrName "custom", BU.bg GV.red)
      ]

theApp :: BM.App MenuState () ()
theApp =
  BM.App
    { BM.appDraw = drawUi,
      BM.appChooseCursor = BM.neverShowCursor,
      BM.appHandleEvent = handleEvent,
      BM.appStartEvent = return,
      BM.appAttrMap = const aMap
    }

initialState :: MenuState
initialState = BL.list () (DV.fromList ["hello world", "snake", "customevent", "tetris", "quit"]) 1

main :: IO ()
main = void $ do
  s <- BM.defaultMain theApp initialState
  case BL.listSelectedElement s of
    Just (_, "hello world") -> HW.startApp >> main
    Just (_, "customevent") -> DC.startApp >> main
    Just (_, "snake") -> SU.startApp >> main
    Just (_, "tetris") -> DT.startApp >> main
    Just (_, "quit") -> return ()
    _ -> return ()
