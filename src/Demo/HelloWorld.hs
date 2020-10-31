module Demo.HelloWorld (startApp)
where

import Brick
import qualified Graphics.Vty as GV 

data HELLOWORLDNAME = HELLOWORLDNAME deriving (Show, Ord, Eq)

data State = State 

ui :: [Widget HELLOWORLDNAME]
ui = [withAttr (attrName "helloworld")  $ str "Hello, world!"]

initialState :: State
initialState = State

handleQuit :: State -> BrickEvent HELLOWORLDNAME ev -> Brick.EventM HELLOWORLDNAME (Next State)
handleQuit s (VtyEvent (GV.EvKey GV.KEsc [])) = Brick.halt s
handleQuit s _ = continue s

theApp :: App State e HELLOWORLDNAME
theApp = App {
    appDraw = const ui,
    appStartEvent = return,
    appHandleEvent = handleQuit,
    appAttrMap = \_ -> attrMap GV.defAttr [(attrName "helloworld", bg GV.red)],
    appChooseCursor = neverShowCursor
}


startApp :: IO State
startApp = defaultMain theApp initialState
