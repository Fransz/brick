module Demo.Tetris
    ( TetrisState (..)
    , Block(..)
    , initialState
    , newTetrisState
    ) where

import Linear.V2 (V2(..))

type Pos = V2 Int

data TetrisState = TetrisState
    { cols :: Int
    , rows :: Int
    , blocks :: [Block]
    , gameover :: Bool
    , counter :: Int
    } 

data Block = Block
    { c :: Pos
    , bps :: [Pos]
    , name :: String
    }

iBlock = Block { c = V2 5 5, bps = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], name = "iblock" }
oBlock = Block { c = V2 10 10, bps = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
tBlock = Block { c = V2 15 15, bps = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], name = "tblock" }
sBlock = Block { c = V2 20 20, bps = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], name = "sblock" }
zBlock = Block { c = V2 25 25, bps = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "zblock" }
jBlock = Block { c = V2 35 35, bps = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], name = "jblock" }
lBlock = Block { c = V2 40 40, bps = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock" }

initialState :: TetrisState
initialState = TetrisState
    { cols = 50
    , rows = 50
    , blocks = [iBlock, oBlock, tBlock, sBlock, zBlock, jBlock, lBlock]
    , gameover = False
    , counter = 0
}

newTetrisState :: TetrisState -> TetrisState
newTetrisState s | gameover s = s
                 | otherwise = tick 1 s 

tick :: Int -> TetrisState -> TetrisState
tick i s = s {counter = counter s + i}

