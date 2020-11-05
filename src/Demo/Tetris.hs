module Demo.Tetris
    ( Game (..)
    , Block(..)
    , Direction(..)
    , initialGame
    , tickGame
    , moveGame
    ) where

import Linear.V2 (V2(..), _x, perp)
import Lens.Micro ((^.))

type Pos = V2 Int

data Direction = TetrisLeft | TetrisRight | TetrisUp | TetrisDown deriving (Show, Eq)


data Block = Block
    { c :: Pos
    , bps :: [Pos]
    , name :: String
    } deriving (Show)

iBlock = Block { c = V2 5 5, bps = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], name = "iblock" }
oBlock = Block { c = V2 10 10, bps = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
tBlock = Block { c = V2 15 15, bps = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], name = "tblock" }
sBlock = Block { c = V2 20 20, bps = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], name = "sblock" }
zBlock = Block { c = V2 25 25, bps = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "zblock" }
jBlock = Block { c = V2 35 35, bps = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], name = "jblock" }
lBlock = Block { c = V2 40 40, bps = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock" }

data Game = Game
    { cols :: Int
    , rows :: Int
    , blocks :: [Block]
    , gameover :: Bool
    , counter :: Int
    }  deriving (Show)

initialGame :: Game
initialGame = Game
    { cols = 36
    , rows = 50
    , blocks = [iBlock, oBlock, tBlock, sBlock, zBlock, jBlock, lBlock]
    , gameover = False
    , counter = 0
}


moveGame :: Direction -> Game -> Game
moveGame d g = g { blocks = map (moveBlock d g) (blocks g)}

moveBlock :: Direction -> Game -> Block -> Block
moveBlock TetrisLeft g b = moveCenter b (V2 (-1) 0) 0 (cols g)
moveBlock TetrisRight g b = moveCenter b (V2 1 0) 0 (cols g)
moveBlock TetrisDown g b =  rotate b 0 (cols g)
moveBlock TetrisUp g b = rotate b 0 (cols g)

moveCenter :: Block -> V2 Int -> Int -> Int -> Block           -- block -> delta -> minx -> maxy
moveCenter b d min max = let bps' = map (+ d) $ bps b          -- new relative positions
                             bps'' = map (+ c b) bps'          -- new absolute positions
                             xs = map (^._x) bps''             -- new absolute x
                          in if all (>= min) xs && all (< max) xs then b { c = c b + d } else b

rotate :: Block -> Int -> Int -> Block
rotate b min max = let bps' = map perp $ bps b
                       bps'' = map (+ c b) bps'
                       xs = map (^._x) bps''
                    in if all (>= min) xs && all (< max) xs then b { bps = bps' } else b

moveGround = undefined


tickGame :: Game -> Game
tickGame s | gameover s = s
                 | otherwise = tick 1 s 

tick :: Int -> Game -> Game
tick i s = s {counter = counter s + i}

