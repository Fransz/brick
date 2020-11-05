{-# LANGUAGE TupleSections #-}

module Demo.Tetris
    ( Game (..)
    , Block(..)
    , TetrisDirection(..)
    , initialGame
    , tickGame
    , moveGame
    , posNameMap
    ) where

import Linear.V2 (V2(..), _x, perp)
import Lens.Micro ((^.))
import qualified Data.Map as Map (Map, fromList)

type Pos = V2 Int

data TetrisDirection = TetrisLeft | TetrisRight | TetrisUp | TetrisDown deriving (Show, Eq)

data BlockStatus = Moving | Dropped deriving (Show)


data Block = Block
    { pos :: Pos                        -- Absolute position of blocks center.
    , poss :: [Pos]                     -- Relative positions (from center) of blocks constituents.
    , name :: String                    -- Blocks name. For widget attributes.
    , status :: BlockStatus             -- is the block moving
    } deriving (Show)

iBlock = Block { pos = V2 5 5, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], name = "iblock", status = Moving }
oBlock = Block { pos = V2 10 10, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Moving }
tBlock = Block { pos = V2 15 15, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], name = "tblock", status = Moving }
sBlock = Block { pos = V2 20 20, poss = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], name = "sblock", status = Moving }
zBlock = Block { pos = V2 25 25, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "zblock", status = Moving }
jBlock = Block { pos = V2 30 30, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], name = "jblock", status = Moving }
lBlock = Block { pos = V2 35 35, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock", status = Moving }

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


moveGame :: TetrisDirection -> Game -> Game
moveGame d g = g { blocks = map (moveBlock d g) (blocks g)}

moveBlock :: TetrisDirection -> Game -> Block -> Block
moveBlock TetrisLeft g b = moveCenter b (V2 (-1) 0) 0 (cols g)
moveBlock TetrisRight g b = moveCenter b (V2 1 0) 0 (cols g)
moveBlock TetrisDown g b =  rotate b 0 (cols g)
moveBlock TetrisUp g b = rotate b 0 (cols g)

moveCenter :: Block -> V2 Int -> Int -> Int -> Block   
moveCenter b d min max = let poss' = map (+ d) $ poss b 
                             poss'' = map (+ pos b) poss'
                          in if inBounds poss'' min max then b { pos = pos b + d } else b

rotate :: Block -> Int -> Int -> Block
rotate b min max = let poss' = map perp $ poss b
                       poss'' = map (+ pos b) poss'
                    in if inBounds poss'' min max then b { poss = poss' } else b

moveGround = undefined

inBounds :: [Pos] -> Int -> Int -> Bool
inBounds ps min max = all (>= min) xs && all (< max) xs 
    where xs = map (^._x) ps

tickGame :: Game -> Game
tickGame s | gameover s = s
                 | otherwise = tick 1 s 

tick :: Int -> Game -> Game
tick i s = s {counter = counter s + i}

-- Map of all blocks, all positions with the blocks attr.
posNameMap :: Game -> Map.Map Pos String                      
posNameMap g = Map.fromList (concatMap posNameTpl (blocks g))

-- List off absolute positions of a block, in a tuple with the blocks attrName
posNameTpl :: Block -> [(Pos, String)]               
posNameTpl b = map ((, name b) . (+ pos b)) (poss b)

