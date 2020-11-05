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
moveBlock dir game block = let block' = case dir of
                                           TetrisLeft -> moveCenter block (V2 (-1) 0)
                                           TetrisRight -> moveCenter block (V2 1 0)
                                           TetrisDown -> moveCenter block (V2 0 1)
                                           TetrisUp -> rotate block
                             in if inBounds block' 0 (cols game) then block' else block

moveCenter :: Block -> V2 Int -> Block
moveCenter b d = b { pos = pos b + d }

rotate :: Block -> Block
rotate b = b { poss = map perp $ poss b }

moveGround = undefined

inBounds :: Block -> Int -> Int -> Bool
inBounds b min max = all (>= min) xs && all (< max) xs
    where xs = map ((^._x) . (+ pos b)) (poss b)

tickGame :: Game -> Game
tickGame g | gameover g = g
           | otherwise = tick 1 $ moveGame TetrisDown g

tick :: Int -> Game -> Game
tick i g = g {counter = counter g + i}

-- Map of all blocks, all positions with the blocks attr.
posNameMap :: Game -> Map.Map Pos String
posNameMap g = Map.fromList (concatMap posNameTpl (blocks g))

-- List off absolute positions of a block, in a tuple with the blocks attrName
posNameTpl :: Block -> [(Pos, String)]
posNameTpl b = map ((, name b) . (+ pos b)) (poss b)
