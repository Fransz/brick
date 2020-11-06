{-# LANGUAGE TupleSections #-}

module Demo.Tetris
    ( Game (..)
    , Block(..)
    , TetrisDirection(..)
    , initialGame
    , moveGame
    , tickGame
    , freeFall
    , posNameMap
    ) where

import Linear.V2 (V2(..), _x, _y, perp)
import Lens.Micro ((^.))
import qualified Data.Map as Map (Map, fromList)
import qualified Data.List as List (groupBy, sortOn, partition)

type Pos = V2 Int

data TetrisDirection = TetrisLeft | TetrisRight | TetrisUp | TetrisDown deriving (Show, Eq)

data BlockStatus = Moving | Dropped deriving (Show, Eq)


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
lBlock = Block { pos = V2 8 (-1), poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock", status = Moving }
testBlocks = [ Block { pos = V2 0 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 2 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 4 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 6 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 10 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 12 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 14 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 16 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 18 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 20 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 22 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 24 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 26 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 28 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 30 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 32 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             , Block { pos = V2 34 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock", status = Dropped }
             ]

data Game = Game
    { cols :: Int
    , rows :: Int
    , blocks :: [Block]
    , wall :: [Pos]
    , gameover :: Bool
    , counter :: Int
    }  deriving (Show)

initialGame :: Game
initialGame = Game
    { cols = 36
    , rows = 50
    , blocks = [iBlock, oBlock, tBlock, sBlock, zBlock, jBlock, lBlock] ++ testBlocks
    , wall = []
    , gameover = False
    , counter = 0
}


moveGame :: TetrisDirection -> Game -> Game
moveGame d g = let g' = g { wall = buildWall g ++ ground g}
                   moving = filter ((== Moving) . status) $ blocks g'
                   dropped = filter ((== Dropped) . status) $ blocks g'
                in g' { blocks = map (moveBlock d g') moving ++ dropped }

moveBlock :: TetrisDirection -> Game -> Block -> Block
moveBlock dir game block = let block' = case dir of
                                           TetrisLeft -> moveCenter block (V2 (-1) 0)
                                           TetrisRight -> moveCenter block (V2 1 0)
                                           TetrisDown -> moveCenter block (V2 0 1)
                                           TetrisUp -> rotate block
                            in if inWall block' (wall game) then block { status = Dropped }
                               else if inBounds block' 0 (cols game) then block' else block

moveCenter :: Block -> V2 Int -> Block
moveCenter b d = b { pos = pos b + d }

rotate :: Block -> Block
rotate b = b { poss = map perp $ poss b }

freeFall :: Game -> Game
freeFall g = let wall = buildWall g ++ ground g
                 moving = filter ((== Moving) . status) $ blocks g
                 dropped = filter ((== Dropped) . status) $ blocks g
              in g { wall = wall, blocks = map (freeFallBlock wall) moving ++ dropped }

freeFallBlock :: [Pos] -> Block -> Block
freeFallBlock w b = let b' = moveCenter b $ V2 0 1
                        in if inWall b' w then b {status = Dropped} else freeFallBlock w b'


inBounds :: Block -> Int -> Int -> Bool
inBounds b min max = all (>= min) xs && all (< max) xs
    where xs = map ((^._x) . (+ pos b)) (poss b)

buildWall :: Game -> [Pos]
buildWall g = concatMap absPos dropped
    where
        dropped = filter ((== Dropped) . status)  $ blocks g
        absPos b = map (+ pos b) $ poss b

ground :: Game -> [Pos]
ground g = map (\x -> V2 x (rows g)) $ take (cols g) . iterate (+1) $ 0

inWall :: Block -> [Pos] -> Bool
inWall b w = any ((`elem` w) . (+ pos b)) (poss b)

tickGame :: Game -> Game
tickGame g | gameover g = g
           | otherwise = tick 1 . newBlock . shrinkWall . moveGame TetrisDown $ g

newBlock :: Game -> Game
newBlock = id

shrinkWall :: Game -> Game
shrinkWall g = let grouped = List.groupBy (\v1 v2 -> v1 ^. _y == v2 ^. _y) . List.sortOn (^. _y) . buildWall $ g
                   dels = concat $ filter ((== cols g) . length) grouped

                   (dropped, moving) = List.partition (\b -> status b == Dropped) $ blocks g
                   dropped' = map (delete dels) dropped
                   delete ds b = b { poss = filter (not . (`elem` ds) . (+ pos b)) $ poss b }
                
                   dropped'' = filter (not . null . poss) dropped'
                in g { blocks = moving ++ dropped'' }


tick :: Int -> Game -> Game
tick i g = g {counter = counter g + i}

-- Map of all blocks, all positions with the blocks attr.
posNameMap :: Game -> Map.Map Pos String
posNameMap g = Map.fromList (concatMap posNameTpl (blocks g))

-- List off absolute positions of a block, in a tuple with the blocks attrName
posNameTpl :: Block -> [(Pos, String)]
posNameTpl b = map ((, name b) . (+ pos b)) (poss b)
