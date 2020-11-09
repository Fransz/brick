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
import qualified Data.List as List (groupBy, sortOn, (\\))
import qualified Data.Maybe as Maybe (isNothing)

type Pos = V2 Int

data TetrisDirection = TetrisLeft | TetrisRight | TetrisUp | TetrisDown deriving (Show, Eq)

data BlockStatus = Moving | Dropped deriving (Show, Eq)


data Block = Block
    { pos :: Pos                        -- Absolute position of blocks center.
    , poss :: [Pos]                     -- Relative positions (from center) of blocks constituents.
    , name :: String                    -- Blocks name. For widget attributes.
    } deriving (Show)

iBlock = Block { pos = V2 5 4, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], name = "iblock" }
oBlock = Block { pos = V2 10 2, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
tBlock = Block { pos = V2 15 5, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], name = "tblock" }
sBlock = Block { pos = V2 20 5, poss = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], name = "sblock" }
zBlock = Block { pos = V2 25 5, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "zblock" }

lBlock = Block { pos = V2 32 0, poss = [V2 1 (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock" }
jBlock = Block { pos = V2 10 45, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], name = "jblock" }

testBlocks = [ Block { pos = V2 0 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 2 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 4 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 6 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 10 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 12 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 14 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 16 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 18 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 20 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 22 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 24 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 26 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 28 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 30 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 32 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 34 49, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }

             , Block { pos = V2 0 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 2 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 4 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 6 46, poss = [V2 0 0, V2 1 0, V2 2 0, V2 3 0], name = "iblock" }
             , Block { pos = V2 6 47, poss = [V2 0 0, V2 1 0, V2 2 0, V2 3 0], name = "iblock" }
             , Block { pos = V2 10 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 12 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 14 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 16 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 18 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 20 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 22 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 24 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 26 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 28 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 30 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             , Block { pos = V2 34 47, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
             ]

data Game = Game
    { cols :: Int
    , rows :: Int
    , block :: Maybe Block
    , wall :: [Pos]
    , gameover :: Bool
    , counter :: Int
    }  deriving (Show)

initialGame :: Game
initialGame = Game
    { cols = 36
    , rows = 50
    , block = Just iBlock
    , wall = []
    , gameover = False
    , counter = 0
}


moveGame :: TetrisDirection -> Game -> Game
moveGame dir game = case block game of
                      Nothing -> game
                      Just block -> case moveBlock dir game block of
                                      Nothing -> game { block = Nothing, wall = buildWall block (wall game) }
                                      Just block' -> game { block = Just block' }

moveBlock :: TetrisDirection -> Game -> Block -> Maybe Block
moveBlock dir game block = let block' = case dir of
                                            TetrisLeft -> moveCenter block (V2 (-1) 0)
                                            TetrisRight -> moveCenter block (V2 1 0)
                                            TetrisDown -> moveCenter block (V2 0 1)
                                            TetrisUp -> rotate block
                            in if inWall block' $ wall game ++ ground game then Nothing
                               else if inBounds block' 0 (cols game) then Just block' else Just block

moveCenter :: Block -> V2 Int -> Block
moveCenter b d = b { pos = pos b + d }

rotate :: Block -> Block
rotate b = b { poss = map perp $ poss b }

freeFall :: Game -> Game
freeFall g = case block g of
               Nothing -> g
               Just b -> g { wall = fallWall b (wall g)}

fallWall :: Block -> [Pos] -> [Pos]
fallWall block wall = let block' = moveCenter block (V2 0 1)
                      in if inWall block' wall then buildWall block wall else fallWall block' wall

inBounds :: Block -> Int -> Int -> Bool
inBounds b min max = all (>= min) xs && all (< max) xs
    where xs = map ((^._x) . (+ pos b)) (poss b)

buildWall :: Block -> [Pos] -> [Pos]
buildWall block wall = wall ++ map (+ pos block) (poss block)

ground :: Game -> [Pos]
ground g = map (\x -> V2 x (rows g)) $ take (cols g) . iterate (+1) $ 0

inWall :: Block -> [Pos] -> Bool
inWall b w = any ((`elem` w) . (+ pos b)) (poss b)

tickGame :: Game -> Game
tickGame game
  | isGameOver game = game { gameover = True }
  | Maybe.isNothing (block game) = newBlock game
  | otherwise = tick 1 . newBlock . shrinkWall . moveGame TetrisDown $ game

newBlock :: Game -> Game
newBlock = id

isGameOver :: Game -> Bool
isGameOver game = False

shrinkWall :: Game -> Game
shrinkWall game = let grouped = reverse . List.groupBy (\v1 v2 -> v1 ^. _y == v2 ^. _y) . List.sortOn (^. _y) $ wall game
                      dels = concat $ filter ((== cols game) . length) grouped
                      score = 10.0 ** fromIntegral (length (filter ((== cols game) . length) grouped))
                      wall' = wall game List.\\ dels
                   in game { wall = wall' }

tick :: Int -> Game -> Game
tick i g = g {counter = counter g + i}

-- Map of all blocks, all positions with the blocks attr.
posNameMap :: Game -> Map.Map Pos String
posNameMap game = Map.fromList (maybe []  posNameTpl (block game))

-- List off absolute positions of a block, in a tuple with the blocks attrName
posNameTpl :: Block -> [(Pos, String)]
posNameTpl b = map ((, name b) . (+ pos b)) (poss b)
