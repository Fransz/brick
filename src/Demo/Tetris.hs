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

data Brick = Brick
    { brPos :: Pos
    , brName :: String
    } deriving (Show)

instance Eq Brick where
    (==) b1 b2 = brPos b1 == brPos b2

iBlock = Block { pos = V2 5 4, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 0 1], name = "iblock" }
oBlock = Block { pos = V2 10 2, poss = [V2 0 (-1), V2 1 (-1), V2 0 0, V2 1 0], name = "oblock" }
tBlock = Block { pos = V2 15 5, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 (-1)], name = "tblock" }
sBlock = Block { pos = V2 20 5, poss = [V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 0 0], name = "sblock" }
zBlock = Block { pos = V2 25 5, poss = [V2 (-1) (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "zblock" }

lBlock = Block { pos = V2 32 0, poss = [V2 1 (-1), V2 0 (-1), V2 0 0, V2 1 0], name = "lblock" }
jBlock = Block { pos = V2 10 45, poss = [V2 0 (-2), V2 0 (-1), V2 0 0, V2 (-1) 0], name = "jblock" }

testWall :: [Brick]
testWall = [
      Brick (V2 0 49) "iblock",  Brick (V2 1 49) "iblock",                             Brick (V2 3 49) "iblock",  Brick (V2 4 49) "iblock"
    ,                            Brick (V2 6 49) "iblock",  Brick (V2 7 49) "iblock",  Brick (V2 8 49) "iblock",  Brick (V2 9 49) "iblock"
    , Brick (V2 10 49) "iblock", Brick (V2 11 49) "iblock", Brick (V2 12 49) "iblock", Brick (V2 13 49) "iblock", Brick (V2 14 49) "iblock"
    , Brick (V2 15 49) "iblock", Brick (V2 16 49) "iblock", Brick (V2 17 49) "iblock", Brick (V2 18 49) "iblock", Brick (V2 19 49) "iblock"
    , Brick (V2 20 49) "iblock", Brick (V2 21 49) "iblock", Brick (V2 22 49) "iblock", Brick (V2 23 49) "iblock", Brick (V2 24 49) "iblock"
    , Brick (V2 25 49) "iblock", Brick (V2 26 49) "iblock", Brick (V2 27 49) "iblock", Brick (V2 28 49) "iblock", Brick (V2 29 49) "iblock"
    , Brick (V2 30 49) "iblock", Brick (V2 31 49) "iblock", Brick (V2 32 49) "iblock", Brick (V2 33 49) "iblock", Brick (V2 34 49) "iblock"
    , Brick (V2 35 49) "iblock"

    , Brick (V2 0 48) "iblock",  Brick (V2 1 48) "oblock",  Brick (V2 2 48) "iblock",  Brick (V2 3 48) "iblock",  Brick (V2 4 48) "iblock"
    ,                            Brick (V2 6 48) "zblock",  Brick (V2 7 48) "iblock",  Brick (V2 8 48) "iblock",  Brick (V2 9 48) "iblock"
    , Brick (V2 10 48) "iblock", Brick (V2 11 48) "sblock", Brick (V2 12 48) "iblock", Brick (V2 13 48) "iblock", Brick (V2 14 48) "iblock"
    , Brick (V2 15 48) "iblock", Brick (V2 16 48) "lblock", Brick (V2 17 48) "iblock", Brick (V2 18 48) "iblock", Brick (V2 19 48) "iblock"
    , Brick (V2 20 48) "iblock", Brick (V2 21 48) "oblock", Brick (V2 22 48) "iblock", Brick (V2 23 48) "iblock", Brick (V2 24 48) "iblock"
    , Brick (V2 25 48) "iblock", Brick (V2 26 48) "zblock", Brick (V2 27 48) "iblock", Brick (V2 28 48) "iblock", Brick (V2 29 48) "iblock"
    , Brick (V2 30 48) "iblock", Brick (V2 31 48) "lblock", Brick (V2 32 48) "iblock", Brick (V2 33 48) "iblock", Brick (V2 34 48) "iblock"
    , Brick (V2 35 48) "iblock"

    , Brick (V2 0 47) "iblock",  Brick (V2 1 47) "iblock",  Brick (V2 2 47) "iblock",  Brick (V2 3 47) "iblock",  Brick (V2 4 47) "iblock"
    ,                            Brick (V2 6 47) "iblock",  Brick (V2 7 47) "iblock",  Brick (V2 8 47) "iblock",  Brick (V2 9 47) "iblock"
    , Brick (V2 10 47) "iblock", Brick (V2 11 47) "iblock", Brick (V2 12 47) "iblock", Brick (V2 13 47) "iblock", Brick (V2 14 47) "iblock"
    , Brick (V2 15 47) "iblock", Brick (V2 16 47) "iblock", Brick (V2 17 47) "iblock", Brick (V2 18 47) "iblock", Brick (V2 19 47) "iblock"
    , Brick (V2 20 47) "iblock", Brick (V2 21 47) "iblock", Brick (V2 22 47) "iblock", Brick (V2 23 47) "iblock", Brick (V2 24 47) "iblock"
    , Brick (V2 25 47) "iblock", Brick (V2 26 47) "iblock", Brick (V2 27 47) "iblock", Brick (V2 28 47) "iblock", Brick (V2 29 47) "iblock"
    , Brick (V2 30 47) "iblock", Brick (V2 31 47) "iblock", Brick (V2 32 47) "iblock", Brick (V2 33 47) "iblock", Brick (V2 34 47) "iblock"
    , Brick (V2 35 47) "iblock"
    ]

data Game = Game
    { cols :: Int
    , rows :: Int
    , block :: Maybe Block
    , wall :: [Brick]
    , gameover :: Bool
    , counter :: Int
    }  deriving (Show)

initialGame :: Game
initialGame = Game
    { cols = 36
    , rows = 50
    , block = Just zBlock
    , wall = testWall
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
freeFall game = case block game of
                    Nothing -> game
                    Just b -> game { wall = fallWall b (wall game) (ground game), block = Nothing }

fallWall :: Block -> [Brick] -> [Brick] -> [Brick]
fallWall block wall ground = let block' = moveCenter block (V2 0 1)
                              in if inWall block' (wall ++ ground) then buildWall block wall else fallWall block' wall ground

inBounds :: Block -> Int -> Int -> Bool
inBounds b min max = all (>= min) xs && all (< max) xs
    where xs = map ((^._x) . (+ pos b)) (poss b)

buildWall :: Block -> [Brick] -> [Brick]
buildWall block wall = wall ++ map (\p -> Brick (p + pos block) (name block)) (poss block)

ground :: Game -> [Brick]
ground g = map (`Brick` "") $ take (cols g) . iterate (\v -> V2 (v ^. _x + 1) (rows g)) $ V2 0 0

inWall :: Block -> [Brick] -> Bool
inWall b w = any ((`elem` map brPos w) . (+ pos b)) (poss b)

tickGame :: Game -> Game
tickGame game
  | isGameOver game = game { gameover = True }
  | Maybe.isNothing (block game) = newBlock . shrinkWall $ game
  | otherwise = tick 1 . newBlock . shrinkWall . moveGame TetrisDown $ game

newBlock :: Game -> Game
newBlock = id

isGameOver :: Game -> Bool
isGameOver game = False

shrinkWall :: Game -> Game
shrinkWall game = let sorted = List.sortOn ((^. _y) . brPos)  $ wall game
                      grouped = reverse . List.groupBy (\b1 b2 -> brPos b1 ^. _y == brPos b2 ^. _y) $ sorted
                      dels = filter ((== cols game) . length) grouped
                      rows = length dels
                      dels' = concat dels
                      belows = concat $ takeWhile ((/= cols game) . length) grouped
                      aboves = (wall game List.\\ dels') List.\\ belows
                      aboves' = map mkBrick aboves
                      mkBrick br = Brick (brPos br + V2 0 rows) (brName br)

                      score = 10.0 ** fromIntegral (length (filter ((== cols game) . length) grouped))
                      wall' = belows ++ aboves'
                   in game { wall = wall' }

tick :: Int -> Game -> Game
tick i g = g {counter = counter g + i}

-- Map of all blocks, all positions with the blocks attr.
posNameMap :: Game -> Map.Map Pos String
posNameMap game = Map.fromList (maybe []  posNameTpl (block game) ++ posNameWall (wall game))

-- List off absolute positions of a block, in a tuple with the blocks attrName
posNameTpl :: Block -> [(Pos, String)]
posNameTpl b = map ((, name b) . (+ pos b)) (poss b)

posNameWall :: [Brick] -> [(Pos, String)]
posNameWall = map (\b -> (brPos b, brName b))
