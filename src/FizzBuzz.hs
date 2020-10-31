module FizzBuzz (fb, fizzBuzz, FizzBuzz(..)) where

import Prelude 
import Data.Vector (Vector, generate)

data FizzBuzz a = FizzBuzzNone a | FizzBuzzThree a | FizzBuzzFive a | FizzBuzzFifteen a 

instance (Show a) => Show (FizzBuzz a) where
    show (FizzBuzzNone v) = show v
    show (FizzBuzzThree v) = show v <> " fizz"
    show (FizzBuzzFive v) = show v <> " buzz"
    show (FizzBuzzFifteen v) = show v <> " fizzbuzz"

fb :: Integral a => a -> FizzBuzz a
fb n | mod3 && mod5 = FizzBuzzFifteen n
     | mod5         = FizzBuzzFive n
     | mod3         = FizzBuzzThree n
     | otherwise    = FizzBuzzNone n
    where
    mod3 = n `mod` 3 == 0 
    mod5 = n `mod` 5 == 0

-- fizzBuzz :: (Integral a) => a -> [FizzBuzz a]
-- fizzBuzz n = Prelude.map fb [ 1 .. n ]

fizzBuzz :: Int -> Vector (FizzBuzz Int)
fizzBuzz n = generate n fb 
