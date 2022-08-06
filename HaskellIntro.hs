{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit x = if x < 10 then x else mod x 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = if x < 10 then 0 else div x 10

toDigits :: Integer -> [Integer]
toDigits x = map (\n -> read [n] :: Integer) (show x)

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:xs:y) = x : xs * 2 : (doubleEveryOtherFromLeft y)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther x = reverse (doubleEveryOtherFromLeft (reverse x))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = if x < 10 then x + sumDigits xs else sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = if (lastDigit (sumDigits (doubleEveryOther (toDigits x)))) == 0 then True else False

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow = error "pow not yet defined"

g :: Integer -> Integer
g = error "g not yet defined"

h :: Integer -> Integer
h = error "h not yet defined"

d :: Int -> Integer -> Integer
d = error "d not yet defined"

--
-- Problem 3
--

powerSetHelper :: (a, Set a) -> Set (Set a)
powerSetHelper (x, xs) = mapSet (insert x) (powerSet xs) `union` powerSet xs

powerSet :: Set a -> Set (Set a)
powerSet isEmpty = empty
powerSet x = powerSetHelper (split x)