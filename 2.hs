module AOS2 where

import Data.List
import Data.Char

type Red = Integer
type Green = Integer
type Blue = Integer

data Cubes = Cubes Red Green Blue
  deriving(Show,Eq)

newtype Game = Game (Integer,[Cubes])

instance Semigroup Cubes where
    Cubes a b c <> Cubes d e f = Cubes (a + d) (b + e) (c + f)

instance Monoid Cubes where
    mempty = Cubes 0 0 0

cubeProduct :: Game  -> Integer
cubeProduct (Game (i,cs)) = x*y*z
  where Cubes x y z = foldr findMax (Cubes 1 1 1)cs
        findMax (Cubes a b c) (Cubes d e f) = Cubes (max a d) (max b e) (max c f)

readInputs :: String -> String
readInputs s = let ':':' ':str = dropWhile (/= ':')s in str

breakApart :: Char -> String -> [String]
breakApart c xs = case break (==c) xs of
    (x,y:ys') -> x:breakApart c ys'
    ("","") -> []
    (x,"")-> [x]


mkCube :: [String] -> Cubes
mkCube  = foldr cube mempty 
      where cube x y = 
             case words x of
                [x,"red"] -> Cubes (read x :: Integer) 0 0 <> y
                [x,"green"] -> Cubes 0 (read x :: Integer) 0 <> y
                [x,"blue"] -> Cubes 0 0 (read x :: Integer) <> y

isPossible :: Cubes -> Cubes -> Bool
isPossible (Cubes a b c) (Cubes d e f) = a >= d && b >= e && c >= f

allPossible :: [Cubes] -> Bool
allPossible = all $ isPossible (Cubes 12 13 14 )

getPossibleCount :: [Game] -> Integer
getPossibleCount = foldr possible 0
  where possible (Game(x,xs)) y
         | allPossible xs = y+ x
         | otherwise = y

game :: String -> [Cubes]
game s = mkCube <$> breakApart ',' <$> breakApart ';' (readInputs s)

main = do
    f <- readFile "./2.txt"
    let gamesList = zipWith (\x y -> Game(x,y)) [1..] (game <$> lines f)
        total =  getPossibleCount gamesList
        products = fmap cubeProduct gamesList
    print total
    print $ sum products







