{-
	UNIX Life (Haskell Redux): The Game of Life following the UNIX Philosophy
-}

module Main where

import System.IO
import Data.Maybe (fromJust)

data Cell = ALIVE | DEAD deriving (Show, Eq)
type World = [[Cell]]
type Coor = (Int, Int)

readWorld :: String -> Maybe World
readWorld =  (mapM . mapM) readCell . lines

showWorld :: World -> String
showWorld = unlines . (map . map) showCell

showCell :: Cell -> Char
showCell ALIVE = 'O'
showCell DEAD  = '.'

readCell :: Char -> Maybe Cell
readCell 'O' = Just ALIVE
readCell '.' = Just DEAD
readCell  _  = Nothing

neighbors :: World -> Coor -> Int
neighbors world (r, c) = length $ filter (isAlive world) adjustedCoors
    where nCoors = [(r-1, c-1),
                    (r-1, c),
                    (r-1, c+1),
                    (r, c-1),
                    (r, c+1),
                    (r+1, c-1),
                    (r+1, c),
                    (r+1, c+1)
                   ]
          adjustedCoors = (map (adjustCoor worldHeight worldWidth) nCoors)
          worldWidth = length (world !! 0)
          worldHeight = length world

adjustCoor :: Int -> Int -> Coor -> Coor
adjustCoor h w (r,c) = (wrap h r, wrap w c)

wrap :: Int -> Int -> Int
wrap max val
    | val == (-1)   = max-1
    | val == max    = 0
    | otherwise     = val 

isAlive :: World -> Coor -> Bool
isAlive world (x, y) = ALIVE == (world !! x !! y)

tick :: World -> World
tick world = reshape worldHeight worldWidth flatWorld 
    where worldWidth  = length (world !! 0)
          worldHeight = length world
          allCoors    = [(x,y) | x <- [0..(worldHeight-1)], y <- [0..(worldWidth-1)]]
          flatWorld   = map tickCell $ zip (flatten world) (map (neighbors world) allCoors)

tickCell :: (Cell, Int) -> Cell
tickCell (ALIVE, neighborCount)
    | or [neighborCount > 3, neighborCount < 2] = DEAD
    | otherwise                                 = ALIVE
tickCell (DEAD, neighborCount)
    | neighborCount == 3 = ALIVE
    | otherwise          = DEAD

flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ flatten xss

reshape :: Int -> Int -> [a] -> [[a]]
reshape _ _ [] = []
reshape r c l  = (take c l):reshape (r-1) c (drop c l)

main :: IO ()
main = do world <- fmap readWorld (hGetContents stdin)
          putStr . showWorld . tick $ fromJust world
