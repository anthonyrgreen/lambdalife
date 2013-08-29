UNIX Life (Haskell Redux)
=========================

This is a literate Haskell program which can be used to play 
the Game of Life the UNIX way. Specifically, this program reads
a "world" of '.'s (dead cells) and 'O's (live cells) from stdin
and outputs the resultant world to stdout.

> module Main where

> import System.IO
> import Data.Maybe (fromJust)

To model the game of life, we create a matrix of cells

> data Cell = ALIVE | DEAD deriving (Show, Eq)
> type World = [[Cell]]
> type Coor = (Int, Int)

> readCell :: Char -> Maybe Cell
> readCell 'O' = Just ALIVE
> readCell '.' = Just DEAD
> readCell  _  = Nothing

Here, we map readCell onto every char element of the text input.
Calling `lines stdin` produces a value of type [[Char]],
and (mapM . mapM) is of the type 
`Monad m => (a -> m b) -> [[a]] -> m [[b]]`;
above we that readCell maps from `Char` to `Maybe Cell`, which
implies that `(mapM . mapM) readCell` will produce a value of 
type `Maybe [[Cell]]` which is equivalent to `Maybe World`

> readWorld :: String -> Maybe World
> readWorld =  (mapM . mapM) readCell . lines

> showCell :: Cell -> Char
> showCell ALIVE = 'O'
> showCell DEAD  = '.'

This function's operation is very similar to the one above 
except that we use `map` instead of  

> showWorld :: World -> String
> showWorld = unlines . (map . map) showCell

Using these neighbor rules creates the game of life on a torus

> neighbors :: World -> Coor -> Int
> neighbors world (r, c) = length $ filter (isAlive world) adjustedCoors
>     where nCoors = [(r-1, c-1),
>                     (r-1, c),
>                     (r-1, c+1),
>                     (r, c-1),
>                     (r, c+1),
>                     (r+1, c-1),
>                     (r+1, c),
>                     (r+1, c+1)
>                    ]
>           adjustedCoors = (map (adjustCoor worldHeight worldWidth) nCoors)
>           worldWidth = length (world !! 0)
>           worldHeight = length world

> adjustCoor :: Int -> Int -> Coor -> Coor
> adjustCoor h w (r,c) = (wrap h r, wrap w c)

> wrap :: Int -> Int -> Int
> wrap max val
>     | val == (-1)   = max-1
>     | val == max    = 0
>     | otherwise     = val 

> isAlive :: World -> Coor -> Bool
> isAlive world (x, y) = ALIVE == (world !! x !! y)

> tick :: World -> World
> tick world = reshape worldHeight worldWidth flatWorld 
>     where worldWidth  = length (world !! 0)
>           worldHeight = length world
>           allCoors    = [(x,y) | x <- [0..(worldHeight-1)], y <- [0..(worldWidth-1)]]
>           flatWorld   = map tickCell $ zip (flatten world) (map (neighbors world) allCoors)

> tickCell :: (Cell, Int) -> Cell
> tickCell (ALIVE, neighborCount)
>     | or [neighborCount > 3, neighborCount < 2] = DEAD
>     | otherwise                                 = ALIVE
> tickCell (DEAD, neighborCount)
>     | neighborCount == 3 = ALIVE
>     | otherwise          = DEAD

> flatten :: [[a]] -> [a]
> flatten [] = []
> flatten (xs:xss) = xs ++ flatten xss

> reshape :: Int -> Int -> [a] -> [[a]]
> reshape _ _ [] = []
> reshape r c l  = (take c l):reshape (r-1) c (drop c l)

hGetContents lazily reads all input from a given buffer

> main :: IO ()
> main = do world <- fmap readWorld (hGetContents stdin)
>           putStr . showWorld . tick $ fromJust world