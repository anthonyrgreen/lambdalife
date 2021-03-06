UNIX Life (Haskell Redux)
=========================

This is a literate Haskell program which can be used to play 
the Game of Life _à la_ UNIX. Specifically, this program reads
a "world" of '.'s (dead cells) and 'O's (live cells) from stdin
and outputs the resultant world to stdout.

> module Main where

> import Data.Maybe (fromJust)

To model the game of life, we create a matrix of cells

> data Cell = ALIVE | DEAD deriving (Show, Eq)
> type World = [[Cell]]
> type Coor = (Int, Int)

> readCell :: Char -> Maybe Cell
> readCell 'O' = Just ALIVE
> readCell '.' = Just DEAD
> readCell  c  = error (show c ++ errormsg)
>       where errormsg = " is not a valid cell character, must be '0' or '.'"

Here, we map `readCell` onto every char element of the text input.
Calling `lines stdin` produces a value of type `[[Char]]`,
and `(mapM . mapM)` is of the type
`Monad m => (a -> m b) -> [[a]] -> m [[b]]`;
above we see that readCell maps from `Char` to `Maybe Cell`, which
implies that `(mapM . mapM) readCell` will produce a value of
type `Maybe [[Cell]]` which is equivalent to `Maybe World`

> readWorld :: String -> Maybe World
> readWorld =  (mapM . mapM) readCell . lines

> showCell :: Cell -> Char
> showCell ALIVE = 'O'
> showCell DEAD  = '.'

This function's operation is very similar to the one above 
except that we use `map` instead of `mapM` since our `World` input
is not a Monad like `Maybe Cell` above.

> showWorld :: World -> String
> showWorld = unlines . (map . map) showCell

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
>           adjustedCoors = map (adjustCoor worldHeight worldWidth) nCoors
>           worldWidth = length $ head world
>           worldHeight = length world

The `adjustCoor` function wraps the world coordinates such that the game is 
played on a torus

> adjustCoor :: Int -> Int -> Coor -> Coor
> adjustCoor h w (r,c) = (wrap h r, wrap w c)
>   where wrap mc v
>            | v == -1   = mc-1
>            | v == mc   = 0
>            | otherwise = v

Checks if a cell at `(x,y)` is alive

> isAlive :: World -> Coor -> Bool
> isAlive world (x, y) = ALIVE == (world !! x !! y)

A tick is a single time step in the game of life and logically, this 
tick function takes as input a world at time _t_ and outputs the world at time
_t+1_

> tick :: World -> World
> tick world = reshape worldHeight worldWidth flatWorld 
>     where worldWidth  = length $ head world
>           worldHeight = length world
>           allCoors    = [(x,y) | x <- [0..(worldHeight-1)], y <- [0..(worldWidth-1)]]
>           flatWorld   = zipWith tickCell (concat world) (map (neighbors world) allCoors)

each cell evolves according to the following rules

- Any live cell with fewer than two live neighbors dies, as if caused by under-population.
- Any live cell with two or three live neighbors lives on to the next generation.
- Any live cell with more than three live neighbors dies, as if by overcrowding.
- Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.

> tickCell :: Cell -> Int -> Cell
> tickCell ALIVE neighborCount
>     | neighborCount > 3 || neighborCount < 2 = DEAD
>     | otherwise                              = ALIVE
> tickCell DEAD neighborCount
>     | neighborCount == 3 = ALIVE
>     | otherwise          = DEAD

simple function to take a list and transform it into a matrix
(2D list) with `c` columns and `r` rows

> reshape :: Int -> Int -> [a] -> [[a]]
> reshape _ _ [] = []
> reshape r c l  = take c l : reshape (r-1) c (drop c l)

getContents lazily reads all input from stdin

> main :: IO ()
> main = do world <- fmap readWorld getContents
>           putStr . showWorld . tick $ fromJust world
