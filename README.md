lambdalife
==========

inspired by lambdal's [unix-life](https://github.com/lambdal/unix-life)

a game of life that reads a world from stdin and prints the resulting world
(a single tick) to stdout

rules
-----

- Any live cell with fewer than two live neighbors dies, as if caused by under-population.
- Any live cell with two or three live neighbors lives on to the next generation.
- Any live cell with more than three live neighbors dies, as if by overcrowding.
- Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.

build
-----

```
make 
```

run
---

```
./life < start_R-pentomino
```

or 

```
./life < start_R-pentomino | ./life
```

or to find the fixed point of life

```
./fp ./life start_R-pentomino
```

note: fp script taken from lambdal's [unix-life](https://github.com/lambdal/unix-life)

test
----

