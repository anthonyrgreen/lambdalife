lambdalife
==========

inspired by lambdal's [unix-life](https://github.com/lambdal/unix-life)

a game of life that reads a world from stdin and prints the resulting world
(a single tick) to stdout

build
-----

```
ghc --make life.hs -o life
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
