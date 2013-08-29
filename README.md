lambdalife
==========

inspired by lambdal's [unix-life](https://github.com/lambdal/unix-life)

a game of life that reads a world from stdin and prints the resulting world
(a single tick) to stdout

build
-----

```
make 
```

run
---

to see a single time step in the game of life

```
./life < start_R-pentomino
```

to see an input evolve two time steps

```
./life < start_R-pentomino | ./life
```

or to watch the game evolve (until a fixed point)

```
./fp ./life start_R-pentomino -v
```

note: fp script taken from lambdal's [unix-life](https://github.com/lambdal/unix-life)

test
----

TODO
