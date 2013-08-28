lambdalife
==========

inspired by lambdal's [unix-life](https://github.com/lambdal/unix-life)

build
-----

```
ghc --make life.hs -o life
```

test
----

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

note fp script taken from unix [unix-life](https://github.com/lambdal/unix-life)
