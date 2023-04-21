# arghs

A simple program to list the arguments passed on the command line.

This is a port of the original [args](https://github.com/FedericoStra/args) to Haskell.

## Demo

```
$ arghs helps you debug command line arguments
0: `arghs`
1: `helps`
2: `you`
3: `debug`
4: `command`
5: `line`
6: `arguments`
```

## Usage

Simply call `arghs` with whatever command line arguments and it'll print them.
This can be useful to verify commands generated with [`find`]/[`fd`] before executing them,
for example

```
$ fd -e hs -X args ormolu -i
0: `args`
1: `ormolu`
2: `-i`
3: `./app/Main.hs`
4: `./src/Arghs.hs`
```

[`find`]: https://www.gnu.org/software/findutils/
[`fd`]: https://github.com/sharkdp/fd
