# hwk [![Build Status](https://travis-ci.org/lukasmartinelli/hwk.svg?branch=master)](https://travis-ci.org/lukasmartinelli/hwk) ![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)

<img align="right" alt="hwk" src="hwk.png" />

**hwk** tries to demonstrate how a modern Haskell based stream manipulation tool could look like.
It is similar to tools like **awk** or **sed**.
`hwk` allows compact command sequences that operate on a list of strings. Because Haskell is lazy and has a powerful arsenal of functions, there is no need to invent another DSL and hopefully
push people into using functional goodness. `hwk` also provides a method to store small functions for
reuse in environment variables.

## Example

```bash
# prepend a string to each line
seq 1 10 | hwk 'map ("number " ++)'

# sum all negative numbers.
eval $(hwk env ints 'map (\l -> read l :: Int)')
seq -100 100 \
  | hwk '\lines -> filter (0 >) $ ints lines' \
  | hwk '\lines -> [foldl (+) 0 $ ints lines]}'
```

The `hwk env` command loads the statement as `ints` function into the environment for later reuse.
The argument passed to `hwk` must be a valid Haskell statement.
It should always return a fucntion that takes takes a list of strings and returns a new list `[String] -> [String]`.

## Install

To install `hwk` locally you need [Haskell](https://www.haskell.org/platform/) and
the [stack build tool](http://docs.haskellstack.org/en/stable/install_and_upgrade.html).
You can build the binary with stack or cabal.

```bash
git clone https://github.com/lukasmartinelli/hwk
cd hwk
stack build

# or with cabal
cabal install
```

## What does `hwk` do?

- `hwk` will generate code and execute it under `runhaksell`
- `hwk` will try to lookup functions in the passed environment variables.
- `hwk` provides additional functions that help with converting streams

## What happens under the Hood?

The `hwk` code above can be translated directly into a valid Haskell program.

```haskell
ints = map (\l -> read l :: Int)
hwk = \lines -> filter (0 >) $ ints lines

main = do
    contents <- getContents
    mapM_ putStrLn $ map (\r -> show r) $ hwk $ lines contents
```

The second `hwk 'foldl (+) 0'` command will translate to.

```haskell
ints = map (\l -> read l :: Int)
hwk = \lines -> foldl (+) 0 $ ints lines

main = do
    contents <- getContents
    mapM_ putStrLn $ map (\r -> show r) $ hwk $ lines contents
```

## Alternatives

- https://code.google.com/p/pyp/
- https://en.wikipedia.org/wiki/AWK
- https://en.wikipedia.org/wiki/Sed
