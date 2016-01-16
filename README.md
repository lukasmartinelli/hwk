# hwk

<img align="right" alt="hwk" src="hwk.png" />

[awk](https://en.wikipedia.org/wiki/AWK) is a really powerful tool when operating on streams
of data. **hwk** tries to demonstrate how a modern Haskell based replacement could look like.

## Example

Sum all negative numbers.

```bash
eval $(hwk env ints 'map (\l -> read l :: Int)')
seq -100 100 \
  | hwk '\lines -> filter (0 >) $ ints lines' \
  | hwk '\lines -> [foldl (+) 0 $ ints lines]}'
```

The `hwk env` command loads the statement as `ints` function into the environment for later reuse.
The argument passed to `hwk` are valid Haskell statements. They should always
return a function that takes takes a list of strings and returns a new list `[String] -> [String]`.

## What does `hwk` do?

- `hwk` will parse the Haskell statement, generate code and compiles it on the fly into a executable binary that can then later be reused.
- `hwk` will try to lookup functions in the passed environment variables.
- `hwk` provides some additional functions that work well with streams

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
