# hwk

<img align="right" alt="hwk" src="hwk.png" />

I often write small shell scripts and when operating on streams of data
[awk](https://en.wikipedia.org/wiki/AWK) is a really powerful tool. But it is from the 1970s and
**hwk** should demonstrate how a modern Haskell based replacement could look like.

## Example

Sum all negative numbers. The `negative` function is defined as normal bash variable
that is passed to `hwk`.

```bash
seq -100 100 \
  | negative='0 >' \
    hwk 'filter negative' \
  | hwk 'foldl (+) 0'
```

The Haskell expression passed to `hwk` can only receive one parameter.
In the example above `negative` is a partially applied function.

## What `hwk` do?

- `hwk` will parse the expression a Haskell AST, generate code and compile it on the fly into a executable binary.
- `hwk` will try to lookup functions in the passed environment variables.
- `hwk` will try to figure out the types needed for your functions to work with the input.

## What happens under the Hood?

The `hwk` code above can be translated directly into a valid Haskell program.
The only magic is the conversion from `String` to `Int` and back - which is taken care of automatically.

```haskell
negative :: Int -> Bool
negative = (0 >)

hwk :: [Int] -> [Int]
hwk = filter negative

main = do
    contents <- getContents
    mapM_ putStrLn $ map (\r -> show r) $ hwk $ map (\l -> read l :: Int) $ lines contents
```

The second `hwk 'foldl (+) 0'` command will translate to.

```haskell
hwk :: [Int] -> Int
hwk = foldl (+) 0

main = do
    contents <- getContents
    putStrLn $ show $ hwk $ map (\l -> read l :: Int) $ lines contents
```
