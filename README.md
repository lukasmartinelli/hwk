# hwk

A modern Haskell based AWK replacement.
I am a big fan of UNIX pipes and the great commands.

`awk` is a really powerful tool. But it is from the Seventies and my father told
me that a lot has changed since then. Therefore I have a new functional
take on AWK.

```bash
seq 1 100 \
  | is18='(_,_,18) = True; _ = False' \
    hwk 'filter is18' \
  | hwk 'map (+)'
```

