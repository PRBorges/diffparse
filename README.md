# diffparse

A parser for diff output in normal, unified, and git formats.

This repo is mainly to provide the code for my blog's article at: <https://www.prborges.com/2023/parsing-diff-output-in-haskell/>.

The module DiffParse defines the function

```haskell
diffParse :: ByteString -> Either String Diff
```

This function returns either an error or a `Diff` if the input is parsed successfully. The `Diff` is defined in the `Types` module.

The `Main` module is a small program that calls `diffParse` on the file given as the argument on the CLI or reads from the standard input if no argument is given.
In the `sample` folder, there are a few sample input files.
These files are also used for the included tests.
