module Main (main) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as B
import DiffParse
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode (ReadMode), stdin, withBinaryFile)

main :: IO ()
main =
    do
        args <- getArgs
        diff <- case args of
            [] -> diffParse <$> B.hGetContents stdin
            [fp] ->
                withBinaryFile fp ReadMode (fmap diffParse . B.hGetContents)
            _more_than_2_args ->
                die
                    "Please provide one file to read from,\
                    \ or none to read from standard input."
        print diff
        `catch` ( \(e :: IOException) -> do
                    die ("Problem reading input: " <> show e)
                )
