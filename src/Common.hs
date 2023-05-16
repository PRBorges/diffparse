{-# LANGUAGE OverloadedStrings #-}

module Common (
    intOrPairParser,
    leftNoteParser,
    rightNoteParser,
    bothNoteParser,
    lineAfterParser,
    rangeLength,
) where

import Control.Monad (void)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as AC (decimal)
import Data.ByteString (ByteString)
import Data.Word8 (_comma, _lf)
import Types

intOrPairParser :: Parser (Either Int (Int, Int))
intOrPairParser = do
    int1 <- AC.decimal
    A.option (Left int1) $
        Right . (int1,) <$> (A.word8 _comma *> AC.decimal)

rangeLength :: Range -> Int
rangeLength (OneLine _ln) = 1
rangeLength (StartEnd start end) = end - start + 1
rangeLength (StartLen _start len) = len

leftNoteParser, rightNoteParser, bothNoteParser :: Parser EndNote
leftNoteParser = A.option None (LeftNote <$ noteLineParser)
rightNoteParser = A.option None (RightNote <$ noteLineParser)
bothNoteParser = A.option None (BothNote <$ noteLineParser)

-- "\\" as prefix would work, since no other line starts with '\'
-- and the rest of the line is discarded
noteLineParser :: Parser ()
noteLineParser = void $ lineAfterParser "\\ "

lineAfterParser :: ByteString -> Parser Line
lineAfterParser prefix =
    A.string prefix *> A.takeTill (== _lf) <* A.take 1
