{-# LANGUAGE OverloadedStrings #-}

module NormalHunk (normalHunkParser) where

import Common
import Control.Exception (assert)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as AC (endOfLine)
import Data.List.NonEmpty (fromList)
import Data.Word8 (_a, _c, _d)
import Types

normalHunkParser :: Parser NormalHunk
normalHunkParser = do
    (lRange, hunkParser, rRange) <- descriptorParser
    hunkParser lRange rRange

descriptorParser :: Parser (Range, Range -> Range -> Parser NormalHunk, Range)
descriptorParser =
    (,,)
        <$> normalRangeParser
        <*> hunkParserParser
        <*> normalRangeParser
        <* AC.endOfLine
    where
        hunkParserParser =
            A.choice
                [ A.word8 _a >> pure addHunkParser
                , A.word8 _d >> pure deleteHunkParser
                , A.word8 _c >> pure changeHunkParser
                ]

normalRangeParser :: Parser Range
normalRangeParser = either OneLine (uncurry StartEnd) <$> intOrPairParser

addHunkParser :: Range -> Range -> Parser NormalHunk
addHunkParser leftRange rightRange =
    AddHunk leftRange rightRange
        <$> addedLinesParser (rangeLength rightRange)
        <*> rightNoteParser

deleteHunkParser :: Range -> Range -> Parser NormalHunk
deleteHunkParser leftRange rightRange =
    DeleteHunk leftRange rightRange
        <$> deletedLinesParser (rangeLength leftRange)
        <*> leftNoteParser

changeHunkParser :: Range -> Range -> Parser NormalHunk
changeHunkParser leftRange rightRange = do
    deleted <- deletedLinesParser (rangeLength leftRange)
    leftNote <- leftNoteParser
    _dashes <- lineAfterParser "-"
    added <- addedLinesParser (rangeLength rightRange)
    rightNote <- rightNoteParser
    pure $
        ChangeHunk leftRange rightRange deleted added (leftNote <> rightNote)

addedLinesParser, deletedLinesParser :: Int -> Parser (NonEmpty Line)
addedLinesParser nLines =
    assert (nLines > 0) $
        fromList <$> A.count nLines (lineAfterParser "> ")
deletedLinesParser nLines =
    assert (nLines > 0) $
        fromList <$> A.count nLines (lineAfterParser "< ")
