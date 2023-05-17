{-# LANGUAGE OverloadedStrings #-}

module DiffParse (diffParse, module Types) where

import Common (lineAfterParser)
import Control.Applicative ((<|>))
import Control.Monad.Combinators.NonEmpty (some)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word8 (Word8, _space, _tab)
import NormalHunk (normalHunkParser)
import Types
import UniHunk (uniHunkParser)

diffParse :: ByteString -> Either String Diff
diffParse =
    A.parseOnly $
        emptyDiffParser
            <|> normalDiffParser
            <|> uniDiffParser
            <|> gitDiffParser

emptyDiffParser :: Parser Diff
emptyDiffParser = A.endOfInput >> pure EmptyDiff

normalDiffParser :: Parser Diff
normalDiffParser = NormalDiff <$> some normalHunkParser <* A.endOfInput

uniDiffParser :: Parser Diff
uniDiffParser = do
    leftFile <- "--- " *> nameTimeParser
    rightFile <- "+++ " *> nameTimeParser
    uniHunks <- some uniHunkParser
    A.endOfInput
    pure $ UniDiff leftFile rightFile uniHunks

nameTimeParser :: Parser NameTime
nameTimeParser =
    NameTime
        <$> takeTillAsText (_tab ==)
        <* A.take 1 -- take _tab
        <*> takeRestAsText

gitDiffParser :: Parser Diff
gitDiffParser = do
    _gitHeader <- lineAfterParser "diff --git"
    (leftHash, rightHash) <- "index " *> hashesParser
    leftName <- "--- " *> takeRestAsText
    rightName <- "+++ " *> takeRestAsText
    uniHunks <- some uniHunkParser
    A.endOfInput
    let leftNameHash = NameHash leftName leftHash
    let rightNameHash = NameHash rightName rightHash
    pure $ GitDiff leftNameHash rightNameHash uniHunks

hashesParser :: Parser (Hash, Hash)
hashesParser =
    (,)
        <$> takeTillAsText (_space ==)
        <* A.take 1 -- take _space
        <*> takeRestAsText

takeRestAsText :: Parser Text
takeRestAsText = takeTillAsText AC.isEndOfLine <* AC.endOfLine

takeTillAsText :: (Word8 -> Bool) -> Parser Text
takeTillAsText p = decodeUtf8With lenientDecode <$> A.takeTill p
