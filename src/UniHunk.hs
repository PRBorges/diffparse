{-# LANGUAGE OverloadedStrings #-}

module UniHunk (uniHunkParser) where

import Common
import Control.Applicative (optional, (<|>))
import Control.Foldl qualified as Fold
import Control.Monad.Combinators.NonEmpty (some)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as AC (endOfLine, isEndOfLine)
import Data.Bifunctor (first)
import Data.List.NonEmpty (fromList)
import Data.Word8 (_space)
import Types

uniHunkParser :: Parser UniHunk
uniHunkParser = do
  leftRange <- "@@ -" *> uniRangeParser
  rightRange <- " +" *> uniRangeParser <* " @@"
  section <- optional sectionParser
  AC.endOfLine
  (groups, note) <- combineNotes <$> some groupParser
  pure $ UniHunk leftRange rightRange section groups note

uniRangeParser :: Parser Range
uniRangeParser = either OneLine (uncurry StartLen) <$> intOrPairParser

sectionParser :: Parser Line
sectionParser = A.word8 _space *> A.takeTill AC.isEndOfLine

groupParser :: Parser (LinesGroup, EndNote)
groupParser = leftGroupParser <|> rightGroupParser <|> contextGroupParser

leftGroupParser :: Parser (LinesGroup, EndNote)
leftGroupParser =
  (,) . Deleted
    <$> some (lineAfterParser "-")
    <*> leftNoteParser

rightGroupParser :: Parser (LinesGroup, EndNote)
rightGroupParser =
  (,) . Added
    <$> some (lineAfterParser "+")
    <*> rightNoteParser

contextGroupParser :: Parser (LinesGroup, EndNote)
contextGroupParser =
  (,) . Context
    <$> some (lineAfterParser " ")
    <*> bothNoteParser

combineNotes :: NonEmpty (LinesGroup, EndNote) -> (NonEmpty LinesGroup, EndNote)
combineNotes =
  first fromList . Fold.fold ((,) <$> collect <*> combine)
  where
    collect = Fold.premap fst Fold.list
    -- combine = Fold.foldMap snd id -- alternative definition
    combine = Fold.premap snd Fold.mconcat
