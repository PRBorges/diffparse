{-# LANGUAGE StrictData #-}

module Types (
    Diff (..),
    EndNote (..),
    Hash,
    Line,
    LNumber,
    LinesGroup (..),
    NameTime (..),
    NameHash (..),
    NormalHunk (..),
    Range (..),
    Section,
    TimeStamp,
    UniHunk (..),
    NonEmpty,
) where

import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)

type Hash = Text
type Line = ByteString
type LNumber = Int
type Section = Maybe Line
type TimeStamp = Text

data Diff
    = EmptyDiff
    | NormalDiff (NonEmpty NormalHunk)
    | UniDiff NameTime NameTime (NonEmpty UniHunk)
    | GitDiff NameHash NameHash (NonEmpty UniHunk)

data NormalHunk
    = AddHunk Range Range (NonEmpty Line) EndNote
    | DeleteHunk Range Range (NonEmpty Line) EndNote
    | ChangeHunk Range Range (NonEmpty Line) (NonEmpty Line) EndNote

data UniHunk = UniHunk Range Range Section (NonEmpty LinesGroup) EndNote

data NameTime = NameTime Text TimeStamp
    deriving (Show)

data NameHash = NameHash Text Hash
    deriving (Show)

data Range
    = OneLine LNumber
    | StartEnd LNumber LNumber
    | StartLen LNumber Int
    deriving (Show)

data LinesGroup
    = Added (NonEmpty Line)
    | Deleted (NonEmpty Line)
    | Context (NonEmpty Line)

data EndNote = None | LeftNote | RightNote | BothNote
    deriving (Show)

-- Describes how to "combine" notes
instance Semigroup EndNote where
    None <> other = other
    other <> None = other
    -- LeftNote <> LeftNote = LeftNote -- This should not occur
    -- RightNote <> RightNote = RightNote -- This should not occur
    _ <> _ = BothNote

instance Monoid EndNote where
    mempty = None

-- Show instances

instance Show Diff where
    show EmptyDiff = "Empty Diff"
    show (NormalDiff hunks) =
        "Normal Diff:\n\n" <> unlines (map show (toList hunks))
    show (UniDiff leftFile rightFile uniHunks) =
        unlines $
            ["Unified Diff:", show leftFile, show rightFile]
                ++ map show (toList uniHunks)
    show (GitDiff lFile rFile hunks) =
        unlines $
            ["Git Diff", show lFile, show rFile]
                ++ map show (toList hunks)

instance Show NormalHunk where
    show (AddHunk lRange rRange hLines note) =
        unlines $
            ["AddHunk", show lRange, show rRange]
                ++ map show (toList hLines)
                ++ [show note]
    show (DeleteHunk lRange rRange hLines note) =
        unlines $
            ["DeleteHunk", show lRange, show rRange]
                ++ map show (toList hLines)
                ++ [show note]
    show (ChangeHunk lRange rRange lLines rLines note) =
        unlines $
            ["ChangeHunk", show lRange, show rRange]
                ++ map show (toList lLines)
                ++ ["---"]
                ++ map show (toList rLines)
                ++ [show note]

instance Show UniHunk where
    show (UniHunk lRange rRange section hLines note) =
        unlines $
            ["UniHunk", show lRange, show rRange, show section]
                ++ map show (toList hLines)
                ++ [show note]

instance Show LinesGroup where
    show (Added aLines) =
        intercalate "\n" $
            "Added:" : map show (toList aLines)
    show (Deleted dLines) =
        intercalate "\n" $
            "Deleted:" : map show (toList dLines)
    show (Context cLines) =
        intercalate "\n" $
            "Context:" : map show (toList cLines)
