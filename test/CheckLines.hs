{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CheckLines (checkUniLines) where

import Common
import Control.Monad (zipWithM_)
import Data.List.NonEmpty qualified as NE
import Data.Monoid (Sum (..))
import Test.Hspec
import Types

checkUniLines :: String -> Either String Diff -> Spec
checkUniLines name (Left _str) = it (name <> " parses to a Diff") $ True `shouldBe` False
checkUniLines name (Right (UniDiff _lFile _rFile hunks)) = checkHunks name hunks
checkUniLines name (Right (GitDiff _lFile _rFile hunks)) = checkHunks name hunks

checkHunks :: String -> NonEmpty UniHunk -> Spec
checkHunks name hunks =
    describe ("Number of lines in " <> name) $ do
        zipWithM_ checkHunk [1 :: Int ..] (NE.toList hunks)
    where
        checkHunk n hunk =
            it ("Lines check - hunk " <> show n) $
                inRanges hunk `shouldBe` inHunk hunk
        inRanges (UniHunk lRange rRange _ _ _) = rangeLength lRange + rangeLength rRange
        inHunk (UniHunk _ _ _ lGroups _) = getSum $ foldMap (Sum . count) lGroups
        count (Added ls) = NE.length ls
        count (Deleted ls) = NE.length ls
        count (Context ls) = 2 * NE.length ls
