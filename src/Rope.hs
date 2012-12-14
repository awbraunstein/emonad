module Rope where

import Prelude hiding (take,drop,splitAt,length)
import qualified Data.List as L
import Data.Monoid
import Data.Rope
import Control.Monad
import Test.QuickCheck

-- | O(log(n)). Insert a character at a particular index in a rope.
insert :: Int -> Char -> Rope -> Rope
insert i c r =
  let (prefix, suffix) = splitAt i r in
  prefix `mappend` fromChar c `mappend` suffix

-- | O(log(n)). Delete the character at a particular index in a rope.
delete :: Int -> Rope -> Rope
delete i r = take (i - 1) r `mappend` drop (max 1 i) r

-- | O(n). Find the index of a character in the rope.
elemIndex :: Rope -> Char -> Maybe Int
elemIndex r c = L.elemIndex c (toString r)

-- | O(n). Find the index of a character searching forward from a particular index.
elemIndexForward :: Rope -> Int -> Char -> Maybe Int
elemIndexForward r i c = do j <- elemIndex (drop (i) r) c
                            return $ i + j

-- | O(n). Find the index of a character searching backward from a particular
--   index. Includes the character at the index in its search.
elemIndexBackward :: Rope -> Int -> Char -> Maybe Int
elemIndexBackward r i c =
  case L.elemIndices c (toString (take i r)) of
    [] -> Nothing
    xs -> Just $ L.last xs

-- | Find the character at the current index given
index :: Int -> Rope -> Maybe Char
index i r
       | i >= length r || i < 0 = Nothing
       | otherwise = Just $ L.head $ toString $ take 1 $ drop i r

-- QuickCheck instances and properties follow
instance Arbitrary Rope where
  arbitrary = liftM fromString arbitrary
  shrink r = map fromString (shrink $ toString r)

prop_insert :: Char -> Rope -> Bool
prop_insert c r =
  let (bef,aft) = L.splitAt (length r) (toString r) in
  toString (insert (length r) c r) == bef ++ [c] ++ aft

prop_delete :: Rope -> Property
prop_delete r =
  length r >= 1 ==>
  toString (delete (length r - 1) r) == L.take (length r - 1) (toString r)
