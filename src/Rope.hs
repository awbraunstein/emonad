module Rope where

import Prelude hiding (take,drop,splitAt,length)
import qualified Data.List as L
import Data.Monoid
import Data.Rope

-- | O(log(n)). Insert a character at a particular index in a rope.
insert :: Int -> Char -> Rope -> Rope
insert i c r =
  let (prefix, suffix) = splitAt i r in
  prefix `mappend` fromChar c `mappend` suffix

-- | O(log(n)). Delete the character at a particular index in a rope.
delete :: Int -> Rope -> Rope
delete i r = take (i - 1) r `mappend` drop i r

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
