module Rope where

import Prelude hiding (take,drop)
import qualified Data.List as L
import Data.Monoid
import Data.Rope

-- | Delete the character at a particular index in a rope. O(log(n))
delete :: Rope -> Int -> Rope
delete r i = take (i - 1) r `mappend` drop i r

-- | Find the index of a character in the rope. O(n)
elemIndex :: Rope -> Char -> Maybe Int
elemIndex r c = L.elemIndex c (toString r)

-- | Find the index of a character searching forward from a particular index. O(n)
elemIndexForward :: Rope -> Int -> Char -> Maybe Int
elemIndexForward r i c = do j <- elemIndex (drop (i - 1) r) c
                            return $ i + j - 1

-- | O(n). Find the index of a character searching backward from a particular index.
--   Includes the character at the index in its search.
elemIndexBackward :: Rope -> Int -> Char -> Maybe Int
elemIndexBackward r i = elemIndex (take (i + 1) r)