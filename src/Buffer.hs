-- Buffer Module
module Buffer where

import Data.Rope(Rope)
import qualified Data.Rope as R
import qualified Rope as R
-- Text of the buffer
-- Point
-- Mark
-- Undo

data Buffer = B { name :: String, text :: Rope, point :: Int, mark :: Int , page :: (Int, Int)}
            deriving Show

instance Eq Buffer where
  (B n1 _ _ _ _) == (B n2 _ _ _ _) = n1 == n2

instance Ord Buffer where
  (B n1 _ _ _ _) `compare` (B n2 _ _ _ _) = n1 `compare` n2

-- | Rename a buffer
renameBuffer :: String -> Buffer -> Buffer
renameBuffer n (B _ t p m pg) = B n t p m pg

-- | Make an empty buffer with a given name
mkEmptyBuffer :: String -> Buffer
mkEmptyBuffer = flip mkBuffer $ ""

-- | Make a new buffer given a name and text
mkBuffer :: String -> String -> Buffer
mkBuffer n t = B n (R.fromString t) 0 0 (0,0)

-- | Move relative to the point. Positive indices move the point forward,
--   negative indices move the point backward.
move :: Int -> Buffer -> Buffer
move i (B n t p m pg) = B n t p' m pg where
  p' = max 0 (min (p + i) (R.length t))

-- | Move the point forward one character.
moveForward :: Buffer -> Buffer
moveForward = move 1

-- | Move the point backward one character.
moveBackward :: Buffer -> Buffer
moveBackward = move (-1)

-- | Move to the same position in the previous line.
movePrevious :: Buffer -> Buffer
movePrevious = moveBackwardToDelimiter '\n'

-- | Move to the same position in the next line.
moveNext :: Buffer -> Buffer
moveNext b@(B n t p m _) =
  let start = case R.elemIndexBackward t p '\n' of
                Nothing -> 0
                Just x -> x in
  let b'@(B n t' p' m' pg') = moveToEndOfLine b in
  moveForwardUntilDelimiter (p - start) '\n' b'

-- | Delete a character at an index.
deleteChar :: Int -> Buffer -> Buffer
deleteChar i (B n t p m pg) = B n (R.delete i t) p m pg

-- | Delete the character at the point.
deleteCharAtPoint :: Buffer -> Buffer
deleteCharAtPoint b@(B n t p m _) = deleteChar p b

-- | Delete the character before the point.
deleteCharBeforePoint :: Buffer -> Buffer
deleteCharBeforePoint b@(B n t p m _) = deleteChar (p - 1) b

-- | Insert a character at an index.
insertChar :: Int -> Char -> Buffer -> Buffer
insertChar i c (B n t p m pg) = B n (R.insert i c t) p m pg

-- | Insert a character at the point.
insertCharAtPoint :: Char -> Buffer -> Buffer
insertCharAtPoint c b = insertChar (point b) c b

-- | Place the mark at an index.
placeMark :: Int -> Buffer -> Buffer
placeMark i (B n t p m pg) = B n t p i pg

-- | Place the mark at the point.
placeMarkAtPoint :: Buffer -> Buffer
placeMarkAtPoint b = placeMark (point b) b

-- | Swap the point and mark.
swapPointAndMark :: Buffer -> Buffer
swapPointAndMark (B n t p m pg) = B n t m p pg

-- | Move forward n times or until you see the specified character
moveForwardUntilDelimiter :: Int -> Char -> Buffer -> Buffer
moveForwardUntilDelimiter 0 _ b = b
moveForwardUntilDelimiter n c b@(B _ t p _ _) =
  case R.index p t of
    Nothing -> b
    Just c' -> if c' == c then
                 b
               else
                 moveForwardUntilDelimiter (n-1) c (moveForward b)

-- | Move the point forward to the next instance of a character, the delimiter.
--   If the character doesn't exist after the point, don't move the point.
moveForwardToDelimiter :: Char -> Buffer -> Buffer
moveForwardToDelimiter c b@(B n t p m pg) = case R.elemIndexForward t p c of
  Nothing -> b
  Just x -> B n t x m pg

-- | Move the point backward to the last instance of a character, the delimiter.
--   If the character doesn't exist before the point, don't move the point.
moveBackwardToDelimiter :: Char -> Buffer -> Buffer
moveBackwardToDelimiter c b@(B n t p m pg) = case R.elemIndexBackward t p c of
  Nothing -> b
  Just x -> B n t x m pg

-- | Move to the beginning of the current line.
moveToBeginningOfLine :: Buffer -> Buffer
moveToBeginningOfLine = moveForward . moveBackwardToDelimiter '\n'

-- | Move to the end of the current line.
moveToEndOfLine :: Buffer -> Buffer
moveToEndOfLine = moveForwardToDelimiter '\n'

-- | Go to the same position in a line specified by an int, or do nothing
--   if that line doesnt't exist.
goToLine :: Int -> Buffer -> Buffer
goToLine n b@(B nm t p m pg) = B nm t np m pg where
  np = max 0 $ pred $ sum $ map length $ take n $ lines $ R.toString t

-- | Get the total number of lines
lineCount :: Buffer -> Int
lineCount b@(B n t p m pg) = length $ lines $ R.toString t

lineAtPoint :: Buffer -> Int
lineAtPoint b@(B n t p m pg) = aux (lines $ R.toString t) p 0 0 where
  aux (x:xs) p' n ls
    | n >= p    = ls
    | otherwise = aux xs p' (n + (length x)) (ls + 1)
  aux [] p' n ls = ls

-- | This will bring the point into the page if it goes past and
--   make sure the mark and point are never out of bounds
updatePage :: Buffer -> Buffer
updatePage b@(B n t p m (start, end)) = let len = length $ R.toString t
                                            lap = lineAtPoint b
                                        in
                                          if p > len then -- rewrite this shit
                                            error "point outside of length"
                                          else if m > len then
                                                 error "mark outside of length"
                                               else if lap > end then
                                                      B n t p m (start + 1, end + 1)
                                                    else if lap < start then
                                                           B n t p m (start - 1, end - 1)
                                                         else b
