-- Buffer Module
module Buffer where

import Data.Rope(Rope)
import qualified Data.Rope as R
import qualified Rope as R
-- Text of the buffer
-- Point
-- Mark
-- Undo

data Buffer = B { text :: Rope, point :: Int, mark :: Int }

-- | Move relative to the point. Positive indices move the point forward,
--   negative indices move the point backward.
move :: Int -> Buffer -> Buffer
move i (B t p m) = B t p' m where
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
moveNext b@(B t p m) =
  let start = case R.elemIndexBackward t p '\n' of
                Nothing -> 0
                Just x -> x in
  let b'@(B t' p' m') = moveToEndOfLine b in
  moveForwardUntilDelimiter (p - start) '\n' b'

-- | Delete a character at an index.
deleteChar :: Int -> Buffer -> Buffer
deleteChar i (B t p m) = B (R.delete i t) p m

-- | Delete the character at the point.
deleteCharAtPoint :: Buffer -> Buffer
deleteCharAtPoint b@(B t p m) = deleteChar p b

-- | Delete the character before the point.
deleteCharBeforePoint :: Buffer -> Buffer
deleteCharBeforePoint b@(B t p m) = deleteChar (p - 1) b

-- | Insert a character at an index.
insertChar :: Int -> Char -> Buffer -> Buffer
insertChar i c (B t p m) = B (R.insert i c t) p m

-- | Insert a character at the point.
insertCharAtPoint :: Char -> Buffer -> Buffer
insertCharAtPoint c b = insertChar (point b) c b

-- | Place the mark at an index.
placeMark :: Int -> Buffer -> Buffer
placeMark i (B t p m) = B t p i

-- | Place the mark at the point.
placeMarkAtPoint :: Buffer -> Buffer
placeMarkAtPoint b = placeMark (point b) b

-- | Swap the point and mark.
swapPointAndMark :: Buffer -> Buffer
swapPointAndMark (B t p m) = B t m p

moveForwardUntilDelimiter :: Int -> Char -> Buffer -> Buffer
moveForwardUntilDelimiter 0 _ b = b
moveForwardUntilDelimiter n c b@(B t p m) =
  case R.index p t of
    Nothing -> b
    Just c' -> if c' == c then
                 b
               else
                 moveForwardUntilDelimiter (n-1) c (moveForward b)

-- | Move the point forward to the next instance of a character, the delimiter.
--   If the character doesn't exist after the point, don't move the point.
moveForwardToDelimiter :: Char -> Buffer -> Buffer
moveForwardToDelimiter c b@(B t p m) = case R.elemIndexForward t p c of
  Nothing -> b
  Just x -> B t x m

-- | Move the point backward to the last instance of a character, the delimiter.
--   If the character doesn't exist before the point, don't move the point.
moveBackwardToDelimiter :: Char -> Buffer -> Buffer
moveBackwardToDelimiter c b@(B t p m) = case R.elemIndexBackward t p c of
  Nothing -> b
  Just x -> B t x m

-- | Move to the beginning of the current line.
moveToBeginningOfLine :: Buffer -> Buffer
moveToBeginningOfLine = moveForward . moveBackwardToDelimiter '\n'

-- | Move to the end of the current line.
moveToEndOfLine :: Buffer -> Buffer
moveToEndOfLine = moveForwardToDelimiter '\n'

-- | Go to the same position in a line specified by an int, or do nothing
--   if that line doesnt't exist.
goToLine :: Int -> Buffer -> Buffer
goToLine n b@(B t p m) = goToLine' n (B t 0 m)
  where goToLine' 0 b = b
        goToLine' n b = goToLine' (n-1) $ moveForwardToDelimiter '\n' b
