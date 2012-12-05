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

move :: Int -> Buffer -> Buffer
move i (B t p m) = B t p' m where
  p' = max 0 (min (p + i) (R.length t))

moveForward :: Buffer -> Buffer
moveForward = move 1

moveBackward :: Buffer -> Buffer
moveBackward = move (-1)

movePrevious :: Buffer -> Buffer
movePrevious = moveBackwardToDelimiter '\n'

moveNext :: Buffer -> Buffer
moveNext = moveForward . moveToEndOfLine

deleteChar :: Int -> Buffer -> Buffer
deleteChar i (B t p m) = B (R.delete i t) p m

deleteCharAtPoint :: Buffer -> Buffer
deleteCharAtPoint b@(B t p m) = deleteChar p b

deleteCharBeforePoint :: Buffer -> Buffer
deleteCharBeforePoint b@(B t p m) = deleteChar (p - 1) b

insertChar :: Int -> Char -> Buffer -> Buffer
insertChar i c (B t p m) = B (R.insert i c t) p m 

insertCharAtPoint :: Char -> Buffer -> Buffer
insertCharAtPoint c b = insertChar (point b) c b

placeMark :: Int -> Buffer -> Buffer
placeMark i (B t p m) = B t p i

placeMarkAtPoint :: Buffer -> Buffer
placeMarkAtPoint b = placeMark (point b) b

swapPointAndMark :: Buffer -> Buffer
swapPointAndMark (B t p m) = B t m p

moveForwardToDelimiter :: Char -> Buffer -> Buffer
moveForwardToDelimiter c b@(B t p m) = case R.elemIndexForward t p c of
  Nothing -> b
  Just x -> B t x m

moveBackwardToDelimiter :: Char -> Buffer -> Buffer
moveBackwardToDelimiter c b@(B t p m) = case R.elemIndexBackward t p c of
  Nothing -> b
  Just x -> B t x m

moveToBeginningOfLine :: Buffer -> Buffer
moveToBeginningOfLine = moveForward . movePrevious

moveToEndOfLine :: Buffer -> Buffer
moveToEndOfLine = moveForwardToDelimiter '\n'

goToLine :: Int -> Buffer -> Buffer
goToLine = undefined

