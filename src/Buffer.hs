-- Buffer Module
module Buffer where

-- Text of the buffer
-- Point
-- Mark
-- Undo

data Buffer = B { text :: String, point :: Int, mark :: Int }

move :: Int -> Buffer -> Buffer
move i (B t p m) = B t p' m where
  p' = max 0 (min (p + i) (length t))

moveForward :: Buffer -> Buffer
moveForward = move 1

moveBackward :: Buffer -> Buffer
moveBackward = move (-1)

movePrevious :: Buffer -> Buffer
movePrevious = undefined

moveNext :: Buffer -> Buffer
moveNext = undefined

deleteChar :: Int -> Buffer -> Buffer
deleteChar = undefined

deleteCharAtPoint :: Buffer -> Buffer
deleteCharAtPoint b@(B t p m) = deleteChar p b

deleteCharBeforePoint :: Buffer -> Buffer
deleteCharBeforePoint b@(B t p m) = deleteChar (p - 1) b

insertChar :: Int -> Char -> Buffer -> Buffer
insertChar = undefined

insertCharAtPoint :: Char -> Buffer -> Buffer
insertCharAtPoint = undefined

placeMark :: Int -> Buffer -> Buffer
placeMark = undefined

placeMarkAtPoint :: Buffer -> Buffer
placeMarkAtPoint = undefined

swapPointAndMark :: Buffer -> Buffer
swapPointAndMark = undefined

moveForwardToDelimiter :: Char -> Buffer -> Buffer
moveForwardToDelimiter = undefined

moveBackwardToDelimiter :: Char -> Buffer -> Buffer
moveBackwardToDelimiter = undefined

moveToBeginningOfLine :: Buffer -> Buffer
moveToBeginningOfLine = undefined

moveToEndOfLine :: Buffer -> Buffer
moveToEndOfLine = undefined

goToLine :: Int -> Buffer -> Buffer
goToLine = undefined