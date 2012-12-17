-- Buffer Module
module Buffer where

import Test.HUnit hiding(path)
import Data.Rope(Rope)
import Data.Maybe
import qualified Data.Rope as R
import qualified Rope as R
import System.Directory
import Scratch
import KillRing

data Buffer = B { name :: String,
                  text :: Rope,
                  point :: Int,
                  mark :: Int ,
                  page :: (Int, Int) }
            deriving Show

tests :: Test
tests = test [tRenameBuffer,
              tMkBufferName,
              tMkBufferText,
              tMove1,
              tMove2,
              tMove3,
              tMove4,
              tMovePrev1,
              tMovePrev2,
              tMovePrev3,
              tMoveNext1,
              tMoveNext2,
              tMoveNext3,
              tDeleteChar,
              tInsertChar
             ]

-- | Rename a buffer
renameBuffer :: String -> Buffer -> Buffer
renameBuffer n (B _ t p m pg) = B n t p m pg

tRenameBuffer :: Test
tRenameBuffer = "renameBuffer" ~: name (renameBuffer "new" (mkEmptyBuffer "t")) ~?= "new"

scratchBuffer :: Buffer
scratchBuffer = mkBuffer "*scratch*" scratch

-- | Make an empty buffer with a given name
mkEmptyBuffer :: String -> Buffer
mkEmptyBuffer = flip mkBuffer $ ""

tMkBufferName :: Test
tMkBufferName = "mkBufferName" ~: name b ~?= "name"
  where b = mkBuffer "name" "text"

tMkBufferText :: Test
tMkBufferText = "mkBufferText" ~: R.toString (text b) ~?= "text"
  where b = mkBuffer "name" "text"

-- | Make a new buffer given a name and text
mkBuffer :: String -> String -> Buffer
mkBuffer n t = B n (R.fromString t) 0 0 (0,24)

-- | Move relative to the point. Positive indices move the point forward,
--   negative indices move the point backward.
move :: Int -> Buffer -> Buffer
move i (B n t p m pg) = updatePage (B n t p' m pg) where
  p' = max 0 (min (p + i) (R.length t - 1))

tMove1 :: Test
tMove1 = "tMove1" ~: point (move 1 b) ~?= 1
  where b = mkBuffer "name" "text"

tMove2 :: Test
tMove2 = "tMove2" ~: point (move (-1) b) ~?= 0
  where b = mkBuffer "name" "text"

tMove3 :: Test
tMove3 = "tMove3" ~: point (move 10 b) ~?= 3
  where b = mkBuffer "name" "text"

tMove4 :: Test
tMove4 = "tMove4" ~: point (move (-1) (move 10 b)) ~?= 2
  where b = mkBuffer "name" "text"

-- | Move the point forward one character.
moveForward :: Buffer -> Buffer
moveForward = move 1

-- | Move the point backward one character.
moveBackward :: Buffer -> Buffer
moveBackward = move (-1)

-- | Move to the same position in the previous line.
movePrevious :: Buffer -> Buffer
movePrevious b@(B _ t p _ _) =
  let start = case R.elemIndexBackward t p '\n' of
        Nothing -> 0
        Just x -> (x + 1) in
  let b' = (moveToBeginningOfLine . moveBackward . moveToBeginningOfLine) b in
  moveForwardUntilDelimiter ((point b) - start ) '\n' b'

tMovePrev1 :: Test
tMovePrev1 = "tMovePrev1" ~: point (movePrevious (move 10 b)) ~?= 3
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMovePrev2 :: Test
tMovePrev2 = "tMovePrev2" ~: point (movePrevious (move 9 b)) ~?= 2
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMovePrev3 :: Test
tMovePrev3 = "tMovePrev3" ~: point (movePrevious (move 0 b)) ~?= 0
  where b = mkBuffer "name" (unlines ["012345","012345"])

-- | Move to the same position in the next line.
moveNext :: Buffer -> Buffer
moveNext b@(B _ t p _ _) =
  let start = case R.elemIndexBackward t p '\n' of
                Nothing -> 0
                Just x -> (x + 1) in
  let b' = (moveForward . moveToEndOfLine) b in
  moveForwardUntilDelimiter ((point b) - start) '\n' b'

tMoveNext1 :: Test
tMoveNext1 = "tMoveNext1" ~: point (moveNext (move 10 b)) ~?= 13
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMoveNext2 :: Test
tMoveNext2 = "tMoveNext2" ~: point (moveNext (move 2 b)) ~?= 9
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMoveNext3 :: Test
tMoveNext3 = "tMoveNext3" ~: point (moveNext (move 0 b)) ~?= 7
  where b = mkBuffer "name" (unlines ["012345","012345"])

-- | Delete a character at an index
deleteChar :: Int -> Buffer -> Buffer
deleteChar i (B n t p m pg) = B n (R.delete (i + 1) t) p m pg

tDeleteChar :: Test
tDeleteChar = "tDeleteChar" ~: R.toString (text (deleteChar 0 b)) ~?= "est"
  where b = mkBuffer "name" "test"

-- | Delete the character at the point.
deleteCharAtPoint :: Buffer -> Buffer
deleteCharAtPoint b@(B _ _ p _ _) = deleteChar p b

-- | Delete the character before the point.
deleteCharBeforePoint :: Buffer -> Buffer
deleteCharBeforePoint b@(B _ _ p _ _) = deleteChar (p - 1) b

-- | Insert a character at an index.
insertChar :: Int -> Char -> Buffer -> Buffer
insertChar i c (B n t p m pg) = B n (R.insert i c t) p m pg

tInsertChar :: Test
tInsertChar = "tInsertChar" ~: R.toString (text (insertChar 0 'a' b)) ~?= "atest"
  where b = mkBuffer "name" "test"

-- | Insert a character at the point.
insertCharAtPoint :: Char -> Buffer -> Buffer
insertCharAtPoint c b = insertChar (point b) c b

-- | Place the mark at an index.
placeMark :: Int -> Buffer -> Buffer
placeMark i (B n t p _ pg) = B n t p i pg

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

-- | Move backward n times or until you see the specified character
moveBackwardUntilDelimiter :: Int -> Char -> Buffer -> Buffer
moveBackwardUntilDelimiter 0 _ b = b
moveBackwardUntilDelimiter n c b@(B _ t p _ _) =
  case R.index p t of
    Nothing -> b
    Just c' -> if c' == c then
                 b
               else
                 moveBackwardUntilDelimiter (n-1) c (moveBackward b)

-- | Move the point forward to the next instance of a character, the delimiter.
--   If the character doesn't exist after the point, move to the end of the file.
moveForwardToDelimiter :: Char -> Buffer -> Buffer
moveForwardToDelimiter c (B n t p m pg) = updatePage $ case R.elemIndexForward t p c of
  Nothing -> B n t (R.length t) m pg
  Just x -> B n t x m pg

-- | Move the point backward to the last instance of a character, the delimiter.
--   If the character doesn't exist before the point, move to the start of the file.
moveBackwardToDelimiter :: Char -> Buffer -> Buffer
moveBackwardToDelimiter c (B n t p m pg) = updatePage $ case R.elemIndexBackward t p c of
  Nothing -> B n t 0 m pg
  Just x -> B n t x m pg

-- | Move to the beginning of the current line.
moveToBeginningOfLine :: Buffer -> Buffer
moveToBeginningOfLine b = let b' = moveBackwardToDelimiter '\n' b in
                          if point b' == 0 then
                            b'
                          else
                            moveForward b'

-- | Move to the end of the current line.
moveToEndOfLine :: Buffer -> Buffer
moveToEndOfLine = moveForwardToDelimiter '\n'

-- | Go to the same position in a line specified by an int, or do nothing
--   if that line doesnt't exist.
goToLine :: Int -> Buffer -> Buffer
goToLine n (B nm t _ m pg) = updatePage $ B nm t np m pg where
  np = max 0 $ pred $ sum $ map length $ take n $ lines $ R.toString t

-- | Get the total number of lines
lineCount :: Buffer -> Int
lineCount (B _ t _ _ _) = length $ lines $ R.toString t

lineAtPoint :: Buffer -> Int
lineAtPoint (B _ t p _ _) = aux (map (++ " ") (lines $ R.toString t)) 0 0 where
  aux (x:xs) n ls
    | n > p    = ls
    | otherwise = aux xs (n + (length x)) (ls + 1)
  aux [] _ ls = ls

columnAtPoint :: Buffer -> Int
columnAtPoint b = foldl (\p (_, mc) -> p + fromMaybe 0 mc) 0 (getLinesForPage b)

-- | This will bring the point into the page if it goes past and
--   make sure the mark and point are never out of bounds
updatePage :: Buffer -> Buffer
updatePage b@(B n t p m (start, end))
  | p > len = error "point outside of length"
  | m > len = error "mark outside of length"
  | lap >= end = B n t p m (start + 1, end + 1)
  | lap <= start = B n t p m (start - 1, end - 1)
  | otherwise = b where

    len = length $ R.toString t
    lap = lineAtPoint b

-- | Move to the next page
moveNextPage :: Buffer -> Buffer
moveNextPage b = goToLine (lineCount b + (end - start)) b where
  (start, end) = page b

-- | Move to the previous page
movePreviousPage :: Buffer -> Buffer
movePreviousPage b = goToLine (lineCount b - (end - start)) b where
  (start,end) = page b

-- | Create a buffer from a file, given that file's path
bufferFromFile :: String -> IO Buffer
bufferFromFile path = do
  b <- doesFileExist path
  t <- (if b then
            readFile path
        else
            return "")
  return $ mkBuffer path t

-- | Sync the contents of a buffer to a file, given that file's path.
syncBuffer :: String -> Buffer -> IO ()
syncBuffer path b = writeFile path (R.toString (text b))

-- | Gets the text from the buffer
getText :: Buffer -> String
getText = R.toString . text

-- | Gets the text as a list of lines with the possibility of a point value
getLinesForPage :: Buffer -> [(String, Maybe Int)]
getLinesForPage b@(B _ _ p _ (st, end)) = aux (map (++ " ") (lines ts)) 0 0
  where
  ts = getText b
  aux :: [String] -> Int -> Int -> [(String, Maybe Int)]
  aux [] _ _ = []
  aux (x:xs) count ls
    | ls < st = aux xs (count + (length x)) (ls + 1)
    | ls >= st && ls < end =
      if p < count + (length x) && p >= count then
        (x, Just (p - count)) : aux xs (count + (length x)) (ls + 1)
      else
        (x, Nothing) : aux xs (count + (length x)) (ls + 1)
    | ls >= end = []
    | otherwise = []

killRegion :: KillRing -> Buffer -> (Buffer, KillRing)
killRegion kr b = (b', addToKillRing s kr)
  where
    (s, b') = aux ("", b) (min (point b) (mark b)) (max (point b) (mark b))
    aux :: (String, Buffer) -> Int -> Int -> (String, Buffer)
    aux (s', b'') st en
      | st == en = (s', b'')
      | otherwise =
        case R.index en (text b'') of
          Nothing -> (s', b'')
          Just c -> aux (c:s', deleteChar en b'') st (en - 1)

yank :: KillRing -> Buffer -> (Buffer, KillRing)
yank = undefined
