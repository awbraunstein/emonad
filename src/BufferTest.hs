module BufferTest where
import Test.HUnit

import Buffer
import qualified Data.Rope as R
import qualified Rope as R

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

tRenameBuffer :: Test
tRenameBuffer = "renameBuffer" ~: name (renameBuffer "new" (mkEmptyBuffer "t")) ~?= "new"

tMkBufferName :: Test
tMkBufferName = "mkBufferName" ~: name b ~?= "name"
  where b = mkBuffer "name" "text"

tMkBufferText :: Test
tMkBufferText = "mkBufferText" ~: R.toString (text b) ~?= "text"
  where b = mkBuffer "name" "text"

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

tMovePrev1 :: Test
tMovePrev1 = "tMovePrev1" ~: point (movePrevious (move 10 b)) ~?= 3
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMovePrev2 :: Test
tMovePrev2 = "tMovePrev2" ~: point (movePrevious (move 9 b)) ~?= 2
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMovePrev3 :: Test
tMovePrev3 = "tMovePrev3" ~: point (movePrevious (move 0 b)) ~?= 0
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMoveNext1 :: Test
tMoveNext1 = "tMoveNext1" ~: point (moveNext (move 10 b)) ~?= 13
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMoveNext2 :: Test
tMoveNext2 = "tMoveNext2" ~: point (moveNext (move 2 b)) ~?= 9
  where b = mkBuffer "name" (unlines ["012345","012345"])

tMoveNext3 :: Test
tMoveNext3 = "tMoveNext3" ~: point (moveNext (move 0 b)) ~?= 7
  where b = mkBuffer "name" (unlines ["012345","012345"])

tDeleteChar :: Test
tDeleteChar = "tDeleteChar" ~: R.toString (text (deleteChar 0 b)) ~?= "est"
  where b = mkBuffer "name" "test"

tInsertChar :: Test
tInsertChar = "tInsertChar" ~: R.toString (text (insertChar 0 'a' b)) ~?= "atest"
  where b = mkBuffer "name" "test"
