module BufferList where

import Data.List
import Data.Maybe
import Buffer

data BufferList = BL { buffers :: [Buffer], current :: Maybe Buffer }

{- Design decisions:
   When the user asks to see a buffer list, show as an association list with (name, index)
   Switch to buffer filters on string and then switches by index
   Kill buffer should work the same way
-}

mkBufferList :: BufferList
mkBufferList = BL [] Nothing

-- | Add a buffer to the buffer list.
addBuffer :: Buffer -> BufferList -> BufferList
addBuffer b (BL bs mc) = BL ((maybeToList mc) ++ bs) (Just b)

-- | Switch to a buffer by index.
switchToBuffer :: Int -> BufferList -> BufferList
switchToBuffer n (BL bs mc) = BL (fst ++ end) (Just cx) where
  (fst, cx : end) = splitAt n (bs ++ (maybeToList mc))

-- | Buffer name, index association list with the last element as the current buffer
getBuffersForSwitch :: BufferList -> [(String, Int)]
getBuffersForSwitch (BL bs mc) = zip (map name (bs ++ (maybeToList mc))) [0..] where

-- | Buffer name, index association list with the first element as the current buffer
getBuffersForKill :: BufferList -> [(String, Int)]
getBuffersForKill (BL bs mc) = zip (map name ((maybeToList mc) ++ bs)) [0..] where

maybeHeadSplit :: [a] -> (Maybe a, [a])
maybeHeadSplit []       = (Nothing, [])
maybeHeadSplit (x : xs) = (Just x, xs)

-- | Kill a buffer by index. If it is the current buffer, switch to the last buffer
killBuffer :: Int -> BufferList -> BufferList
killBuffer n (BL bs mc) = BL bs' cx where
  (fst, c' : end) = splitAt n ((maybeToList mc) ++ bs)
  (cx, bs') = maybeHeadSplit (fst ++ end)