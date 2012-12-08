module BufferList where

import Buffer

data BufferList = BL { buffers :: [Buffer], current :: Buffer }

-- | Add a buffer to the buffer list.
addBuffer :: Buffer -> BufferList -> BufferList
addBuffer = undefined

-- | Switch to a buffer by name.
switchToBuffer :: String -> BufferList -> BufferList
switchToBuffer = undefined

-- | Kill a buffer by name. If it is the current buffer, switch to the last buffer
killBuffer :: String -> BufferList -> BufferList
killBuffer = undefined
