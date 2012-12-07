module BufferList where

import Buffer

data BufferList = BL { buffers :: [Buffer], current :: Buffer }

-- | Add a buffer to the buffer list.
addBuffer :: Buffer -> Editor -> Editor
addBuffer = undefined

-- | Switch to a buffer by name.
switchToBuffer :: String -> Editor -> Editor
switchToBuffer = undefined

-- | Kill a buffer by name. If it is the current buffer, switch to the last buffer
killBuffer :: String -> Editor -> Editor
killBuffer = undefined
