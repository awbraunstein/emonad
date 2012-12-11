module Editor where

import Graphics.Vty
import Control.Monad.State
import BufferList

data Editor = Editor { done :: Bool, bufferList :: BufferList }
