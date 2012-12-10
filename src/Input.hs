module Input where

import Graphics.Vty
import Editor

-- | Route keypresses to appropriate editor actions
updateEditor :: Editor -> Event -> Vty -> IO ()
updateEditor e (EvKey (KASCII 'q') [MCtrl]) v = shutdown v

updateEditor _ _ _ = return ()