module Input where

import Graphics.Vty
import Editor

-- | Route keypresses to appropriate editor actions
updateEditor :: Editor -> Event -> Vty -> IO ()
updateEditor e (EvKey (KASCII 'q') [MCtrl]) v = shutdown v
updateEditor e (EvKey (KASCII 'x') [MCtrl]) v = do
  e' <- next_event v
  case e' of
    EvKey (KASCII 'x') [MCtrl] -> undefined -- exchange-point-and-mark
    EvKey (KASCII 'f') _ -> undefined -- find-file
    EvKey (KASCII 's') _ -> undefined -- save-buffer
    EvKey (KASCII 'c') [MCtrl] -> undefined -- quit
    EvKey (KASCII 'b') [] -> undefined -- switch-buffer
    _ -> return ()
updateEditor e (EvKey (KASCII 'b') [MCtrl]) v = undefined -- backward-char
updateEditor e (EvKey (KASCII 'f') [MCtrl]) v = undefined -- forward-char
updateEditor e (EvKey (KASCII 'n') [MCtrl]) v = undefined -- next-line
updateEditor e (EvKey (KASCII 'p') [MCtrl]) v = undefined -- previous-line
updateEditor e (EvKey (KASCII 'd') [MCtrl]) v = undefined -- delete-char
updateEditor e (EvKey (KASCII ' ') [MCtrl]) v = undefined -- set-mark-command
updateEditor e (EvKey (KASCII 'a') [MCtrl]) v = undefined -- move-beginning-of-line
updateEditor e (EvKey (KASCII 'e') [MCtrl]) v = undefined -- move-end-of-line
updateEditor _ _ _ = return ()
