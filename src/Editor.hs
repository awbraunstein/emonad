module Editor where

import Graphics.Vty
import Control.Monad.State
import Buffer
import BufferList
import UI

type EditorState = (Bool,BufferList)
type Editor = StateT EditorState IO ()

-- | Run an editor.
runEditor :: IO ()
runEditor = evalStateT loop (False,mkBufferList) where
  loop = do v <- liftIO iv
            (done,bs) <- get
            liftIO $ drawEditor bs v
            k <- liftIO $ next_event v
            updateEditor k v
            if done then liftIO $ shutdown v else loop
  iv = mkVty

-- | Set an editor session as done.
setDone :: Editor
setDone = modify (\(_,bs) -> (True,bs))

-- | Modify the buffer list in an editor session to a new buffer list.
modifyBufferList :: (BufferList -> BufferList) -> Editor
modifyBufferList f = modify (\(d,bs) -> (d,f bs))

modifyCurrentBuffer :: (Buffer -> Buffer) -> Editor
modifyCurrentBuffer f = modifyBufferList (transformCurrentBuffer f)

-- | Route keypresses to appropriate editor actions
updateEditor :: Event -> Vty -> Editor
updateEditor (EvKey (KASCII 'q') [MCtrl]) v = setDone
updateEditor (EvKey (KASCII 'x') [MCtrl]) v = do
  e' <- liftIO $ next_event v
  case e' of
    EvKey (KASCII 'x') [MCtrl] -> modifyCurrentBuffer swapPointAndMark
    EvKey (KASCII 'f') _ -> undefined -- find-file
    EvKey (KASCII 's') _ -> undefined -- save-buffer
    EvKey (KASCII 'c') [MCtrl] -> setDone
    EvKey (KASCII 'b') [] -> undefined -- switch-buffer
    _ -> return ()
updateEditor (EvKey (KASCII 'b') [MCtrl]) v = modifyCurrentBuffer moveBackward
updateEditor (EvKey (KASCII 'f') [MCtrl]) v = modifyCurrentBuffer moveForward
updateEditor (EvKey (KASCII 'n') [MCtrl]) v = modifyCurrentBuffer moveNext
updateEditor (EvKey (KASCII 'p') [MCtrl]) v = modifyCurrentBuffer movePrevious
updateEditor (EvKey (KASCII 'd') [MCtrl]) v = modifyCurrentBuffer deleteCharAtPoint
updateEditor (EvKey (KASCII ' ') [MCtrl]) v = modifyCurrentBuffer placeMarkAtPoint
updateEditor (EvKey (KASCII 'a') [MCtrl]) v = modifyCurrentBuffer moveToBeginningOfLine
updateEditor (EvKey (KASCII 'e') [MCtrl]) v = modifyCurrentBuffer moveToEndOfLine
updateEditor (EvKey (KASCII 'v') [MCtrl]) v = modifyCurrentBuffer moveNextPage
updateEditor (EvKey (KASCII 'v') [MMeta]) v = modifyCurrentBuffer movePreviousPage
updateEditor _ _ = return ()
