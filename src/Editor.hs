module Editor where

import Graphics.Vty
import Control.Monad.State
import Buffer
import BufferList

data Context = Editing | MiniBuffer (String -> Editor) | CtrlPrefix Char

data EditorState = ES { bufferList :: BufferList,
                        done :: Bool,
                        context :: Context,
                        miniBuffer :: (String, String),
                        vty :: Vty }

-- | Make a buffer with correct pages.
mkIdealBuffer :: Vty -> Buffer -> IO Buffer
mkIdealBuffer v (B n t _ _ _) = do
  dims <- display_bounds (terminal v)
  return $ B n t 0 0 (0, fromEnum (region_height dims) - 4)

-- | Make a fresh editor state, with correct pages (we must be in IO to know
--   the display size).
mkIdealEditorState :: Vty -> IO EditorState
mkIdealEditorState v = do
  b <- mkIdealBuffer v scratchBuffer
  return $ ES (BL [] (Just b)) False Editing ("","") v

type Editor = StateT EditorState IO ()

-- | Set an editor session as done.
setDone :: Editor
setDone = modify (\(ES bs _ c mb v) -> ES bs True c mb v)

-- | Set the context for an editor.
setContext :: Context -> Editor
setContext c = modify (\(ES bs d _ mb v) -> ES bs d c mb v)

-- | Modify the minibuffer.
modifyMiniBuffer :: (String -> String) -> Editor
modifyMiniBuffer f = modify (\(ES bs d c (p, mb) v) -> ES bs d c (p, (f mb)) v)

-- | Set the minibuffer
setMiniBuffer :: String -> Editor
setMiniBuffer = modifyMiniBuffer . const

-- | Clear the minibuffer
clearMiniBuffer :: Editor
clearMiniBuffer = modify (\(ES bs d c _ v) -> ES bs d c ("","") v)

-- | Set the minibuffer prompt
setMiniBufferPrompt :: String -> Editor
setMiniBufferPrompt s = modify (\(ES bs d c (_, mb) v) -> ES bs d c (s, mb) v)

-- | Put a character onto the minibuffer
putMiniBuffer :: Char -> Editor
putMiniBuffer c = modifyMiniBuffer (\s -> s ++ [c])

-- | Delete the last character from the minibuffer
deleteCharMiniBuffer :: Editor
deleteCharMiniBuffer = modifyMiniBuffer safeInit where
  safeInit [] = []
  safeInit ts = init ts

-- | Modify the buffer list in an editor session to a new buffer list.
modifyBufferList :: (BufferList -> BufferList) -> Editor
modifyBufferList f = modify (\(ES bs d c mb v) -> ES (f bs) d c mb v)

-- | Modify the current buffer in an editor session.
modifyCurrentBuffer :: (Buffer -> Buffer) -> Editor
modifyCurrentBuffer f = modifyBufferList (transformCurrentBuffer f)

-- | Route keypresses to appropriate editor actions. This function is big, but
--   there are quite a few shortcuts to consider.
updateEditor :: Event -> Context -> Editor
-- Editing actions
updateEditor ev Editing =
  case ev of
    EvKey (KASCII 'q') [MCtrl] -> setDone
    EvKey (KASCII 'x') [MCtrl] -> setContext (CtrlPrefix 'x')
    EvKey (KASCII 'b') [MCtrl] -> modifyCurrentBuffer moveBackward
    EvKey (KASCII 'f') [MCtrl] -> modifyCurrentBuffer moveForward
    EvKey (KASCII 'p') [MCtrl] -> modifyCurrentBuffer movePrevious
    EvKey (KASCII 'n') [MCtrl] -> modifyCurrentBuffer moveNext
    EvKey (KASCII 'd') [MCtrl] -> modifyCurrentBuffer deleteCharAtPoint
    EvKey (KASCII ' ') [MCtrl] -> modifyCurrentBuffer placeMarkAtPoint
    EvKey (KASCII 'a') [MCtrl] -> modifyCurrentBuffer moveToBeginningOfLine
    EvKey (KASCII 'e') [MCtrl] -> modifyCurrentBuffer moveToEndOfLine
    EvKey (KASCII c) [] -> modifyCurrentBuffer $ moveForward . insertCharAtPoint c
    EvKey KDel [] -> modifyCurrentBuffer deleteCharAtPoint
    EvKey KEnter [] -> modifyCurrentBuffer $ moveForward . insertCharAtPoint '\n'
    EvKey KBS [] -> modifyCurrentBuffer $ moveBackward . deleteCharBeforePoint
    _ -> return ()
-- C-x prefix
updateEditor ev (CtrlPrefix 'x') =
  case ev of
    EvKey (KASCII 'b') [] -> do
      st <- get
      case buffers (bufferList st) of
        [] -> return ()
        b:_ -> (setMiniBuffer $ name b) >> setMiniBufferPrompt "Switch to buffer:"
      setContext (MiniBuffer (\s -> modifyBufferList $ switchToBuffer s))
    EvKey (KASCII 'c') [MCtrl] -> setDone
    EvKey (KASCII 'f') _ -> do
      setMiniBufferPrompt "Find file:"
      setContext $ MiniBuffer (\s -> do
                                  b <- liftIO $ bufferFromFile s
                                  st <- get
                                  ib <- liftIO $ mkIdealBuffer (vty st) b
                                  modifyBufferList (addBuffer ib))
    EvKey (KASCII 'k') [] -> do
      st <- get
      case current (bufferList st) of
        Nothing -> return ()
        Just b -> (setMiniBuffer $ name b) >> setMiniBufferPrompt "Kill buffer:"
      setContext $ MiniBuffer (\s -> modifyBufferList $ killBuffer s)
    EvKey (KASCII 's') _ -> do
      st <- get
      case current (bufferList st) of
        Nothing -> return ()
        Just b -> do
          setMiniBuffer $ name b
          setMiniBufferPrompt "Save buffer to file:"
          setContext $ MiniBuffer (\s -> liftIO $ syncBuffer s b)
    EvKey (KASCII 'x') [MCtrl] -> setContext Editing >> modifyCurrentBuffer swapPointAndMark
    _ -> return ()
-- minibuffer actions
updateEditor ev (MiniBuffer f) =
  case ev of
    EvKey (KASCII 'g') [MCtrl] -> clearMiniBuffer >> setContext Editing
    EvKey (KASCII c) [] -> putMiniBuffer c
    EvKey KBS [] -> deleteCharMiniBuffer
    EvKey KEnter [] -> do -- Perform a saved minibuffer action
      setContext Editing
      s <- get
      f $ snd (miniBuffer s)
      clearMiniBuffer
    _ -> return ()
updateEditor _ _ = return ()
