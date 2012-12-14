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

-- | Route keypresses to appropriate editor actions
updateEditor :: Event -> Context -> Editor
updateEditor (EvKey (KASCII 'q') [MCtrl]) Editing = setDone
updateEditor (EvKey (KASCII 'x') [MCtrl]) Editing = setContext (CtrlPrefix 'x')
updateEditor (EvKey (KASCII 'b') []) (CtrlPrefix 'x') =
  (do
    st <- get
    case buffers (bufferList st) of
      [] -> return ()
      b:_ -> (setMiniBuffer $ name b) >> (setMiniBufferPrompt "Switch to buffer:")
  ) >>
  setContext (MiniBuffer (\s -> do
                             modifyBufferList $ switchToBuffer s))
updateEditor (EvKey (KASCII 'c') [MCtrl]) (CtrlPrefix 'x') = setContext Editing >> setDone
updateEditor (EvKey (KASCII 'f') _) (CtrlPrefix 'x') =
  (setMiniBufferPrompt "Find file:") >>
  setContext (MiniBuffer (\s -> do
                             b <- liftIO $ bufferFromFile s
                             st <- get
                             ib <- liftIO $ mkIdealBuffer (vty st) b
                             modifyBufferList (addBuffer ib)))
updateEditor (EvKey (KASCII 'k') []) (CtrlPrefix 'x') =
  (do
    st <- get
    case current (bufferList st) of
      Nothing -> return ()
      Just b -> (setMiniBuffer $ name b) >> (setMiniBufferPrompt "Kill buffer:")
  ) >>
  setContext (MiniBuffer (\s -> do
                             modifyBufferList $ killBuffer s))
updateEditor (EvKey (KASCII 's') _) (CtrlPrefix 'x') =
  (do
    st <- get
    case current (bufferList st) of
      Nothing -> return ()
      Just b -> (setMiniBuffer $ name b) >> (setMiniBufferPrompt "Save buffer to file:")
  ) >>
  setContext (MiniBuffer (\s -> do
                             st <- get
                             case current (bufferList st) of
                               Nothing -> return ()
                               Just b -> liftIO $ syncBuffer s b))
updateEditor (EvKey (KASCII 'x') [MCtrl]) (CtrlPrefix 'x') = setContext Editing >> modifyCurrentBuffer swapPointAndMark
updateEditor (EvKey (KASCII 'b') [MCtrl]) Editing = modifyCurrentBuffer moveBackward
updateEditor (EvKey (KASCII 'f') [MCtrl]) Editing = modifyCurrentBuffer moveForward
updateEditor (EvKey (KASCII 'n') [MCtrl]) Editing = modifyCurrentBuffer moveNext
updateEditor (EvKey (KASCII 'p') [MCtrl]) Editing = modifyCurrentBuffer movePrevious
updateEditor (EvKey (KASCII 'g') [MCtrl]) _       = clearMiniBuffer >> setContext Editing
updateEditor (EvKey KLeft []) Editing             = modifyCurrentBuffer moveBackward
updateEditor (EvKey KRight []) Editing            = modifyCurrentBuffer moveForward
updateEditor (EvKey KUp []) Editing               = modifyCurrentBuffer movePrevious
updateEditor (EvKey KDown []) Editing             = modifyCurrentBuffer moveNext
updateEditor (EvKey (KASCII 'd') [MCtrl]) Editing = modifyCurrentBuffer deleteCharAtPoint
updateEditor (EvKey (KASCII ' ') [MCtrl]) Editing = modifyCurrentBuffer placeMarkAtPoint
updateEditor (EvKey (KASCII 'a') [MCtrl]) Editing = modifyCurrentBuffer moveToBeginningOfLine
updateEditor (EvKey (KASCII 'e') [MCtrl]) Editing = modifyCurrentBuffer moveToEndOfLine
updateEditor (EvKey (KASCII c) []) Editing        = modifyCurrentBuffer $ moveForward . insertCharAtPoint c
updateEditor (EvKey KDel []) Editing              = modifyCurrentBuffer $ deleteCharAtPoint
updateEditor (EvKey KEnter []) Editing            = modifyCurrentBuffer $ moveForward . insertCharAtPoint '\n'
updateEditor (EvKey KBS []) Editing               = modifyCurrentBuffer $ moveBackward . deleteCharBeforePoint
updateEditor (EvKey KBackTab []) Editing          = modifyCurrentBuffer $ moveForward . insertCharAtPoint '\t'
updateEditor (EvKey KEnter []) (MiniBuffer a)     = do
  setContext Editing
  s <- get
  a $ snd (miniBuffer s) -- run the action on the contents of the minibuffer
  clearMiniBuffer
updateEditor (EvKey (KASCII c) []) (MiniBuffer _) = putMiniBuffer c
updateEditor (EvKey KBS []) (MiniBuffer _) = deleteCharMiniBuffer
updateEditor _ _                            = return ()
