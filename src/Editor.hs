module Editor where

import Graphics.Vty
import Control.Monad.State
import Buffer
import BufferList

data Context = Editing | MiniBuffer (String -> Editor) | CtrlPrefix Char

data EditorState = ES { bufferList :: BufferList,
                        done :: Bool,
                        context :: Context,
                        miniBuffer :: String }

mkEditorState :: EditorState
mkEditorState = ES mkBufferList False Editing ""

type Editor = StateT EditorState IO ()

-- | Set an editor session as done.
setDone :: Editor
setDone = modify (\(ES bs _ c mb) -> ES bs True c mb)

-- | Set the context for an editor.
setContext :: Context -> Editor
setContext c = modify (\(ES bs d _ mb) -> ES bs d c mb)

-- | Modify the minibuffer.
modifyMiniBuffer :: (String -> String) -> Editor
modifyMiniBuffer f = modify (\(ES bs d c mb) -> ES bs d c (f mb))

-- | Set the minibuffer
setMiniBuffer :: String -> Editor
setMiniBuffer = modifyMiniBuffer . const

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
modifyBufferList f = modify (\(ES bs d c mb) -> ES (f bs) d c mb)

-- | Modify the current buffer in an editor session.
modifyCurrentBuffer :: (Buffer -> Buffer) -> Editor
modifyCurrentBuffer f = modifyBufferList (transformCurrentBuffer f)

-- | Route keypresses to appropriate editor actions
updateEditor :: Event -> Context -> Editor
updateEditor (EvKey (KASCII 'q') [MCtrl]) Editing = setDone
updateEditor (EvKey (KASCII 'x') [MCtrl]) Editing = setContext (CtrlPrefix 'x')
updateEditor (EvKey (KASCII 'b') []) (CtrlPrefix 'x') = undefined
updateEditor (EvKey (KASCII 'c') [MCtrl]) (CtrlPrefix 'x') = setContext Editing >> setDone
updateEditor (EvKey (KASCII 'f') _) (CtrlPrefix 'x') =
  setContext (MiniBuffer (\s -> do
                             b <- liftIO $ bufferFromFile s
                             modifyBufferList (addBuffer b)))
updateEditor (EvKey (KASCII 'k') []) (CtrlPrefix 'x') = setContext Editing >> undefined
updateEditor (EvKey (KASCII 's') _) (CtrlPrefix 'x') =
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
updateEditor (EvKey (KASCII 'g') [MCtrl]) _       = setMiniBuffer "" >> setContext Editing
updateEditor (EvKey KLeft []) Editing             = modifyCurrentBuffer moveBackward
updateEditor (EvKey KRight []) Editing            = modifyCurrentBuffer moveForward
updateEditor (EvKey KUp []) Editing               = modifyCurrentBuffer moveNext
updateEditor (EvKey KDown []) Editing             = modifyCurrentBuffer movePrevious
updateEditor (EvKey (KASCII 'd') [MCtrl]) Editing = modifyCurrentBuffer deleteCharAtPoint
updateEditor (EvKey (KASCII ' ') [MCtrl]) Editing = modifyCurrentBuffer placeMarkAtPoint
updateEditor (EvKey (KASCII 'a') [MCtrl]) Editing = modifyCurrentBuffer moveToBeginningOfLine
updateEditor (EvKey (KASCII 'e') [MCtrl]) Editing = modifyCurrentBuffer moveToEndOfLine
updateEditor (EvKey (KASCII 'v') [MCtrl]) Editing = modifyCurrentBuffer moveNextPage
updateEditor (EvKey (KASCII 'v') [MMeta]) Editing = modifyCurrentBuffer movePreviousPage
updateEditor (EvKey (KASCII c) []) Editing        = modifyCurrentBuffer $ moveForward . insertCharAtPoint c
updateEditor (EvKey KDel []) Editing              = modifyCurrentBuffer $ deleteCharAtPoint
updateEditor (EvKey KEnter []) Editing            = modifyCurrentBuffer $ moveForward . insertCharAtPoint '\n'
updateEditor (EvKey KBS []) Editing               = modifyCurrentBuffer $ moveBackward . deleteCharBeforePoint
updateEditor (EvKey KBackTab []) Editing          = modifyCurrentBuffer $ moveForward . insertCharAtPoint '\t'
updateEditor (EvKey KEnter []) (MiniBuffer a)     = do
  setContext Editing
  s <- get
  a (miniBuffer s) -- run the action on the contents of the minibuffer
  setMiniBuffer ""
updateEditor (EvKey (KASCII c) []) (MiniBuffer _) = putMiniBuffer c
updateEditor (EvKey KBS []) (MiniBuffer _) = deleteCharMiniBuffer
updateEditor _ _                            = return ()
