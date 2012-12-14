module Editor where

import Graphics.Vty
import Control.Monad.State
import Buffer
import BufferList
import UI

data Context = Editing | MiniBuffer Editor | CtrlPrefix Char

data EditorState = ES { bufferList :: BufferList,
                        done :: Bool,
                        context :: Context,
                        miniBuffer :: String }

mkEditorState :: EditorState
mkEditorState = ES mkBufferList False Editing ""

type Editor = StateT EditorState IO ()

-- | Run an editor.
runEditor :: IO ()
runEditor = evalStateT loop mkEditorState where
  loop = do v <- liftIO iv
            s <- get
            liftIO $ drawEditor (bufferList s) v
            k <- liftIO $ next_event v
            updateEditor k (context s)
            s' <- get
            if done s' then liftIO $ shutdown v else loop
  iv = mkVty

-- | Set an editor session as done.
setDone :: Editor
setDone = modify (\(ES bs _ c mb) -> ES bs True c mb)

-- | Set the context for an editor.
setContext :: Context -> Editor
setContext c = modify (\(ES bs d _ mb) -> ES bs d c mb)

-- | Modify the buffer list in an editor session to a new buffer list.
modifyBufferList :: (BufferList -> BufferList) -> Editor
modifyBufferList f = modify (\(ES bs d c mb) -> ES (f bs) d c mb)

-- | Modify the current buffer in an editor session.
modifyCurrentBuffer :: (Buffer -> Buffer) -> Editor
modifyCurrentBuffer f = modifyBufferList (transformCurrentBuffer f)

getString :: Vty -> IO String
getString v = do
  e <- next_event v
  case e of
    EvKey (KASCII c) [] -> do
      rest <- getString v
      return $ c : rest
    _ -> return []

getInt :: Vty -> IO Int
getInt v = fmap read (getString v)

-- | Route keypresses to appropriate editor actions
updateEditor :: Event -> Context -> Editor
updateEditor (EvKey (KASCII 'q') [MCtrl]) Editing = setDone
updateEditor (EvKey (KASCII 'x') [MCtrl]) Editing = setContext (CtrlPrefix 'x')
updateEditor (EvKey (KASCII 'b') []) (CtrlPrefix 'x') = undefined
updateEditor (EvKey (KASCII 'c') [MCtrl]) (CtrlPrefix 'x') = setContext Editing >> setDone
updateEditor (EvKey (KASCII 'f') _) (CtrlPrefix 'x') = do
  b <- liftIO $ bufferFromFile "/Users/rafekett/test.py"
  liftIO $ putStrLn $ show b
  setContext Editing
  modifyBufferList $ addBuffer b
updateEditor (EvKey (KASCII 'k') []) (CtrlPrefix 'x') = setContext Editing >> undefined
updateEditor (EvKey (KASCII 's') _) (CtrlPrefix 'x') = setContext Editing >> undefined
updateEditor (EvKey (KASCII 'x') [MCtrl]) (CtrlPrefix 'x') = setContext Editing >> modifyCurrentBuffer swapPointAndMark
updateEditor (EvKey (KASCII 'b') [MCtrl]) Editing = modifyCurrentBuffer moveBackward
updateEditor (EvKey (KASCII 'f') [MCtrl]) Editing = modifyCurrentBuffer moveForward
updateEditor (EvKey (KASCII 'n') [MCtrl]) Editing = modifyCurrentBuffer moveNext
updateEditor (EvKey (KASCII 'p') [MCtrl]) Editing = modifyCurrentBuffer movePrevious
updateEditor (EvKey (KASCII 'g') [MCtrl]) _       = setContext Editing
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
updateEditor _ _                            = return ()
