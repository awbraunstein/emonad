module Main where

import Graphics.Vty
import Control.Monad.State
import Editor
import UI

-- | Run an editor.
runEditor :: IO ()
runEditor = evalStateT loop mkEditorState where
  loop = do v <- liftIO iv
            s <- get
            liftIO $ drawEditor s v
            k <- liftIO $ next_event v
            updateEditor k (context s)
            s' <- get
            if done s' then liftIO $ shutdown v else loop
  iv = mkVty

main :: IO ()
main = runEditor