{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}

module UI where

import Graphics.Vty
import Input
import Editor

type Location = (Int, Int)

class Drawable d where
  draw :: Location -> d -> IO ()

data UI where
  Widget :: (Drawable d) => Location -> d -> UI
  UIList :: [UI] -> UI

instance Drawable String where
  draw (x,y) s = undefined

instance Drawable UI where
  draw _ (Widget l w) = draw l w
  draw l (UIList uis) = mapM_ (draw l) uis

drawUI :: UI -> IO ()
drawUI = draw (0,0)

mainLoop :: IO ()
mainLoop = loop where
  loop = do v <- iv
            drawEditor editor v
            e <- next_event v
            updateEditor editor e v
            if done editor then shutdown v else loop
  iv = mkVty
  editor = undefined

drawEditor :: Editor -> Vty -> IO ()
drawEditor e v = return ()
