{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}

module UI where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

type Location = (Int, Int)

class Drawable d where
  draw :: Location -> d -> IO ()

data UI where
  Widget :: (Drawable d) => Location -> d -> UI
  UIList :: [UI] -> UI

instance Drawable String where
  draw (x,y) s = mvWAddStr stdScr x y s

instance Drawable UI where
  draw _ (Widget l w) = draw l w
  draw l (UIList uis) = mapM_ (draw l) uis

drawUI :: UI -> IO ()
drawUI = draw (0,0)
