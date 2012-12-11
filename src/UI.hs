{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}

module UI where

import Graphics.Vty
import BufferList

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

drawEditor :: BufferList -> Vty -> IO ()
drawEditor bs v = update v $ pic_for_image screen where
  reverseAttr = with_style current_attr reverse_video
  screen = titleLine <-> buffer <-> modeLine <-> miniBuffer

  titleLine = string reverseAttr " emonad 0.1.0.0"
  buffer = empty_image
  modeLine = empty_image
  miniBuffer = empty_image
