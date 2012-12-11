{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}

module UI where

import Graphics.Vty
import BufferList

drawEditor :: BufferList -> Vty -> IO ()
drawEditor bs v = update v $ pic_for_image screen where
  reverseAttr = with_style current_attr reverse_video
  screen = titleLine <-> buffer <-> modeLine <-> miniBuffer

  titleLine = string reverseAttr " emonad 0.1.0.0"
  buffer = empty_image
  modeLine = empty_image
  miniBuffer = empty_image
