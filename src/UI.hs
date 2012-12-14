{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}

module UI where

import Graphics.Vty
import BufferList
import Buffer

drawEditor :: BufferList -> Vty -> IO ()
drawEditor (BL bs cb) v = update v $ pic_for_image screen where
  mainAttr = with_style current_attr default_style_mask
  reverseAttr = with_style current_attr reverse_video
  screen = titleLine <-> buffer <-> modeLine <-> miniBuffer

  titleLine = string reverseAttr " emonad 0.1.0.0"
  buffer = case cb of
    Nothing -> empty_image
    Just b -> vert_cat $ map decorateLine (getLinesForPage b)
  modeLine = empty_image
  miniBuffer = empty_image
  decorateLine :: (String, Maybe Int) -> Image
  decorateLine (str, Nothing) = string mainAttr str
  decorateLine (str, Just i) = (horiz_cat $ pointifyLine str i 0) <|> char mainAttr ' '

  pointifyLine (x:xs) ind count = (if ind == count then
                                    char reverseAttr x
                                  else
                                    char mainAttr x) : pointifyLine xs ind (count + 1)
  pointifyLine [] ind count = []