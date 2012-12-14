{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}

module UI where

import Graphics.Vty
import Editor
import BufferList
import Buffer

drawEditor :: EditorState -> Vty -> IO ()
drawEditor (ES (BL bs cb) _ c mb) v = update v $ pic_for_image screen where
  mainAttr = with_style current_attr default_style_mask
  reverseAttr = with_style current_attr reverse_video
  screen = titleLine <-> buffer <-> modeLine <-> miniBuffer

  titleLine = string reverseAttr " emonad 0.1.0.0"
  buffer = case cb of
    Nothing -> empty_image
    Just b -> vert_cat $ map decorateLine (getLinesForPage b)
  modeLine = string reverseAttr $
             case cb of
               Nothing -> "No Buffer -- Use C-x C-f to open a new file"
               Just b -> (name b) ++
                         "  " ++
                         "(" ++
                         show (point b) ++
                         "," ++
                         show (lineAtPoint b) ++
                         ")"
  miniBuffer = char mainAttr ' ' <|> case c of
                                          Editing -> empty_image
                                          MiniBuffer _ -> string mainAttr mb
                                          CtrlPrefix c -> string mainAttr ['C','-',c]
  decorateLine :: (String, Maybe Int) -> Image
  decorateLine (str, Nothing) = string mainAttr str
  decorateLine (str, Just i) = (horiz_cat $ pointifyLine str i 0) <|> char mainAttr ' '

  pointifyLine (x:xs) ind count = (if ind == count then
                                    char reverseAttr x
                                  else
                                    char mainAttr x) : pointifyLine xs ind (count + 1)
  pointifyLine [] ind count = []