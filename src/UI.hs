module UI where

import Graphics.Vty
import Editor
import BufferList
import Buffer

drawEditor :: EditorState -> Vty -> IO ()
drawEditor (ES (BL _ cb) _ c (p,mb) _) v = update v $ pic_for_image screen where
  mainAttr = with_style current_attr default_style_mask
  reverseAttr = with_style current_attr reverse_video
  screen = titleLine <-> buffer <-> modeLine <-> minibuffer

  titleLine = string reverseAttr " emonad 0.1.0.0"

  buffer = case cb of
    Nothing -> empty_image
    Just b -> let (s,e) = page b in
      pad (1, toEnum (e - s)) $ vert_cat $ map decorateLine (getLinesForPage b) ++
      [char mainAttr ' ']

  modeLine = string reverseAttr $
             case cb of
               Nothing -> "No Buffer -- Use C-x C-f to open a new file"
               Just b -> (name b) ++
                         "  " ++
                         "(" ++
                         show (columnAtPoint b) ++
                         "," ++
                         show (lineAtPoint b) ++
                         ")"

  minibuffer = char mainAttr ' ' <|> case c of
    Editing -> empty_image
    MiniBuffer _ -> string mainAttr p <|>
                    char mainAttr ' ' <|>
                    string mainAttr mb <|>
                    char reverseAttr ' ' <|>
                    char mainAttr ' '
    CtrlPrefix ch -> string mainAttr ['C','-',ch]

  decorateLine (str, Nothing) = string mainAttr str
  decorateLine (str, Just i) = (horiz_cat $ pointifyLine str i 0) <|>
                               char mainAttr ' '

  pointifyLine (x:xs) ind count =
    let p' = (if ind == count then char reverseAttr else char mainAttr) x in
    p' : pointifyLine xs ind (count + 1)
  pointifyLine [] _ _ = []