module BufferList where

import Data.List
import Data.Maybe
import Buffer

data BufferList = BL { buffers :: [Buffer], current :: Maybe Buffer }

{- Design decisions:
   When the user asks to see a buffer list, show as an association list with (name, index)
   Switch to buffer filters on string and then switches by index
   Kill buffer should work the same way
-}

mkBufferList :: BufferList
mkBufferList = BL [] Nothing

-- | Transform the current buffer for the buffer list.
transformCurrentBuffer :: (Buffer -> Buffer) -> BufferList -> BufferList
transformCurrentBuffer _ b@(BL _ Nothing) = b
transformCurrentBuffer f (BL bs (Just c)) = BL bs (Just (f c))

-- | Add a buffer to the buffer list.
addBuffer :: Buffer -> BufferList -> BufferList
addBuffer b (BL bs mc) = BL ((maybeToList mc) ++ bs) (Just b)

-- | Switch to a buffer by name.
switchToBuffer :: String -> BufferList -> BufferList
switchToBuffer n b@(BL bs mc) = case mb' of
                                  Nothing -> b
                                  _ -> BL bl mb'
  where (mb', bl) = lookupByName n $ bs ++ (maybeToList mc)


lookupByName :: String -> [Buffer] -> (Maybe Buffer, [Buffer])
lookupByName s bs = let (s', ds) = partition (\b -> (name b) == s) bs in
  case s' of
    []   -> (Nothing, ds)
    [x]  -> (Just x, ds)
    x:xs -> (Just x, xs ++ ds)

defaultTail :: [a] -> [a]
defaultTail (_:xs) = xs
defaultTail []     = []

-- | Kill a buffer by name. If it is the current buffer, switch to the last buffer
killBuffer :: String -> BufferList -> BufferList
killBuffer n b@(BL bs mc) = case mb' of
                              Nothing -> b
                              _ -> BL (defaultTail bl) (listToMaybe bl)
  where (mb', bl) = lookupByName n $ bs ++ (maybeToList mc)
