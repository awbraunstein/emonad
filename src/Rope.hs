module Rope where

data Rope a = Empty | Branch Int (Rope a) (Maybe [a]) (Rope a)

index :: Int -> Rope a -> Maybe a
index a = index' (a + 1) where
  index' _ Empty = Nothing
  index' i (Branch w l a r)
    | w < i = index' (i-w) r
    | otherwise =
      case l of
        Empty -> do
               as <- a
               return $ as !! (i - 1)
        otherwise -> index' i l

insert :: a -> Int -> Rope a -> Rope a











testRope :: Rope Char
testRope = (Branch 22
                     (Branch 9
                               (Branch 9
                                         (Branch 6 Empty (Just "Hello_")
                                                   (Branch 3 Empty (Just "my_") Empty))
                                         Nothing
                                         Empty
                               )
                      Nothing
                      (Branch 7
                                (Branch 6
                                          (Branch 2
                                                  Empty
                                                  (Just "na")
                                                  (Branch 4 Empty (Just "me_i") Empty)
                                          )
                                          Nothing
                                          (Branch 1
                                                    (Branch 1 Empty (Just "s") Empty)
                                                    Nothing
                                                    Empty
                                          )
                                )
                                Nothing
                                (Branch 6
                                          (Branch 6
                                                    (Branch 6
                                                            Empty
                                                            (Just "_Simon")
                                                            Empty
                                                    )
                                                    Nothing
                                                    Empty
                                          )
                                          Nothing
                                          Empty
                                )

                      )
                     )
                     Nothing
                     Empty
           )
