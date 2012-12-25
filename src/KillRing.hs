module KillRing where

data KillRing = KR { ring :: [String],
                     seen :: [String]}

mkKillRing :: KillRing
mkKillRing = KR [] []

addToKillRing :: String -> KillRing -> KillRing
addToKillRing st (KR r s) = KR (st:r) s

yank :: KillRing -> (String, KillRing)
yank (KR (x : xs) s) = (x, (KR (s ++ xs) [x]))
yank (KR [] _) = ("", mkKillRing)

yankNext :: KillRing -> (String, KillRing)
yankNext (KR (x : xs) s) = (x, KR xs (x:s))
yankNext (KR [] [] ) = ("", mkKillRing)
yankNext (KR [] s ) = yankNext (KR s [])