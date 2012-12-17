module KillRing where

type KillRing = [String]

mkKillRing :: KillRing
mkKillRing = []

addToKillRing :: String -> KillRing -> KillRing
addToKillRing s = (:) s

yank :: KillRing -> String
yank (x : xs) = x
yank [] = ""
