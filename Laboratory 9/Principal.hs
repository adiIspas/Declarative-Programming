module Principal where

import Arbore

main :: IO [Int]
main = do
  text <- putStrLn "Introdu numerele:"
  numere_string <- getLine

  let result = words numere_string

  let numere_int = map (read) result::[Int]

  return numere_int

  --let arbore = ini numere_int

  --return (parcurgere arbore)