module Principal where

import Arbore

main :: IO [Int]
main = do
     {-text <- putStrLn "Numerele pentru sortare"
     line <- getLine

     let inputString = words line
     let inputInt = map (read) inputString :: [Int]

     text <- putStrLn "Numerele sortate"
     return (parcurgere (ini inputInt))-}

     text <- putStrLn "Limita inferioara"
     numberOne <- getLine
     let n = read numberOne :: Int

     text <- putStrLn "Limita superioara"
     numberTwo <- getLine
     let m = read numberTwo :: Int

     let numbers = [n..m]

     return (parcurgere (ini numbers))
