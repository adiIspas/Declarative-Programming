
import System.IO
import Data.Char

afisare :: String -> IO ()
afisare str =  putStr str >> putChar '\n'

done :: IO ()
done = return ()

afisareSir :: String -> Char -> IO ()
afisareSir [] _ = putChar '\n' -- sau putem pune done
afisareSir (x:xs) ch = putChar x >> putChar ch >> afisareSir xs ch

getMyLine :: IO String
getMyLine = getChar >>= \x ->
            if x == '|' then
              return []
            else
              getMyLine >>= \xs -> return (x:xs)

getNChars :: Int -> IO String
getNChars n = getChar >>= \x ->
                if n == 0 then
                  return []
                else
                  getNChars (n-1) >>= \xs -> return (x:xs)

echo :: IO ()
echo = getLine >>= \line ->
        if line == "" then
          return ()
        else
          putStrLn ("You said: " ++ line) >> echo

echoDo :: IO ()
echoDo = do {
        line <- getLine;
        if line == "" then
          return ()
        else do {
          putStrLn ("You said:" ++ line);
          echoDo
        }
}
