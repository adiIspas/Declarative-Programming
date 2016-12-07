-- Informatics 1 - Functional Programming
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit            = []
split (cmd1 :#: cmd2) =  split cmd1 ++ split cmd2
split cmd            = [cmd]

-- 1b. join
join :: [Command] -> Command
join cmds = foldr (:#:) Sit cmds

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent cmd1 cmd2 = split cmd1 == split cmd2

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join cmd = equivalent (join (split cmd)) cmd

prop_split = undefined


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n cmd | n <= 0 = Sit
           | n == 1 = cmd
           | otherwise = cmd :#: copy (n-1) cmd

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon dist = copy 5 (Go dist:#:Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon dist nr = copy nr (Go dist:#:Turn (fromIntegral (div 360 nr)) )


-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral dist lns step ang = sp dist lns
  where
  sp dist lns | dist <= 0 || lns == 0 = Sit
         | otherwise        = Go dist :#: Turn ang :#: sp (dist+step) (lns-1)


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise = undefined


-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

main :: IO ()
main = display (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30)
