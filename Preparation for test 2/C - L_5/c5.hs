data Season = Spring | Summer | Autumn | Winter

nextSeason :: Season -> Season
nextSeason Spring = Summer
nextSeason Summer = Autumn
nextSeason Autumn = Winter
nextSeason Winter = Spring

eqSeason :: Season -> Season -> Bool
eqSeason Spring Spring = True
eqSeason Summer Summer = True
eqSeason Autumn Autumn = True
eqSeason Winter Winter = True
eqSeason _ _ = False

showSeason :: Season -> String
showSeason Spring = "Spring"
showSeason Summer = "Summer"
showSeason Autumn = "Autumn"
showSeason Winter = "Winter"

toInt :: Season -> Int
toInt Spring = 0
toInt Summer = 1
toInt Autumn = 2
toInt Winter = 3

fromInt :: Int -> Season
fromInt 0 = Spring
fromInt 1 = Summer
fromInt 2 = Autumn
fromInt 3 = Winter

type Radius = Float
type Width = Float
type Height = Float

data Shape = Circle Radius
           | Rectangle Width Height

area :: Shape -> Float
area (Circle radius) = pi * radius ^ 2
area (Rectangle width height) = width * height

eqShape :: Shape -> Shape -> Bool
eqShape (Circle r1) (Circle r2) = r1 == r2
eqShape (Rectangle w1 h1) (Rectangle w2 h2) = (w1 == w2) &&  (h1 == h2)
eqShape _ _ = False

showShape :: Shape -> String
showShape ( Circle r ) = " Circle " ++ showF r
showShape ( Rectangle w h ) = " Rectangle " ++ showF w ++ " " ++ showF h

showF :: Float -> String
showF x | x >= 0 = show x
        | otherwise = " ( " ++ show x ++ " ) "

-- VARIANTA 1
--type FirstName = String
--type LastName = String
--type Age = Int
--type Height = Float
--type PhoneNumber = String
--type Flavor = String

--data Person = Person FirstName LastName Age Height PhoneNumber Flavor

--firstName :: Person -> String
--firstName (Person firstname _ _ _ _ _) = firstname

--lastName :: Person -> String
--lastName (Person _ lastname _ _ _ _) = lastname

--age :: Person -> Int
--age (Person _ _ age _ _ _) = age

--height :: Person -> Float
--height (Person _ _ _ height _ _) = height

--phoneNumber :: Person -> String
--phoneNumber (Person _ _ _ _ phonenumber _ ) = phonenumber

--flavor :: Person -> String
--flavor (Person _ _ _ _ _ flavor) = flavor

-- VARIANTA 2
data Person = Person { firstName :: String
                      , lastName :: String
                      , age :: Int
                      , height :: Float
                      , phoneNumber :: String
                      , flavor :: String
                      }

gigel = Person { firstName = "Gheorghe "
                , lastName="Georgescu "
                , age = 30 , height = 192.3
                , phoneNumber = " 0798765432 "
                , flavor = " Vanilie " }
