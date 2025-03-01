head'' xs = 
    case xs of
        [] -> error "Empty list"
        (x:_) -> x

cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r * r * 2
    in sideArea + 2 * topArea

max' a b
    | a <= b = b
    | otherwise = a

initials firstName lastName = [f] ++ " " ++ [l] ++ "."
    where (f: _) = firstName
          (l: _) = lastName

numsCalc a b 
    | sum < 10.0 = 10.0
    | sum < 20.0 = 20.0
    | sum < 30.0 = 30.0
    | sum < 40.0 = 40.0
    | sum < 50.0 = 50.0
    | otherwise = 0.0
    where sum = a + b

yearTell :: Integer -> String
yearTell y
    | y <= 17 = "School"
    | y <= 19 = "University"
    | y <= 20 = "UniversityAndWork"
    | otherwise = "DeadMan"

firstChar str@(x:xs) = "First letter in string " ++ str ++ " is " ++ [x]

sumThreeElements (x:y:z:_) = x + y + z

tellAboutList :: (Show a) => [a] -> String
tellAboutList [] = "Empty list"
tellAboutList (x:[]) = "One element in list " ++ (show x)
tellAboutList (x:y:[]) = "Two elements in list " ++ (show x) ++ " " ++ (show y)
tellAboutList (x:y:_) = "List have more than tho elements"

head' :: [a] -> a
head' [] = error "Empty list"

head' (x:_) = x

hello :: Int -> String
hello n = "Hello! " ++ (show n)

sumVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
sumVectors a b = (fst a + fst b, snd a + snd b)

sumVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
sumVectors' (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

sayNumber :: Int -> String
sayNumber 1 = "One"
sayNumber 2 = "Two"
sayNumber 3 = "Three"
sayNumber 4 = "Four"
sayNumber 5 = "Five"