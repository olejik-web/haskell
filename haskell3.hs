fn x = celling (negate (tan (cos (max 10 x))))

fn = celling . negate . tan . cos . max 10

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f(g x)

sqrtSum :: Int
sqrtSum = length(takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

reverse' xs = foldl (\acc x -> x:acc) []
product xs = foldl (*) 1

map'' f xs = foldr (\x acc -> f x: acc) [] xs
map''' f xs = foldl (\x acc -> acc ++ [f x]) [] xs

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

sumThree :: Int -> (Int -> (Int -> Int))
sumThree a b c = a + b + c


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let sSort = quicksort [a | a <- xs, a <= x]
      bSort = quicksort [a | a <- xs, a > x]
  in sSort ++ [x] ++ bSort

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

repeat' :: a -> [a]
repeat' x = x: repeat' x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <=0 = []
take' _ [] = []
take' n (x:xs) = x: take' (n - 1) xs

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate'(n - 1) x

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No maximum in empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)