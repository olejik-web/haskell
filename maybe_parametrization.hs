import qualified Data.Map as Map

data Box a = Box a deriving Show

data Triple a = Triple a a a deriving Show
data List a = Empty | Cons a (List a) deriving Show

data Food = Breed | Onion | Tomato | Cucumber | Cheese | Apple deriving (Show, Eq, Ord)

allFoods :: [Food]
allFoods = [Onion, Breed, Tomato, Cucumber, Cheese, Apple]

foodsList = [Breed, Breed, Breed, Breed, Breed, Cheese, Cheese, Cheese, Onion, Onion, Apple]

makeStat [] = []
makeStat (x:xs) = [(x, calcCount x foodsList)] ++ (makeStat xs)

-- makeStatIds [] = []
-- makeStatIds (s:stat) = [((foodId (fst s) idsFood), snd s)] ++ makeStatIds stat

problemDict = Map.fromList (makeStat allFoods)

calcCount f [] = 0
calcCount f (x:xs) = (if f == x then 1 else 0) + calcCount f xs

counts :: [Integer]
counts = [10, 20, 30, 40, 50, 60]

ids :: [Integer]
ids = [1, 22, 15, 10, 5, 7]

idsFood :: [(Integer, Food)]
idsFood = zip ids allFoods

foodsCount :: [(Integer, Integer)]
foodsCount = zip ids counts

foodDict :: Map.Map Integer Food
foodDict = Map.fromList idsFood

countDict :: Map.Map Integer Integer
countDict = Map.fromList foodsCount

foodId food [] = error "Not find food"
foodId food (x:xs) = if (snd x) == food then fst x else foodId food xs

foodCount food = show (Map.lookup (foodId food idsFood) countDict)

type Point3D = Triple Double
type FullName = Triple String

newList :: List Integer
newList = Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))

c_map :: (a -> b) -> List a -> List b
c_map _ Empty = Empty
c_map f (Cons a rest) = Cons (f a) (c_map f rest)

-- indexElement :: (a -> b) -> List a -> List b
indexElement _ Empty = error "IndexError"
indexElement 0 (Cons a rest) = a
indexElement number (Cons a rest) = indexElement (number - 1) rest

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ y _) = y

third :: Triple a -> a
third (Triple _ _ z) = z

tripleToList :: Triple a -> [a]
tripleToList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

warp :: a -> Box a
warp n = Box n

unwarp :: Box a -> a
unwarp (Box n) = n

userData = ['1', '2', '3', '\n', '9', '1', '2', '\n']