import qualified Data.Map as Map

data Box a = Box a deriving Show
data Triple a = Triple a a a deriving Show
data List a = Empty | Cons a (List a) deriving Show
data Food = Bread | Onion | Tomato | Cucumber | Cheese | Apple deriving (Show, Ord, Eq)

type Point3D = Triple Double
type FullName = Triple String

maybeMap :: (a -> Maybe b) -> [a] -> [Maybe b]
maybeMap f xs = map f xs

possibleIds :: [Integer]
possibleIds = [1..100]
findFoodContent ids catalogue = map findContents ids
    where findContents = \id -> Map.lookup id catalogue

availableFoods :: [Maybe Food]
availableFoods = findFoodContent possibleIds foodDict

isSomething :: Maybe Food -> Bool
isSomething Nothing = False
isSomething (Just _) = True

showFood :: Maybe Food -> String
showFood (Just a) = show a

showFindFoods :: [String]
showFindFoods = foldl (\acc x -> if isSomething x then (showFood x):acc else acc) [] availableFoods

foods :: [Food]
foods = [Onion, Bread, Cucumber, Tomato, Apple, Cheese]

ids :: [Integer]
ids = [1, 22, 15, 10, 5, 7]

idsFood :: [(Integer, Food)]
idsFood = zip ids foods

foodDict :: Map.Map Integer Food
foodDict = Map.fromList idsFood

foodCount :: [Food] -> Map.Map Food Int
foodCount [] = Map.empty
foodCount (food:rest) = Map.insertWith (+) food 1 (foodCount rest)

sampleFoods :: [Food]
sampleFoods = [Onion, Tomato, Tomato, Bread, Onion, Onion]

foodCountDict :: Map.Map Food Int
foodCountDict = foodCount sampleFoods

newList :: List Integer
newList = Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))

c_map :: (a -> b) -> List a -> List b
c_map _ Empty = Empty
c_map f (Cons a rest) = Cons (f a) (c_map f rest)

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

wrap :: a -> Box a
wrap n = Box n

unwrap :: Box a -> a
unwrap (Box n) = n