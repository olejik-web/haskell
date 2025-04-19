import Data.Semigroup

-- data Preference = Preference String, Double
data Preference = Preference {name :: String, probability :: Double}

instance Show Preference where
    show (Preference n p) = mconcat ["{name: ", n, ", probability: ", show p, "}"]

instance Eq Preference where
    (==) (Preference n1 _) (Preference n2 _) = n1 == n2

-- test :: [a] -> String
-- test l@(x:xs) = mconcat ["All = ", show l, ", First = ", show x, " Tail = ", show xs]

instance Ord Preference where
    compare (Preference n1 _) (Preference n2 _) = compare n1 n2

instance Semigroup Preference where
    (<>) p@(Preference n _) (Preference "" _) = p
    (<>) (Preference "" _) p@(Preference n _) = p
    (<>) (Preference n1 p1) (Preference n2 p2)
        | n1 == n2 = Preference n1 (max p1 p2)
        | otherwise = error "Mismached preference types"

instance Monoid Preference where
    mempty = Preference "" 0

instance Monoid User where
    mempty = User "" Name{firstName="", lastName=""} []

data Name = Name {firstName :: String, lastName :: String} | 
    NameWithMiddleName {firstName :: String, middleName :: String, 
        lastName :: String}

instance Show Name where
    show (Name f l) = mconcat ["{firstName: ", f, ", lastName: ", l, "}"]
    show (NameWithMiddleName f m l) = mconcat ["{firstName: ", f, ", middleName: ", m, ", lastName: ", l, "}"]

data User = User {uuid :: String, uname :: Name, preferences :: [Preference]} 

instance Show User where 
    show (User i n p) = mconcat["{uuid: ", i, ", uname: ", show n, ", preferences: ", show p, "}"]

instance Semigroup User where
    (<>) user@(User i n p) (User "" NameWithMiddleName{firstName="", lastName="", middleName=""} []) = user
    (<>) (User "" NameWithMiddleName{firstName="", lastName="", middleName=""} []) user@(User i n p) = user
    (<>) user@(User i n p) (User "" Name{firstName="", lastName=""} []) = user
    (<>) (User "" Name{firstName="", lastName=""} []) user@(User i n p) = user
    (<>) (User i1 n1 p1) (User i2 n2 p2)
        | i1 == i2 = User {uuid=i1, uname=n1, preferences=mergePreferences p1 p2}
        | otherwise = error "Mismached users"

mergePreferences :: [Preference] -> [Preference] -> [Preference]
mergePreferences [] new = new
mergePreferences old [] = old
mergePreferences old new
    | x == y = x <> y : mergePreferences xs ys
    | otherwise = x : mergePreferences xs new
    where (x:xs) = quicksort old
          (y:ys) = quicksort new

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let sSort = quicksort $ filter(<=x) xs
        bSort = quicksort $ filter (>x) xs
    in sSort ++ [x] ++ bSort

user1 = User {uuid="1", uname=Name{firstName = "a", lastName="b"}, preferences=[]}