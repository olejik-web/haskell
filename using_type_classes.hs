import Data.List

data Name = Name (String, String) deriving (Show, Eq)

names :: [Name]
names = [Name ("Oleg", "Eremichev"),
        Name ("Ivan", "Mavanov"),
        Name ("Petr", "Alexeev")]

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

class Describable a where
    describe :: a -> String

data BoolValue = TrueValue | FalseValue

instance Show BoolValue where 
    show TrueValue = "True"
    show FalseValue = "False"

data Icecream = Vanilla | Chocolate deriving (Show, Eq, Ord)

data SixSideCube = E1 | E2 | E3 | E4 | E5 | E6 deriving(Eq, Ord, Enum)

data Coin = One | Two

instance Show Coin where 
    show One = "One"
    show Two = "Two"

instance Show SixSideCube where 
    show E1 = "One"
    show E2 = "Two"
    show E3 = "Three"
    show E4 = "Four"
    show E5 = "Five"
    show E6 = "Six"

{--instance Enum SixSideCube where 
    toEnum 0 = E1
    toEnum 1 = E2
    toEnum 2 = E3
    toEnum 3 = E4
    toEnum 4 = E5
    toEnum 5 = E6
    toEnum _ = error "Index of Edge is incorrect"

    fromEnum E1 = 0
    fromEnum E2 = 1
    fromEnum E3 = 2
    fromEnum E4 = 3
    fromEnum E5 = 4
    fromEnum E6 = 5--}

{-- instance Ord SixSideCube where 
    compare E6 E6 = EQ
    compare E6 _ = GT
    compare _ E6 = LT
    compare E5 E5 = EQ
    compare E5 _ = GT
    compare _ E5 = LT
    compare E4 E4 = EQ
    compare E4 _ = GT
    compare _ E4 = LT
    compare E3 E3 = EQ
    compare E3 _ = GT
    compare _ E3 = LT
    compare E2 E2 = EQ
    compare E2 _ = GT
    compare _ E2 = LT
    compare E1 E1 = EQ
    compare _ E1 = LT --}

{-- instance Eq SixSideCube where 
  (==) E1 E1 = True
  (==) E2 E2 = True
  (==) E3 E3 = True
  (==) E4 E4 = True
  (==) E5 E5 = True
  (==) E6 E6 = True
  (==) _ _ = False --}