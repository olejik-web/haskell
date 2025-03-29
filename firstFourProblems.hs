class Describable a where
    describe :: a -> String

type AutoModel = String
type FuelCount = Double
type FuelFlow = Double
type TravelDistance = Double
type MiddleSpeed = Double

data AutoClass = A | B | C | D | E | F 

auto :: (a, b, c, d, e, f) -> ((a, b, c, d, e, f) -> t) -> t
auto (model, fuelCount, fuelFlow, travelDistance, middleSpeed, autoClass) = 
    \msg -> msg (model, fuelCount, fuelFlow, travelDistance, middleSpeed, autoClass)

model :: (AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> AutoModel
model (m, _, _, _, _, _) = m

fuelCount :: (AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> FuelCount
fuelCount (_, fc, _, _, _, _) = fc

fuelFlow :: (AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> FuelFlow
fuelFlow (_, _, ff, _, _, _) = ff

travelDistance :: (AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> TravelDistance
travelDistance (_, _, _, td, _, _) = td

middleSpeed :: (AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> MiddleSpeed
middleSpeed (_, _, _, _, ms, _) = ms

autoClass :: (AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> AutoClass
autoClass (_, _, _, _, _, ac) = ac

instance Ord AutoClass where 
    compare F F = EQ
    compare F _ = GT
    compare _ F = LT
    compare E E = EQ
    compare E _ = GT
    compare _ E = LT
    compare D D = EQ
    compare D _ = GT
    compare _ D = LT
    compare C C = EQ
    compare C _ = GT
    compare _ C = LT
    compare B B = EQ
    compare B _ = GT
    compare _ B = LT
    compare A A = EQ
    compare _ A = LT

instance Eq AutoClass where 
  (==) A A = True
  (==) B B = True
  (==) C C = True
  (==) D D = True
  (==) E E = True
  (==) F F = True
  (==) _ _ = False

instance Show AutoClass where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"
    show F = "F"

addTravelDistance :: (Ord d, Fractional d) =>
  (((a, d, d, d, e, f) -> ((a, d, d, d, e, f) -> t1) -> t1) -> t2)
  -> d -> t2
addTravelDistance sAuto addedDistance = sAuto(\(m, fc, ff, td, ms, ac) -> auto(m, 
    let newFuelCunt
          | (addedDistance / ff) > fc = 0 
          | otherwise = (fc - addedDistance / ff)
    in newFuelCunt, ff, 
    let newTravelDistance
          | (addedDistance / ff) > fc = (td + fc * ff)
          | otherwise = (td + addedDistance)
    in newTravelDistance, ms, ac))


travelDistances :: [((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) 
    -> TravelDistance) -> a2] -> [a2]
travelDistances (t:ts) = getAutoTravelDistance t : travelDistances ts

moveMinuteRaceAuto :: (Ord e, Fractional e) => (((a, e, e, e, e, f) -> 
    ((a, e, e, e, e, f) -> t1) -> t1) -> t2, e) -> t2
moveMinuteRaceAuto (sAuto, t) = sAuto(\(m, fc, ff, td, ms, ac) -> auto(m, 
    let newFuelCunt
          | (ms / 60 * t / ff) > fc = 0 
          | otherwise = (fc - ms / 60 * t / ff)
    in newFuelCunt, ff, 
    let newTravelDistance
          | (ms / 60 * t / ff) > fc = (td + fc * ff)
          | otherwise = (td + ms / 60 * t)
    in newTravelDistance, ms, ac))

defineWinner :: (a, TravelDistance) -> (a, TravelDistance) -> (a, TravelDistance)
defineWinner (a1, d1) (a2, d2) = if d1 >= d2 then (a1, d1) else (a2, d2)

winner :: [(a, TravelDistance)] -> (a, TravelDistance)
winner [x] = x
winner (x:xs) = defineWinner x (winner xs)

getNums :: Num t => t -> [a] -> [t]
getNums n [] = []
getNums n (x:xs) = n : getNums (n - 1) xs

getWinner :: (Ord e, Fractional e) => e -> [((a, e, e, e, e, f) -> ((a, e, e, e, e, f) -> t1) -> t1)
      -> ((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> TravelDistance) 
      -> TravelDistance] -> [Char]
getWinner h autos = "Победитель под номером: " ++ show(snd (head (filter (\x -> fst x == maxTd) (zip (travelDistances updatedAutos) 
    (getNums (length updatedAutos - 1) updatedAutos)))))
    where updatedAutos = map moveMinuteRaceAuto (zip autos (replicate (length autos) h))
          maxTd = snd (winner (zip updatedAutos (travelDistances updatedAutos)))

startRace ::
  (Show t, Show a1, Show a2, Show a3, Show a4, Show a5, Ord t,
   Fractional t) =>
  t
  -> [((a6, t, t, t, t, f) -> ((a6, t, t, t, t, f) -> t1) -> t1)
      -> (([Char], a1, a2, a3, a4, a5) -> [Char]) -> [Char]]
  -> [Char]
startRace h autos = modellingMinuteRace h h autos

modellingMinuteRace ::
  (Show t, Show a1, Show a2, Show a3, Show a4, Show a5, Ord t, Fractional t) => t
  -> t -> [((a6, t, t, t, t, f) -> ((a6, t, t, t, t, f) -> t1) -> t1)
    -> (([Char], a1, a2, a3, a4, a5) -> [Char]) -> [Char]]
    -> [Char]
modellingMinuteRace h (-1) autos = "----------------------\n Гонка завершена \n----------------------"
modellingMinuteRace h n autos = "----------------------\n" ++ "Время до конца гонки: " ++ 
    show(n) ++ " минут \n" ++ "------------------------\n" ++ printAutosRace (map moveMinuteRaceAuto 
    (zip autos (replicate (length autos) (h - n)))) ++ 
    "\n" ++ modellingMinuteRace h (n - 1) autos

printAutosRace :: (Show a1, Show a2, Show a3, Show a4, Show a5) =>
  [(([Char], a1, a2, a3, a4, a5) -> [Char]) -> [Char]] -> [Char]
printAutosRace [] = []
printAutosRace (t:ts) = (printAutosRace ts) ++ show(length ts) ++ ") " ++ printAutoInfo t  ++ "\n"

addFuel :: (((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> 
    ((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) -> t1) -> t1) -> t2)
    -> FuelCount -> t2
addFuel sAuto addedFuel = sAuto(\(m, fc, ff, td, ms, ac) -> 
    auto(m, fc + addedFuel, ff, td, ms, ac))

printAutoInfo ::
  (Show a1, Show a2, Show a3, Show a4, Show a5) =>
  ((([Char], a1, a2, a3, a4, a5) -> [Char]) -> t) -> t
printAutoInfo sAuto = sAuto (\(m, fc, ff, td, ms, ac) -> "Model: " ++ m ++ ", FuelCount: " 
    ++ (show fc) ++ ", FuelFlow: " ++ (show ff) ++ ", TravelDistance: " ++ show(td)
    ++ ", MiddleSpeed: " ++ show(ms) ++ ", AutoClass: " ++ show(ac))

getAutoModel :: (((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) 
    -> AutoModel) -> t) -> t
getAutoModel sAuto = sAuto model

getAutoFuelCount :: (((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) 
    -> FuelCount) -> t) -> t
getAutoFuelCount sAuto = sAuto fuelCount

getAutoFuelFlow :: (((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) 
    -> FuelFlow) -> t) -> t
getAutoFuelFlow sAuto = sAuto fuelFlow

getAutoTravelDistance :: (((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) 
    -> TravelDistance) -> t) -> t
getAutoTravelDistance sAuto = sAuto travelDistance

getAutoMiddleSpeed :: (((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) 
    -> MiddleSpeed) -> t) -> t
getAutoMiddleSpeed sAuto = sAuto(\(m, fc, ff, td, ms, ac) -> ms)

getAutoClass :: (((AutoModel, FuelCount, FuelFlow, TravelDistance, MiddleSpeed, AutoClass) 
    -> AutoClass) -> t) -> t
getAutoClass sAuto = sAuto autoClass