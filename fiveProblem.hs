class Describable a where
    describe :: a -> String

type AutoModel = String
type FuelCount = Double
type FuelFlow = Double
type TravelDistance = Double
type MiddleSpeed = Double

data AutoClass = A | B | C | D | E | F 

data AutoNote = AutoNote {
    model :: AutoModel,
    fuelCount :: FuelCount,
    fuelFlow :: FuelFlow,
    travelDistance :: TravelDistance,
    middleSpeed :: MiddleSpeed,
    autoClass :: AutoClass
}

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

autoBMW = AutoNote {
    model = "BMW", 
    fuelCount = 100, 
    fuelFlow = 25, 
    travelDistance = 0, 
    middleSpeed = 40,
    autoClass = F
}

autoLada = AutoNote {
    model = "Lada", 
    fuelCount = 100, 
    fuelFlow = 1, 
    travelDistance = 100, 
    middleSpeed = 20,
    autoClass = A
}

autoKIA = AutoNote {
    model = "KIA", 
    fuelCount = 100, 
    fuelFlow = 30, 
    travelDistance = 0, 
    middleSpeed = 60,
    autoClass = B
}

autoBugatti = AutoNote {
    model = "Bugatti", 
    fuelCount = 100, 
    fuelFlow = 2, 
    travelDistance = 20, 
    middleSpeed = 50,
    autoClass = F
}

myAutos = [autoLada, autoBugatti, autoBMW, autoKIA]

addTravelDistance :: AutoNote -> TravelDistance -> AutoNote
addTravelDistance sAuto addedDistance = AutoNote {
    model = model sAuto,
    fuelCount = let newFuelCunt
                      | (addedDistance / (fuelFlow sAuto)) > (fuelCount sAuto) = 0 
                      | otherwise = ((fuelCount sAuto) - addedDistance / (fuelFlow sAuto))
                in newFuelCunt,
    fuelFlow = fuelFlow sAuto,
    travelDistance = let newTravelDistance
                           | (addedDistance / (fuelFlow sAuto)) > (fuelCount sAuto) 
                            = ((travelDistance sAuto) + (fuelCount sAuto) * (fuelFlow sAuto))
                           | otherwise = ((travelDistance sAuto) + addedDistance)
                     in newTravelDistance,
    middleSpeed = middleSpeed sAuto,
    autoClass = autoClass sAuto
}

printAutoInfo :: AutoNote -> String
printAutoInfo sAuto = "Model: " ++ model sAuto ++ ", FuelCount: " 
    ++ show (fuelCount sAuto) ++ ", FuelFlow: " ++ show (fuelFlow sAuto) 
    ++ ", TravelDistance: " ++ show (travelDistance sAuto) ++ ", MiddleSpeed: " ++ 
    show(middleSpeed sAuto) ++ ", AutoClass: " ++ show(autoClass sAuto)

addFuel :: AutoNote -> FuelCount -> AutoNote
addFuel sAuto addedFuel = AutoNote {
    model = model sAuto, 
    fuelCount = (fuelCount sAuto) + addedFuel, 
    fuelFlow = fuelFlow sAuto, 
    travelDistance = travelDistance sAuto, 
    middleSpeed = middleSpeed sAuto,
    autoClass = autoClass sAuto
}

printAutosRace :: [AutoNote] -> String
printAutosRace [] = []
printAutosRace (t:ts) = (printAutosRace ts) ++ show(length ts) ++ ") " ++ printAutoInfo t  ++ "\n"

moveMinuteRaceAuto :: (AutoNote, Double) -> AutoNote
moveMinuteRaceAuto (sAuto, t) = AutoNote {
    model = model sAuto,
    fuelCount = let newFuelCunt
                      | ((middleSpeed sAuto) / 60 * t / (fuelFlow sAuto)) 
                            > (fuelCount sAuto) = 0 
                      | otherwise = ((fuelCount sAuto) - (middleSpeed sAuto) 
                            / 60 * t / (fuelFlow sAuto))
                in newFuelCunt,
    fuelFlow = fuelFlow sAuto,
    travelDistance = let newTravelDistance
                           | ((middleSpeed sAuto) / 60 * t / (fuelFlow sAuto) 
                                / (fuelFlow sAuto)) > (fuelCount sAuto) 
                            = ((travelDistance sAuto) + (fuelCount sAuto) * (fuelFlow sAuto))
                           | otherwise = ((travelDistance sAuto) + (middleSpeed sAuto) 
                                / 60 * t)
                     in newTravelDistance,
    middleSpeed = middleSpeed sAuto,
    autoClass = autoClass sAuto
}

modellingMinuteRace :: Double -> Double -> [AutoNote] -> String
modellingMinuteRace h (-1) autos = "----------------------\n Гонка завершена \n----------------------"
modellingMinuteRace h n autos = "----------------------\n" ++ "Время до конца гонки: " ++ 
    show(n) ++ " минут \n" ++ "------------------------\n" ++ printAutosRace (map moveMinuteRaceAuto 
    (zip autos (replicate (length autos) (h - n)))) ++ 
    "\n" ++ modellingMinuteRace h (n - 1) autos

startRace :: Double -> [AutoNote] -> String
startRace h autos = modellingMinuteRace h h autos

getWinner :: Double -> [AutoNote] -> String
getWinner h autos = "Победитель под номером: " ++ show(snd (head (filter (\x -> fst x == maxTd) (zip (travelDistances updatedAutos) 
    (getNums (length updatedAutos - 1) updatedAutos)))))
    where updatedAutos = map moveMinuteRaceAuto (zip autos (replicate (length autos) h))
          maxTd = snd (winner (zip updatedAutos (travelDistances updatedAutos)))

getNums :: Int -> [AutoNote] -> [Int]
getNums n [] = []
getNums n (x:xs) = n : getNums (n - 1) xs

winner :: [(AutoNote, TravelDistance)] -> (AutoNote, TravelDistance)
winner [x] = x
winner (x:xs) = defineWinner x (winner xs)

defineWinner :: (AutoNote, TravelDistance) -> (AutoNote, TravelDistance) 
    -> (AutoNote, TravelDistance)
defineWinner (a1, d1) (a2, d2) = if d1 >= d2 then (a1, d1) else (a2, d2)

travelDistances :: [AutoNote] -> [TravelDistance]
travelDistances (t:ts) = travelDistance t : travelDistances ts