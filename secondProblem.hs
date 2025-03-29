auto (model, fuelCount, fuelFlow, travelDistance, middleSpeed, autoClass) = 
    \msg -> msg (model, fuelCount, fuelFlow, travelDistance, middleSpeed, autoClass)
model (m, _, _, _, _, _) = m
fuelCount (_, fc, _, _, _, _) = fc
fuelFlow (_, _, ff, _, _, _) = ff
travelDistance (_, _, _, td, _, _) = td
middleSpeed (_, _, _, _, ms, _) = ms
autoClass (_, _, _, _, _, ac) = ac

getAutoModel sAuto = sAuto model
getAutoFuelCount sAuto = sAuto fuelCount
getAutoFuelFlow sAuto = sAuto fuelFlow
getAutoTravelDistance sAuto = sAuto travelDistance
getAutoMiddleSpeed sAuto = sAuto middleSpeed
getAutoClass sAuto = sAuto autoClass

data AutoClass = A | B | C | D | E | F

showAutoClass :: AutoClass -> String
showAutoClass A = "A"
showAutoClass B = "B"
showAutoClass C = "C"
showAutoClass D = "D"
showAutoClass E = "E"
showAutoClass F = "F"

addTravelDistance sAuto addedDistance = sAuto(\(m, fc, ff, td, ms, ac) -> auto(m, 
    let newFuelCunt
          | (addedDistance / ff) > fc = 0 
          | otherwise = (fc - addedDistance / ff)
    in newFuelCunt, ff, 
    let newTravelDistance
          | (addedDistance / ff) > fc = (td + fc * ff)
          | otherwise = (td + addedDistance)
    in newTravelDistance, ms, ac
))

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs

travelOneHour [] = []
travelOneHour (t:ts) = (addTravelDistance t (getAutoMiddleSpeed t)) : travelOneHour ts

autosRace [] = []
autosRace (t:ts) = "(" ++ printAutoInfo t ++ ") " ++ (autosRace ts)

addFuel sAuto addedFuel = sAuto(\(m, fc, ff, td, ms, ac) -> 
    auto(m, fc + addedFuel, ff, td, ms, ac))

printAutoInfo sAuto = sAuto (\(m, fc, ff, td, ms, ac) -> "Model: " ++ m ++ ", FuelCount: " 
    ++ (show fc) ++ ", FuelFlow: " ++ (show ff) ++ ", TravelDistance: " ++ show(td)
    ++ ", MiddleSpeed: " ++ show(ms) ++ ", AutoClass: " ++ showAutoClass ac)