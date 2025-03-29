auto (model, fuelCount, fuelFlow, travelDistance) = 
    \msg -> msg (model, fuelCount, fuelFlow, travelDistance)
model (m, _, _, _) = m
fuelCount (_, fc, _, _) = fc
fuelFlow (_, _, ff, _) = ff
travelDistance (_, _, _, td) = td

addTravelDistance sAuto addedDistance = sAuto(\(m, fc, ff, td) -> auto(m, 
    let newFuelCunt
          | (addedDistance / ff) > fc = 0 
          | otherwise = (fc - addedDistance / ff)
    in newFuelCunt, ff, 
    let newTravelDistance
          | (addedDistance / ff) > fc = (td + fc * ff)
          | otherwise = (td + addedDistance)
    in newTravelDistance
    ))

addFuel sAuto addedFuel = sAuto(\(m, fc, ff, td) -> 
    auto(m, fc + addedFuel, ff, td))

printAutoInfo sAuto = sAuto (\(m, fc, ff, td) -> "Model: " ++ m ++ ", FuelCount: " 
    ++ (show fc) ++ ", FuelFlow: " ++ (show ff) ++ ", TravelDistance: " ++ show(td))

getAutoModel sAuto = sAuto model
getAutoFuelCount sAuto = sAuto fuelCount
getAutoFuelFlow sAuto = sAuto fuelFlow
getAutoTravelDistance sAuto = sAuto travelDistance