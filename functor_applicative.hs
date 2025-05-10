from Data.Map import Map

type Html = String
type LatLong = (Double, Double)

data RobotPart = RobotPart
{
    name :: String,
    description :: String,
    cost :: Double,
    count :: Int,
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart
{
    name = "Left Arm",
    description = "Biological arm",
    cost = 1000,
    count = 3
} 

rightArm :: RobotPart
rightArm = RobotPart
{
    name = "Right Arm",
    description = "Mechanic",
    cost = 1000,
    count = 5
}

robotHead :: RobotPart
robotHead = RobotPart
{
    name = "Robot Head",
    description = "Madness head",
    cost = 5000,
    count = 2
}

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error: City not found"
printDistance (Just dist) = putStrLn (show dist ++ "km")

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>", partName, "</h2>",
                            "<p><h3>desc</h3> ", partDesc,
                            "</p><p><h3>cost</h3> ", partCost,
                            "</p><p><h3>count</h3> ", partCount,
                            "</p>"]
    where partName = name part
          partDesc = description part
          partCost = cost part
          partCount = count part

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys = [1, 2, 3]
        vals = [leftArm, rightArm, robotHead]
        keyVals = zip keys vals

insertSnippet :: Maybe Html -> IO ()

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

succesfulRequest :: Maybe Int
succesfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Moscow", (55.7558, 37.6173)),
                          ("Saint Petersburg", (59.9343, 30.3351)),
                          ("Novosibirsk", (55.0415, 82.9346)),
                          ("Yekaterinburg", (56.8389, 60.6057)),
                          ("Kazan", (55.7963, 49.1088)),
                          ("Yaroslavl", (57.6265, 39.8937))
                          ]
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

latLongToRads :: LAtLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
    where (rlat1, rlong1) = latLongToRads coords1
          (rlat2, rlong2) = latLongToRads coords2
          dlat = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = (sin (dlat/2)^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2)
          c = 2 * atan2 (sqrt a) (sqrt (1 - a))
          earthRadius = 6378.1