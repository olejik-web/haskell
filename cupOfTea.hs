normalBattle n firstTank secondTank = let firstModel = getTankModel firstTank
                                          firstAttack = getTankAttack firstTank
                                          firstHealth = 2 
                                          secondModel = getTankModel firstTank
                                          secondAttack = getTankAttack firstTank
                                          secondHealth = 2 
                                      in battle n firstModel firstAttack 3 "" 3 3

battle 0 firstTankModel firstTankAttack firstTankHealth secondTankModel secondTankAttack secondTankHealth = winnerTankModel
    where winnerTankModel
            | secondTankHealth < firstTankHealth = "Winner: " ++ firstTankModel
            | secondTankHealth > firstTankHealth = "Winner: " ++ secondTankModel
            | otherwise = "NothingWin"
battle n firstTankModel firstTankAttack firstTankHealth secondTankModel secondTankAttack secondTankHealth = winnerTankModel
    where roundSecondTankHealth = secondTankHealth - firstTankAttack
          roundFirstTankHealth
            | roundSecondTankHealth > 0 = firstTankHealth - secondTankAttack
            | otherwise = firstTankHealth
          winnerTankModel
            | roundFirstTankHealth <= 0 && roundSecondTankHealth > 0 = "Winner: " ++ secondTankModel
            | roundFirstTankHealth > 0 && roundSecondTankHealth <= 0 = "Winner: " ++ firstTankModel
            | roundFirstTankHealth > 0 && roundSecondTankHealth > 0 = battle (n - 1) firstTankModel firstTankAttack roundFirstTankHealth secondTankModel secondTankAttack roundSecondTankHealth
            | otherwise = "NothingWin"

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs

tankHealths [] = []
tankHealths (t:ts) = (show (getTankHealth t)) ++ " " ++ (tankHealths ts)

fight aTank dTank = let attack
                          | hpA > 0 = getTankAttack aTank 
                          | otherwise = 0
                            where  hpA = getTankHealth aTank
                    in makeTankDamage dTank attack

makeTankDamage sTank dmg = sTank (\(m, a, h) -> tank (m, a, 
    let newHp 
          | dmg <= 0 = h
          | dmg >= h = 0
          | otherwise = h - dmg
    in newHp))

printTankInfo sTank = sTank (\(m, a, h) -> "Model: " ++ m ++ ", Attack: " 
    ++ (show a) ++ ", Health: " ++ (show h))

setTankHealth sTank nHp = sTank (\(m, a, _) -> tank (m, a, nHp))
setTankModel sTank nM = sTank (\(_, a, hp) -> tank (nM, a, hp))
setTankAttack sTank nA = sTank (\(m, _, hp) -> tank (m, nA, hp))

tank (model, attack, hp) = \msg -> msg (model, attack, hp)
model (m, _, _) = m
attack (_, a, _) = a
health (_, _, hp) = hp
getTankModel sTank = sTank model
getTankAttack sTank = sTank attack
getTankHealth sTank = sTank health


cup ml = \msg -> msg ml

getMl sCup = sCup (\ml -> ml)
makeSipFromCup sCup mlDrank = cup newMl
    where ml = getMl sCup
          newMl
            | mlDrank >= ml = 0
            | mlDrank < 0 = ml
            | otherwise = ml - mlDrank