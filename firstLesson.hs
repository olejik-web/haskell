nums = [1, 2, 3, 4, 5, 6, 7, 8, 9]
add = 10:20:nums
tripleNum x = x + x + x
tripleNums x y = tripleNum x + tripleNum y
dIfSmall x = if x < 50 then x * 2 else x - 20
dIfSmall' x = (if x < 50 then x * 2 else x - 20) + 100
myMin x y = if x < y then x else y