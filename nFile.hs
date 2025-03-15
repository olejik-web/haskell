sumNum2 x y = (x + y) * 2
lastNum x y = x
leftReduce f a [] = a
leftReduce f a (x:xs) = (leftReduce f (f a x) xs)

rightReduce f a [] = a
rightReduce f a (x:xs) = (f (rightReduce f a xs) x)