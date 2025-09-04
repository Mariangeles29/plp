

--2)

--I)
curry::((a, b)->c)->a->b->c
curry f x y = f (x, y)

--II)
uncurry::(a->b->c)->(a, b)->c
uncurry f (x, y) = f x y

--3)

--I)
sumFoldr::[Float]->Float
sumFoldr = foldr (+) 0

elemFoldr::Int->[Int]->Bool
elemFoldr n  = foldr ((||) . (\x -> x==n) ) False 

masmas::[Int]->[Int]->[Int]
masmas xs ys= foldr (:) ys xs

filterFoldr::(a -> Bool)->[a]->[a]
filterFoldr f = foldr (\x rec -> if f x then x:rec else rec ) []

mapFoldr::(a -> b) -> [a] -> [b]
mapFoldr f = foldr ((:) . (\x -> f x)) []



