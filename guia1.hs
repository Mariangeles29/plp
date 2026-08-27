paresDeNat::[(Int,Int)]
paresDeNat = [(x,y-x) | y <- [0..], x <- [0.. y]]

-- 7.
listasQueSuman::Int->[[Int]]
listasQueSuman 1 =[[1]] 
listasQueSuman n = [ k : resto | k <- [1 .. n], resto <-listasQueSuman (n-k)]

-- 8.

-- II).

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- a.
sumF::[Int]->Int
sumF = foldr (+) 0

-- b.
pertenece::Int->[Int]->Bool
pertenece e = foldr (\x rec-> x==e || rec) False

-- c.
masmas::[Int]->[Int]->[Int]
masmas = foldr (\x rec-> (\ys -> x : rec ys)) (\ys -> ys)

-- masmas xs [] = xs
-- masmas (x:xs) ys = x : masmas xs ys 

-- d.
filterF::(a->Bool)->[a]->[a]
filterF f = foldr (\x rec-> if f x then x:rec else rec) []

mapF::(a->b)->[a]->[b]
mapF f = foldr (\x rec -> f x : rec) []

-- III)

mejorSegun::(a->a->Bool)-> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec) 

-- IV)

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f z [] = z
-- foldl f z (x : xs) = foldl f (f z x) xs

sumasParciales::Num a => [a]->[a]
sumasParciales = foldr (\x rec -> x:(map (x+) rec)) []
