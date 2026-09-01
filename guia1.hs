
-- (2)

currymp::((a,b)->c)->a->b->c
currymp f x y = f (x,y)

uncurrymp::(a->b->c)->(a,b)->c
uncurrymp f (x,y) = f x y

-- (4)

-- ------------------ GENERACION INFINITA ------------------

-- gracias a la evaluacion lazy de haskell se facilita trabajar con generacion
-- infinita, ya que los valores se calculan solo cuando los necesito

-- sintaxis de listas: [expresion | generador, condicion]

-- si intento simplemente hacer [(x,y)| x <- [0..], y <- [0..]]
-- la x se queda atrapada en el 0 mientras que y crece al infinito,
-- asi que nunca se generaria el par (1,0)

-- idea ~> genero una lista de 0 a y para la coordenada x, luego 
-- y crece al infinito ,

paresDeNat::[(Int,Int)]
paresDeNat = [(x,y-x) | y <- [0 .. ], x <-[0 .. y]]

-- (5)

pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) |  c <- [1..], b <-[1.. c], a <- [1.. b],  a^2 + b^2 == c^2]


-- (8)

-- I)

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter [] = []
-- filter p (x:xs) =
--      if p x
--      then x : filter p xs
--      else filter p xs

-- map :: (a -> b) -> [a] -> [b]
-- map [] = []
-- map f (x:xs) = f x : map f xs

-- a. 

menoresACinco::[String]->[String]
menoresACinco = filter (\x -> length x <=5)

-- b.

notasAprobadas::[Int]->[Bool]
notasAprobadas = map (\x -> x>6 )

-- c.

paresAlCuadrado::[Int]->[Int]
paresAlCuadrado = map (\y -> y^2 ). (filter (\x -> mod x 2 == 0))

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

-- V)

sumaAlt::[Int]->Int
sumaAlt = foldr (\x rec -> x - rec) 0

-- VI)

sumaAltRev::[Int]->Int
sumaAltRev = foldl (\ac x -> x - ac) 0

-- VII)

componerTodas::[a->a]->a->a
componerTodas = foldr (\f rec -> \n -> f (rec n)) id

-- 9.

-- I)
permutaciones::[a]->[[a]]
permutaciones = foldr (\x rec -> concatMap (\l -> (map (\i-> (take i l) ++ [x] ++ ( drop i l)) [0 .. length l])) rec ) [[]]

-- !) ESTRATEGIA ~> yo tengo al primer elemento (x) y la lista de permutaciones ya armada (rec) sin ese primer elemento x,
--     quiero poder añadir a x en todos los indices posibles de mi lista de permutaciones rec.
--     defino el caso base para [] como [[]]. Sé que rec es de tipo [[a]], por ej/:
--     x:xs = [1,2,3] rec = [[2,3],[3,2]], yo lo que quiero hacer es tomar cada lista y 
--     agregar el 1 en todas las posiciones. De nuevo, como rec = [[a]] me enfoco solo en una lista de rec /:
--     si tengo [a] yo quiero poner a x en su iesima posicion;
--     ¿Como lo hago ? -> (take i l) ++ [x] + (drop i l).
-- Ahora ya tengo a x en una posicion i de UNA lista l. Si yo quiero insertar x en todas las posiciones de
-- la lista l tengo que recorrerla. El rango lo consigno con [0 .. length l]. Si yo quiero aplicar mi 
-- funcion (\i-> (take i l) ++ [x] ++ ( drop i l)) a todos los posibles indices de l [0 .. length l],
-- puedo usar map para que mapee esa funcion (recibo un numero de la lista de indices), y la 
-- concatene con el resto de indices que luego seran listas /:
-- map (\i-> (take i l) ++ [x] ++ ( drop i l)) [0 .. length l].
-- Si uso solo map para aplicar la funcion a cada elemento de rec
-- obtengo una lista de listas [[[a]]]. para aplicar la funcion a cada elto de rec
-- (tomo [a] y devuelvo [[a]]) y al mismo tiempo combinar todos los res en una sola lista [[a]],
-- puedo usar concatMap 

-- Conclusion: 
-- si ya tengo las permutaciones calculadas para el resto (res)
-- de la lista, solo bastaria con insertar x en todos los huecos
-- posibles de cada una de esas parmutaciones previas.
-- map (\i -> ...) se encarga de la insercion, toma una lista y genera
-- todas las versiones de esa lista con x en todas las posiciones posibles.
-- concatMap toma todas las listas que estan en rec, les aplcia el paso anterior
-- a cada lista y une los resultados para que quede una lista de listas.
-- foldr me ayuda a recorrer la lista original de der a izq, repitiendo esto
-- para cada elto.


-- II)

--partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]

partes::[a]->[[a]]
partes = foldr (\x rec -> rec ++ (map (x:) rec)) [[]]

-- III)

prefijos:: [a]->[[a]]
prefijos = foldr (\x rec -> [[]] ++ (map (x:) rec)) [[]] -- (?)

sufijos::[a]->[[a]]
sufijos = foldr (\x rec -> (map (x:) (take 1 rec)) ++ rec ) [[]]

-- IV)

sublistas::[a]->[[a]]
sublistas xs = concatMap prefijos (sufijos xs)

-- 10.

-- a)

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna::Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if x==e then xs else x:rec) []

-- c)

insertarOrdenado::(Ord a)=>a->[a]->[a]
insertarOrdenado e = recr (\x xs rec -> if e<=x then (e:(x:xs)) else x:rec) []

-- 12.

-- I)

mapPares:: (a->b->c)->[(a,b)]->[c]
mapPares f = foldr (\x rec -> (uncurry f x):rec) []

-- II)

armarPares::(Eq b) => [a]->[b]-> [(a,b)]
armarPares = foldr (\x rec -> \ys -> if (ys==[]) then [] else (x,head ys):rec(tail ys)) (const [])

-- III)

mapDoble::(a->b->c)->[a]->[b]->[c]
mapDoble f = foldr (\x rec -> \ys -> (f x (head ys)):(rec (tail ys))) (const []) 


-- ------------ OTRAS ESTRUCTURAS DE DATOS --------------

-- 14.

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

data Nat = Zero | Succ Nat
   -- CB: 0 -> (succ -> b)
foldNat::(b->b)->b->Integer->b
foldNat cSucc cZero 0 = cZero
foldNat cSucc cZero n =  cSucc (foldNat cSucc cZero (n-1))

potencia::Integer-> Integer -> Integer
potencia n = foldNat ((*) n) (1)



