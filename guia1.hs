
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
masmas = foldr (\x rec-> (\ys -
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

-- 17.

data AB a = Nil | Bin (AB a) a (AB a)


-- I) 

-- ¿por que Nil en foldAB es de tipo b y no (AB a -> b), o (a->b) o (b -> b)?
--     ~> la funcion de reemplazo pide exactamente la misma # de datos que 
--     guarda el constructor original. si miro el arbol, para construir un nodo 
--     uso Bin AB a a AB a. Este constructor necesita 3 datos para existir. Por 
--     eso, su funcion de reempplazo en el fold pide 3 args (2 recursivos y uno simple).
--     Para construir un vacio uso Nil, este necesita 0 datos para existir.
--     Nil no guarda datos. 

foldAB::b -> (b->a->b->b) -> AB a -> b
foldAB cNil _ Nil = cNil
foldAB cNil cAB (Bin i r d) = cAB (rec i) r (rec d)
    where rec = foldAB cNil cAB  

-- recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
-- recr _ z [] = z
-- recr f z (x : xs) = f x xs (recr f z xs)
recrAB::b->(AB a -> a -> AB a -> b -> b -> b) -> (AB a)-> b
recrAB cNil _ Nil = cNil
recrAB cNil cAB (Bin i r d) = cAB i r d (rec i) (rec d)
    where rec = recrAB cNil cAB 

-- II)

esNil::(AB a) -> Bool
esNil Nil = True
esNil (Bin _ _ _) = False

altura::(AB a) -> Int
altura = foldAB (0) (\altura_i _ altura_d -> 1 + (max altura_i altura_d))

cantNodos::(AB a)->Int 
cantNodos = foldAB (0) (\cNodosI _ cNodosD -> 1 + cNodosI + cNodosD)


-- III)


-- mejorSegun::(a->a->Bool)-> [a] -> a
-- mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec) 

--- ?) no entiendo del todo como se ataja el caso nil

mejorSegunAB::(a->a->Bool)-> AB a -> a
mejorSegunAB f (Bin i r d) = foldAB r (\ri r rd -> case (f ri rd) of
    True -> if (f r ri) then r else ri
    False -> if (f r rd) then r else rd) (Bin i r d)

-- IV) 
-- Recordar que, en un árbol binario de búsqueda, el valor de un nodo es mayor o igual que los valores que
-- aparecen en el subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol
-- derecho.

-- recrAB::b->(AB a -> a -> AB a -> b -> b -> b) -> (AB a)-> b
-- recrAB cNil _ Nil = cNil
-- recrAB cNil cAB (Bin i r d) = cAB i r d (rec i) (rec d)
--     where rec = recrAB cNil cAB 

esABB::(Ord a)=> AB a -> Bool -- (?) ¿como se que i y d estan ordenados? ¿como maneja a Nil mejorSegunAB?
esABB = recrAB (True) (\i r d ri rd -> 
    ri && rd && ((esNil i) || ((mejorSegunAB (>) i) <= r)) && ((esNil d ) || ((mejorSegunAB (<) d)> r )))
-- condiciones-> 1. ri es ABB ( o sea ri = True)
-- 2. rd es ABB 
-- 3. i es nil o el mayor elemento del subarbol i es menor o igual a la raiz
-- 4. d es nil o el menor elemento del subarbol d es mayor que la raiz

-- 18.

-- I)

ramas::(AB a ) -> [a] -- (?)
ramas = foldAB ([]) (\lista_Izq r lista_Der -> lista_Izq ++ [r] ++lista_Der) 

cantHojas::(AB a) -> Int -- ?)
cantHojas = recrAB (0) (\ i r d ri rd -> case (esNil i) of
    True -> if (esNil d) then 1 else rd
    False -> if (esNil d) then ri else ri+rd)

espejo::(AB a)-> (AB a)
espejo = foldAB (Nil) (\ri r rd -> Bin rd r ri)


-- 1. El árbol más básico (vacío)
arbolVacio :: AB Int
arbolVacio = Nil

-- 2. Una sola hoja
arbolHoja :: AB Int
arbolHoja = Bin Nil 10 Nil

-- 3. Un árbol de números balanceado
--       5
--      / \
--     2   8
arbolB :: AB Int
arbolB = Bin (Bin Nil 2 Nil) 5 (Bin Nil 8 Nil)

arbolDB :: AB Int
arbolDB = Bin (Bin (Bin Nil 3 Nil) 5 Nil) 10 (Bin Nil 15 Nil)
-- True
abb1 :: AB Int
abb1 = Bin (Bin (Bin Nil 2 Nil) 3 (Bin Nil 4 Nil)) 5 (Bin Nil 8 (Bin Nil 9 Nil))

abb2 :: AB Int
abb2 = Bin (Bin Nil 5 Nil) 10 Nil

abb8 :: AB Int
abb8 = Bin (Bin Nil 5 Nil) 10 Nil

abb3 :: AB Int
abb3 = Nil

--false

noAbb1 :: AB Int
noAbb1 = Bin (Bin Nil 6 Nil) 5 Nil

noAbb2 :: AB Int
noAbb2 = Bin (Bin Nil 3 (Bin Nil 6 Nil)) 5 (Bin Nil 8 Nil)

noAbb3 :: AB Int
noAbb3 = Bin Nil 10 (Bin Nil 8 Nil)
