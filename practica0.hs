--2)

--a.
valorAbsoluto :: Float -> Float
valorAbsoluto n | n>=0 = n
                | otherwise = -n

--b.
bisiesto :: Int -> Bool
bisiesto n | mod n 400 ==0 =True
    | mod n 4 ==0 && (mod n 100 /= 0) = True
    | otherwise = False

--c.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

--d.
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 1 = 0
cantDivisoresPrimos n = cantDivisoresPrimosAux n 2


esPrimo:: Int->Int->Bool
esPrimo 1 _ = False
esPrimo n q | n == q = True
            | n>=2 && mod n q /=0 = esPrimo n (q+1)
            | otherwise = False

esDivisor::Int->Int->Bool
esDivisor n m = mod n m == 0 

cantDivisoresPrimosAux::Int->Int->Int
cantDivisoresPrimosAux 1 _ = 0
cantDivisoresPrimosAux n m 
    | m > n                 = 0
    |esDivisor n m && esPrimo m 2 = 1+ cantDivisoresPrimosAux n (m+1)
    | otherwise = cantDivisoresPrimosAux n (m+1)

--3)
--data Maybe a = Nothing | Just a
--data Either a b = Left a | Right b

--safeDiv :: Int -> Int -> Maybe Int
--safeDiv = _

--a.
inverso :: Float -> Maybe Float
inverso n  = if n/=0 then Just (1/n) else Nothing

--b.
aEntero :: Either Int Bool -> Int
aEntero (Left a) = a 
aEntero (Right b) = if b then 1 else 0

--4)

--a. que elimina todas las apariciones de cualquier carácter de la primera cadena en la segunda
limpiar :: String -> String -> String
limpiar [] xs = xs
limpiar xs [] = []
limpiar (x:xs) ys = limpiar xs (filter (/=x) ys) 

--b.que dada una lista de números devuelve la diferencia de cada uno con el promedio general
longitud::[Float]->Int
longitud [] =0
longitud (x:xs) = 1 + longitud xs

suma::[Float]->Float
suma [] =0
suma (x:xs) = x + suma xs

promedio:: [Float] -> Float
promedio xs = suma xs / fromIntegral (longitud xs)

difPromedio:: [Float] -> [Float] 
difPromedio xs = map (\x -> x - promedio xs) xs

--c.
todosIguales :: [Int] -> Bool 
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) | x==y = todosIguales (x:xs)
                        | otherwise= False

--5)
data AB a = Nil | Bin (AB a) a (AB a)

--a.
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

--b.
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin hi a hd) = Bin (negacionAB hi) (not a) (negacionAB hd)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin hi nodo hd) = (productoAB hi) * nodo * (productoAB hd)
