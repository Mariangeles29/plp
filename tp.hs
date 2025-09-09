--actualizarElem :: Int -> (a -> a) -> [a] -> [a]
--actualizarElem n f xs = take n xs ++ foldr  ++ drop (n+1) xs 

actualizarElem ::  Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = zipWith(\x y -> if x == n then f y else y) [0,1..] xs

elemALista::[a]->a
elemALista [x]=x

a=[1,2,3,4,5,6]
f=(\x -> x-1)

data Histograma = Histograma Float Float [ Int ] deriving ( Show , Eq )

--El primer Float es el inicio del intervalo de la segunda casilla.

--El segundo Float es el tama˜no del intervalo de cada casillero, que debe ser mayor que 0.

--La lista de enteros representa la cuenta de cu´antos elementos caen en el intervalo de cada
--casillero. Tiene al menos 3 elementos

vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l ((u-l)/fromIntegral n) (replicate (n+1) 0)

agregar :: Float -> Histograma -> Histograma
agregar f (Histograma inicio tamaño_intervalo cs) = case indice f inicio tamaño_intervalo of 
                                    i | i< 0 -> Histograma inicio tamaño_intervalo (actualizarElem 0 (+1) cs)
                                        | i >= length cs -> Histograma inicio tamaño_intervalo (actualizarElem (length cs - 1) (+1) cs)
                                        | otherwise -> Histograma inicio tamaño_intervalo (actualizarElem i (+1) cs)
    

indice:: Float-> Float ->Float ->Int
indice f inicio tamaño_intervalo = (floor ((f-inicio)/tamaño_intervalo)) + 1
h = Histograma 10.0 5.0 [3,7,2,0,4]

