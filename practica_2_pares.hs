--3.2. Suma de n numeros:
--La funcion sumaLista recibe una lista de tipo numerico y devuelve un entero que es la suma
--total de los elementos de la lista.

sumaLista :: (Num a) => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs


--3.4. Maximo de una lista:
--La funcion maximoLista recibe una lista de tipo numerico y devuelve el máximo de esta lista,
--es decir, el elemento con mayor valor.

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:y:xs) = maximoLista (if x >= y then x : xs else y : xs)


--4.1. Divisores de un numero entero.:
--La funcion divisores recibe un numero n de tipo entero y devuelve una lista con los divisores
--de tipo entero de n.

divisores :: Int -> [Int]
divisores x = [div | div <- [1..x], x `mod` div == 0]


--4.3. Obtener los numeros pares de una lista:
--La funcion numerosPares recibe una lista de tipo numerico, y devuelve una lista  ́unicamente
--con los numeros que sean pares de la lista original.

numerosPares :: [Int] -> [Int]
numerosPares [] = []
numerosPares xs = [x | x <- xs, x `mod` 2 ==0]
