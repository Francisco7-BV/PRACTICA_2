--3.1. Longitud de una lista.:
--La funcion longitud recibe una lista de cualquier tipo y devuelve un entero que significa el
--tamanio de la lista, es decir cuantos elementos tiene.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs) 

--3.2. Suma de n numeros:
--La funcion sumaLista recibe una lista de tipo numerico y devuelve un entero que es la suma
--total de los elementos de la lista.

sumaLista :: (Num a) => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

--3.3 Agregar elemento a una lista:
--La funcion agregaElemento recibe una lista de cualquier tipo, un elemento de cualquier tipo y
--un valor booleano, donde si el valor booleano es True inserta al principio de la lista (para este
--caso deben utilizar el operador :) y si es False inserta al final de la lista (para este caso deben
--utilizar el operador ++). La función devuelve la lista con un nuevo elemento ya sea al principio
--o al final.

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento (x:xs) a True = if True == True
                            then a:(x:xs) 
                            else (x:xs) ++ [a]


--3.4. Maximo de una lista:
--La funcion maximoLista recibe una lista de tipo numerico y devuelve el máximo de esta lista,
--es decir, el elemento con mayor valor.

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs) = if x > maximoLista xs 
                    then x
                    else maximoLista xs

--3.5. Recuperar un elemento de una lista de acuerdo a su indice:
--La funcion indice recibe una lista de cualquier tipo y un entero que representa un indice valido
--dentro de la lista. La funcion devuelve el elemento del indice especificado en la lista.

indice :: [a] -> Int -> a
indice [] n = error "no hay nada en la lista"
indice (x:xs) 0 = x
indice (x:xs) n = if n >= longitud (x:xs) || n<0
                    then error "índice fuera del rango"
                    else indice (xs) (n-1)

--Ademas tendran que verificar que el entero que representa el indice, este dentro del rango siguiente:
--0 ≤ indice < longitud(lista) − 1

--4.1. Divisores de un numero entero.:
--La funcion divisores recibe un numero n de tipo entero y devuelve una lista con los divisores
--de tipo entero de n.

divisores :: Int -> [Int]
divisores x = [div | div <- [1..x], x `mod` div == 0]

--4.2. Convertir una lista en conjunto:
--La funcion conjunto recibe una lista de cualquier tipo y devuelve una lista sin elementos repeti-
--dos a partir de la lista original.

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs , y /= x ] 


--4.3. Obtener los numeros pares de una lista:
--La funcion numerosPares recibe una lista de tipo numerico, y devuelve una lista  ́unicamente
--con los numeros que sean pares de la lista original.

numerosPares :: [Int] -> [Int]
numerosPares xs = [x | x <- xs, x `mod` 2 ==0]
