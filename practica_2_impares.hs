--3.1. Longitud de una lista.:
--La función longitud recibe una lista de cualquier tipo y devuelve un entero que significa el
--tamaño de la lista, es decir cuantos elementos tiene.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs)=1+longitud(xs) 

--3.3 Agregar elemento a una lista:
--La función agregaElemento recibe una lista de cualquier tipo, un elemento de cualquier tipo y
--un valor booleano, donde si el valor booleano es True inserta al principio de la lista (para este
--caso deben utilizar el operador :) y si es False inserta al final de la lista (para este caso deben
--utilizar el operador ++). La función devuelve la lista con un nuevo elemento ya sea al principio
--o al final.

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] a False = [a]
agregaElemento [] a True = [a]
agregaElemento (x:xs) a True = [a] ++ (x:xs) 
agregaElemento (x:xs) a False = [x] ++ agregaElemento xs a False

--3.5. Recuperar un elemento de una lista de acuerdo a su índice:
--La función indice recibe una lista de cualquier tipo y un entero que representa un índice válido
--dentro de la lista. La función devuelve el elemento del índice especificado en la lista.

indice :: [a] -> Int -> a
indice (x:xs) 0 = x
indice (x:xs) n = if n >= longitud (x:xs) || n<0
                    then error "índice fuera del rango"
                    else indice (xs) (n-1)

--Además tendrán que verificar que el entero que representa el índice, esté dentro del rango siguiente:

--0 ≤ indice < longitud(lista) − 1



--4.2. Convertir una lista en conjunto:

--La función conjunto recibe una lista de cualquier tipo y devuelve una lista sin elementos repeti-
--dos a partir de la lista original.

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs , y /= x ] 