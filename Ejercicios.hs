{-
 ejercicios de listas
-}

module Ejercicioslistas where 

producto::Int->Int->Int
producto a b = if b==0 then 0
               else if b==1 then a
               else a + producto a (b-1)

division::Float->Float->Float
division a b = if b==0 then 0
               else if b==1 then a
               else if a==0 then 0
               else 1 + division (a-b) b 

fibonacci::Int->Int
fibonacci a = if a==0 then 0
              else if a==1 then 1
              else fibonacci(a-1)+fibonacci(a-2)

potencia::Int->Int->Int
potencia a b = if b == 0 then 1
	       else if a == 1 then 1
	       else a*potencia a (b-1)

sumDig::Int->Int
sumDig a = if a < 10 then a 
	   else sumaDigitos(div a 10) + mod a 10

digiMay :: Int->Int
digiMay a
 if a < 10 then a
 else if  (mod a 10) > mayorDigito(div a 10) = (mod a 10)
 else otherwise = mayorDigito(div a 10)      


invertir :: Int -> Int
invertir n
 | n <10 = n
 | otherwise = (10^((cantidadDigitos n)-1))*(mod n 10) + invertir (div n 10)
	

palindromo :: Int -> [Char]
palindromo n
 | cantidadDigitos n <= 1 =  "Palindromo"
 | (mod n 10) == (mod (invertir n) 10) = palindromo(div (n - ((10^(cantidadDigitos(n)-1))* (div n (10^((cantidadDigitos n)-1))))) 10)
| otherwise = "No palindromo" 

cantidadDigitos :: Int->Int
cantidadDigitos n 
 | n < 10 = 1 
| otherwise = 1 + (cantidadDigitos (div n 10))