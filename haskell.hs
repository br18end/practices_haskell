import Data.Char
import Prelude hiding (max,min)

--DOBLE DE N
doble :: Int -> Int
doble n = 2 * n

--CUADRADO DE N
cuadrado :: Float -> Float
cuadrado n = n * n

--AREA CIRCULO
areacir :: Float -> Float
areacir r = 3.1416 * (cuadrado r)

--COMPUERTA XOR
xorej :: Bool -> Bool -> Bool
xorej x y = (x || y) && not(x && y)

--COMPUERTA NOT
minot :: Bool -> Bool
minot True = False
minot False = True

--COMPUERTA XOR
xorej2 :: Bool -> Bool -> Bool
xorej2 True x = not x
xorej2 False x = x

--p01 DOS BOOLEANOS DIFERENTES
xor :: Bool -> Bool -> Bool
xor x y = (x /= y)

--p04 CONFIGURACION NAND
nand :: Bool -> Bool -> Bool
nand x y = not((x == y) && (x == True))
--nand x y = not(x && y)
--nand x y  True x = not x
--nand x y  False x = True

--p05 TRES IGUALES DA TRUE
iguales3 :: Int -> Int -> Int -> Bool
iguales3 x y z = (x == y) && (y == z)

--p06 TRES IGUALES DA FALSE MYSTERY
mystery :: Int -> Int -> Int -> Bool
mystery x y z = not((x == y) && (y == z))

--p07 TRES DIFERENTES DA TRUE
diferentes3 :: Int -> Int -> Int -> Bool
diferentes3 x y z = (x /= y) && (y /= z) && (z /= x)

--p08 CUATRO IGUALES DA TRUE Y EN LA SEGUNDA OPCION SE REUTILIZA LA DE TRES IGUALES
iguales4 :: Int -> Int -> Int -> Int -> Bool
--teq4 x y z w = (x == y) && (y == z) && (z == w)
iguales4 x y z w = (iguales3 x y z) && (y == w)

--p09 EJERCICIOS
--iguales3 (2+3) 5 (11 `div` 2) >> T
--mystery (2+4) 5 (11 `div` 2) >> T
--diferentes3 (2+4) 5 (11 `div` 2) >> F
--iguales4 (2+3) 5 (11 `div` 2) (21 `mod` 11) >> F

{-
SOBRECARGA
se refiere a los operadores
GUARDS
dar alternativas para dar definiciones a las funciones
-}

--MAX
max :: Int -> Int -> Int
max x y
	| (x >= y) = x
	| otherwise = y

--MAX 3
max3 :: Int -> Int -> Int -> Int
max3 x y z
	| (x >= y) && (x >= z) = x
	| (y > z) = y
	| otherwise = z

{-
CONDICIONES
if condicion then var1 else var2
-}

--MAX IF
maxc :: Int -> Int -> Int
maxc x y = if x >= y then x else y

{-
IMPORT
para redefinir funciones del Prelude
-}

--p10
--max (3-2) (3*8) R = 24
--max3 (4+5) (2*6) (100 `div` 7) R = 14

--p11 MINIMO DE DOS Y DE TRES
mini :: Int -> Int -> Int
mini x y = if x <= y then x else y

min3 :: Int -> Int -> Int -> Int
min3 x y z
	| (x <= y) && (x <= z) = x
	| (y < z) = y
	| otherwise = z

--min3 x y z = if x <= y && x <= z  then x else z

{-
CARACTERES ESPECIALES
tab '\t'
newline '\n'
backslash(\) '\\'
comilla simple(') '\''
comilla doble(")x '\"'

ASCII
el caracter con codigo ascii 34 se escribe:
ejemplo '\34'

import Data.Char
ord :: Char -> Int
chr :: Int -> Char
-}

--RESUELVE LA OPERACION ASCII DE A MENOS ASCII DE a
despla :: Int
despla = ord 'A' - ord 'a'

--REGRESA MAYUSCULAS
enmayus :: Char -> Char
enmayus ch = chr (ord ch + despla)

--ES DIGITO?
isdig :: Char -> Bool
isdig ch = ('0' <= ch) && (ch <= '9')

--ES LETRA?
ischar :: Char -> Bool
ischar ch = ('a' <= ch) && (ch <= 'z')

--p12 REGRESAR MAYUSCULA Y SI ES MAYUSCULA REGRESAR LA MISMA
enmayus2 :: Char -> Char
enmayus2 ch = if ischar ch then enmayus ch else ch

--	| ischar ch = chr ((ord ch) + despla)
--	| otherwise = ch

-- 	= if ('a' <= ch) && (ch <= 'z') then chr ((ord ch) + despla) else ch

--	| ('a' <= ch) && (ch <= 'z') = chr ((ord ch) + despla)
--	| otherwise = ch

--p13 DE CHAR A INT
charint :: Char -> Int
charint ch
	| isdig ch = (ord ch) - 48
	| otherwise = -1

{-
float
raiz
exp
log
trigo

fromInt
ceiling
floor and round

+ - * / == /= < > <= >= f f f
^ f int f
**f f f
-}

--ejemplo FLOAT
--sin (pi / 4) * sqrt 2

--p14 PROMEDIO DE TRES ENTEROS Y CUALES DE ELLOS SON MAYORES QUE EL PROMEDIO
--prom3 :: Float -> Float -> Float -> Float
prom3 :: Integer -> Integer -> Integer -> Float
prom3 x y z = fromInteger (x + y + z)/3

mayorprom :: Integer -> Integer -> Integer -> Integer
mayorprom x y z
	|((prom3 x y z) > fromInteger (x)) && ((prom3 x y z) <= fromInteger (y)) && ((prom3 x y z) <= fromInteger (z)) = 1
	|((prom3 x y z) <= fromInteger (x)) && ((prom3 x y z) > fromInteger (y)) && ((prom3 x y z) <= fromInteger (z)) = 1
	|((prom3 x y z) <= fromInteger (x)) && ((prom3 x y z) <= fromInteger (y)) && ((prom3 x y z) > fromInteger (z)) = 1
		
	|((prom3 x y z) <= fromInteger (x)) && ((prom3 x y z) > fromInteger (y)) && ((prom3 x y z) > fromInteger (z)) = 2
	|((prom3 x y z) > fromInteger (x)) && ((prom3 x y z) <= fromInteger (y)) && ((prom3 x y z) > fromInteger (z)) = 2
	|((prom3 x y z) > fromInteger (x)) && ((prom3 x y z) > fromInteger (y)) && ((prom3 x y z) <= fromInteger (z)) = 2
	|otherwise = 0

--p15 ECUACION CUADRATICA
--a*x^2 + b*x + c = 0.0
nraizsub :: Float -> Float -> Float -> Int
nraizsub a b c
	|b^2 > (4.0 * a * c) = 2
	|b^2 == (4.0 * a * c) = 1
	|b^2 < (4.0 * a * c) = 0

--p16 CUBRE AMBOS CASOS, GENERATIVO Y NO GENERATIVO
nraiz :: Float -> Float -> Float -> Int
nraiz a b c
	| (a /= 0) = nraizsub a b c
	|(a == 0.0) && (b /= 0.0) = 1
	|(a == 0.0) && (b == 0.0) && (c /= 0.0) = 0
	|(a == 0.0) && (b == 0.0) && (c == 0.0) = 3

--p17 RAIZ  CUADRATICA
rsmall :: Float -> Float -> Float -> Float
rsmall a b c
	|(nraiz a b c == 1) = (-c/b)
	|(nraiz a b c == 3) = 0.0
	|(nraiz a b c > 1) = (-(b) - sqrt(b^2-4.0*a*c)) / (2.0*a)
	|otherwise = 0

rlarge :: Float -> Float -> Float -> Float
rlarge a b c
	|(nraiz a b c == 1) = (-c/b)
	|(nraiz a b c == 3) = 0.0
	|(nraiz a b c > 1) = (-(b) + sqrt(b^2-4.0*a*c)) / (2.0*a)
	|otherwise = 0

{-
; Para definiciones en una misma linea

error porque lo toma (el 1) como otra linea:
diversion x = x +
1

sumarDos :: Int -> Int -> Int
sumarDos primero segundo = primero+segundo

DEFINICION DE OPERADORES
(+) :: Int -> Int -> Int
EJEMPLO
(+) 2 3 = 2 + 3
2 `max` 3 = 3
-}

(&&&)  :: Int -> Int -> Int
x &&& y
	|(x > y) = y
	|otherwise=x

--p18
(!=) :: Bool -> Bool -> Bool
x != y
	|(x /= y) = True
	|otherwise = False

--p19 NO LO CARGA PORQUE HAY UN ERROR
--divertido x = x + x
--	peculiar y = y



---------UNIDAD 4---------



--p01 EL VALOR MAXIMO DE 4 ENTERO
max4 :: Int -> Int -> Int -> Int -> Int
max4 x y z w
	|(x > y) && (x > z) && (x > w) = x
	|(y > z) && (y > x) && (y > w) = y
	|(z > y) && (z > x) && (z > w) = z
	|otherwise = w

--	= max(max x y) (max z w)

--	= max w (max3 x y z)

--p02 N ESTA ENTRE M Y P?
entre :: Int -> Int -> Int -> Bool
entre m n p = (n >= m) && (n <= p)

--p02.5 LO MISMO QUE LA ANTERIOR REVISANDO QUE P >= M
entre2 :: Int -> Int -> Int -> Bool
entre2 m n p = (n >= m) && (n <= p) && (p >= m)

--p03 CUANTOS DE LOS VALORES SON IGUALES
howeq :: Int -> Int -> Int -> Int
howeq x y z
	|(x == y) && (y == z) = 3
	|(x == y) || (x == z) || (y == z) = 2
	|otherwise = 0

--p04
howeq4 :: Int -> Int -> Int -> Int -> Int
howeq4 x y z w
	|(x == y) && (y == z) && (z == w) = 4
	|((x == y) && (y == z)) || ((y == z) && (z == w)) || ((x == y) && (y == w)) || ((x == z) && (z == w)) = 3
	|(x == y) || (x == z) || (x == w) || (y == z) || (y == w) || (z == w) = 2
	|otherwise = 0



------RECURSION------


--FACTORIAL
fac :: Int -> Int
fac n
	|(n == 0) = 1
	|(n > 0) = fac (n - 1) * n

--fac 3*4		24
--(fac 2*3)*4		24
--((fac 1*2)*3)*4	24
--(((fac 0*1)*2)*3)*4	24

--p05 PRODUCTORANGO
--armandodani y beto
pdr :: Int -> Int -> Int
pdr m n
	|(m > n) = 0
	|(m == n) = n
	|(m < n) = (pdr m (n - 1)) * n

--	|otherwise = (fac n)  - (fac m)

--p06 FACTORIAL PRODUCTORANGO
facpr :: Int -> Int
facpr n
	|(n == 0) = 1
	|(n > 0) = pdr 1 n

--POTENCIA DE 2
pot2 :: Int -> Int
pot2 n
	|(n == 0) = 1
	|(n > 0) = 2 * pot2 (n-1)

--SUMA FACTORIALES
sumfac :: Int -> Int
sumfac n
	|(n == 0) = 1
	|(n > 0) = sumfac (n - 1) + fac n

--SUMA FUNCIONES
sumfun :: (Int -> Int) -> Int -> Int
sumfun f n
	|(n == 0) = f 0
	|(n > 0) = sumfun f (n-1) + f n

--NUMERO MAXIMO DE REGIONES
regmax :: Int -> Int
regmax n
	|(n == 0) = 1
	|(n > 0) = regmax (n-1) + n

--p07 MULTIPLICA SUMATORIAMENTE
multsum :: Int -> Int -> Int
multsum x y
	|(x == 0) || (y == 0) = 0
	|(y > 0) = (multsum x (y-1)) + x


---------SIN TERMINAR---------


--p08 RAIZ CUADRADA ENTERA DE UN NUMERO POSITIVO
--EJEMPLO RAIZ DE 15 = 3 Y 16 = 4
introot :: Int -> Int
introot n
--	|(n < 0) = -1
--	|((n^2) > n) = introot (n-1)
	|((n^2) <= n) = introot (n+1)
--	|otherwise = -1

--p09
maxfun :: (Int -> Int) -> Int -> Int
maxfun f n
	|(n == 0) = f 0
	|(n > 0) = maxfun f (n-1) + f n


---------SIN TERMINAR---------


--p12 NUMERO MAXIMO DE REGIONES
regmax2 :: Int -> Int
regmax2 n = ((n^3) + (5*n) + 6) `div` 6

--p12 forma recursiva utilizando factorial

--FIBONACCI
fib :: Int -> Int
fib n
	|(n == 0) = 0
	|(n == 1) = 1
	|(n > 1) = fib (n-2) + fib (n-1)

--RESIDUO
res :: Int -> Int -> Int
res m n
	|(m < n) = m
	|otherwise = res (m-n) n

--DIVISION
divi :: Int -> Int -> Int
divi m n
	|(m < n) = 0
	|otherwise = 1 + divi (m-n) n


---------SIN TERMINAR---------


--p13 NUMERO MAXIMO DE FACTOR
--maxfactor :: Int -> Int -> Int
--maxfactor n
--	|


---------SIN TERMINAR---------


---------UNIDAD 5---------


{-
TUPLAS
ejemplos
("sal",10)


LISTAS
ejemplos
[("sal",10),("pollo",20)]


Dar nombre a los tipos
type articulo = (String, Int)
type canasta = [articulo]


fst(1,2)->1
snd(1,2)->2
-}

--MAXIMO DE DOS INT
minmax :: Int -> Int -> (Int,Int)
minmax x y
	|(x >= y) = (y,x)
	|otherwise = (x,y)

--p01 VECES QUE EXISTE EL NUMERO MAYOR DE DOS
maxist :: Int -> Int -> (Int,Int)
maxist x y
	|(x == y) = (x,2)
	|(x > y) = (x,1)
	|otherwise = (y,1)

--VECES QUE EXISTE EL NUMERO MAYOR DE TRES
maxist3 :: Int -> Int -> Int -> (Int,Int)
maxist3 x y z
	|(x == y) && (y == z) = (x,3)
	|(x == y) && (x > z) = (x,2)
	|(x == z) && (x > y) = (x,2)
	|(z == y) && (z > x) = (z,2)
	|(x > y) && (x > z) = (x,1)
	|(y > x) && (y > z) = (y,1)
	|(z > y) && (z > x) = (z,1)


--FIBONACCI
fibpaso :: (Int,Int) -> (Int,Int)
fibpaso (u,v) = (v,u+v)

fibpar :: Int -> (Int,Int)
fibpar n
	|(n == 0) = (0,1)
	|otherwise = fibpaso (fibpar (n-1))

fibfast :: Int -> Int
fibfast = fst.fibpar

--fibpaso2

medio :: Int -> Int -> Int -> Int
medio x y z
	|(x <= y) && (x >= z) = x
	|(y <= z) && (y >= x) = y
	|(z <= y) && (z >= x) = z
	|(x >= y) && (x <= z) = x
	|(y >= z) && (y <= x) = y
	|(z >= y) && (z <= x) = z


---------SIN TERMINAR---------


--p02 ORDENAR DE FORMA ASCENDENTE
--ord3 :: (Int,Int,Int) -> (Int,Int,Int)
--ord3 x y z = (min3 x y z,medio  x y z,max3 x y z)

--p03 ECUACION DE LA RECTA y=mx+b.....x=(y-b)/m
--ecrec :: Int -> Int -> Int
--ecrec m b = -(b) / m

{-caso cuando esta sobre x
cuando es paralela
y cuando si la corta-}


---------SIN TERMINAR---------
{-
Ejemplo
[2*n|n <- [2,4,7]]

[even n|n <- ex]
ex = lista
[2*n|n<-[2,4,7],even n,n>3]

even :: Int -> Int
even n = (n 'mod' 2 == 0)
-}

--SUMA LOS PARES
addpar :: [(Int,Int)] -> [Int]
addpar lista = [m+n | (m,n) <- lista]

--SUMAR PARES ORDENADOS
addparord :: [(Int,Int)] -> [Int]
addparord lista = [m+n | (m,n) <- lista, m<n]

--LISTA DE DIGITOS
digitos :: String -> String
digitos st = [ch | ch <-st, isDigit ch]

--TODOS SON PARES O IMPARES?
allpar xs = ( xs == [x | x <- xs,even x])

allimpar xs = ([] == [x | x <- xs, even x])

alldoble :: [Int] -> [Int]
alldoble lista = [2*b|b <- lista]

--p09 CONVERTIR STRING MINUSCULAS A MAYUSCULAS
ischarm :: Char -> Bool
ischarm ch = ischar ch || ('A' <= ch) && (ch <= 'Z')

mayustring :: String -> String
mayustring cadena = [enmayus2 ch| ch <- cadena]

mayustring2 :: String -> String
mayustring2 cadena = [enmayus2 ch| ch <- cadena,ischarm ch]

-p10 MOSTRAR LOS DIVISORES DE UN NUMERO
divisores :: Int -> [Int]
divisores n 
   | n > 0 = [x | x <- [1..n], n `mod` x == 0 ]
   | otherwise = []

--p11 MOSTRAR SI UN NUMERO ES PRIMO O NO
primo :: Int -> Bool
primo n 
   | length (divisores n) == 2 = True
   | otherwise = False

--MOSTRAR TODOS LOS NUMEROS PRIMOS HASTA EL QUE RECIBE 
numerosPrimos :: Int -> [Int]
numerosPrimos n = [x | x <- [1..n], primo x]

primo2 :: Int -> Bool
primo2 n
	| divisores (abs n) == [1,abs n] = True
	|n == 1 = True
	|otherwise = False
	
--POTENCIA DE 2
pot2 :: Int -> Int
pot2 n
	|(n == 0) = 1
	|(n > 0) = 2 * pot2 (n-1)

--SUMA FACTORIALES
sumfac :: Int -> Int
sumfac n
	|(n == 0) = 1
--	|(n > 0) = sumfac (n-1) + fac n

--SUMA FUNCIONES
sumfun :: (Int -> Int) -> Int -> Int
sumfun f n
	|(n == 0) = f 0
	|(n > 0) = sumfun f (n-1) + f n

--NUMERO MAXIMO DE REGIONES
regmax :: Int -> Int
regmax n
	|(n == 0) = 1
	|(n > 0) = regmax (n-1) + n

--p07 MULTIPLICA SUMATORIAMENTE
multsum :: Int -> Int -> Int
multsum x y
	|(x == 0) || (y == 0) = 0
	|(y > 0) = (multsum x (y-1)) + x
