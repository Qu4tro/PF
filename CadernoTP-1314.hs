import Data.Char
import Data.List

--1

perimetro :: Float -> Float
perimetro r = 2*3.14*r

dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

primUlt :: [a] -> (a, a)
primUlt [x]    = (x, x)
primUlt (x:xs) = (x, last xs)

multiplo :: Int -> Int -> Bool
multiplo x y = x `mod` y == 0

truncaImpar :: [a] -> [a]
truncaImpar l = if multiplo (length l) 2 then (tail l)
                                         else l
max2 :: Int -> Int -> Int
max2 x y = if x > y then x
                    else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) y

max3' :: Int -> Int -> Int -> Int
max3' x y z = if x > y && x > z then x
                                else if y > x && y > z then y
                                                       else z
--2

desigualdadeTriangular :: Int -> Int -> Int -> Bool
desigualdadeTriangular x y z = (x + y) > z && (x + z) > y && (y + z) > x


--3
type Ponto = (Float, Float)

comprimentoVerticesTriangulo :: Ponto -> Ponto -> Ponto -> (Float, Float, Float)
comprimentoVerticesTriangulo x y z = (dist x y, dist y z, dist z x)

perimetroTriangulo :: Ponto -> Ponto -> Ponto -> Float
perimetroTriangulo x y z = dist x y + dist y z + dist z x

completaRectangulo :: Ponto -> Ponto -> [Ponto]
completaRectangulo (x1, y1) (x2, y2) = [(x1, y1), (x2, y1),
                                        (x1, y2), (x2, y2)]

--4

nRaizesReais :: Float -> Float -> Float -> Int
nRaizesReais a b c
    | insideSqrt >  0 = 2
    | insideSqrt == 0 = 1
    | insideSqrt <  0 = 0
    where insideSqrt = b^2 - 4*a*c

--5

raizesReais :: Float -> Float -> Float -> [Float]
raizesReais a b c
    | nRaizesReais a b c == 0 = []
    | nRaizesReais a b c == 1 = [-b / 2*a]
    | nRaizesReais a b c == 2 = [(-b + sqrt(b^2 - 4*a*c)),
                                 (-b - sqrt(b^2 - 4*a*c))]

--6

nRaizesReais' :: (Float, Float, Float) -> Int
nRaizesReais' (a, b, c)
    | insideSqrt >  0 = 2
    | insideSqrt == 0 = 1
    | insideSqrt <  0 = 0
    where insideSqrt = b^2 - 4*a*c


raizesReais' :: (Float, Float, Float) -> [Float]
raizesReais' (a, b, c)
    | nRaizesReais a b c == 0 = []
    | nRaizesReais a b c == 1 = [-b / 2*a]
    | nRaizesReais a b c == 2 = [(-b + sqrt(b^2 - 4*a*c)),
                                 (-b - sqrt(b^2 - 4*a*c))]

--7

isLower' :: Char -> Bool
isLower' c = c >= 'a' && c <= 'z'

isDigit' :: Char -> Bool
isDigit' c = c >= '0' && c <= '9'

isAlpha' :: Char -> Bool
isAlpha' c = c >= 'A' && c <= 'Z' || isLower c

toUpper' :: Char -> Char
toUpper' c = if isLower c then chr (ord c - (ord 'a' - ord 'A'))
                         else c

intToDigit' :: Int -> Char
intToDigit' n = chr (n + (ord '0'))

digitToInt' :: Char -> Int
digitToInt' c = (ord c) - (ord '0')

--8

primMai :: String -> Bool
primMai (x:xs) = isUpper x

segMin :: String -> Bool
segMin (x:y:xs) = isLower y

--9

type Hora = (Int, Int)

horaValida :: Hora -> Bool
horaValida (h, m) = h > 0 && h < 24 && m > 0 && m < 60

depoisDe :: Hora -> Hora -> Bool
depoisDe (h1, m1) (h2, m2)
    | h1 > h2   = True
    | h1 < h2   = False
    | otherwise =  m1 > m2

horasParaMinutos :: Hora -> Int
horasParaMinutos (h, m) = h*60 + m

minutosParaHoras :: Int -> Hora
minutosParaHoras m = (m `div` 60, m `mod` 60)

deltaHoras :: Hora -> Hora -> Int
deltaHoras h1 h2 = horasParaMinutos h1 - horasParaMinutos h2

addHoras :: Hora -> Int -> Hora
addHoras h n = minutosParaHoras ((horasParaMinutos h) + n)

--10

opp :: (Int, (Int, Int)) -> Int
opp (1, (y, z)) = y + z
opp (2, (y, z)) = y - z
opp _           = 0

--11

funA :: [Float] -> [Float]
funA [] = []
funA (h:t) = if h >= 0 then h : (funA t)
                       else (funA t)

-- [3, 0, 2]
-- A função está a discartar os números negativos na condição.

funB :: [Float] -> Float
funB [] = 1
funB (x:xs) = x * (funB xs)

-- 30
-- A função multiplica os números todos.

funC :: [Float] -> Float
funC [] = 0
funC (y:ys) = y^2 + (funC ys)

-- 2^2 + 3^2 + 5^2 = 38
-- A função é a soma dos quadrados

funD :: [Int] -> [Int]
funD [] = []
funD (h:t) = if (mod h 2) == 0 then h : (funD t)
                               else (funD t)

-- [8, 12]
-- Esta função discarda os números impares

p :: Int -> Bool
p 0 = True
p 1 = False
p x | x > 1 = p (x - 2)

-- False
-- Esta função verifica se x é par. 

f l = g [] l
g l [] = l
g l (h:t) = g (h:l) t

-- "certo"
-- A função reverte uma lista

--12

dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = 2*x : dobros xs

ocorre :: Char -> String -> Int
ocorre c "" = 0
ocorre c (x:xs) = if c == x then 1 + ocorre c xs
                            else     ocorre c xs

pmaior :: Int -> [Int] -> Int
pmaior n [] = n
pmaior n (x:xs) = if x > n then x
                           else pmaior n xs

repetidos :: (Eq a) => [a] -> Bool
repetidos [] = False
repetidos (x:xs) = x `elem` xs || repetidos xs

nums :: String -> [Int]
nums "" = []
nums (c:cs) = if isDigit c then digitToInt c : nums cs
                           else                nums cs

tresUlt :: [a] -> [a]
tresUlt []        = []
tresUlt [x]       = [x]
tresUlt [x, y]    = [x, y]
tresUlt [x, y, z] = [x, y, z]
tresUlt (x:xs)    = tresUlt xs

posImpares :: [a] -> [a]
posImpares []       = []
posImpares [x]      = []
posImpares (x:y:xs) = y : posImpares xs

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) = if x < 0 then x + somaNeg xs
                          else     somaNeg xs

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if isDigit x then x : soDigitos xs
                                else     soDigitos xs

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if isLower x then 1 + minusculas xs
                                 else     minusculas xs

delete' :: (Eq a) => a -> [a] -> [a]
delete' _ []     = []
delete' e (x:xs) = if e == x then xs
                            else x : delete' e xs

ordena :: (Ord a) => [a] -> [a]
ordena [] = []
ordena xs = minVal : (ordena (delete' minVal xs))
    where minVal = minimum xs

--13

type Jogo = (String, Int, String, Int)

golosEquipa :: Jogo -> String -> Int
golosEquipa (s1, g1, s2, g2) equipa
    | s1 == equipa = g1
    | s2 == equipa = g2
    | otherwise    = -1

resJogo :: Jogo -> Char
resJogo (_, g1, _, g2)
	|g1 > g2   = '1'
	|g1 < g2   = '2'
	|otherwise = 'x' --caso seja empate.

resJogo' :: Jogo -> String
resJogo' (_, g1, _, g2)
	|g1 > g2   = "ganhou a equipa da casa"
	|g1 < g2   = "ganhou a equipa visitante"
	|otherwise = "empate"

empates :: [Jogo] -> Int
empates []                  = 0
empates ((_, g1, _, g2):xs) = if g1 == g2 then 1 + empates xs
                                          else     empates xs


jogosComXGolos :: [Jogo] -> Int -> Int
jogosComXGolos [] _                      = 0
jogosComXGolos ((_, g1, _, g2):xs) golos = if g1 + g2 == golos then 1 + jogosComXGolos xs golos
                                                               else     jogosComXGolos xs golos

jogosGanhosFora :: [Jogo] -> Int
jogosGanhosFora []     = 0
jogosGanhosFora (x:xs) = if resJogo x == '2' then 1 + jogosGanhosFora xs
                                             else     jogosGanhosFora xs



type Circulo = (Ponto, Float) --(Centro, Raio)


distP :: Ponto -> Ponto -> Float
distP (a,b) (c,d) = sqrt ((a-c)^2 +(b-d)^2)

fora :: Ponto -> Circulo -> Bool
fora p (c, r) = distP p c > r

filtraFora :: Circulo -> [Ponto] -> Int
filtraFora c []     = 0
filtraFora c (p:ps) = if fora p c then 1 + filtraFora c ps
                                  else     filtraFora c ps

dentro :: Ponto -> Circulo -> Bool
dentro p (c, r) = distP p c < r

filtraDentro :: Ponto -> [Circulo] -> Int
filtraDentro p []     = 0
filtraDentro p (c:cs) = if dentro p c then 1 + filtraDentro p cs
                                      else     filtraDentro p cs

type Rectangulo = (Ponto, Ponto)

quadrado :: Rectangulo -> Bool
quadrado ((x1, y1), (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (x:xs) = if quadrado x then 1 + contaQuadrados xs
                                      else     contaQuadrados xs

roda :: Rectangulo -> Rectangulo
roda (p1@(x1, y1), (x2, y2)) = (p1, (x1 + (y1 - y2), y1 + (x1 - x2)))

rodaTudo :: [Rectangulo] -> [Rectangulo]
rodaTudo [] = []
rodaTudo (x:xs) = roda x : rodaTudo xs

area :: Rectangulo -> Float
area ((x1, y1), (x2, y2)) = abs (x1 - x2) * abs (y1 - y2)

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (x:xs) = area x + areaTotal xs

escala :: Float -> Rectangulo -> Rectangulo
escala s ((x1, y1), (x2, y2)) = ((x1, y1), ((s*(x2 - x1)) + x1, s*(y2 - y1) + y1))

escalaTudo :: Float -> [Rectangulo] -> [Rectangulo]
escalaTudo _ [] = []
escalaTudo e (x:xs) = escala e x : escalaTudo e xs

--16

(><) :: Int -> Int -> Int
(><) 0 y = 0
(><) x 0 = 0
(><) x 1 = x
(><) 1 y = y
(><) x y = x + (x >< (y - 1))

div' :: Int -> Int -> Int
div' n d
    | n <  d = 0
    | n >= d = 1 + div' (n - d) d

mod' :: Int -> Int -> Int
mod' n d = n - (d * (div' n d))

power :: Int -> Int -> Int
power b 0 = 1
power b e = b * power b (e - 1)

uns :: Int -> Int 
uns 1 = 1
uns n
    | even n =     uns (div' n 2)
    | odd  n = 1 + uns (div' n 2)


primo :: Int -> Bool
primo 1 = False
primo n = testePrimo n (n - 1)
    where testePrimo :: Int -> Int -> Bool
          testePrimo n 1 = True
          testePrimo n t = mod' n t /= 0 && testePrimo n (t - 1)

--17

primeiros :: [(a, b)] -> [a]
primeiros []          = []
primeiros ((a, _):xs) = a : primeiros xs

nosPrimeiros :: (Eq a) => a -> [(a, b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((a, _):xs) = x == a || nosPrimeiros x xs

minFst :: (Ord a) => [(a, b)] -> a
minFst [(a, b)] = a
minFst ((a, b):xs) = min a (minFst xs)

minimumFst :: (Ord a) => [(a, b)] -> (a, b)
minimumFst [x]    = x
minimumFst (x:xs) = minFstTuple x (minimumFst xs)

sndMinFst :: (Ord a) => [(a, b)] -> b
sndMinFst xs = snd (minimumFst xs)

minFstTuple :: (Ord a) => (a, b) -> (a, b) -> (a, b)
minFstTuple t1@(a, b) t2@(c, d) = if min a c == a then t1
                                                  else t2

ordenaSnd :: (Ord b, Eq a) => [(a, b)] -> [(a, b)]
ordenaSnd [] = []
ordenaSnd xs = minVal : (ordenaSnd (delete' minVal xs))
    where minVal = minimumSnd xs

minSndTuple :: (Ord b) => (a, b) -> (a, b) -> (a, b)
minSndTuple t1@(a, b) t2@(c, d) = if min b d == b then t1
                                                  else t2

minimumSnd :: (Ord b) => [(a, b)] -> (a, b)
minimumSnd [x]    = x
minimumSnd (x:xs) = minSndTuple x (minimumSnd xs)

--18

type Aluno   = (Numero, Nome, ParteI, ParteII)
type Numero  = Int
type Nome    = String
type ParteI  = Float
type ParteII = Float
type Turma   = [Aluno]

turmaValida :: Turma -> Bool
turmaValida []                  = True
turmaValida ((n, _, p1, p2):xs) = p1 >= 0 && p1 <= 12 && p2 >= 0 && p2 <= 8 && notElem n (numerosAluno xs) && turmaValida xs
    where numerosAluno :: Turma -> [Int]
          numerosAluno [] = []
          numerosAluno ((n, _, _, _):xs) = n : numerosAluno xs

alunosTransitados :: Turma -> Turma
alunosTransitados [] = []
alunosTransitados (aluno@(_, _, p1, p2):xs) = if p1 >= 8 && (p1 + p2) >= 9.5 then aluno : alunosTransitados xs
                                                                             else         alunosTransitados xs


notaFinal :: Turma -> [Float]
notaFinal [] = []
notaFinal xs = (p1 + p2) : notaFinal as
    where ((_, _, p1, p2):as) = alunosTransitados xs

mediaNotaFinal :: Turma -> Float
mediaNotaFinal xs = realToFrac (sum notas) / genericLength notas
    where notas = notaFinal xs

melhorNota :: Turma -> String
melhorNota t = nome
    where (_, nome, _, _) = melhorAluno t

maxNota :: Aluno -> Aluno -> Aluno
maxNota a1@(_, _, p11, p21) a2@(_, _, p12, p22) = if (p11 + p21) >= (p12 + p22) then a1
                                                                                else a2

melhorAluno :: Turma -> Aluno
melhorAluno [x]    = x
melhorAluno (x:xs) = maxNota x (melhorAluno xs)

--19

type Etapa = (Hora, Hora)
type Viagem = [Etapa]

etapaValida :: Etapa -> Bool
etapaValida (h1, h2) = horaValida h1 && horaValida h2 && h2 `depoisDe` h1

viagemValida :: Viagem -> Bool
viagemValida []  = True
viagemValida [x] = True
viagemValida (e1@(_, h1):e2@(h2, _):xs) = etapaValida e1 && etapaValida e2 && h2 `depoisDe` h1 && viagemValida (e2:xs)

partidaChegada :: Viagem -> Etapa
partidaChegada ((partida, _):xs) = (partida, snd (last xs))

tempoViagemEfectiva :: Viagem -> Int
tempoViagemEfectiva []     = 0
tempoViagemEfectiva ((h1, h2):xs) = abs (deltaHoras h1 h2) + tempoViagemEfectiva xs

tempoEspera :: Viagem -> Int
tempoEspera [] = 0
tempoEspera [(h1, h2)] = 0
tempoEspera ((h1, h2), x@(h3, h4):xs) = abs (deltaHoras h3 h2) + tempoEspera (x:xs)

tempoViagemTotal :: Viagem -> Int
tempoViagemTotal viagem = tempoViagemEfectiva viagem + tempoEspera viagem

--20

type Rectangulo2 = (Ponto, Float, Float)
type Triangulo = (Ponto, Ponto, Ponto)
type Poligonal = [Ponto]

comprimentoLinhaPoligonal :: Poligonal -> Int
comprimentoLinhaPoligonal [] = 0
comprimentoLinhaPoligonal [x] = 0
comprimentoLinhaPoligonal (x:y:xs) = dist x y + comprimentoLinhaPoligonal (y:xs)

trianguloParaPoligonal :: Triangulo -> Poligonal
trianguloParaPoligonal (x, y, z) = [x, y, z, x]

rectanguloParaPoligonal :: Rectangulo2 -> Poligonal
rectanguloParaPoligonal (p@(x, y), w, h) = [p,             (x + w, y), 
                                           (x + w, y - h), (x, y - h), p]

fechada :: Poligonal -> Bool
fechada []     = False
fechada [x]    = False
fechada [x, y] = False
fechada (x:xs) = x == last xs

triangula :: Poligonal -> [Triangulo]
triangula [] = []
triangula (x:y:z:xs) = (x, y, z) : triangula (x:z:xs)

areaTriangulo :: Triangulo -> Float
areaTriangulo (x, y, z)
    = let a = dist x y
          b = dist y z
          c = dist z y
          s = (a + b + c) / 2 --semi-perimetro
      in --formula de Heron
          sqrt (s * (s - a) * (s - b) * (s - c))

areaPoligono :: Poligonal -> Float
areaPoligono xs = sum (map areaTriangulo (triangula xs))

mover :: Poligonal -> Poligonal

