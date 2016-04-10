import Data.Char
--Questão 1: Escreva uma função addSuffix :: String -> [String] -> [String] usando list comprehension, para adicionar um dado sufixo às strings contidas numa lista. 
--Exemplo:

--addSuffix "@inf.ufsm.br" ["fulano","beltrano"]
--["fulano@inf.ufsm.br","beltrano@inf.ufsm.br]

addSuffix :: String -> [String] -> [String]
addSuffix str list = [element ++ str | element <- list]

--Questão 2: Escreva uma função countShorts :: [String] -> Int, que receba uma lista de palavras e retorne a quantidade de palavras dessa lista que possuem 
--menos de 5 caracteres. Use recursão.
aux :: String -> Int
aux "" = 0
aux str = 1 + aux(tail str)

pegapalavra :: [String] -> [String]
pegapalavra [] = []
pegapalavra list
	 |aux(head list) < 5 = (head list) : pegapalavra (tail list)
	 |otherwise = pegapalavra (tail list)

countShorts :: [String] -> Int
countShorts [] = 0
countShorts list = 1 + (countShorts (tail(pegapalavra list)))

--Questão 3:Reescreva a função do exercício acima, desta vez usando list comprehension.
countShortsCompreensao :: [String] -> Int
countShortsCompreensao list = length([a | a <- list, length a < 5])

--Questão 4:Escreva uma função ciclo :: Int -> [Int] -> [Int] que receba um número N e uma lista de inteiros, retornando uma nova lista com N repetições da lista original,
-- conforme o exemplo abaixo:

-- > ciclo 4 [1,3]
--[1,3,1,3,1,3,1,3]

ciclo :: Int -> [Int] -> [Int]
ciclo 0 lista = []
ciclo n lista = (lista) ++ ciclo (n-1) lista


--Questão 5: Escreva uma função numera :: [String] -> [(Int,String)], que receba uma lista de palavras e retorne outra lista contendo tuplas com as palavras 
--numeradas a partir de 1. Use recursão. Exemplo de uso da função:

-- > numera ["abacaxi","mamao","banana"]
--[(1,"abacaxi"),(2,"mamao"),(3,"banana")]

numera :: [String] -> [(Int, String)]
numera [] = []
numera lista = reverse((length(lista),(last lista)) : numera (init lista))

--Questão 6:Explique, em forma de comentário, o resultado de cada expressão abaixo.

--a) [ (x,y) | x <- [1..5], even x, y <- [(x + 1)..6], odd y ]
--A função cria uma lista de tuplas por compreensão. Cada tupla contém dois elementos, x e y.
--Para o x é criada uma lista de 1 a 5, mas somente os numero pares são utilizados, através da função "even"
--Para o y é criada uma lista (x+1) até 6, mas é pego somente os numero impares.
--Assim, combina-se os elementos que satisfazem essas condições e formam-se as tuplas

--b) [ a ++ b | a <- ["lazy","big"], b <- ["frog", "dog"]]
--A função parte de duas listas contendo strings, e realiza todas as concatenações possiveis
--entre os elementos destas listas.

--c) concat [ [a,'-'] | a <- "paralelepipedo", not (elem a "aeiou")]
--Para cada letra da string paralelepipedo, é verificada se a mesma está contida na string "aeiou"(vogais)
--caso esteja, irá satisfazer a condição e sera colocada na lista, e cada elemento colocado na lista, será colocado
--um '-' em conjunto.

--Questão 7: (G. Malcolm, Univ. Liverpool) Write a function crossProduct :: [a] -> [b] -> [(a,b)] that takes two lists 
--xs and ys, and returns the list of all possible pairings:
--[ (x,y) | x <- xs, y <- ys ]
--without using the above list comprehension. (As an exercise in problem decomposition, try first defining a "helper" function pairWithAll :: a -> [b] -> [(a,b)] that pairs its
 --first argument with each element in its second.)

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct xs [] = []
crossProduct [] ys = []
crossProduct (x:xs) ys = map (\y -> (x,y)) ys ++ crossProduct xs ys

--Questão 8:Nesta questão você deverá usar list comprehension. Suponha que um retângulo seja representado por uma tupla (Float,Float,Float,Float), 
--contendo respectivamente as coordenadas x e y do ponto no seu canto superior esquerdo, seguidas das suas medidas de largura e altura. 
--Sabendo que o eixo x cresce de cima para baixo e o eixo y da esquerda para direita, crie uma função genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)] que receba um número N e um ponto (x,y) e gere uma sequência de N retângulos não sobrepostos. Os retângulos devem ser alinhados pelos seus topos, a partir do ponto dado, com largura e altura constantes. Por exemplo, usando largura e altura iguais a 5.5:

-- > genRects 3 (0,0) 
--[(0.0,0.0,5.5,5.5),(5.5,0.0,5.5,5.5),(11.0,0.0,5.5,5.5)]
--Obs.: Use conversão explícita de tipos quando misturar Int e Float.

genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)]
genRects x coord = genRects' x (fromIntegral (fst coord), fromIntegral (snd coord))

genRects' :: Int -> (Float,Float) ->[(Float,Float,Float,Float)]
genRects' 0_ = []
genRects' x coord = (fst coord, snd coord, 5.5, 5.5) : genRects' (x-1) (fst coord + 5.5, snd coord)

--Questão 9: Escreva uma função recursiva que receba uma lista de tuplas e decomponha cada uma delas, gerando uma tupla de listas, conforme o exemplo abaixo:
func9 :: [(Int,Int)] -> ([Int],[Int])
func9 [] = ([],[])
func9 (x:xs) = (fst x:(fst (func9 xs)), snd x: (snd (func9 xs)))


--Questão 10:
func10 :: [(Int,Int)] -> ([Int],[Int])
func10 list = ([fst x|x<-list],[snd x|x<-list])

--Questão 11:
func11 :: [(Int,Int)] -> ([Int],[Int])
func11 list = (map (\x-> fst(x)) list, map (\x-> snd(x)) list)



