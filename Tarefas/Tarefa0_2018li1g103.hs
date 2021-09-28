-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g103 where

import LI11819
import Data.List
-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores  (a,b) (c,d) = (a+c,b+d)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (a,b) (c,d) = (a-c,b-d)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor c (a,b) = (c*a,c*b)

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distânciaa à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (a,b) = (b,-a)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (a,b) = (a,-b)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (a,b) = (-a,b)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor C = (-1,0) 
direcaoParaVetor B = (1,0)
direcaoParaVetor D = (0,1)
direcaoParaVetor E = (0,-1)

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido a l = a>=0 && a<length l



-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
dimensaoMatriz :: Eq a => Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz (x:xs) | x/=[] = ((((length (x:xs)))), ((length x)))
                      | otherwise = (0,0)


-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (l,c) (l1:m) = l<length (l1:m) && l>=0 && c>=0 && c<length (l1)

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (a,b) [] = False
eBordaMatriz (a,b) c = (a==((length c)-1)) || b==(length(head c)-1) || a==0 || b==0

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I = [[False,True,False,False],[False,True,False,False],[False,True,False,False],[False,True,False,False]]
tetrominoParaMatriz J = [[False,True,False],[False,True,False],[True,True,False]]
tetrominoParaMatriz L = [[False,True,False],[False,True,False],[False,True,True]]
tetrominoParaMatriz O = [[True,True],[True,True]]
tetrominoParaMatriz S = [[False,True,True],[True,True,False],[False,False,False]]
tetrominoParaMatriz T = [[False,False,False],[True,True,True],[False,True,False]]
tetrominoParaMatriz Z = [[True,True,False],[False,True,True],[False,False,False]]

-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 [a] = a
encontraIndiceLista a (h:t) = if a==0 then h else encontraIndiceLista (a-1) t
-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista a b c | a>length c || a<0 = c
                          | a==0 = b:tail c
                          | otherwise = take (a) c ++ [b] ++ drop (a+1) c

-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz a = transpose (reverse a)

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH a = map reverse a

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV a = reverse a

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (l,c) a = replicate l (replicate c a)

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (l,c) b =  encontraIndiceLista c (encontraIndiceLista l b)

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) e m = atualizaIndiceLista l x' m
               where x= encontraIndiceLista l m
                     x'= atualizaIndiceLista c e x 
