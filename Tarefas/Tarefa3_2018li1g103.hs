-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g103 where

import LI11819
import Tarefa0_2018li1g103
import Tarefa1_2018li1g103
import Data.Char


-- * Relatorio
 
-- | A Tarefa3 consiste na compressão e descompressão do estado de jogo atual.
--Inicialmente, este projeto pareceu que não seria fácil, porém à medida que se foi elaborando, ficou evidente que apenas seria necessário aplicar algumas técnicas específicas de modo a se poder descomprimir o resultado da compressão. 
--No que toca aos objetivos nesta tarefa pretendemos criar um sistema que tornasse possível guardar o jogo, caso o jogador pretendesse.
--No entanto, o importante não era apenas conseguir guardar o jogo, outro aspeto a ter em consideração foi a compressão dos dados no menor número de caratéres possíveis. 
--Inicialmente, decidiu-se atribuir um carater a cada tipo de bloco e antes destes carateres eram colocados o número de blocos daquele tipo que apareciam seguidos, no entanto isso levou a que da compressão resultassem linhas demasiado extensas.
--Como esta foi a primeira ideia, concordou-se que seria adequado tentar implementá-la e depois caso surgissem ideias melhores este método substituído.
--De facto, tornou-se claro que um novo método de compressão seria mais adequado, assim para que o número de carateres pudesse ser reduzido implementou-se que cada caratér iria guardar mais informações, o que levou a que cada carater correspondesse não só um tipo de blocos, mas também o número de blocos seguidos.
--Para tornar possível a descompressão, foi necessário colocar carateres a separar a compressão do mapa da dos jogadores e o mesmo foi feito entre os jogadores e os disparos. Assim, tanto o processo de compressão como o de descompressão faz-se por partes, o que torna possível uma melhor organização das ideias.
--Em suma, o resultado desta tarefa mostrou-se melhor do que era esperado inicialmente, especialmente no que toca à compressão do mapa, uma vez que foi a parte mais complicada do projeto e a que demorou mais tempo a implementar.
--Na verdade, todo o projeto poderia ter sido realizado de forma simples, mas eficaz, no entanto não teria o resultado que se tinha idealizado. Assim sendo, foram implementadas várias modificações depois da versão inicial, o que tornaram esta fase um desafio.
--Por fim, sentímos grande orgulho no que fora realizado, uma vez que foi uma nova experiência com um resultado positivo. -}

relatorio3 :: String -- ^ O Relatorio da Tarefa3
relatorio3 = "Relatorio3"

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18] 
         where t1 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) B 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (2,2) E)])
               t2 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 1 (2,2) E)])
               t3 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) D 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,2) E)])
               t4 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) B 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (2,2) B)])
               t5 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) B 1 0 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,2) B)]) 
               t6 = (Estado (mapaInicial (10,10)) [(Jogador (1,1) B 1 1 1), (Jogador (1,7) B 1 1 1), (Jogador (7,1) B 1 1 1), (Jogador (7,7) B 1 1 1)] [(DisparoCanhao 0 (2,2) B)]) 
               t7 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) B 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (2,2) E)])
               t8 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) B 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 2 (2,2) E)])
               t9 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) B 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 3 (2,2) C)])
               t10 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) E 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (2,2) D), (DisparoChoque 0 5)])
               t11 = (Estado (mapaInicial (3,3)) [] [])
               t12 = (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (2,1) E)])    
               t13 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) E 1 0 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,2) D), (DisparoChoque 0 5)])
               t14 = (Estado (mapaInicial (60,60)) [(Jogador (2,2) E 0 1 1), (Jogador (40,43) B 1 5 1), (Jogador (6,40) B 1 1 41), (Jogador (2,50) B 1 600 1)] [(DisparoCanhao 3 (2,3) D), (DisparoChoque 1 5), DisparoCanhao 3 (4,6) C, DisparoChoque 2 60, (DisparoLaser 3 (6,6) D)])
               t15 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) E 1 0 0), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,2) D), (DisparoChoque 0 5)])
               t16 = (Estado (mapaInicial (10,10)) [(Jogador (2,2) E 1 1 0), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,2) D), (DisparoChoque 0 5), (DisparoCanhao 1 (6,3) B)])
               t17 = (Estado (mapaInicial (10,10)) [(Jogador (1,1) E 1 0 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,2) D), (DisparoChoque 0 5)])
               t18 = (Estado (mapaInicial (10,10)) [(Jogador (1,1) E 1 0 1), (Jogador (3,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,2) D), (DisparoChoque 0 5)])

-- ** Compressão

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
--
-- __NB:__ Foram usadas as funções auxiliares 'dimensaoChar', 'mapaComprimido', 'comprimeMapa', 'listaPecaChar', 'juntalinhas', 'removeBordas', 'aux32' e 'aux33'

comprime :: Estado -- ^ Recebe um 'Estado' 
         -> String -- ^ Devolve uma 'String'
comprime (Estado [] [] []) = []
comprime (Estado mapa jog dis) = (chr(ord(dimensaoChar mapa) - 2)):(mapaComprimido (comprimeMapa (listaPecaChar(juntalinhas (removeBordas mapa))))) ++ "&" ++ (aux32 jog) ++ (aux33 dis) 

-- * Compressão do Mapa

-- | Transforma a Dimensao do Mapa num Caracter

dimensaoChar :: Mapa -- ^ 'Mapa' de Jogo 
             -> Char -- ^ 'Char' referente a 'Dimensao' do 'Mapa'
dimensaoChar (h:t) = chr (length h)

-- | Comprime a String correspondente ao 'Mapa' numa String mais curta

mapaComprimido :: String -- ^ 'String' do 'Mapa'
               -> String -- ^ 'String' do 'Mapa' reduzida
mapaComprimido [] = []
mapaComprimido (h:t) = (chr(fromDigits(charToChar (firstToLast ((head (dropWhileElem (h:t))):(takeWhileElem (h:t))))))):mapaComprimido (tail(dropWhileElem (h:t)))

-- | Remove as Peças repetidas seguidas e devolve o numero desse tipo de Peças que tinha 

comprimeMapa :: String -- ^ 'String' de 'Peca' 
             -> String -- 'String' correspondente ao numero de 'Peca's repetidas seguidas
comprimeMapa [] = []
comprimeMapa l@(h:t) = show(contaIgual h l) ++ ((removeIgual h l):comprimeMapa (dropWhile (==h) l))

-- | Transforma a lista de Peças numa String

listaPecaChar :: [Peca] -- ^ Lista de 'Peca's  
              -> String -- ^ 'String' correspondente a lista de 'Peca's
listaPecaChar  = map (blocoParaChar)

-- | Junta as linhas para que fiquem todas numa lista de Peças 

juntalinhas :: Mapa -- ^ 'Mapa' do Jogo 
            -> [Peca] -- ^ Lista de 'Peca's correspondentes a todas as linhas 
juntalinhas (h:t) = h ++ juntalinhas t
juntalinhas [] = []

-- | Remove as Bordas do Mapa

removeBordas :: Mapa -- ^ 'Mapa' Inicial 
             -> Mapa -- ^ 'Mapa' Inical com as Bordas Removidas
removeBordas (h:t) = map (retirap) (reverse (tail (reverse t)))

-- | Coverte as peças para caracteres
   
blocoParaChar :: Peca -- ^ Recebe uma 'Peca' 
              -> Char -- ^ Devolve um caracter correspondente a 'Peca'  
blocoParaChar ((Bloco Indestrutivel)) = 'i'
blocoParaChar ((Bloco Destrutivel)) = 'd'
blocoParaChar ((Vazia)) = 'v'

-- | Remove de uma String os caracteres que sao iguais a um dado caracter

removeIgual :: Char -- ^ Um 'Char' que se pretende remover a 'String'
            -> String -- ^ Uma 'String' 
            -> Char -- ^ 'Char' sem o caracter que queremos remover
removeIgual c [] = c
removeIgual c (h:t) | c==h = removeIgual c t
                    | otherwise = c

-- | Conta o numero de caracteres iguais a um dado caracter numa string

contaIgual :: Char -- ^ 'Caracter' que se pretende encontrar 
           -> String -- ^ 'String'
           -> Int -- ^ Numero de caracteres na 'String' iguais ao caracter que se pretendia identificar 
contaIgual c [] = 0
contaIgual c (h:t) | h==c = 1 + contaIgual c t
                   | otherwise = 0

-- | Cria um Mapa com a Dimensao pedida, removendo-lhe as Bordas

mapaSemBorda :: Dimensao -- ^ 'Dimensao' do Mapa desejado
             -> Mapa -- ^ 'Mapa' desejado sem as Bordas
mapaSemBorda (x,y) = replicate (x-2) f 
                 where f = replicate (y-2) (Vazia)

-- | Coverte uma String de caracteres alfabeticos numa String de caracteres numericos 

charToChar :: String -- ^ 'String' de caracteres alfabeticos  
           -> String -- ^ 'String' de caracteres numericos  
charToChar [] = []
charToChar (h:t) | h=='v' = ('0':t)
                 | h=='i' = ('1':t)
                 | h=='d' = ('2':t)
                 | otherwise = h:charToChar t

-- | Converte uma lista de Caracteres num numero inteiro

compNum :: [Char] -- ^ Lista de 'Char' 
        -> Int -- ^ Valor  
compNum [] = 0
compNum (h:t) | (h>='0' && h<='9') = 1 + compNum t
              | otherwise = 0

-- | Adiciona um elemento no fim de uma lista

firstToLast :: [a] -- ^ Lista Inicial 
            -> [a] -- ^ Lista com o novo elemento
firstToLast [] = []
firstToLast (h:t) = reverse (h:reverse t)

-- | Retira o primeiro e último elemento de uma lista

retirap :: [Peca] -- ^ Lista de 'Peca's  
        -> [Peca] -- ^ Lista de 'Peca's sem o Primeiro e Ultimo Elemento
retirap [] = []
retirap (h:t) = init (t)

-- * Compressão dos Jogadores

-- | Comprime uma Lista de Jogadores

aux32 :: [Jogador] -- ^ Lista de 'Jogador'es
      -> String -- ^ 'String' correspondente a Lista de 'Jogador'es
aux32 [] = []
aux32 ((Jogador (l,c) d v las ch):t) = (fp (l,c)) ++ (fd d) ++ (fv v) ++ (flas las) ++ (fch ch) ++ aux32 t

-- | Comprime a Posicao de um Jogador

fp :: PosicaoGrelha -- ^ 'Posicao' do Jogador no 'Mapa' 
   -> String -- ^ 'String' correspondente a sua 'Posicao'
fp (l,c) = "j" ++ show l ++ "c" ++ show c

-- | Comprime a Direcao do Jogaor

fd :: Direcao -- ^ 'Direcao' do Jogador 
   -> String -- ^ 'String' correspondente a essa 'Direcao'
fd d | (d == C) = "C"
     | (d == D) = "D"
     | (d == B) = "B"
     | (d == E) = "E"

-- | Comprime as Vidas de um Jogador

fv :: Int -- ^ Vidas do Jogador 
   -> String -- ^ 'String' correspondente as Vidas do Jogador 
fv v = "v" ++ show v

-- | Comprime a quantidade de Laser que um Jogador tem disponivel

flas :: Int -- ^ Qunatidade de Laser Disponivel
     -> String -- ^ 'String' correspondente aos Lasers Disponiveis
flas las = "~" ++ show las

-- | Comprime a quantidade de Choques que um Jogador tem Disponivel
  
fch :: Int -- ^ Quantidade de Choques Disponiveis 
    -> String -- ^ 'String' correspondente aos Choques Disponiveis
fch ch = "^" ++ show ch ++ "@"

-- * Compressão dos Disparos

-- | Comprime uma Lista de Disparos

aux33 :: [Disparo] -- ^ Lista de 'Disparo's 
      -> String -- ^ Lista comprimida
aux33 [] = []
aux33 ((DisparoCanhao j (l,c) d):t) = "." ++ (gj j) ++ (gp (l,c)) ++ (gd d) ++ aux33 t
aux33 ((DisparoLaser j (l,c) d):t) = "," ++ (gj j) ++ (gp (l,c)) ++ (gd d) ++ aux33 t
aux33 ((DisparoChoque j ti):t) = ";" ++ (gj j) ++ (gj ti) ++ aux33 t

-- | Comprime o Indice do Jogador que Dispara

gj :: Int -- ^ Indice do Jogador que Dispara 
   -> String -- ^ Valor comprimido
gj j = show j

-- | Comprime a Posição do Disparo

gp :: PosicaoGrelha -- ^ Posicao do 'Disparo'
   -> String -- ^ 'Posicao' comprimida
gp (l,c) = ('\9':show l) ++ ('\8':show c)

-- | Comprime a Direção do Disparo

gd :: Direcao -- ^ 'Direcao' do 'Disparo' 
   -> String -- 'Direcao' comprimida
gd d | (d == C) = "C"
     | (d == D) = "D"
     | (d == B) = "B"
     | (d == E) = "E"

-- * Descompressão
                                                  
-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
descomprime :: String -- ^ 'Estado' comprimido.
            -> Estado -- ^ 'Estado' resultante.
descomprime (h:t) = (Estado (insereBordaF (replicate (descomprimeDim (h:t) +2) (Bloco Indestrutivel)) (insereBorda (replicate (descomprimeDim (h:t) +2) (Bloco Indestrutivel)) (map (insereIF) (controiMapa (h:t) (controiLinha t))))) (descomprimeListJog (takeWhile'' (dropWhile (/= 'j') (h:t)))) (descomprimeDisp (h:t)))

-- * Descompressão do Mapa

-- | Descomprime o número de colunas do Mapa. 

descomprimeDim :: String -- ^ Estado Comprimido.
               -> Int -- ^ Número de Colunas.
descomprimeDim (h:t) = ord h

-- | Conta o número de peças seguidas.

contaPecas :: String -- ^ 'Estado' Comprimido.
           -> Int -- ^ Número de 'Peca's seguidas.
contaPecas [] = 0
contaPecas (h:t) = contaPrimPeca (h:t) + contaPecas (tail (takeWhile (/= '&') (h:t)))

-- | Descomprime o tipo de peça

descomprimePecas :: String -- ^ 'Estado' Comprimido.
                 -> Peca -- ^ Tipo de 'Peca'
descomprimePecas (h:t) | mod (ord h) 10 == 0 = Vazia
                       | mod (ord h) 10 == 1 = Bloco Indestrutivel
                       | mod (ord h) 10 == 2 = Bloco Destrutivel

-- | Conta o número de peças seguidas para a primeira peça.

contaPrimPeca :: String -- ^ 'Estado' Comprimido.
              -> Int -- ^ Número de vezes que a primeira 'Peca' se encontra repetida seguida.
contaPrimPeca [] = 1
contaPrimPeca (h:t) = div (ord h) 10

-- | Transforma a string numa lista de peças.

controiLinha :: String -- ^ 'Estado' Comprimido.
             -> [Peca] -- ^ Lista de todas as 'Peca's do 'Mapa'.
controiLinha [] = []
controiLinha (h:t) = replicate (contaPrimPeca (h:t)) (descomprimePecas (h:t)) ++ controiLinha (takeWhile(/= '&') t)

-- | Transforma uma string num mapa.

controiMapa :: String -- ^ 'Estado' comprimido com número de colunas do 'Mapa'.  
            -> [Peca] -- ^ Lista de 'Peca's.
            -> Mapa -- ^ 'Mapa' resultante de separar a lista de 'Peca's.
controiMapa b [] = []
controiMapa b l = fst (splitAt (descomprimeDim b) l):controiMapa b (drop (descomprimeDim b) l)

-- | Insere borda de cima num mapa.

insereBorda :: [Peca] -- ^ Lista de 'Peca's a inserir como cabeça do 'Mapa'.
            -> Mapa -- ^ 'Mapa' a alterar.
            -> Mapa -- ^ 'Mapa' com uma linha nova na primeira posição.
insereBorda [] m = m
insereBorda p m = (p:m)


-- | Insere borda de baixo num mapa.

insereBordaF :: [Peca] -- ^ Lista de 'Peca's a inserir como último elemento do 'Mapa'.
            -> Mapa -- ^ 'Mapa' a alterar.
            -> Mapa -- ^ 'Mapa' com uma linha nova na última posição.
insereBordaF [] m = m
insereBordaF p m = reverse (p:(reverse m))

-- | Insere bordas laterais numa linha.

insereIF :: [Peca] -- ^ Linha sem borda.
         -> [Peca] -- ^ Linha com borda.
insereIF [] = []
insereIF l = (Bloco Indestrutivel):(reverse ((Bloco Indestrutivel):reverse l))


-- * Descompressão do Jogador

-- | Descomprime a Posição de um Jogador.

descomprimePos :: String -- ^ 'Estado' comprimido.
               -> Posicao -- ^ 'Posicao' do 'Jogador'.
descomprimePos l = (fromDigits (tail(takeWhile (/= 'c') (dropWhile (/= 'j') l))), fromDigits (tail(takeWhile' (dropWhile (/= 'c') l))))

-- | Descomprime a Direção de um Jogador.

descomprimeDir :: String  -- ^ 'Estado' comprimido.
               -> Direcao -- ^ 'Direcao' do 'Jogador'.
descomprimeDir l = verificaDir (dropWhile (/= 'j') l)

-- | Verifica qual a Direção comprimida.

verificaDir :: String -- ^ 'Estado' comprimido.
            -> Direcao -- ^ 'Direcao' resultante.
verificaDir ('D':t) = D
verificaDir ('C':t) = C
verificaDir ('B':t) = B
verificaDir ('E':t) = E
verificaDir (h:t) = verificaDir t

-- | Descomprime Vida.

descomprimeV :: String -- ^ 'Estado' comprimido. 
             -> Int -- ^ 'Vida' de um 'Jogador'.
descomprimeV (h:t) = fromDigits (tail(takeWhile (/= '~') (dropWhile (/= 'v') (h:t))))

-- | Descomprime Lasers de um Jogador.

descomprimeL :: String -- ^ 'Estado' comprimido.
             -> Int -- ^ 'Laser's de um 'Jogador'.
descomprimeL (h:t) = fromDigits (tail(takeWhile (/= '^') (dropWhile (/= '~') (h:t))))

-- | Descomprime Choques de um Jogador.

descomprimeC :: String -- ^ 'Estado' comprimido.
             -> Int -- ^ 'Choque's de um 'Jogador'.
descomprimeC (h:t) = fromDigits (tail(takeWhile (/= '@') (dropWhile (/= '^') (h:t))))


-- | Descomprime um Jogador por completo.

descomprimeJog :: String -- ^ 'Estado' comprimido. 
               -> Jogador -- 'Jogador' descomprimido.
descomprimeJog (h:t) = (Jogador (descomprimePos (h:t)) (descomprimeDir (h:t)) (descomprimeV (h:t)) (descomprimeL (h:t)) (descomprimeC (h:t)))

-- | Descomprime uma lista de Jogadores.

descomprimeListJog :: String -- ^ 'Estado' comprimido. 
                   -> [Jogador] -- ^ Lista de 'Jogador'es descomprimida.
descomprimeListJog "" = []
descomprimeListJog (h:t) = (descomprimeJog (h:t)):(descomprimeListJog (dropWhile(/='j') (tail (dropWhile (/= 'j') (h:t)))))
-----

-- * Descompressão do Disparo

-- | Descomprime uma lista de Disparos.

descomprimeDisp :: String -- ^ 'Estado' comprimido. 
                -> [Disparo] -- ^ Lista de 'Disparo's descomprimida.
descomprimeDisp [] = []
descomprimeDisp (h:t) | h=='.' = (DisparoCanhao (descomprimeJogDisp (t)) (descomprimePosDisp (h:t)) (verificaDir (h:t))):descomprimeDisp t
                      | h==',' = (DisparoLaser (descomprimeJogDisp (t)) (descomprimePosDisp (h:t)) (verificaDir (h:t))):descomprimeDisp t
                      | h==';' = (DisparoChoque (descomprimeJogDisp (t)) (descomprimeTick (h:t))):descomprimeDisp t
                      | otherwise = descomprimeDisp t

-- | Descomprime o indice do Jogador responsável pelo Disparo.

descomprimeJogDisp :: String -- ^ 'Estado' comprimido.
                   -> Int -- ^ Indice do 'Jogador' que disparou.
descomprimeJogDisp (h:t) =  ord(h) - 48

-- | Descomprime a Posicao do Disparo.

descomprimePosDisp :: String -- ^ 'Estado' comprimido. 
                   -> Posicao -- ^ 'Posicao' do 'Disparo'
descomprimePosDisp l =  (fromDigits (tail(dropWhile (/= '\9') l)) , fromDigits (tail(dropWhile (/= '\8') l)))

-- | Descomprime Tick.

descomprimeTick :: String -- ^ 'Estado' comprimido.
                -> Int -- ^ 'Tick' do 'Choque' descomprimido.
descomprimeTick l =  fromDigits (drop 2 (dropWhile (/= ';') l))

-- * Funções auxiliares

-- | Função que remove os últimos elementos de uma lista conforme uma condição.

takeWhile' :: String -- ^ String inicial. 
           -> String -- ^ String sem os últimos elementos.
takeWhile' [] = []
takeWhile' (x:xs) 
            | elem x "EDBC" ==False = x:takeWhile' xs
            | otherwise = []

-- | Função que remove os últimos elementos de uma lista conforme uma condição.

takeWhile'' :: String -- ^ String inicial. 
            -> String -- ^ String sem os últimos elementos.
takeWhile'' [] = []
takeWhile'' (x:xs) 
            | elem x ".,;" ==False = x:takeWhile'' xs
            | otherwise = []

-- | Função que remove os últimos elementos de uma lista conforme uma condição.

takeWhileElem :: String -- ^ String inicial. 
              -> String -- ^ String sem os últimos elementos.
takeWhileElem [] = []
takeWhileElem (x:xs) 
            | elem x "ivd" ==False = x:takeWhileElem xs
            | otherwise = []

-- | Função que remove os primeiros elementos de uma lista conforme uma condição.

dropWhileElem :: String -- ^ String inicial.
              -> String -- ^ String sem os primeiros elementos.
dropWhileElem [] = []
dropWhileElem xs@(x:xs')
            | elem x "ivd" == False = dropWhileElem xs'
            | otherwise =  xs

-- | Função que remove os primeiros elementos de uma lista conforme uma condição.

dropWhileElem' :: String -- ^ String inicial.
               -> String -- ^ String sem os primeiros elementos.
dropWhileElem' [] = []
dropWhileElem' xs@(x:xs')
            | elem x "0123456789" == False =  dropWhileElem' xs'
            | otherwise =  xs


-- | Converte os primeiros digitos de uma string num Inteiro.

fromDigits :: String -- ^ String inical. 
           -> Int -- ^ Inteiro resultante.
fromDigits [] = 0
fromDigits (h:t) | isDigit h = ((ord h) - 48)*10^(compNum t) + fromDigits t
                 | otherwise = 0

