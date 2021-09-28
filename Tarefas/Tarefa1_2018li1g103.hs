-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g103 where

import LI11819
import Tarefa0_2018li1g103

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30]
         where t1 = [Move C]
               t2 = [Move B]
               t3 = [Move D]
               t4 = [Move E]
               t5 = [MudaTetromino]
               t6 = [MudaTetromino, MudaTetromino]
               t7 = [MudaTetromino, MudaTetromino, MudaTetromino]
               t8 = [MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino]
               t9 = [MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino]
               t10 = [MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino]
               t11 = [MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino]
               t12 = [Roda]
               t13 = [Roda, Roda]
               t14 = [Roda, Roda, Roda]
               t15 = [Roda, Roda, Roda, Roda]
               t16 = [Desenha]
               t17 = [MudaParede]
               t18 = [MudaParede, MudaParede]
               t19 = [Roda, Desenha]
               t20 = [Roda, Roda, Desenha]
               t21 = [Roda, Roda, Roda, Desenha]
               t22 = [Roda, Roda, Roda, Roda, Desenha]
               t23 = [Move E, Move E, Move E, Move E]
               t24 = [MudaTetromino, Roda, Desenha]
               t25 = [MudaTetromino, MudaParede, Desenha]
               t26 = [MudaTetromino, Desenha, Roda, Desenha]
               t27 = [Desenha, Desenha]
               t28 = [Roda, Desenha, Move D, MudaTetromino, MudaParede, Desenha]
               t29 = [MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, Desenha]
               t30 = [MudaParede, MudaTetromino, Roda, Desenha]
t28 = [Desenha, Roda, Desenha]
-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
         
instrucao MudaTetromino e@Editor{tetrominoEditor = t} | (t == I) = e{tetrominoEditor = J} -- {A instrucao recebe um editor que transforma o tetronimo "I" no tetronimo "J"} 
                                                      | (t == J) = e{tetrominoEditor = L} -- {A instrucao recebe um editor que transforma o tetronimo "J" no tetronimo "L"}
                                                      | (t == L) = e{tetrominoEditor = O} -- {A instrucao recebe um editor que transforma o tetronimo "L" no tetronimo "O"}
                                                      | (t == O) = e{tetrominoEditor = S} -- {A instrucao recebe um editor que transforma o tetronimo "O" no tetronimo "S"}
                                                      | (t == S) = e{tetrominoEditor = T} -- {A instrucao recebe um editor que transforma o tetronimo "S" no tetronimo "T"}
                                                      | (t == T) = e{tetrominoEditor = Z} -- {A instrucao recebe um editor que transforma o tetronimo "T" no tetronimo "Z"}
                                                      | otherwise = e{tetrominoEditor = I} -- {A instrucao recece um editor que transforma o tetronimo "Z" no tetronimo "I"}
                                                    
instrucao Roda e@Editor{direcaoEditor = t} | (t == C) = e{direcaoEditor = D} -- {Se o tetronimo estiver orientado para cima "C", o editor orienta-lo-a para a direita "D"} 
                                           | (t == B) = e{direcaoEditor = E} -- {Se o tetronimo estiver orientado para a direita "D", o editor orienta-lo-a para baixo "B"}
                                           | (t == D) = e{direcaoEditor = B} -- {Se o tetronimo estiver orientado para baixo "B", o editor orienta-lo-a para a esquerda "E"}
                                           | (t == E) = e{direcaoEditor = C} -- {Se o tetronimo estiver orientado para a esquerda "E", o editor orienta-lo-a para cima "C"}                                                     

instrucao MudaParede e@Editor{paredeEditor = t} | (t == Indestrutivel) = e{paredeEditor = Destrutivel} -- {Se a parede do tetronimo for do tipo "Indestrutivel", o editor transformara a parede deste no tipo "Destrutivel" 
                                                | otherwise = e{paredeEditor = Indestrutivel} -- {Se a parede do tetronimo for do tipo "Destrutivel", o editor transformara a parede deste no tipo "Indestrutivel" 

instrucao Desenha (Editor (l,c) d tetro parede m) = (Editor (l,c) d tetro parede (atualizaPosMapa (tetroParaMatriz tetro d) (0,0) (l,c) (Bloco parede) m))


instrucao (Move t) e@Editor{posicaoEditor = (x,y)} | (t == C) && x>=2 = e{posicaoEditor = (x-1,y)} -- {Se o tetronimo se mover na direcao "C", a sua posicao subira uma unidade}
                                                   | (t == B) = e{posicaoEditor = (x+1,y)} -- {Se o tetronimo se mover na direcao "B", a sua posicao ira descer uma unidade} 
                                                   | (t == D) = e{posicaoEditor = (x,y+1)} -- {Se o tetronimo se mover na direcao "D", este andara uma casa para a direita}
                                                   | (t == E) && y>=2 = e{posicaoEditor = (x,y-1)} -- {Se o tetronimo se mover na direcao "E", este andara uma casa para a esquerda}   
                                                   | otherwise = e
-- | Com base num tetronimo e na sua Direcao, devolve a matriz correspondende ao tetronimo 
--
-- __NB:__ Foram utilizadas as funções auxiliares 'rodaMatriz', 'inverteMatrizH' e 'inverteMatrizV' 
tetroParaMatriz:: Tetromino -- ^ 'Tetronimo'
               -> Direcao -- ^ 'Direcao' do 'Tetronimo'
               -> Matriz Bool -- ^ Matriz do 'Tetronimo' resultante
tetroParaMatriz t C = tetrominoParaMatriz t
tetroParaMatriz t D = rodaMatriz (tetrominoParaMatriz t)
tetroParaMatriz t B = inverteMatrizH (inverteMatrizV (tetrominoParaMatriz t))
tetroParaMatriz t E = inverteMatrizV (inverteMatrizH (rodaMatriz (tetrominoParaMatriz t)))

-- | Devolve o mapa com a matriz do tetronimo alterada
--
-- __NB:__ Foram usadas as funções auxiliares 'encontraPosicaoMatriz' e 'atualizaPosicaoMatriz'
atualizaPosMapa :: Matriz Bool -- ^ Matriz correspondente a um 'Tetronimo'
                -> Posicao -- ^ 'Posicao' do 'Tetronimo' na Matriz 
                -> Posicao -- ^ 'Posicao' do 'Tetronimo' no 'Mapa'   
                -> Peca -- ^ Tipo de Peca  
                -> Mapa -- ^ 'Mapa' inicial
                -> Mapa -- ^ 'Mapa' modificado
atualizaPosMapa [] (lm,cm) (l,c) p m = m
atualizaPosMapa (h:t) (lm,cm) (l,c) p m | lm>(length (h:t) -1) = m
                                        | cm>(length h -1) = atualizaPosMapa (h:t) (lm+1,0) (l+1,c - (length h)) p m
                                        | (encontraPosicaoMatriz (lm,cm) (h:t)) = atualizaPosMapa (h:t) (lm,cm+1) (l,c+1) p (atualizaPosicaoMatriz (l,c) p m)
                                        | otherwise = atualizaPosMapa (h:t) (lm,cm+1) (l,c+1) p m

-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] e = e
instrucoes (h:t) e = instrucoes t (instrucao h e) -- {Primeiro recebe a primeira instrucao da lista de instrucao dada e o editor dado, dando um novo editor, seguindo por fazer o mesmo para o resto da lista de intrucao} 


-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (x,y) = [h] ++ replicate (x-2) f ++ [h] -- {Replicamos as linhas nao pertencentes as extremidades da matriz pelo numero de linhas da Matriz menos a primeira e ultima linha, adicionado no final as linhas constituidas apenas por "Blocos Indestrutiveis" no inicio e no fim} 
                     where h = replicate y (Bloco Indestrutivel) -- {Sabendo que a primeira e ultima linha da matriz serao ocupadas apenas por "Blocos Indestrutiveis", iremos replicar os "Blocos Indestrutiveis" pelo numero de colunas da matriz (y)}
                           f = [Bloco Indestrutivel]++replicate (y-2) (Vazia)++[Bloco Indestrutivel] -- {Sabendo que as linhas que nao sejam nem a primeira nem a ultima contem nas extremidades "Blocos Indestrutiveis" e no resto "Vazia", iremos replicar a "Vazia" pelo numero de colunas menos as colunas das extremidades (2), e no final adicionar, quer no inicio, quer no fim, os "Blocos Indestrutiveis"}
                      

-- | Cria um 'Editor' inicial.

  --
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial [] = Editor (posicaoInicial []) C I Indestrutivel (mapaInicial(dimensaoInicial []))
editorInicial (h:t) = Editor
    { posicaoEditor = posicaoInicial (h:t)
    , direcaoEditor = C 
    , tetrominoEditor = I 
    , paredeEditor = Indestrutivel 
    , mapaEditor = mapaInicial (dimensaoInicial (h:t))
    }

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'. 
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi l = mapaEditor (instrucoes l (editorInicial l)) -- {Dada uma lista de "Instrucao", iremos atualizar o mapa para que apresente as alterações realizadas pelas instrucoes aplicadas} 
