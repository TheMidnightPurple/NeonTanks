-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g103 where

import LI11819
import Tarefa0_2018li1g103
import Tarefa1_2018li1g103
import Tarefa2_2018li1g103
import Tarefa3_2018li1g103
import Data.List
-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12]
         where t1 = (Estado (mapaInicial (10,10)) [(Jogador (7,5) C 2 3 4),(Jogador (7,1) D 2 3 5)] [(DisparoLaser 1 (7,3) D)])
               t2 = (Estado (mapaInicial (10,10)) [(Jogador (7,5) C 2 3 4),(Jogador (7,1) D 2 3 5), (Jogador (3,1) D 2 3 5)] [(DisparoLaser 1 (7,3) D), (DisparoLaser 2 (4,4) D)])
               t3 = (Estado (mapaInicial (10,10)) [(Jogador (7,5) C 2 3 4),(Jogador (7,2) D 2 3 5)] [(DisparoChoque 1 2)])
               t4 = (Estado m [(Jogador (7,5) C 2 3 4),(Jogador (7,1) D 2 3 5), (Jogador (3,1) D 2 3 5)] [DisparoCanhao 1 (6,6) D, DisparoCanhao 1 (6,5) E])
               t5 = (Estado m [(Jogador (7,5) C 2 3 4),(Jogador (7,1) D 2 3 5), (Jogador (3,1) D 2 3 5)] [DisparoCanhao 1 (5,5) C])
               t6 = (Estado m [(Jogador (7,5) C 2 3 4),(Jogador (7,1) D 2 3 5), (Jogador (3,1) D 2 3 5)] [DisparoLaser 1 (5,5) C])
               t7 = (Estado m [(Jogador (7,5) C 2 3 4),(Jogador (7,1) D 2 3 5), (Jogador (3,1) D 2 3 5)] [DisparoCanhao 1 (5,1) D, DisparoLaser 1 (5,3) E])
               t8 = (Estado m [(Jogador (7,5) C 2 3 4),(Jogador (7,1) D 2 3 5), (Jogador (3,1) D 2 3 5)] [DisparoCanhao 1 (6,5) D, DisparoCanhao 1 (6,5) B])
               t9 = (Estado (mapaInicial (10,10)) [(Jogador (7,7) C 2 3 4),(Jogador (7,2) D 2 3 5)] [(DisparoCanhao 1 (6,6) D)])
               t10 = (Estado (mapaInicial (10,10)) [(Jogador (7,7) C 0 3 4),(Jogador (7,2) D 3 3 5)] [(DisparoCanhao 1 (6,6) D)])
               t11 = (Estado (mapaInicial (10,10)) [(Jogador (7,7) C 1 3 4),(Jogador (7,2) D 3 3 5)] [(DisparoChoque 1 1)])
               t12 = (Estado (mapaInicial (10,10)) [(Jogador (7,7) C 1 3 4),(Jogador (7,2) D 0 3 5)] [(DisparoCanhao 1 (1,1) C)])
-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.

tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- Laser

------ Expansão Laser  

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -- ^ O 'Estado' anterior.
           -> Estado -- ^ O 'Estado' após um 'Tick' com efeitos dos 'Laser's.
tickLasers (Estado mapa jog disp) = Estado (mapaPosLaserTdsDisparos (escolheLaser disp) mapa) (jogLas jog (posJogLaserTdsLasers (escolheLaser disp) mapa)) (dispColis (pegaDisparos disp) (escolheLaser disp) mapa)           

-- | Separa os Canhoes e os Choques dos Lasers.
pegaDisparos :: [Disparo] -- ^ Lista De 'Disparo's.
             -> [Disparo] -- ^ Lista De 'Disparo's resultante de remover os 'DisparoLaser'.        
pegaDisparos [] = []
pegaDisparos (h@(DisparoChoque _ _):t) = h:pegaDisparos t
pegaDisparos (h@(DisparoCanhao _ _ _):t) = h:pegaDisparos t
pegaDisparos (h:t) = pegaDisparos t

-- | Separa os Lasers dos Restantes 'Disparo's.
escolheLaser :: [Disparo] -- ^ Lista De 'Disparo's.
             -> [Disparo] -- ^ Lista De de 'DisparoLaser'. 
escolheLaser [] = []
escolheLaser (h@(DisparoLaser _ _ _):t) = h:escolheLaser t
escolheLaser (h:t) = escolheLaser t

-- | Cria a lista de 'Posicao' que um laser percorre.
expandeLaserPos :: Disparo -- ^ Disparo Laser.
                -> Mapa -- ^ 'Mapa' do 'Estado'.
                -> [Posicao] -- ^ Lista de 'Posicao' percorrida pelo laser.
expandeLaserPos (DisparoLaser n (l,c) C) mapa | encontraPosicaoMatriz (l,c) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (l,c+1) mapa /= Bloco Indestrutivel = (l,c):expandeLaserPos (DisparoLaser n (l-1,c) C) mapa
                                              | otherwise = []
expandeLaserPos (DisparoLaser n (l,c) D) mapa | encontraPosicaoMatriz (l,c+1) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (l+1,c+1) mapa /= Bloco Indestrutivel = (l,c):expandeLaserPos (DisparoLaser n (l,c+1) D) mapa
                                              | otherwise = []
expandeLaserPos (DisparoLaser n (l,c) B) mapa | encontraPosicaoMatriz (l+1,c+1) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (l+1,c) mapa /= Bloco Indestrutivel = (l,c):expandeLaserPos (DisparoLaser n (l+1,c) B) mapa
                                              | otherwise = []
expandeLaserPos (DisparoLaser n (l,c) E) mapa | encontraPosicaoMatriz (l+1,c) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (l,c) mapa /= Bloco Indestrutivel = (l,c):expandeLaserPos (DisparoLaser n (l,c-1) E) mapa
                                              | otherwise = []
expandeLaserPos x mapa = []

-- | Condições para que o laser continue a percorrer.
condicaoDisp' :: Posicao -- ^ 'Posicao' inicial do Laser.
              -> Direcao -- ^ 'Direcao' do Laser.
              -> Peca -- ^ 'Peca' desejada.
              -> Mapa -- ^ 'Mapa' do jogo.
              -> Bool
condicaoDisp' (l,c) C p mapa = encontraPosicaoMatriz (l,c) mapa == p || encontraPosicaoMatriz (l,c+1) mapa == p
condicaoDisp' (l,c) D p mapa = encontraPosicaoMatriz (l,c+1) mapa == p || encontraPosicaoMatriz (l+1,c+1) mapa == p
condicaoDisp' (l,c) E p mapa = encontraPosicaoMatriz (l+1,c) mapa == p || encontraPosicaoMatriz (l,c) mapa == p
condicaoDisp' (l,c) B p mapa = encontraPosicaoMatriz (l+1,c) mapa == p || encontraPosicaoMatriz (l+1,c+1) mapa == p


-- | Retorna o 'Mapa' após os efeitos de um laser para a 'Posicao' imediatamente a seguir.
mapaPosLaser :: Posicao -- ^ 'Posicao' do Laser.
             -> Direcao -- ^ 'Direcao' do Laser.
             -> Mapa -- ^ 'Mapa' do 'Estado'.
             -> Mapa -- ^ 'Mapa' resultante dos efeitos do laser.
mapaPosLaser (l,c) C mapa | encontraPosicaoMatriz (l,c) mapa == Bloco Destrutivel && encontraPosicaoMatriz (l,c+1) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia (atualizaPosicaoMatriz (l,c) Vazia mapa)
                          | encontraPosicaoMatriz (l,c) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia mapa
                          | encontraPosicaoMatriz (l,c+1) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia mapa
                          | otherwise = mapa

mapaPosLaser (l,c) D mapa | encontraPosicaoMatriz (l,c+1) mapa == Bloco Destrutivel && encontraPosicaoMatriz (l+1,c+1) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l+1,c+1) Vazia (atualizaPosicaoMatriz (l,c+1) Vazia mapa)
                          | encontraPosicaoMatriz (l,c+1) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia mapa
                          | encontraPosicaoMatriz (l+1,c+1) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l+1,c+1) Vazia mapa
                          | otherwise = mapa

mapaPosLaser (l,c) E mapa | encontraPosicaoMatriz (l+1,c) mapa == Bloco Destrutivel && encontraPosicaoMatriz (l,c) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l+1,c) Vazia (atualizaPosicaoMatriz (l,c) Vazia mapa)
                          | encontraPosicaoMatriz (l+1,c) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l+1,c) Vazia mapa
                          | encontraPosicaoMatriz (l,c) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia mapa
                          | otherwise = mapa

mapaPosLaser (l,c) B mapa | encontraPosicaoMatriz (l+1,c) mapa == Bloco Destrutivel && encontraPosicaoMatriz (l+1,c+1) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l+1,c+1) Vazia (atualizaPosicaoMatriz (l+1,c) Vazia mapa)
                          | encontraPosicaoMatriz (l+1,c) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l+1,c) Vazia mapa
                          | encontraPosicaoMatriz (l+1,c+1) mapa == Bloco Destrutivel = atualizaPosicaoMatriz (l+1,c+1) Vazia mapa
                          | otherwise = mapa

-- | Retorna o 'Mapa' após o efeito de um laser em todas as 'Posicao' que afeta.
mapaPosLaserTPos :: Disparo -- ^ Laser disparado.
                 -> [Posicao] -- ^ Lista de 'Posicao' que um laser afeta.
                 -> Mapa -- ^ 'Mapa' do 'Estado'.
                 -> Mapa -- ^ 'Mapa' resultante dos efeitos do laser.
mapaPosLaserTPos _ [] mapa = mapa
mapaPosLaserTPos h1@(DisparoLaser _ (l,c) d) (h:t) mapa = mapaPosLaserTPos h1 t (mapaPosLaser h d mapa)

-- | Retorna o 'Mapa' após o efeito de todos os lasers.
mapaPosLaserTdsDisparos :: [Disparo] -- ^ Lista de 'DisparoLaser'.
                        -> Mapa -- ^ 'Mapa' do 'Estado'.
                        -> Mapa -- ^ 'Mapa' resultante dos efeitos do laser.
mapaPosLaserTdsDisparos [] mapa = mapa
mapaPosLaserTdsDisparos (h@(DisparoLaser n (l,c) d):t) mapa = mapaPosLaserTdsDisparos t (mapaPosLaserTPos h (expandeLaserPos h mapa) mapa)

-- | Retorna a lista de 'Posicao' onde os 'Jogador'es sofrem dano de um laser.
posJogLaser :: Disparo -- ^ Laser disparado.
            -> Mapa -- ^ 'Mapa' do Estado
            -> [Posicao] -- ^ Lista de 'Posicao' onde um 'Jogador' pode ser afetado.
posJogLaser (DisparoLaser n (l,c) d) mapa | condicaoDisp' (l,c) d (Bloco Indestrutivel) mapa = []
                                          | d==D = (l+1,c+1):(l,c+1):(l-1,c+1):posJogLaser (DisparoLaser n (l,c+1) d) mapa
                                          | d==C = (l-1,c+1):(l-1,c):(l-1,c-1):posJogLaser (DisparoLaser n (l-1,c) d) mapa
                                          | d==B = (l+1,c+1):(l+1,c):(l+1,c-1):posJogLaser (DisparoLaser n (l+1,c) d) mapa
                                          | d==E = (l-1,c-1):(l,c-1):(l+1,c-1):posJogLaser (DisparoLaser n (l,c-1) d) mapa

-- | Retorna a lista de 'Posicao' onde os 'Jogador'es sofrem dano de um laser.
posJogLaserTdsLasers :: [Disparo] -- ^ Lista de Lasers.
                     -> Mapa -- ^ 'Mapa' do Estado
                     -> [Posicao] -- ^ Lista de 'Posicao' onde um 'Jogador' pode ser afetado. 
posJogLaserTdsLasers [] mapa = []
posJogLaserTdsLasers (h:t) mapa = (posJogLaser h mapa) ++ posJogLaserTdsLasers t mapa

-- | Remove vidas aos 'Jogador'es afetados por lasers.
jogLas :: [Jogador] -- ^ Lista de 'Jogador'es.
       -> [Posicao] -- ^ Lista de 'Posicao' afetada por lasers.
       -> [Jogador] -- ^ Lista de 'Jogador'es resultante.
jogLas [] _ = []
jogLas a [] = a
jogLas (h@(Jogador p d v las ch):t) ps | elem (posicaoJogador h) ps && v/=0 = (Jogador p d (v-1) las ch):jogLas t ps
                                       | otherwise = h:jogLas t ps

-- | Retorna a lista de 'Posicao' que um laser percorre.
posDispLaser :: Disparo -- ^ Laser disparado.
             -> Mapa -- ^ 'Mapa' do 'Estado'
             -> [Posicao] -- ^ Lista de 'Posicao' resultante.
posDispLaser (DisparoLaser n (l,c) d) mapa | condicaoDisp' (l,c) d (Bloco Indestrutivel) mapa = []
                                           | d==D = (l,c):posDispLaser (DisparoLaser n (l,c+1) d) mapa
                                           | d==C = (l,c):posDispLaser (DisparoLaser n (l-1,c) d) mapa
                                           | d==B = (l,c):posDispLaser (DisparoLaser n (l+1,c) d) mapa
                                           | d==E = (l,c):posDispLaser (DisparoLaser n (l,c-1) d) mapa

-- | Retorna a lista de 'Posicao' que todos os lasers percorrem.
posDispLaserTdsLasers :: [Disparo] -- ^ Lista de 'DisparoLaser'.
                      -> Mapa -- ^ 'Mapa' do 'Estado'
                      -> [Posicao] -- ^ Lista de 'Posicao' resultante.
posDispLaserTdsLasers [] mapa = []
posDispLaserTdsLasers (h:t) mapa = (posDispLaser h mapa) ++ posDispLaserTdsLasers t mapa

-- | Remove os canhoes que colidiram com os laser da lista de 'Disparo's.
dispColis :: [Disparo] -- ^ Lista de 'DisparoCanhao'.
          -> [Disparo] -- ^ Lista de 'DisparoLaser'.
          -> Mapa -- ^ 'Mapa' do 'Estado'
          -> [Disparo] -- ^ Lista de 'DisparoCanhao' resultante.
dispColis [] _ _ = []
dispColis a [] _ = a
dispColis (h@(DisparoCanhao n p d):t) las mapa | elem p (posDispLaserTdsLasers las mapa) =  dispColis t las mapa
                                               | otherwise = h:dispColis t las mapa 
dispColis (h:t) las mapa = h:dispColis t las mapa
                                               

-- Canhao

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.

tickCanhoes :: Estado -> Estado
tickCanhoes (Estado mapa jogadores disp) = Estado (listaCanhao (retiraDisparoCanhao disp) mapa) (posCanhoes (retiraDisparoCanhao disp) jogadores) ((canhaoLista (listaCanhaoMapa (colisaoDisp (retiraDisparoCanhao disp) jogadores mapa) mapa)) ++ (pegaDisparos3 disp))

--Mapa--
-- | Retorna o 'Mapa' após as colisões dos Canhões.
listaCanhao :: [Disparo] -- ^ Lista de 'DisparoCanhao'.
            -> Mapa -- ^ 'Mapa' do 'Estado'.
            -> Mapa -- ^ 'Mapa' após os efeitos dos canhões.
listaCanhao [] mapa = mapa
listaCanhao ((DisparoCanhao n p d):t) mapa = listaCanhao t (mapaPosLaser p d mapa)

--Jogadores--
posCanhao :: Disparo -> [Jogador] -> [Jogador]
posCanhao _ [] = []
posCanhao l1@(DisparoCanhao n (l,c) d) jog@(h@(Jogador p d1 v las ch):t) | d==D && elem p [(l,c+1),(l+1,c+1),(l-1,c+1),(l,c),(l-1,c),(l+1,c)] && v/=0 = (Jogador p d1 (v-1) las ch):posCanhao l1 t
                                                                         | d==E && elem p [(l,c-1),(l-1,c-1),(l+1,c-1),(l,c),(l-1,c),(l+1,c)] && v/=0 = (Jogador p d1 (v-1) las ch):posCanhao l1 t
                                                                         | d==C && elem p [(l-1,c),(l-1,c-1),(l-1,c+1),(l,c),(l,c+1),(l,c-1)] && v/=0 = (Jogador p d1 (v-1) las ch):posCanhao l1 t
                                                                         | d==B && elem p [(l+1,c),(l+1,c-1),(l+1,c+1),(l,c),(l,c+1),(l,c-1)] && v/=0 = (Jogador p d1 (v-1) las ch):posCanhao l1 t
                                                                         | otherwise = h:posCanhao l1 t
-- | Retorna a lista de 'Jogador'es resultante da remoção de vidas.
posCanhoes :: [Disparo] -- ^ Lista de 'DisparoCanhao'.
          -> [Jogador] -- ^ Lista de 'Jogador'es.
          -> [Jogador] -- ^ Lista de 'Jogador'es após remoção das vidas.
posCanhoes [] l = l
posCanhoes (h:t) l = posCanhoes t (posCanhao h l)

--Disparos--
colideJog :: [Disparo] -> [Jogador] -> [Disparo]
colideJog [] _ = []
colideJog (x:xs) l | posCanhao x l == l = x:colideJog xs l
                   | otherwise = colideJog xs l

-- | Separa os 'DisparoLaser' e os 'DisparoChoque' dos 'DisparoCanhao'.
pegaDisparos3 :: [Disparo] -- ^ Lista de 'Disparo'.
              -> [Disparo] -- ^ Lista de 'Disparo' sem canhões.
pegaDisparos3 [] = []
pegaDisparos3 (h@(DisparoLaser _ _ _):t) = h:pegaDisparos3 t
pegaDisparos3 (h@(DisparoChoque _ _):t) = h:pegaDisparos3 t
pegaDisparos3 (h:t) = pegaDisparos3 t

-- | Avança os 'DisparoCanhao' numa dada 'Direcao'.
canhaoLista :: [Disparo] -- ^ Lista de 'DisparoCanhao'.
            -> [Disparo] -- ^ Lista de 'DisparoCanhao' depois de se deslocarem.
canhaoLista [] = []
canhaoLista ((DisparoCanhao n (l,c) d):t)  | d==D = (DisparoCanhao n (l,c+1) d):canhaoLista t 
                                           | d==E = (DisparoCanhao n (l,c-1) d):canhaoLista t 
                                           | d==C = (DisparoCanhao n (l-1,c) d):canhaoLista t 
                                           | d==B = (DisparoCanhao n (l+1,c) d):canhaoLista t 

-- | Retorna a lista de 'Disparo' sem os canhões que colidiram com paredes.
listaCanhaoMapa :: [Disparo] -- ^ Lista de 'DisparoCanhao'.
                -> Mapa -- ^ 'Mapa' do 'Estado'.
                -> [Disparo] -- ^ Lista de 'DisparoCanhao' após colisões com paredes.
listaCanhaoMapa [] _ = []
listaCanhaoMapa (h@(DisparoCanhao n (l,c) C):t) mapa | encontraPosicaoMatriz (l,c) mapa == Vazia && encontraPosicaoMatriz (l,c+1) mapa == Vazia = h:listaCanhaoMapa t mapa
                                                     | otherwise = listaCanhaoMapa t mapa
listaCanhaoMapa (h@(DisparoCanhao n (l,c) D):t) mapa | encontraPosicaoMatriz (l,c+1) mapa == Vazia && encontraPosicaoMatriz (l+1,c+1) mapa == Vazia = h:listaCanhaoMapa t mapa
                                                     | otherwise = listaCanhaoMapa t mapa
listaCanhaoMapa (h@(DisparoCanhao n (l,c) B):t) mapa | encontraPosicaoMatriz (l+1,c+1) mapa == Vazia && encontraPosicaoMatriz (l+1,c) mapa == Vazia = h:listaCanhaoMapa t mapa
                                                     | otherwise = listaCanhaoMapa t mapa
listaCanhaoMapa (h@(DisparoCanhao n (l,c) E):t) mapa | encontraPosicaoMatriz (l+1,c) mapa == Vazia && encontraPosicaoMatriz (l,c) mapa == Vazia = h:listaCanhaoMapa t mapa
                                                     | otherwise = listaCanhaoMapa t mapa

embateCanhao :: [Disparo] -> [Disparo] -> [Jogador] -> Mapa -> [Disparo]
embateCanhao [] _ _ _ = []
embateCanhao (h:t) b x mapa | desaparece h (delete h b) || posCanhao h x /= x = embateCanhao t b x mapa
                            | otherwise = h:embateCanhao t b x mapa ---o primeiro sai mas os outros ficam

desaparece :: Disparo -> [Disparo] -> Bool
desaparece h [] = False
desaparece h@(DisparoCanhao n (l,c) d) ((DisparoCanhao n1 (l1,c1) d1):t) = c==c1 && l1-l==1 && d==C && d1 == B || c==c1 && l1-l==(-1) && d==B && d1 == C || l==l1 && c1-c==1 && d1==D && d==E || l==l1 && c1-c==(-1) && d1==E && d==D || (l,c)==(l1,c1) || desaparece h t


colisaoDisp :: [Disparo] -> [Jogador] -> Mapa -> [Disparo]
colisaoDisp [] _ _ = []
colisaoDisp l x mapa = embateCanhao (retiraDisparoCanhao l) (retiraDisparoCanhao l) x mapa
-- Cria Lista com apenas disparos canhao

-- | Separa os Canhoes dos Restantes 'Disparo's.
retiraDisparoCanhao :: [Disparo] -- ^ Lista de 'Disparo'.
                    -> [Disparo] -- ^ Lista de 'DisparoCanhao'.
retiraDisparoCanhao [] = []
retiraDisparoCanhao ((DisparoCanhao n (l,c) dir):t) = (DisparoCanhao n (l,c) dir):retiraDisparoCanhao t 
retiraDisparoCanhao (h:t) = retiraDisparoCanhao t

-- Choques

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -- ^ O 'Estado' anterior.
            -> Estado -- ^ O 'Estado' após um 'Tick' com efeitos dos Choques.
tickChoques (Estado mapa jog disp) = Estado mapa jog ((removeTick (escolheChoque disp)) ++ (pegaDisparos2 disp))

-- | Separa os Choques dos Restantes 'Disparo's.
escolheChoque :: [Disparo] -- ^ Lista de 'Disparo'.
              -> [Disparo] -- ^ Lista de 'DisparoChoque'.
escolheChoque [] = []
escolheChoque (h@(DisparoChoque _ _):t) = h:escolheChoque t
escolheChoque (h:t) = escolheChoque t

-- | Remove um tick aos 'DisparoChoque'.
removeTick :: [Disparo] -- ^ Lista de 'DisparoChoque'.
           -> [Disparo] -- ^ Lista de 'DisparoChoque' depois de dimuir um tick.
removeTick [] = []
removeTick ((DisparoChoque n tik):t) | (tik ==0) = removeTick t
                                     | otherwise = (DisparoChoque n (tik-1)):removeTick t

-- | Separa os 'DisparoChoque' dos restantes 'Disparo's.
pegaDisparos2 :: [Disparo] -- ^ Lista de 'Disparo'.
              -> [Disparo] -- ^ Lista de 'DisparoCanhao' e 'DisparoLaser'.
pegaDisparos2 [] = []
pegaDisparos2 (h@(DisparoLaser _ _ _):t) = h:pegaDisparos2 t
pegaDisparos2 (h@(DisparoCanhao _ _ _):t) = h:pegaDisparos2 t
pegaDisparos2 (_:t) = pegaDisparos2 t





-- | Mapa Com Alguns Blocos Destrutiveis Para Testes

m =
  [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
