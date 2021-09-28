-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g103 where

import LI11819
import Tarefa0_2018li1g103
import Tarefa2_2018li1g103
import Tarefa1_2018li1g103
import Tarefa3_2018li1g103
import Tarefa4_2018li1g103
import Data.List


-- * Relatorio

-- | Nesta tarefa, o objetivo é definir um robot que seja capaz de jogar contra o jogador. 
-- Para que tal seja possível é necessário torná-lo o mais próximo possível de um jogador humano, de modo a que faça decisões de acordo com o que está a decorrer no jogo.
-- Neste sentido, começamos por definir uma lista de funções obrigatórias para que o bot não se limitasse a ficar parado. 
-- Assim sendo, tornámos o bot capaz de se desviar de balas que se aproximam ou de as destruir caso fosse mais adequado, de seguida fizemos com que ele pudesse aproximar-se de outros jogadores
-- e por fim tornámo-lo capaz de disparar para destruir blocos destrutíveis ou para atacar outros jogadores.
-- Entre estas funções a que foi mais difícil foi a do movimento uma vez que não fomos capazes de implementar uma maneira do bot se desviar de paredes de modo a que se pudesse aproximar de outros jogadores sem ficar encravado.
-- Por isso, o bot é capaz de se desviar de balas e de atacar outros jogadores, no entanto em mapas com muitos blocos indestrutíveis é possível que ele não consiga encontrar um caminho até outros jogadores.
-- Em suma, o robot é completamente funcional, tendo apenas alguns problemas para mapas específicos.
 
relatorio6 :: String -- ^ O Relatorio da Tarefa6
relatorio6 = "Relatorio6"

-- * Bot

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.

bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n (Estado mapa jog disp) | laser bot jogSBot mapa && botlasers > 0 = Just (Dispara Laser)
                             | blocoDestrutivel bot mapa = Just (Dispara Canhao)
                             | choque bot jogSBot && semChoque n choques && botchoques > 0 = Just (Dispara Choque)
                             | vemNestaDir posBot disp = dispPerigoso bot disp jogSBot mapa
                             | olhaPraMim posBot jogSBot && escolheLaser disp /= [] = antecipaLaser bot jogSBot
                             | colideJogador (DisparoCanhao n posBot dirBot) jogSBot mapa = Just (Dispara Canhao)
                             | useiChoque n disp && verificaLaser n jog = Just (Dispara Laser)
                             | useiChoque n disp = Just (Dispara Canhao)
                             | otherwise = aproximaJog bot maisProx n canhoes jog mapa
                             where bot = encontraIndiceLista n jog
                                   posBot = posicaoJogador bot
                                   dirBot = direcaoJogador bot
                                   jogSBot = delete bot jog
                                   maisProx = jogMaisProx posBot jogSBot jogSBot 1000 (5,5)
                                   canhoes = retiraDisparoCanhao disp
                                   choques = escolheChoque disp
                                   botlasers = lasersJogador bot
                                   botchoques = choquesJogador bot


-- | Verifica se o bot está a usar Choque

useiChoque :: Int -- ^ Indice do bot
           -> [Disparo] -- ^ Lista de 'Disparo's
           -> Bool -- ^ 'True' quando o bot está a usar o choque
useiChoque n [] = False
useiChoque n ((DisparoChoque n1 tick):ts) | (n == n1) = True
                                          | otherwise = useiChoque n ts  
useiChoque n ((DisparoCanhao n1 (l,c) dir ):ts) | (n == n1) = False
                                                | otherwise = useiChoque n ts 
useiChoque n ((DisparoLaser n1 (l,c) dir ):ts) | (n == n1) = False
                                               | otherwise = useiChoque n ts                                                 


-- | Verifica se o Bot tem Lasers (numa lista de jogadores)

verificaLaser :: Int -- ^ Indice Bot 
              -> [Jogador] -- ^ Lista de 'Jogador'es 
              -> Bool -- ^ 'True' se o Bot tiver lasers
verificaLaser n [] = False
verificaLaser n (h:t) | temLaser(encontraIndiceLista n (h:t)) = True
                      | otherwise = False


-- | Verifica se um jogador tem Lasers
 
temLaser :: Jogador -- ^ O 'Jogador' 
         -> Bool -- ^ 'True' se o jogador tiver lasers
temLaser (Jogador (l,c) dir v las ch) | (las > 0) = True
                                      | otherwise = False


-- | Verifica se existe algum adversário ao alcance do Jogador

olhaPraMim :: Posicao -- ^ 'Posicao' no 'Mapa'
           -> [Jogador] -- ^ Lista de 'Jogador'es
           -> Bool -- ^ 'True' se houver Jogadores no alcance do Jogador
olhaPraMim (l,c) [] = False
olhaPraMim (l,c) ((Jogador (l1,c1) d v _ _):t) = 
                    case d of 
                      C -> l<l1 && (c==c1 || c==c1+1 || c==c1-1) && v>0 || olhaPraMim (l,c) t
                      B -> l>l1 && (c==c1 || c==c1+1 || c==c1-1) && v>0 || olhaPraMim (l,c) t
                      D -> (l==l1 || l==l1+1 || l==l1-1) && c>c1 && v>0 || olhaPraMim (l,c) t
                      E -> (l==l1 || l==l1+1 || l==l1-1) && c<c1 && v>0 || olhaPraMim (l,c) t


-- | Jogador desvia-se de um Adversário que possa utilizar um laser 

antecipaLaser :: Jogador -- ^ 'Jogador'  
              -> [Jogador] -- ^ Lista de 'Jogador'es 
              -> Maybe Jogada -- ^ Movimenta-se ou Não
antecipaLaser _ [] = Nothing
antecipaLaser j@(Jogador (l,c) d _ _ _) ((Jogador (l1,c1) _ _ _ _):t) =
                         case d of
                          D | c1==c-1 -> Just (Movimenta D)
                            | otherwise -> antecipaLaser j t
                          E | c1==c+1 -> Just (Movimenta E)
                            | otherwise -> antecipaLaser j t
                          C | l1==l+1 -> Just (Movimenta C)
                            | otherwise -> antecipaLaser j t
                          B | l1==l-1 -> Just (Movimenta B)
                            | otherwise -> antecipaLaser j t


-- | Indica se um Jogador tem ou não choques disponiveis

semChoque :: Int -- ^ Indice de um Jogador
          -> [Disparo] -- ^ Lista de 'Disparo's
          -> Bool -- ^ 'True' se um Jogador não tiver Choques
semChoque _ [] = True
semChoque n ((DisparoChoque n1 _):t) | n==n1 = False
                                     | otherwise = semChoque n t


-- | Indica se há um choque que possa afetar o Jogador

choque :: Jogador -- ^ 'Jogador'
       -> [Jogador] -- ^ Lista de 'Jogador'es
       -> Bool -- ^ 'True' se o jogador corre perigo de ser atingido por um choque
choque _ [] = False
choque j@(Jogador (l,c) _ _ _ _) ((Jogador (l1,c1) _ v _ _):t) = v>0 && y<=3 && x<=3 || choque j t
                                                          where (y,x) = posChoque (l,c) (l1,c1)


-- | Devolve lista de Posicao conforme a direção do Jogador

dispPos :: Jogador -- ^ 'Jogador' 
        -> [Posicao] -- ^ Lista de 'Jogador'es
dispPos (Jogador (l,c) d _ _ _) =
             case d of 
               D -> [(l,c+2), (l+1,c+2), (l-1,c+2), (l-2,c+2), (l+2,c+2)]
               E -> [(l,c-2), (l+1,c-2), (l-1,c-2), (l-2,c-2), (l+2,c-2)]
               B -> [(l+2,c-1), (l+2,c), (l+2,c+1), (l+2,c-2), (l+2,c+2)]
               C -> [(l-2,c-1), (l-2,c), (l-2,c+1), (l-2,c-2), (l-2,c+2)]


-- | Indica se o Jogador nao se precisa de desviar de uma bala

naoMoveParaBala :: Jogador -- ^ 'Jogador' 
                -> Int -- ^ Indice de um 'Jogador'
                -> [Disparo] -- ^ Lista de 'Disparo's
                -> Bool -- ^ 'True' se não se precisar de desviar
naoMoveParaBala _ _ [] = True
naoMoveParaBala j n ((DisparoCanhao n1 p d1):t) = not(elem p (dispPos j)) && n/=n1 && naoMoveParaBala j n t

-- | Encontra o adversário mais próximo

jogMaisProx :: Posicao -- ^ 'Posicao' 
            -> [Jogador] -- ^ Lista de 'Jogador'es
            -> [Jogador] -- ^ Lista de 'Jogador'es
            -> Int -- ^ Indice de um 'Jogador'
            -> Posicao -- ^ 'Posicao' do Jogador
            -> Maybe Jogador -- ^ Talvez um 'Jogador'
jogMaisProx _ [] b _ (l,c) = encontraPos (l,c) b
                        where encontraPos (l,c) [] = Nothing
                              encontraPos p (h@(Jogador p1 _ _ _ _):t) | p==p1 = Just h
                                                                       | otherwise = encontraPos p t 
jogMaisProx (l,c) jog@((Jogador (l1,c1) _ v _ _):t) b n (p1,p2) | dJog <= n && v>0 = jogMaisProx (l,c) t b dJog (l1,c1)
                                                                | otherwise = jogMaisProx (l,c) t b n (p1,p2)
                                                        where dJog = abs (l1-l) + abs (c-c1)

-- | Bot aproxima-se do adversário

aproximaJog :: Jogador -- ^ 'Jogador'
            -> Maybe Jogador -- ^ Talvez um 'Jogador'
            -> Int -- ^ Indice de um 'Jogador'
            -> [Disparo] -- ^ Lista de 'Disparo's
            -> [Jogador] -- ^ Lista de 'Jogador'es
            -> Mapa -- ^ 'Mapa' do Jogo
            -> Maybe Jogada -- ^ Talvez se realize uma Jogada
aproximaJog _ Nothing _ _ _ _ = Nothing
aproximaJog h@(Jogador (l,c) _ _ _ _) (Just (Jogador (l1,c1) _ _ _ _)) n disp jog mapa | l1-1>l && podeMoverB B jog mapa (l,c) && naoMoveParaBala h n disp  = Just (Movimenta B)
                                                                                       | l1+1<l && podeMoverC C jog mapa (l,c) && naoMoveParaBala h n disp  = Just (Movimenta C)
                                                                                       | c1-1>c && podeMoverD D jog mapa (l,c) && naoMoveParaBala h n disp  = Just (Movimenta D)
                                                                                       | c1+1<c && podeMoverE E jog mapa (l,c) && naoMoveParaBala h n disp  = Just (Movimenta E)
                                                                                       | elem (l1,c1) [(l,c+2),(l-1,c+2),(l+1,c+2)] = Just (Movimenta D)
                                                                                       | elem (l1,c1) [(l,c-2),(l-1,c-2),(l+1,c-2)] = Just (Movimenta E)
                                                                                       | elem (l1,c1) [(l+2,c-1),(l+2,c),(l+2,c+1)] = Just (Movimenta B)
                                                                                       | elem (l1,c1) [(l-2,c),(l-2,c+1),(l-2,c-1)] = Just (Movimenta C)
                                                                                       | l1-1>l && c<c1 && podeMoverD D jog mapa (l,c) && naoMoveParaBala h n disp  = Just (Movimenta D)
                                                                                       | l1+1<l && c1<c && podeMoverE E jog mapa (l,c) && naoMoveParaBala h n disp  = Just (Movimenta E)
                                                                                       | c1-1>c && l1<l && podeMoverC C jog mapa (l,c) && naoMoveParaBala h n disp  = Just (Movimenta C)
                                                                                       | c1+1<c && l1>l && podeMoverB B jog mapa (l,c) && naoMoveParaBala h n disp  = Just (Movimenta B)
                                                                                       | l1-1>l && podeMoverD D jog mapa (l,c) = Just (Movimenta D)
                                                                                       | l1+1<l && podeMoverE E jog mapa (l,c) = Just (Movimenta E)
                                                                                       | c1-1>c && podeMoverB B jog mapa (l,c) = Just (Movimenta B)
                                                                                       | c1+1<c && podeMoverC C jog mapa (l,c) = Just (Movimenta C)
                                                                                       | otherwise = Nothing

-- | Expande a Posicao de um Disparo Canhao

expandeCanhaoPos :: Disparo -- ^ 'Disparo' Canhao 
                 -> Mapa -- ^ 'Mapa' do Jogo
                 -> [Posicao] -- ^ Lista de 'Posicao'
expandeCanhaoPos (DisparoCanhao n (l,c) C) mapa | encontraPosicaoMatriz (l,c) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (l,c+1) mapa /= Bloco Indestrutivel = (l,c):(l+1,c):(l-1,c):expandeCanhaoPos (DisparoCanhao n (l-1,c) C) mapa
                                                | otherwise = []
expandeCanhaoPos (DisparoCanhao n (l,c) D) mapa | encontraPosicaoMatriz (l,c+1) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (l+1,c+1) mapa /= Bloco Indestrutivel = (l,c):(l+1,c):(l-1,c):expandeCanhaoPos (DisparoCanhao n (l,c+1) D) mapa
                                                | otherwise = []
expandeCanhaoPos (DisparoCanhao n (l,c) B) mapa | encontraPosicaoMatriz (l+1,c+1) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (l+1,c) mapa /= Bloco Indestrutivel = (l,c):(l,c+1):(l,c-1):expandeCanhaoPos (DisparoCanhao n (l+1,c) B) mapa
                                                | otherwise = []
expandeCanhaoPos (DisparoCanhao n (l,c) E) mapa | encontraPosicaoMatriz (l+1,c) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (l,c) mapa /= Bloco Indestrutivel = (l,c):(l,c+1):(l,c-1):(expandeCanhaoPos) (DisparoCanhao n (l,c-1) E) mapa
                                                | otherwise = []
expandeCanhaoPos x mapa = []


-- | Verifica se algum jogador está nas posições afetadas pelo laser caso o Bot dispare

laser :: Jogador -- ^ Um 'Jogador'
      -> [Jogador] -- ^ Lista de 'Jogador'es
      -> Mapa -- ^ 'Mapa' do Jogo
      -> Bool -- ^ 'True' se houverem Jogadores nessas posições
laser _ [] _ = False
laser j@(Jogador (l,c) d _ _ _) ((Jogador (l1,c1) d1 v _ _):t) mapa =
                     case d of
                      B -> c==c1 && l1>l && elem (l1,c1) (expandeLaserPos (DisparoLaser 1 (l,c) B) mapa) && v>0 || laser j t mapa
                      C -> c==c1 && l>l1 && elem (l1,c1) (expandeLaserPos (DisparoLaser 1 (l,c) C) mapa) && v>0 || laser j t mapa
                      D -> l==l1 && c1>c && elem (l1,c1) (expandeLaserPos (DisparoLaser 1 (l,c) D) mapa) && v>0 || laser j t mapa
                      E -> l==l1 && c>c1 && elem (l1,c1) (expandeLaserPos (DisparoLaser 1 (l,c) E) mapa) && v>0 || laser j t mapa


-- | Dado um Disparo Canhao devolve a propagação desse canhão, numa lista com todas essas posições

avanca :: Disparo -- ^ 'Disparo' Canhao 
      -> Int -- ^ Inteiro
      -> [Posicao] -- ^ Lista de 'Posicao' do canhão
avanca (DisparoCanhao _ (l,c) d) 0 = []
avanca (DisparoCanhao n1 (l,c) d) n = 
               case d of
                D -> (l,c+2):(l+1,c+2):(l-1,c+2):avanca (DisparoCanhao n1 (l,c+1) D) (n-1)
                E -> (l,c-2):(l+1,c-2):(l-1,c-2):avanca (DisparoCanhao n1 (l,c-1) E) (n-1)
                C -> (l-2,c-1):(l-2,c+1):(l-2,c):avanca (DisparoCanhao n1 (l-1,c) C) (n-1)
                B -> (l+2,c+1):(l+2,c-1):(l+2,c):avanca (DisparoCanhao n1 (l+1,c) B) (n-1)


-- | Verifica se um Disparo irá atingir o Bot

colideJogador :: Disparo -- ^ Um 'Disparo' 
              -> [Jogador] -- ^ Lista de 'Jogador'es
              -> Mapa -- ^ 'Mapa' do Jogo
              -> Bool -- ^ 'True' se atingir o Bot
colideJogador _ [] _ = False
colideJogador b (h:t) mapa = elem (posicaoJogador h) (avanca b 4) && vidasJogador h > 0 && elem (posicaoJogador h) (expandeCanhaoPos b mapa) || colideJogador b t mapa


-- | Verifica se o Bot está seguro (em relação aos disparos) e realiza uma jogada em função das condições em que se encontra

dispPerigoso :: Jogador -- ^ O Bot 
             -> [Disparo] -- ^ Lista de 'Disparo's
             -> [Jogador] -- ^ Lista de 'Jogador'es
             -> Mapa -- ^ 'Mapa' do Jogo
             -> Maybe Jogada -- ^ Talvez realize uma Jogada
dispPerigoso _ [] _ _ = Nothing
dispPerigoso b@(Jogador (l,c) d1 _ _ _) (h@(DisparoCanhao _ (l1,c1) d):t) jog mapa | c==c1+1 && l1-l<=4 && l1-l>0 && d==C && podeMoverD D jog mapa (l,c) = Just (Movimenta D)
                                                                                   | c==c1+1 && l-l1<=4 && l-l1>0 && d==B && podeMoverD D jog mapa (l,c) = Just (Movimenta D)
                                                                                   | c==c1+1 && l-l1<=4 && l-l1>0 && d==B = Just (Movimenta E)
                                                                                   | c==c1+1 && l1-l<=4 && l1-l>0 && d==C = Just (Movimenta E)
                                                                                   | c==c1-1 && l-l1<=4 && l-l1>0 && d==B && podeMoverE E jog mapa (l,c) = Just (Movimenta E)
                                                                                   | c==c1-1 && l1-l<=4 && l1-l>0 && d==C && podeMoverE E jog mapa (l,c) = Just (Movimenta E)
                                                                                   | c==c1-1 && l1-l<=4 && l1-l>0 && d==C = Just (Movimenta D)
                                                                                   | c==c1-1 && l-l1<=4 && l-l1>0 && d==B = Just (Movimenta D)
                                                                                   | c == c1 && abs(l-l1)<=4 && d==B && d1==C = Just (Dispara Canhao)
                                                                                   | c == c1 && abs(l-l1)<=4 && d==C && d1==B = Just (Dispara Canhao)
                                                                                   | c == c1 && l1-l<=4 && l1-l>0 && d==C && podeMoverD D jog mapa (l,c) = Just (Movimenta D)
                                                                                   | c == c1 && l-l1<=4 && l-l1>0 && d==B && podeMoverD D jog mapa (l,c) = Just (Movimenta D)
                                                                                   | c == c1 && l-l1<=4 && l-l1>0 && d==B = Just (Movimenta E)
                                                                                   | c == c1 && l1-l<=4 && l1-l>0 && d==C = Just (Movimenta E)
                                                                                   | l==l1-1 && c-c1<=4 && c-c1>0 && d==D && podeMoverC C jog mapa (l,c) = Just (Movimenta C)
                                                                                   | l==l1-1 && c1-c<=4 && c1-c>0 && d==E && podeMoverC C jog mapa (l,c) = Just (Movimenta C)
                                                                                   | l==l1-1 && c-c1<=4 && c-c1>0 && d==D = Just (Movimenta B)
                                                                                   | l==l1-1 && c1-c<=4 && c1-c>0 && d==E = Just (Movimenta B)
                                                                                   | l==l1+1 && c-c1<=4 && c-c1>0 && d==D && podeMoverB B jog mapa (l,c) = Just (Movimenta B)
                                                                                   | l==l1+1 && c1-c<=4 && c1-c>0 && d==E && podeMoverB B jog mapa (l,c) = Just (Movimenta B)
                                                                                   | l==l1+1 && c-c1<=4 && c-c1>0 && d==D = Just (Movimenta C)
                                                                                   | l==l1+1 && c1-c<=4 && c1-c>0 && d==E = Just (Movimenta C)
                                                                                   | l == l1 && abs(c-c1)<=4 && d==D && d1==E = Just (Dispara Canhao)
                                                                                   | l == l1 && abs(c-c1)<=4 && d==E && d1==D = Just (Dispara Canhao)
                                                                                   | l==l1 && c-c1<=4 && c-c1>0 && d==D && podeMoverC C jog mapa (l,c) = Just (Movimenta C)
                                                                                   | l==l1 && c1-c<=4 && c1-c>0 && d==E && podeMoverC C jog mapa (l,c) = Just (Movimenta C)
                                                                                   | l==l1 && c-c1<=4 && c-c1>0 && d==D = Just (Movimenta B)
                                                                                   | l==l1 && c1-c<=4 && c1-c>0 && d==E = Just (Movimenta B)
                                                                                   | otherwise = dispPerigoso b t jog mapa
dispPerigoso a (h:t) b c = dispPerigoso a t b c


-- | Verifica se existe um Bloco Destrutivel perto do Bot

blocoDestrutivel :: Jogador -- ^ O Bot
                -> Mapa -- ^ 'Mapa' do Jogo
                -> Bool -- ^ 'True' se houver Bloco Destrutivel
blocoDestrutivel (Jogador (l,c) C _ _ _) mapa   = encontraPosicaoMatriz (l-1,c) mapa == Bloco Destrutivel || encontraPosicaoMatriz (l-1,c+1) mapa == Bloco Destrutivel 
blocoDestrutivel (Jogador (l,c) D _ _ _) mapa   = encontraPosicaoMatriz (l,c+2) mapa == Bloco Destrutivel || encontraPosicaoMatriz (l+1,c+2) mapa == Bloco Destrutivel 
blocoDestrutivel (Jogador (l,c) B _ _ _) mapa   = encontraPosicaoMatriz (l+2,c+1) mapa == Bloco Destrutivel || encontraPosicaoMatriz (l+2,c) mapa == Bloco Destrutivel 
blocoDestrutivel (Jogador (l,c) E _ _ _) mapa   = encontraPosicaoMatriz (l+1,c-1) mapa == Bloco Destrutivel || encontraPosicaoMatriz (l,c-1) mapa == Bloco Destrutivel 


-- | Verifica se um disparo vem na direção do Bot

vemNestaDir :: Posicao -- ^ 'Posicao' do Bot
            -> [Disparo] -- ^ Lista de 'Disparo's
            -> Bool -- ^ 'True' se vier um disparo na direcao do Bot
vemNestaDir _ [] = False
vemNestaDir (l,c) ((DisparoCanhao _ (l1,c1) d):t) = ((l==l1 || l==l1+1 ||l==l1-1) && c-c1<=4 && d==E)
                                                  || ((l==l1 || l==l1+1 ||l==l1-1) && c1-c<=4 && d==D)
                                                  || ((c==c1 || c==c1+1 ||c==c1-1) && l-l1<=4 && d==C)
                                                  || ((c==c1 || c==c1+1 ||c==c1-1) && l1-l<=4 && d==B)
                                                  || vemNestaDir (l,c) t
vemNestaDir (l,c) (_:t) = vemNestaDir (l,c) t