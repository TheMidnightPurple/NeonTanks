-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import LI11819
import Tarefa0_2018li1g103
import Tarefa1_2018li1g103
import Tarefa2_2018li1g103 
import Tarefa3_2018li1g103
import Tarefa4_2018li1g103
import Tarefa6_2018li1g103 
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Data.Char
 

-- * Relatorio

-- | A Tarefa 5 trata-se de uma Tarefa completamente livre, onde os grupos, através de biblioteca Gloss, tratam da componente gráfica do Jogo. O nosso grupo decidiu que o tema do nosso jogo ia ser 
--baseado no Neon. Para isso, e através do uso do Photoshop, foram criadas imagens que se inspirassem à volta desse tema.
-- Ficou decidido que o nome do jogo seria "Neon Tanks"  e que o jogo teria: um Menu Principal, um segundo menu onde o jogador poderia selecionar se queria jogar no Mapa predefinido pelo Grupo,
--ou se queria construir o seu próprio mapa. Também é possivel ao jogador selecionar o número de jogadores que irão jogar, sabendo que haverão bots quando não existirem 4 jogadores para jogar. 
--Caso o jogador esteja num Menu e queira voltar ao Menu anterior, pode faze-lo pressionado a tecla 'Tab'. Foram também usadas as funções das Tarefas anteriores, importantes para a o correto 
--funcionamento do Jogo.
-- Já quando se encontrano jogo, é possivel ao(s) jogador(es) visualizar informações como número de Vidas, número de Lasers, choques e canhoes disponiveis, bem como os controlos para cada jogador.
--Caso o jogador queira, é lhe dada a hipótese de pausar o jogo, podendo depois retomar a partida ou regressar ao Menu Principal.
-- Todos estes aspetos do jogo foram conseguidos através do uso de direntes funções. A função reageEvento, dependendo do lugar onde o jogador se encontra (menus ou no próprio jogo), atribui as 
--teclas que irão realizar diferentes ações que afetarão o jogo. Seguidamente, a função reage tempo transmite ao jogador a opção que se encontra selecionada (opção selecionada surge com um traçado 
--branco e posteriormente passa para uma imagem sem nenhum traçado, fazendo com que essa opção apareça a "piscar"). Segue-se a função estadoInicial que representa o Menu Principal, pois é onde o 
--jogo começa e posteriormente a função desenhaEstado. Esta função é responsável por fazer com que toda a informação do jogo necessária seja apresentada em Imagens (desde os Menus até aos Jogadores).
--Por fim, vêm as funções frameRate e display, que representam as frames com que o jogo é jogado e o tamanho da tela de Jogo, respetivamente.
--Todas estas funções conectam-se numa função prinicipal, denominada main, que recebe as imagens todas a utilizar e tornar o jogo jogável.
--Ficou assim a componente gráfica do jogo pronta, baseada no tema Neon. 

relatorio5 :: String -- ^ O Relatorio da Tarefa5
relatorio5 = "Relatorio5"

-- * Estado Jogo

-- | Função que dada uma lista e um inteiro devolve o elemento dessa Lista que corresponde ao Inteiro (primeiro elemento da lista corresponde ao indice 0)

myIndex :: [a] -- ^ Lista de elementos 
        -> Int -- ^ Inteiro 
        -> a -- ^ Elemento da lista correspondente ao Inteiro
myIndex (h:t) 0 = h
myIndex (h:t) a = myIndex t (a-1)


-- | Função que dado um Evento e o Estado atual do Jogo modifica o Estado do Jogo

reageEvento :: Event -- ^ Evento a ocorrer 
            -> NewState -- ^ Estado atual do Jogo 
            -> NewState -- ^ Estado Jogo Modificado pelo Evento

-- Apenas 1 jogador a jogar (Pegasus)

-- Eventos que afetam o Jogador 1(Pegasus)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo1",q,ed,n) = ((jogada 0 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo1",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo1",q,ed,n) = ((jogada 0 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo1",q,ed,n)
reageEvento (EventKey (SpecialKey KeyRight)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo1",q,ed,n) = ((jogada 0 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo1",q,ed,n)
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo1",q,ed,n) = ((jogada 0 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo1",q,ed,n)
reageEvento (EventKey (Char ',')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo1",q,ed,n) = ((jogada 0 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo1",q,ed,n)
reageEvento (EventKey (Char '.')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo1",q,ed,n) = ((jogada 0 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo1",q,ed,n)
reageEvento (EventKey (Char '-')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo1",q,ed,n) = ((jogada 0 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo1",q,ed,n)


--Apenas 2 jogadores a jogar (Pegasus e Lion)

-- Eventos que afetam o Jogador 1(Pegasus)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 0 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 0 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (SpecialKey KeyRight)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 0 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 0 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char ',')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 0 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char '.')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 0 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char '-')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 0 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)


-- Eventos que afetam o Jogador 2(Lion)
reageEvento (EventKey (Char 'w')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 1 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char 's')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 1 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char 'd')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 1 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char 'a')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 1 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char '1')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 1 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char '2')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 1 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (Char '3')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((jogada 1 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo2",q,ed,n)


-- Apenas 3 jogadores a jogar (Pegasus, Lion e Dragon)

-- Eventos que afetam o Jogador 1(Pegasus)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 0 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 0 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (SpecialKey KeyRight)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 0 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 0 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char ',')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 0 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char '.')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 0 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char '-')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 0 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)


-- Eventos que afetam o Jogador 2(Lion)
reageEvento (EventKey (Char 'w')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 1 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char 's')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 1 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char 'd')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 1 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char 'a')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 1 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char '1')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 1 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char '2')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 1 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char '3')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 1 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)


-- Eventos que afetam o Jogador 3(Dragon)
reageEvento (EventKey (Char 'y')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 2 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char 'h')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 2 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char 'j')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 2 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char 'g')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 2 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char '4')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 2 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char '5')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 2 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (Char '6')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((jogada 2 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo3",q,ed,n)


-- 4 jogadores a jogar (Pegasus, Lion, Dragon e Eagle)

-- Eventos que afetam o Jogador 1(Pegasus)          
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 0 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 0 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyRight)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 0 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 0 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char ',')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 0 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '.')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 0 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '-')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 0 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)


-- Eventos que afetam o Jogador 2(Lion)
reageEvento (EventKey (Char 'w')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 1 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 's')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 1 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 'd')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 1 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 'a')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 1 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '1')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 1 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '2')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 1 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '3')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 1 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)


-- Eventos que afetam o Jogador 3(Dragon)
reageEvento (EventKey (Char 'y')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 2 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 'h')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 2 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 'j')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 2 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 'g')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 2 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '4')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 2 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '5')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 2 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '6')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 2 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)


-- Eventos que afetam o Jogador 4(Eagle)
reageEvento (EventKey (Char 'f')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 3 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 'v')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 3 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 'b')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 3 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char 'c')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 3 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '7')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 3 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '8')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 3 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (Char '9')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((jogada 3 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2, "Jogo",q,ed,n)


-- Eventos a ocorrer no Menu de Pausa

reageEvento (EventKey (SpecialKey KeySpace)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu",q,ed,n) = ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Menu", q,ed,n)


reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu1",q,ed,n) = ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Menu", q,ed,n)

reageEvento (EventKey (SpecialKey KeySpace)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar01",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar01",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Jogo1",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu01",q,ed,n) = ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu01",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar01",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar01",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Menu01", q,ed,n)


reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar11",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Jogo1",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu11",q,ed,n) = ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu11",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar01",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar11",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Menu01", q,ed,n)

reageEvento (EventKey (SpecialKey KeySpace)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo2",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar2",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar2",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu2",q,ed,n) = ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu2",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar2",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar2",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Menu2", q,ed,n)


reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar21",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Jogo2",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu21",q,ed,n) = ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu21",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar2",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar21",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Menu2", q,ed,n)

reageEvento (EventKey (SpecialKey KeySpace)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Jogo3",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar3",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar3",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu3",q,ed,n) = ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu3",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar3",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar3",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Menu3", q,ed,n)


reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar31",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Jogo3",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu31",q,ed,n) = ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo",[],editorInicial [],0)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Menu31",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Retomar3",q,ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Pausa Retomar31",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Pausa Menu3", q,ed,n)


-- Eventos a ocorrer no Menu Inicial

reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Menu Jogo1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Menu Jogo",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",q,ed,n)


-- Eventos a ocorrer no Constroi Mapa

reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa",q ++ [Move C],instrucao (Move C) ed,n)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa", q ++ [Move B],instrucao (Move B) ed,n)
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa", q ++ [Move E],instrucao (Move E) ed,n)
reageEvento (EventKey (SpecialKey KeyRight)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa", q ++ [Move D],instrucao (Move D) ed,n)
reageEvento (EventKey (Char 't')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa", q ++ [MudaTetromino],instrucao (MudaTetromino) ed,n)
reageEvento (EventKey (Char 'r')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa", q ++ [Roda],instrucao (Roda) ed,n)
reageEvento (EventKey (Char 'l')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa", q ++ [MudaParede],instrucao (MudaParede) ed,n)
reageEvento (EventKey (Char 'd')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa", q ++ [Desenha],instrucao (Desenha ) ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l), b1, b2, "1JOG",q,ed,n)

-- Eventos a ocorrer no Sub-Menu (Caso queira voltar atrás)

reageEvento (EventKey (SpecialKey KeyTab)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Quick Game",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Menu Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyTab)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Constroi Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Menu Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Constroi Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",q,ed,n)
reageEvento (EventKey (SpecialKey KeyRight)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Quick Game",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Constroi Mapa",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Quick Game",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "1JOG",inst,ed,n)
                                                           where inst = (replicate 9 (Move B) ++ replicate 9 (Move D))
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Constroi Mapa",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa",q,ed,n)


reageEvento (EventKey (SpecialKey KeyTab)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Quick Game1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Menu Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyTab)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Constroi Mapa1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Menu Jogo",q,ed,n)
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Constroi Mapa1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",q,ed,n)
reageEvento (EventKey (SpecialKey KeyRight)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Quick Game1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Constroi Mapa",q,ed,n)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Quick Game1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "1JOG",inst,ed,n)
                                                                           where inst = (replicate 9 (Move B) ++ replicate 9 (Move D))
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, "Constroi Mapa1",q,ed,n) = ((Estado mapa jog disp), (c,l) , b1, b2, "Mapa",q,ed,n)

reageEvento (EventKey (SpecialKey KeyTab)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, st ,q,ed,n) =
                                                        case st of
                                                        "1JOG" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",[],editorInicial [],n)
                                                        "2JOG" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",[],editorInicial [],n)
                                                        "3JOG" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",[],editorInicial [],n)
                                                        "4JOG" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",[],editorInicial [],n)
                                                        "1JOG1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",[],editorInicial [],n)
                                                        "2JOG1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",[],editorInicial [],n)
                                                        "3JOG1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",[],editorInicial [],n)
                                                        "4JOG1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Quick Game",[],editorInicial [],n)
                                                        otherwise -> ((Estado mapa jog disp), (c,l), b1, b2, st ,q,ed,n)

reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, st ,q,ed,n) = 
                                                            case st of
                                                                "2JOG" -> ((Estado mapa jog disp), (c,l) , b1, b2, "1JOG",q,ed,n)
                                                                "3JOG" -> ((Estado mapa jog disp), (c,l), b1, b2, "2JOG" ,q,ed,n)
                                                                "4JOG" -> ((Estado mapa jog disp), (c,l), b1, b2, "3JOG" ,q,ed,n)
                                                                "2JOG1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "1JOG",q,ed,n)
                                                                "3JOG1" -> ((Estado mapa jog disp), (c,l), b1, b2, "2JOG" ,q,ed,n)
                                                                "4JOG1" -> ((Estado mapa jog disp), (c,l), b1, b2, "3JOG" ,q,ed,n)
                                                                "Win Menu" -> ((Estado mapa jog disp), (c,l) , b1, b2, "YOU WIN",q,ed,n)
                                                                "Win Menu1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "YOU WIN",q,ed,n)
                                                                otherwise -> ((Estado mapa jog disp), (c,l), b1, b2, st ,q,ed,n)
                                                                
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, st ,q,ed,n) = 
                                                          case st of
                                                            "1JOG" -> ((Estado mapa jog disp), (c,l) , b1, b2, "2JOG",q,ed,n)
                                                            "2JOG" -> ((Estado mapa jog disp), (c,l) , b1, b2, "3JOG",q,ed,n)
                                                            "3JOG" -> ((Estado mapa jog disp), (c,l) , b1, b2, "4JOG",q,ed,n)
                                                            "1JOG1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "2JOG",q,ed,n)
                                                            "2JOG1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "3JOG",q,ed,n)
                                                            "3JOG1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "4JOG",q,ed,n)
                                                            "YOU WIN" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Win Menu",q,ed,n)
                                                            "YOU WIN1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Win Menu",q,ed,n)
                                                            otherwise -> ((Estado mapa jog disp), (c,l), b1, b2, st ,q,ed,n)

reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2, st ,q,ed,n) = 
                                                          case st of
                                                            "1JOG" -> (est, (c,l) , b1, b2, "Jogo1",q,ed,n)
                                                            "2JOG" -> (est, (c,l) , b1, b2, "Jogo2",q,ed,n)
                                                            "3JOG" -> (est, (c,l) , b1, b2, "Jogo3",q,ed,n)
                                                            "4JOG" -> (est, (c,l) , b1, b2, "Jogo",q,ed,n)
                                                            "1JOG1" -> (est, (c,l) , b1, b2, "Jogo1",q,ed,n)
                                                            "2JOG1" -> (est, (c,l) , b1, b2, "Jogo2",q,ed,n)
                                                            "3JOG1" -> (est, (c,l) , b1, b2, "Jogo3",q,ed,n)
                                                            "4JOG1" -> (est, (c,l) , b1, b2, "Jogo",q,ed,n)
                                                            "YOU WIN" -> ((Estado [] [] []), (c,l) , b1, b2, "1JOG",q,ed,0)
                                                            "YOU WIN1" -> ((Estado [] [] []), (c,l) , b1, b2, "1JOG",q,ed,0)
                                                            "Win Menu" -> ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
                                                            "Win Menu1" -> ((Estado [] [] []), (c,l) , b1, b2, "Menu Jogo1",[],editorInicial [],0)
                                                            otherwise -> ((Estado mapa jog disp), (c,l), b1, b2, st ,q,ed,n)
                       where est = (Estado (constroi q) [Jogador (1,1) C 3 3 3, Jogador (length (constroi q) - 3,1) B 3 3 3, Jogador (1,length (head (constroi q)) - 3) C 3 3 3, Jogador (length (constroi q) - 3,length (head (constroi q)) - 3) B 3 3 3] disp)

-- Ignorar qualquer outro evento

reageEvento _ s = s 

-- | Indica o número de sobreviventes

sobreviventes :: [Int] -- ^ Lista de com indice de Jogadores 
              -> Int -- ^ Número de Jogadores vivos
sobreviventes [] = 0
sobreviventes (h:t) =
      case h of
        0 -> 1 + sobreviventes t
        otherwise -> sobreviventes t


-- | Altera o Estado do Jogo com a Passagem do tempo  (responsável, por exemplo, por demonstrar que uma opção se encontra selecionada)

reageTempo :: Float -- ^ Número de tempo a passar 
           -> NewState -- ^ Estado atual do Jogo
           -> NewState -- ^ Estado do Jogo modificado
reageTempo n ((Estado mapa jog disp),(c,l),b1,b2,b3,q,ed,n1) =
                  case b3 of
                    "Jogo" -> if sobreviventes (map (vidasJogador) jog) == 3  then ((Estado mapa jog disp), (c,l) , b1, b2, "YOU WIN", q, ed,n1) else (tick(Estado mapa jog disp),(c,l),b1,b2,"Jogo",q,ed, (n1+1))
                    "Jogo1" -> if sobreviventes (map (vidasJogador) jog) == 3  then ((Estado mapa jog disp), (c,l) , b1, b2, "YOU WIN", q, ed,n1) else (bots 3 (tick(Estado mapa jog disp)),(c,l),b1,b2,"Jogo1",q,ed, (n1+1))
                    "Jogo2" -> if sobreviventes (map (vidasJogador) jog) == 3  then ((Estado mapa jog disp), (c,l) , b1, b2, "YOU WIN", q, ed,n1) else (bots 2 (tick(Estado mapa jog disp)),(c,l),b1,b2,"Jogo2",q,ed, (n1+1))
                    "Jogo3" -> if sobreviventes (map (vidasJogador) jog) == 3  then ((Estado mapa jog disp), (c,l) , b1, b2, "YOU WIN", q, ed,n1) else (bots 1 (tick(Estado mapa jog disp)),(c,l),b1,b2,"Jogo3",q,ed, (n1+1))
                    "Menu Jogo1" -> ((Estado mapa jog disp),(c,l),b1,b2,"Menu Jogo",q,editorInicial [],0)
                    "Menu Jogo" -> ((Estado mapa jog disp),(c,l),b1,b2,"Menu Jogo1",q,editorInicial [],0)
                    "Quick Game" -> ((Estado mapa jog disp),(c,l),b1,b2,"Quick Game1",q,ed,n1)
                    "Quick Game1" -> ((Estado mapa jog disp),(c,l),b1,b2,"Quick Game",q,ed,n1)
                    "Constroi Mapa" -> ((Estado mapa jog disp),(c,l),b1,b2,"Constroi Mapa1",q,ed,n1)
                    "Constroi Mapa1" -> ((Estado mapa jog disp),(c,l),b1,b2,"Constroi Mapa",q,ed,n1)
                    "Pausa Retomar" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Retomar1",q,ed,n1)
                    "Pausa Retomar1" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Retomar",q,ed,n1)
                    "Pausa Menu" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Menu1",q,ed,n1)
                    "Pausa Menu1" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Menu",q,ed,n1)
                    "Pausa Retomar01" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Retomar11",q,ed,n1)
                    "Pausa Retomar11" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Retomar01",q,ed,n1)
                    "Pausa Menu01" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Menu11",q,ed,n1)
                    "Pausa Menu11" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Menu01",q,ed,n1)
                    "Pausa Retomar2" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Retomar21",q,ed,n1)
                    "Pausa Retomar21" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Retomar2",q,ed,n1)
                    "Pausa Menu2" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Menu21",q,ed,n1)
                    "Pausa Menu21" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Menu2",q,ed,n1)
                    "Pausa Retomar3" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Retomar31",q,ed,n1)
                    "Pausa Retomar31" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Retomar3",q,ed,n1)
                    "Pausa Menu3" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Menu31",q,ed,n1)
                    "Pausa Menu31" -> ((Estado mapa jog disp),(c,l),b1,b2,"Pausa Menu3",q,ed,n1)
                    "Mapa" -> ((Estado mapa jog disp),(c,l),b1,b2,"Mapa",q,ed,n1)
                    "1JOG" -> ((Estado mapa jog disp),(c,l),b1,b2,"1JOG1",q,ed,n1)
                    "2JOG" -> ((Estado mapa jog disp),(c,l),b1,b2,"2JOG1",q,ed,n1)
                    "3JOG" -> ((Estado mapa jog disp),(c,l),b1,b2,"3JOG1",q,ed,n1)
                    "4JOG" -> ((Estado mapa jog disp),(c,l),b1,b2,"4JOG1",q,ed,n1)
                    "1JOG1" -> ((Estado mapa jog disp),(c,l),b1,b2,"1JOG",q,ed,n1)
                    "2JOG1" -> ((Estado mapa jog disp),(c,l),b1,b2,"2JOG",q,ed,n1)
                    "3JOG1" -> ((Estado mapa jog disp),(c,l),b1,b2,"3JOG",q,ed,n1)
                    "4JOG1" -> ((Estado mapa jog disp),(c,l),b1,b2,"4JOG",q,ed,n1)
                    "YOU WIN" -> ((Estado mapa jog disp), (c,l) , b1, b2, "YOU WIN1",q,ed,n1)
                    "YOU WIN1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "YOU WIN",q,ed,n1)
                    "Win Menu" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Win Menu1",q,ed,n1)
                    "Win Menu1" -> ((Estado mapa jog disp), (c,l) , b1, b2, "Win Menu",q,ed,n1)
                    otherwise -> ((Estado mapa jog disp),(c,l),b1,b2,b3,q,ed,n1)
           where bots 1 e = jogadaBot 1 (bot 1 e) e
                 bots n e = bots (n-1) (jogadaBot n (bot n e) e) 
                 
-- * Estado Gloss

-- | O NewState é um tipo de dados por nós definido, que contém todas as informações necessárias para o funcionamento do jogo. Este é constituido por um Estado, (Float.Float), duas [Picture], String, Instrucoes, Editor e um Inteiro.


type NewState = (Estado,(Float,Float), [Picture], [Picture],String, Instrucoes, Editor,Int)
-- Estado corresponde ao Estado atual do Jogo (Mapa, Jogadores e Disparos)
-- (Float, Float) corresponde ao posicionamento da imagem de fundo na tela
-- 1º [Picture] corresponde às imagens fundo,paineis,constroimapa,menu1,menu,constM,quikConst,quickG,jogSelect,jog1,jog2,jog3,jog4,pausaNada,pausaVoltarJogar,pausaVoltarMenu,pegasus,pegasusjn,pegasusmn,lion,lionjn,lionmn,dragon,dragonjn,dragonmn,eagle,eaglejn e eaglemn
-- 2º [Picture] corresponde às imagens tank1,tank2,tank3,tank4,bala,laser,choque,vidas0,vidas1,vidas2,vidas3,choque0,choque1,choque2,choque3,laser0,laser1,laser2,laser3,bloco,blocoind e blocodes
-- A String demonstra em que Estrutura do Jogo nos encontramos, ou seja, se estamos, por exemplo, no menu principal, no jogo com 1 jogador ou no jogo com 2 jogadores
-- Instruções e editor são essenciais para caso o jogador queira construir o seu próprio mapa
-- O Int corresponde ao estado jogo (é 0 quando nos encontramos no Menu Inicial e no Menu quando um jogador ganha a partida)


-- | Estado Inicial do Jogo (Menu Principal)

estadoInicial ::  (Float,Float) -- Posicao do Mapa na tela 
              -> Instrucoes -- Instruções atuais 
              -> [Picture] -- Lista de Imagens
              -> [Picture] -- Lista de Imagens
              -> NewState -- Resultado da aplicação dos dados anteriores
estadoInicial (c,l) e b1 b2 = ((Estado [] [] []), (c,l), b1, b2, "Menu Jogo1",e,editorInicial [],0) 


-- | Transforma as informações do NewState numa Imagem

desenhaEstado :: NewState -- ^ Tipo que contém as informações todas do Jogo 
              ->  Picture -- ^ Transformação dessas Informações uma Imagem
desenhaEstado ((Estado mapa jog disp), (c,l), k@[fundo,paineis,constroimapa,menu1,menu,constM,quikConst,quickG,jogSelect,jog1,jog2,jog3,jog4,pausaNada,pausaVoltarJogar,pausaVoltarMenu,pegasus,pegasusjn,pegasusmn,lion,lionjn,lionmn,dragon,dragonjn,dragonmn,eagle,eaglejn,eaglemn], ç, b, mapaInstruções,Editor (l1,c1) d t p m,n) =
                               case b of
                                "Jogo" ->  scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++
                                                                                                        [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++  
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ (desenhaLasers (expandeLasers disp mapa) mapa ç (escolheLaser disp)))  
                                "Jogo1" ->  scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++
                                                                                                        [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++  
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ (desenhaLasers (expandeLasers disp mapa) mapa ç (escolheLaser disp)))  
                                "Jogo2" ->  scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++
                                                                                                        [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++  
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ (desenhaLasers (expandeLasers disp mapa) mapa ç (escolheLaser disp))) 
                                "Jogo3" ->  scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++  
                                                                                                       (desenhaMapa (c,l) ç mapa) ++
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ (desenhaLasers (expandeLasers disp mapa) mapa ç (escolheLaser disp)))                                                                                                                                                                                                                                                
                                "Mapa" ->  scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ constroimapa] ++ 
                                                                                                       (desenhaMapa (c,l) ç (constroi mapaInstruções)) ++ [translate (-50) 0 $ pictures (desenhaMatBool (fromIntegral (c1*50),fromIntegral (-l1*50)) ç p t (tetroParaMatriz t d))])
                                "Menu Jogo1" ->  scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ menu1])
                                "Menu Jogo" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures  ([scale 0.45 0.45 $ translate (1000) (-800) $ menu])
                                "Quick Game" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ quickG]) 
                                "Quick Game1" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ quikConst])
                                "Constroi Mapa" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ constM])
                                "Constroi Mapa1" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ quikConst])
                                "Pausa Retomar" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++  
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaVoltarJogar])
                                "Pausa Retomar1" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++                                                                                                        (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaNada])
                                "Pausa Menu" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaVoltarMenu])
                                "Pausa Menu1" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaNada])
                                "Pausa Retomar01" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaVoltarJogar])
                                "Pausa Retomar11" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaNada])
                                "Pausa Menu01" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaVoltarMenu])
                                "Pausa Menu11" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaNada])
                                "Pausa Retomar2" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaVoltarJogar])
                                "Pausa Retomar21" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaNada])
                                "Pausa Menu2" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaVoltarMenu])
                                "Pausa Menu21" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaNada])
                                "Pausa Retomar3" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaVoltarJogar])
                                "Pausa Retomar31" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaNada])
                                "Pausa Menu3" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaVoltarMenu])
                                "Pausa Menu31" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1050) (-865) $ paineis] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-200) $ (desenhaLaserJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-600) $ (desenhaLaserJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1000) $ (desenhaLaserJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2520) (-1400) $ (desenhaLaserJog4 jog ç)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-200) $ (desenhaChoqueJog1 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-600) $ (desenhaChoqueJog2 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1000) $ (desenhaChoqueJog3 jog ç)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2680) (-1400) $ (desenhaChoqueJog4 jog ç)] ++ 
                                                                                                       (desenhaMapa (c,l) ç mapa) ++ 
                                                                                                       (desenhaJogador jog ç) ++ 
                                                                                                       (desenhaDisparos disp ç jog) ++ 
                                                                                                       [scale 0.45 0.45 $ translate (1000) (-800) $ pausaNada])
                                "1JOG" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ jog1])
                                "1JOG1" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ jogSelect])
                                "2JOG" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ jog2])
                                "2JOG1" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ jogSelect])
                                "3JOG" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ jog3])
                                "3JOG1" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ jogSelect])
                                "4JOG" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ jog4])                                                                                                      
                                "4JOG1" -> scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ jogSelect])
                                "YOU WIN" -> if indiceVivo (map (vidasJogador) jog) -1 == 0 then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ pegasusjn])
                                             else (if indiceVivo (map (vidasJogador) jog) -1 == 1  then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ lionjn])
                                             else (if indiceVivo (map (vidasJogador) jog) -1 == 2 then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ dragonjn])
                                             else scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ eaglejn])))
                                "YOU WIN1" -> if indiceVivo (map (vidasJogador) jog) -1 == 0 then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ pegasus])
                                             else (if indiceVivo (map (vidasJogador) jog) -1 == 1  then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ lion])
                                             else (if indiceVivo (map (vidasJogador) jog) -1 == 2 then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ dragon])
                                             else scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ eagle])))
                                "Win Menu" -> if indiceVivo (map (vidasJogador) jog) -1 == 0 then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ pegasusmn])
                                             else (if indiceVivo (map (vidasJogador) jog) -1 == 1  then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ lionmn])
                                             else (if indiceVivo (map (vidasJogador) jog) -1 == 2 then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ dragonmn])
                                             else scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ eaglemn])))
                                "Win Menu1" -> if indiceVivo (map (vidasJogador) jog) -1 == 0 then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ pegasus])
                                             else (if indiceVivo (map (vidasJogador) jog) -1 == 1  then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ lion])
                                             else (if indiceVivo (map (vidasJogador) jog) -1 == 2 then scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ dragon])
                                             else scale 0.93 0.93 $ translate (-450) (350) $ pictures ([scale 0.45 0.45 $ translate (1000) (-800) $ eagle])))

-- | Apresenta o número de Jogadores Vivos

indiceVivo :: [Int] -- ^ Lista com Indices dos Jogadores vivos
           -> Int -- ^ Número de Jogadores Vivos
indiceVivo [] = 4
indiceVivo (h:t) =
      case h of
        0 -> 1 + indiceVivo t
        otherwise -> 1 

-- * Desenhar

-- | Desenha os Disparos Canhao e Choque dos Jogadores

desenhaDisparos :: [Disparo] -- ^ Lista de 'Disparo'es 
                -> [Picture] -- ^ Lista de 'Picture's onde vai buscar a imagens dos 'Disparo's
                -> [Jogador] -- ^ Lista de 'Jogador'es
                -> [Picture] -- ^ Lista de 'Picture's final
desenhaDisparos [] _ _ = []
desenhaDisparos _ [] _ = []
desenhaDisparos _ _ [] = []
desenhaDisparos ((DisparoCanhao n (l,c) dir):t) b jog =
                 case dir of 
                      C -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ (myIndex b 4)] ++ desenhaDisparos t b jog
                      B -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 180 $ (myIndex b 4)] ++ desenhaDisparos t b jog
                      D -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 90 $ (myIndex b 4)] ++ desenhaDisparos t b jog
                      E -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 270 $ (myIndex b 4)] ++ desenhaDisparos t b jog   
desenhaDisparos (i@(DisparoChoque n tick):t) b jog = [(translate (fromIntegral(-77+(losChoquesC i jog)*50)) (fromIntegral(-25-(losChoquesL i jog)*50))) $ (myIndex b 6)] ++ desenhaDisparos t b jog                     
desenhaDisparos ((DisparoLaser n (l,c) dir):_) _ _ = []


-- | Através do indice do jogador a disparar vai a uma lista de jogadores e devolve a linha desse Jogador

losChoquesL :: Disparo -- ^ 'Disparo'
            -> [Jogador] -- ^ Lista de 'Jogador'es
            -> Int -- ^ Devolve linha desse 'Jogador'
losChoquesL (DisparoChoque n tick) (h:t) = (daChoquel(encontraIndiceLista n (h:t)))


-- | Através do indice do jogador a disparar vai a uma lista de jogadores e devolve a Coluna desse Jogador

losChoquesC :: Disparo -- ^ 'Disparo' 
            -> [Jogador] -- ^ Lista de 'Jogador'es
            -> Int -- Devolve coluna desse 'Jogador'
losChoquesC (DisparoChoque n tick) (h:t) = (daChoquec(encontraIndiceLista n (h:t)))


-- | Devolve a linha em que um jogador se encontra

daChoquel :: Jogador -- ^ 'Jogador'  
          -> Int -- ^ Linha em que se enconta
daChoquel (Jogador (l,c) dir v las ch) = l


-- | Devolve a coluna em que um jogador se encontra

daChoquec :: Jogador -- ^ 'Jogador' 
          -> Int -- ^ Coluna em que se encontra
daChoquec (Jogador (l,c) dir v las ch) = c          


-- | Desenha o Disparo Laser

desenhaLaser :: [Posicao] -- ^ Lista de 'Posicao' 
             -> Mapa -- ^ 'Mapa' do Jogo
             -> [Picture] -- ^ Lista de Imagens
             -> Disparo -- ^ 'Disparo' Laser
             -> [Picture] -- ^ Lista de 'Picture' do Laser a propagar-se
desenhaLaser [] _ _ _ = []
desenhaLaser ((l1,c1):ts) mapa b (DisparoLaser n (l,c) dir)  =
        case dir of 
            C -> ((translate (fromIntegral(-75+c1*50)) (fromIntegral(25-l1*50))) $ (myIndex b 5)):desenhaLaser ts mapa b (DisparoLaser n (l-1,c) dir)
            B -> ((translate (fromIntegral(-75+c*50)) (fromIntegral(-75-l*50))) $ rotate 180 $ (myIndex b 5)):desenhaLaser ts mapa b (DisparoLaser n (l+1,c) dir)
            D -> ((translate (fromIntegral(-25+c*50)) (fromIntegral(-25-l*50))) $ rotate 90 $ (myIndex b 5)):desenhaLaser ts mapa b (DisparoLaser n (l,c+1) dir)
            E -> ((translate (fromIntegral(-125+c*50)) (fromIntegral(-25-l*50))) $ rotate 270 $ (myIndex b 5)):desenhaLaser ts mapa b (DisparoLaser n (l,c-1) dir)


-- | Desenha os Disparos Laser 

desenhaLasers :: [[Posicao]] -- ^ Lista de Lista de 'Posicao'
              -> Mapa -- ^ 'Mapa' do Jogo
              -> [Picture] -- ^ Lista de 'Picture'
              -> [Disparo] -- ^ Lista de 'Disparo's
              -> [Picture] -- ^ Lista de 'Picture's com todos os lasers que estejam a ocorrer 
desenhaLasers _ _ _ [] = []
desenhaLasers [] _ _ _ = []
desenhaLasers (h:t) mapa b (x:xs) = (desenhaLaser h mapa b x) ++ desenhaLasers t mapa b xs


-- | Devolve lista com a lista de posicoes do laser enquanto de propaga

expandeLasers :: [Disparo] -- ^ Lista de 'Disparo's 
              -> Mapa -- ^ 'Mapa' do Jogo 
              -> [[Posicao]] -- ^ Lista com Lista de 'Posicao' do Laser a propagar-se
expandeLasers (h:t) mapa = expandeLaserPos h mapa:expandeLasers t mapa    


-- | Desenha os Jogadores                                                                                                          
                                                                
desenhaJogador :: [Jogador] -- ^ Lista de 'Jogador'es 
               -> [Picture] -- ^ Lista de 'Picture' que contêm as Imagens dos Tanques
               -> [Picture] -- ^ 'Jogador'es desenhados
desenhaJogador _ [] = []
desenhaJogador [] _ = []
desenhaJogador ((Jogador (l,c) dir v las ch):t) xs | (dir == C) && (v /= 0) = [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ head xs] ++ desenhaJogador t (tail xs)
                                                   | (dir == B) && (v /= 0) = [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 180 $ head xs] ++ desenhaJogador t (tail xs)
                                                   | (dir == D) && (v /= 0) = [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 90 $ head xs] ++ desenhaJogador t (tail xs)
                                                   | (dir == E) && (v /= 0) = [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 270 $ head xs] ++ desenhaJogador t (tail xs)
                                                   | otherwise = desenhaJogador t (tail xs) 
    
                 
-- | Desenha o Mapa de Jogo

desenhaMapa :: (Float,Float) -- ^ Posicionamento das 'Peca's
            -> [Picture] -- ^ Lista de 'Picture's que contêm as Imagens dos Blocos 
            -> Mapa -- ^ 'Mapa' do Jogo
            -> [Picture] -- ^ 'Mapa' de Jogo desenhado 
desenhaMapa (c,l) _ [] = []
desenhaMapa (c,l) b (h:t) =  (desenhaPeca (c,l) b h) ++ (desenhaMapa (c,l-50) b t)


-- | Desenha uma Peça

desenhaPeca :: (Float,Float) -- ^ Posicionamento das 'Peca's
            -> [Picture] -- ^ Lista de 'Picture's que contêm as Imagens dos Blocos 
            -> [Peca] -- ^ Lista de 'Peca's
            -> [Picture] -- 'Peca's desenhadas
desenhaPeca (c,l) _ [] = []
desenhaPeca (c,l) b (h:t) | (h == Bloco Indestrutivel) = (translate c l $ (myIndex b 20)):desenhaPeca (c+50,l) b t
                          | (h == Vazia) = (translate c l $ (myIndex b 19)):desenhaPeca (c+50,l) b t
                          | otherwise = (translate c l $ (myIndex b 21)):desenhaPeca (c+50,l) b t


-- | Desenha uma Matriz com Tetrominos
                   
desenhaMatBool :: (Float,Float) -- ^ Posicionamento das 'Peca's
               -> [Picture] -- ^ Lista de 'Picture's que contêm as Imagens dos Blocos 
               -> Parede -- ^ 'Indestrutivel', 'Destrutivel' e 'Vazia'
               -> Tetromino -- ^ 'Tetromino' Desejado 
               -> [[Bool]] -- ^ Lista de Lista de 'True's e 'False's
               -> [Picture] -- ^ Tetrominos Desenhados
desenhaMatBool (c,l) _ _ _ [] = []
desenhaMatBool (c,l) b p d (h:t) =  (desenhaBool (c,l) b p d h) ++ (desenhaMatBool (c,l-50) b p d t)


-- | Desenha um Tetrómino com o tipo de parede atual

desenhaBool :: (Float,Float) -- ^ Posicionamento das 'Peca's
            -> [Picture] -- ^ Lista de 'Picture's 
            -> Parede -- ^ 'Indestrutivel', 'Destrutivel' e 'Vazia'
            -> Tetromino -- ^ 'Tetromino' Desejado
            -> [Bool] -- ^ Lista de 'True's e 'False's
            -> [Picture] -- ^ Tetrómino Desenhado
desenhaBool (c,l) _ _ _ [] = []
desenhaBool (c,l) b p d (h:t) | h && p == Indestrutivel = (translate (c-50) l $ (myIndex b 20)):desenhaBool (c+50,l) b p d t
                              | h && p == Destrutivel = (translate (c-50) l $ (myIndex b 21)):desenhaBool (c+50,l) b p d t
                              | otherwise = desenhaBool (c+50,l) b p d t


-- | Desenhar Número de Vidas (0,1,2 ou 3 vidas)

desenhaVidas :: Jogador -- ^ 'Jogador' 
             -> [Picture] -- ^ Lista de 'Picture' com Imagens das Vidas
             -> Picture -- ^ Número de Vidas desenhadas
desenhaVidas (Jogador (l,c) dir v las ch) b | (v == 0) = Translate (-200) (-200) $ (myIndex b 7)
                                            | (v == 1) = Translate (-200) (-200) $ (myIndex b 8)
                                            | (v == 2) = Translate (-200) (-200) $ (myIndex b 9) 
                                            | (v == 3) = Translate (-200) (-200) $ (myIndex b 10)


-- | Desenha vidas do Jogador 1 

desenhaVidasJog1 :: [Jogador] -- ^ Lista de 'Jogador'es 
                 -> [Picture] -- ^ Lista de 'Jogador'es com Imagens das Vidas
                 -> Picture -- ^ Vidas do Jogador 1 desenhadas
desenhaVidasJog1 jog b = desenhaVidas (myIndex jog 0) b


-- | Desenha vidas do Jogador 2 

desenhaVidasJog2 :: [Jogador] -- ^ Lista de 'Jogador'es
                 -> [Picture] -- ^ Lista de 'Jogador'es com Imagens das Vidas
                 -> Picture -- ^ Vidas do Jogador 2 desenhadas
desenhaVidasJog2 jog b = desenhaVidas (myIndex jog 1) b


-- | Desenha vidas do Jogador 3 

desenhaVidasJog3 :: [Jogador] -- ^ Lista de 'Jogador'es
                 -> [Picture] -- ^ Lista de 'Jogador'es com Imagens das Vidas
                 -> Picture -- ^ Vidas do Jogador 3 desenhadas
desenhaVidasJog3 jog b = desenhaVidas (myIndex jog 2) b


-- | Desenha vidas do Jogador 4

desenhaVidasJog4 :: [Jogador] -- ^ Lista de 'Jogador'es
                 -> [Picture] -- ^ Lista de 'Jogador'es com Imagens das Vidas
                 -> Picture -- ^ Vidas do Jogador 4 desenhadas
desenhaVidasJog4 jog b = desenhaVidas (myIndex jog 3) b                                        


-- | Desenhar Numero de Disparos Laser Disponiveis num Jogador

desenhaLaserJog :: Jogador -- ^ 'Jogador'
                -> [Picture] -- ^ Lista de 'Picture's com Imagens dos Lasers
                -> Picture -- ^ Número de um Laseres de um Jogador Desenhados
desenhaLaserJog (Jogador (l,c) dir v las ch) b  =
                    case las of
                         0 -> Translate (-200) (-200) $ (myIndex b 15) 
                         1 -> Translate (-200) (-200) $ (myIndex b 16)
                         2 -> Translate (-200) (-200) $ (myIndex b 17)
                         3 -> Translate (-200) (-200) $ (myIndex b 18)


-- | Desenhar Laser Disponiveis no Jogador 1

desenhaLaserJog1 :: [Jogador] -- ^ Lista de 'Jogador'es
                 -> [Picture] -- ^ Lista de 'Picture's com as Imagens dos Lasers
                 -> Picture -- ^ Lasers Jogador 1 disponiveis desenhados
desenhaLaserJog1 jog b = desenhaLaserJog (myIndex jog 0) b


-- | Desenhar Laser Disponiveis no Jogador 2

desenhaLaserJog2 :: [Jogador] -- ^ Lista de 'Jogador'es
                 -> [Picture] -- ^ Lista de 'Picture's com as Imagens dos Lasers
                 -> Picture -- ^ Lasers Jogador 2 disponiveis desenhados
desenhaLaserJog2 jog b = desenhaLaserJog (myIndex jog 1) b


-- | Desenhar Laser Disponiveis no Jogador 3

desenhaLaserJog3 :: [Jogador] -- ^ Lista de 'Jogador'es
                 -> [Picture] -- ^ Lista de 'Picture's com as Imagens dos Lasers
                 -> Picture -- ^ Lasers Jogador 3 disponiveis desenhados
desenhaLaserJog3 jog b = desenhaLaserJog (myIndex jog 2) b


-- | Desenhar Laser Disponiveis no Jogador 4

desenhaLaserJog4 :: [Jogador] -- ^ Lista de 'Jogador'es
                 -> [Picture] -- ^ Lista de 'Picture's com as Imagens dos Lasers
                 -> Picture -- ^ Lasers Jogador 4 disponiveis desenhados
desenhaLaserJog4 jog b = desenhaLaserJog (myIndex jog 3) b                                             


-- | Desenhar Numero de Choques Disponiveis num Jogador

desenhaChoqueJog :: Jogador -- ^ 'Jogador'
                 -> [Picture] -- ^ Lista de 'Picture's com Imagens dos Choque
                 -> Picture -- ^ Número de um Choques de um Jogador Desenhados
desenhaChoqueJog (Jogador (l,c) dir v las ch) b =
                   case ch of
                       0 -> Translate (-200) (-200) $ (myIndex b 11)          
                       1 -> Translate (-200) (-200) $ (myIndex b 12)
                       2 -> Translate (-200) (-200) $ (myIndex b 13)
                       3 -> Translate (-200) (-200) $ (myIndex b 14) 


-- | Desenhar Número de Choques Disponiveis do Jogador 1

desenhaChoqueJog1 :: [Jogador] -- ^ Lista de 'Jogador'es
                  -> [Picture] -- ^ Lista de 'Picture's com Imagens dos Choques 
                  -> Picture -- ^ Número de Choques Jogador 1 Desenhados
desenhaChoqueJog1 jog b = desenhaChoqueJog (myIndex jog 0) b


-- | Desenhar Número de Choques Disponiveis do Jogador 2

desenhaChoqueJog2 :: [Jogador] -- ^ Lista de 'Jogador'es
                  -> [Picture] -- ^ Lista de 'Picture's com Imagens dos Choques
                  -> Picture -- ^ Número de Choques Jogador 2 Desenhados
desenhaChoqueJog2 jog b = desenhaChoqueJog (myIndex jog 1) b


-- | Desenhar Número de Choques Disponiveis do Jogador 3

desenhaChoqueJog3 :: [Jogador] -- ^ Lista de 'Jogador'es
                  -> [Picture] -- ^ Lista de 'Picture's com Imagens dos Choques
                  -> Picture -- ^ Número de Choques Jogador 3 Desenhados
desenhaChoqueJog3 jog b = desenhaChoqueJog (myIndex jog 2) b


-- | Desenhar Número de Choques Disponiveis do Jogador 4

desenhaChoqueJog4 :: [Jogador] -- ^ Lista de 'Jogador'es
                  -> [Picture] -- ^ Lista de 'Picture's com Imagens dos Choques
                  -> Picture -- ^ Número de Choques Jogador 4 Desenhados
desenhaChoqueJog4 jog b = desenhaChoqueJog (myIndex jog 3) b


-- | Frame Rate do Jogo

fr :: Int -- ^ Frame Rate do Jogo
fr = 6


-- | Display do Jogo 

dm :: Display -- ^ 'Display'
dm = FullScreen


-- | Imprime no Ecrã os gráficos completos do nosso jogo, utilizando as funcoes 'dm', 'fr', 'estadoInicial', 'desenhaEstado', 'reageEvento' e 'reageTempo'

main :: IO ()
main = do {
     Just fundo <- loadJuicyPNG "fundoNovo.png";
     Just paineis <- loadJuicyPNG "painel.png";
     Just constroimapa <- loadJuicyPNG "constroimapafundo.png";
     Just menu1 <- loadJuicyPNG "menu1.png";
     Just menu <- loadJuicyPNG "menu.png";
     Just constM <- loadJuicyPNG "constroimapaselecionado.png";
     Just quikConst <- loadJuicyPNG "newgameconstrutormapa.png";
     Just quickG <- loadJuicyPNG "newgameselecionado.png";
     Just jogSelect <- loadJuicyPNG "escolherjogadores.png";
     Just jog1 <- loadJuicyPNG "escolher1jogador.png";
     Just jog2 <- loadJuicyPNG "escolher2jogadores.png";
     Just jog3 <- loadJuicyPNG "escolher3jogadores.png";
     Just jog4 <- loadJuicyPNG "escolher4jogadores.png";
     Just pausaNada <- loadJuicyPNG "pausanadaselecionado.png";
     Just pausaVoltarJogar <- loadJuicyPNG "pausaretomarjogo.png";
     Just pausaVoltarMenu <- loadJuicyPNG "pausasairjogo.png";
     Just pegasus <- loadJuicyPNG "pegasusganhounada.png";
     Just pegasusjn <- loadJuicyPNG "pegasusganhoujn.png";
     Just pegasusmn <- loadJuicyPNG "pegasusganhoumn.png";
     Just lion <- loadJuicyPNG "lionganhounada.png";
     Just lionjn <- loadJuicyPNG "lionganhoujn.png";
     Just lionmn <- loadJuicyPNG "lionganhoumn.png";
     Just dragon <- loadJuicyPNG "dragonganhounada.png";
     Just dragonjn <- loadJuicyPNG "dragonganhoujn.png";
     Just dragonmn <- loadJuicyPNG "dragonganhoumn.png";
     Just eagle <- loadJuicyPNG "eagleganhounada.png";
     Just eaglejn <- loadJuicyPNG "eagleganhoujn.png";
     Just eaglemn <- loadJuicyPNG "eagleganhoumn.png";
     Just tank1 <- loadJuicyPNG "tanque44.png";
     Just tank2 <- loadJuicyPNG "tanque11.png";
     Just tank3 <- loadJuicyPNG "tanque22.png";
     Just tank4 <- loadJuicyPNG "tanque33.png";
     Just bala <- loadJuicyPNG "bala.png";
     Just laser <- loadJuicyPNG "LaserNovo.png";
     Just choque <- loadJuicyPNG "choque.png";
     Just vidas0 <- loadJuicyPNG "vidas0.png";
     Just vidas1 <- loadJuicyPNG "vidas1.png";
     Just vidas2 <- loadJuicyPNG "vidas2.png";
     Just vidas3 <- loadJuicyPNG "vidas3.png";
     Just choque0 <- loadJuicyPNG "choque0.png";
     Just choque1 <- loadJuicyPNG "choque1.png";
     Just choque2 <- loadJuicyPNG "choque2.png";
     Just choque3 <- loadJuicyPNG "choque3.png";
     Just laser0 <- loadJuicyPNG "laser0.png";
     Just laser1 <- loadJuicyPNG "laser1.png";
     Just laser2 <- loadJuicyPNG "laser2.png";
     Just laser3 <- loadJuicyPNG "laser3.png";
     Just bloco <- loadJuicyPNG "blocosfundo.png";
     Just blocoind <- loadJuicyPNG "blocoindestrutivel.png";
     Just blocodes <- loadJuicyPNG "bloco destrutivel3.png";
          play 
            dm                                                                                                                                                                       -- janela onde irá correr o jogo
            (greyN 0.5)                                                                                                                                                               -- côr do fundo da janela
            fr                                                                                                                                                                        -- frame rate
            (estadoInicial  (-100,0) ([]) [fundo,paineis,constroimapa,menu1,menu,constM,quikConst,quickG,jogSelect,jog1,jog2,jog3,jog4,pausaNada,pausaVoltarJogar,pausaVoltarMenu,pegasus,pegasusjn,pegasusmn,lion,lionjn,lionmn,dragon,dragonjn,dragonmn,eagle,eaglejn,eaglemn] [tank1,tank2,tank3,tank4,bala,laser,choque,vidas0,vidas1,vidas2,vidas3,choque0,choque1,choque2,choque3,laser0,laser1,laser2,laser3,bloco,blocoind,blocodes])                                                -- estado inicial
            desenhaEstado                                                                                                                                                             -- desenha o estado do jogo
            reageEvento                                                                                                                                                               -- reage a um evento
            reageTempo                                                                                                                                                                -- reage ao passar do tempo
            }

--            