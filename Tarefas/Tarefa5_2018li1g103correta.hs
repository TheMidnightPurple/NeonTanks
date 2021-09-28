-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import LI11819
import Tarefa0_2018li1g103
import Tarefa1_2018li1g103
import Tarefa2_2018li1g103 
import Tarefa3_2018li1g103
import Tarefa4_2018li1g103 
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe (unsafePerformIO) -- importar png
import Graphics.Gloss.Data.Color
import Data.Char

 

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

-- * Estado Jogo

-- * Estado Jogo

png :: FilePath -> IO Picture
png fname = do
  Just img <- loadJuicyPNG fname
  return img

jpG :: Posicao -> Posicao
jpG (l,c) = (c,-l)

myIndex :: [a] -> Int -> a
myIndex (h:t) 0 = h
myIndex (h:t) a = myIndex t (a-1)

reageEvento :: Event -> NewState -> NewState
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 0 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 0 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (SpecialKey KeyRight)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 0 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (SpecialKey KeyLeft)    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 0 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char ',')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 0 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '.')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 0 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '-')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 0 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2)


reageEvento (EventKey (Char 'w')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 1 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 's')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 1 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 'd')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 1 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 'a')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 1 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '1')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 1 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '2')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 1 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '3')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 1 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2)

reageEvento (EventKey (Char 'y')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 2 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 'h')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 2 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 'j')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 2 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 'g')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 2 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '4')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 2 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '5')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 2 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '6')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 2 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2)

reageEvento (EventKey (Char 'f')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 3 (Movimenta C) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 'v')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 3 (Movimenta B) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 'b')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 3 (Movimenta D) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char 'c')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 3 (Movimenta E) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '7')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 3 (Dispara Canhao) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '8')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 3 (Dispara Laser) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento (EventKey (Char '9')    Down _ _) ((Estado mapa jog disp), (c,l), b1, b2) = ((jogada 3 (Dispara Choque) (Estado mapa jog disp)), (c,l) , b1, b2)
reageEvento _ s = s -- ignora qualquer outro evento

reageTempo :: Float -> NewState -> NewState
reageTempo n ((Estado mapa jog disp),(c,l),b1,b2) = (tick(Estado mapa jog disp),(c,l),b1,b2) 

-- * Estado Gloss

type NewState = (Estado,(Float,Float),IO [Picture],IO [Picture])
--- 1 float e imagem de fundo
--- 2 float e tankes e disparos

type NewPosicao = (Float,Float)


avancaFilhoC :: [(Float,Float)] -> [(Float,Float)]
avancaFilhoC [] = []
avancaFilhoC ((x,y):ys) = ((x,y+50):ys)

avancaFilhoD :: [(Float,Float)] -> [(Float,Float)]
avancaFilhoD [] = []
avancaFilhoD ((x,y):ys) = ((x+50,y):ys)

avancaFilhoE :: [(Float,Float)] -> [(Float,Float)]
avancaFilhoE [] = []
avancaFilhoE ((x,y):ys) = ((x-50,y):ys)

avancaFilhoB :: [(Float,Float)] -> [(Float,Float)]
avancaFilhoB [] = []
avancaFilhoB ((x,y):ys) = ((x,y-50):ys)

avanceC :: (Float,Float) -> (Float,Float)
avanceC (x,y) = (x,y+50)

avanceD :: (Float,Float) -> (Float,Float)
avanceD (x,y) = (x+50,y)

avanceE :: (Float,Float) -> (Float,Float)
avanceE (x,y) = (x-50,y)

avanceB :: (Float,Float) -> (Float,Float)
avanceB (x,y) = (x,y-50)


estadoInicial ::  (Float,Float) ->  [Jogador] -> IO [Picture] -> IO [Picture] -> NewState 
estadoInicial (c,l) jog b1 b2 = ((Estado (mapaInicial (15,15)) jog []), (c,l), b1, b2) 

-- 1 float float da posicao inicial do mapa no ecra
-- 2 float float da posicao inicial dos jogadores no ecra


--estadoInicial ::  (Float,Float) -> [(Float,Float)] ->  [Jogador] -> [Picture] -> [Picture] -> NewState 
--estadoInicial (c,l) ((cj,lj):ts) jog [tank1,tank2,tank3,tank4] [dispB, dispV, dispA, dispVe] = ((Estado (mapaInicial (15,15)) jog []), (c,l), ((cj,lj):ts), [tank1,tank2,tank3,tank4], [dispB, dispV, dispA, dispVe]) 


desenhaEstado :: NewState ->  Picture
desenhaEstado ((Estado mapa jog disp), (c,l), [fundo,paineis], [tank1,tank2,tank3,tank4,bala,laser,choque]) = scale 0.82 0.82 $ translate (-450) (350) $ pictures ([scale 0.49 0.49 $ translate (920) (-890) $ fundo] ++
                                                                                                       [scale 0.4 0.4 $ translate (1100) (-890) $ paineis] ++
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-200) $ (desenhaVidasJog1 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-600) $ (desenhaVidasJog2 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1000) $ (desenhaVidasJog3 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2360) (-1400) $ (desenhaVidasJog4 jog)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2530) (-200) $ (desenhaLaserJog1 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2530) (-600) $ (desenhaLaserJog2 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2530) (-1000) $ (desenhaLaserJog3 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2530) (-1400) $ (desenhaLaserJog4 jog)] ++
                                                                                                       [scale 0.4 0.4 $ translate (2700) (-200) $ (desenhaChoqueJog1 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2700) (-600) $ (desenhaChoqueJog2 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2700) (-1000) $ (desenhaChoqueJog3 jog)] ++ 
                                                                                                       [scale 0.4 0.4 $ translate (2700) (-1400) $ (desenhaChoqueJog4 jog)] ++  
                                                                                                       (desenhaMapa (c,l) mapa) ++ 
                                                                                                       (desenhaJogador jog [tank1,tank2,tank3,tank4]) ++ 
                                                                                                       (desenhaDisparos disp [bala,laser,choque] jog)) 


------------------- PNG IMPORTANTES

-- Imagem de Fundo

fundo :: IO Picture
fundo = png "fundoNovo.png"

paineis :: IO Picture
paineis = png "painel.png"

--Jogador Branco

tank1 :: IO Picture
tank1 = png "tanque44.png"

-- Jogador Verde

tank2 :: IO Picture
tank2 = png "tanque11.png"

-- Jogador Azul

tank3 :: IO Picture
tank3 = png "tanque22.png"

-- Jogador Vermelho

tank4 :: IO Picture
tank4 = png "tanque33.png"

-- Bala Canhao

bala :: IO Picture
bala = png "bala.png"

-- Laser 

laser :: IO Picture
laser = png "laser.png"

-- Choque

choque :: IO Picture
choque = png "choque.png"










desenhaDisparos :: [Disparo] -> [Picture] -> [Jogador] -> [Picture]
desenhaDisparos [] _ _ = []
desenhaDisparos _ [] _ = []
desenhaDisparos _ _ [] = []
desenhaDisparos ((DisparoCanhao n (l,c) dir):t) b jog =
                 case dir of 
                      C -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ head b] ++ desenhaDisparos t b jog
                      B -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 180 $ head b] ++ desenhaDisparos t b jog
                      D -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 90 $ head b] ++ desenhaDisparos t b jog
                      E -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 270 $ head b] ++ desenhaDisparos t b jog
desenhaDisparos ((DisparoLaser n (l,c) dir):t) b jog =
                 case dir of 
                      C -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(25-l*50))) $ myIndex b 1] ++ desenhaDisparos t b jog
                      B -> [(translate (fromIntegral(-75+c*50)) (fromIntegral(-75-l*50))) $ rotate 180 $ myIndex b 1] ++ desenhaDisparos t b jog
                      D -> [(translate (fromIntegral(-25+c*50)) (fromIntegral(-25-l*50))) $ rotate 90 $ myIndex b 1] ++ desenhaDisparos t b jog
                      E -> [(translate (fromIntegral(-125+c*50)) (fromIntegral(-25-l*50))) $ rotate 270 $ myIndex b 1] ++ desenhaDisparos t b jog                      
desenhaDisparos (i@(DisparoChoque n tick):t) b jog = [(translate (fromIntegral(-77+(losChoquesC i jog)*50)) (fromIntegral(-25-(losChoquesL i jog)*50))) $ myIndex b 2] ++ desenhaDisparos t b jog                     





losChoquesL :: Disparo -> [Jogador] -> Int
losChoquesL (DisparoChoque n tick) (h:t) = (daChoquel(encontraIndiceLista n (h:t)))

losChoquesC :: Disparo -> [Jogador] -> Int
losChoquesC (DisparoChoque n tick) (h:t) = (daChoquec(encontraIndiceLista n (h:t)))


daChoquel :: Jogador -> Int
daChoquel (Jogador (l,c) dir v las ch) = l

daChoquec :: Jogador -> Int
daChoquec (Jogador (l,c) dir v las ch) = c                                                                                                          
                                                                 
--------------- Jogadores



desenhaJogador :: [Jogador] -> [Picture] -> [Picture]
desenhaJogador _ [] = []
desenhaJogador [] _ = []
desenhaJogador ((Jogador (l,c) dir v las ch):t) (x:xs) | (dir == C) && (v /= 0) = [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ x] ++ desenhaJogador t xs
                                                       | (dir == B) && (v /= 0) = [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 180 $ x] ++ desenhaJogador t xs
                                                       | (dir == D) && (v /= 0) = [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 90 $ x] ++ desenhaJogador t xs
                                                       | (dir == E) && (v /= 0) = [(translate (fromIntegral(-75+c*50)) (fromIntegral(-25-l*50))) $ rotate 270 $ x] ++ desenhaJogador t xs
                                                       | otherwise = desenhaJogador t xs 

                 
--------------- Mapa


desenhaMapa :: (Float,Float) -> Mapa ->  [Picture]
desenhaMapa (c,l) [] = []
desenhaMapa (c,l) (h:t) =  (desenhaPeca (c,l) h) ++ (desenhaMapa (c,l-50) t)



--- ideias


linhaSem1U :: [Peca] -> [Peca]
linhaSem1U [] = []
linhaSem1U (h:t) = init t


ultimaLinhaSem1U :: Mapa -> Mapa
ultimaLinhaSem1U [] = []
ultimaLinhaSem1U ((h:t):ts) = (linhaSem1U (h:t)):init ts


desenhaPecas1 :: NewPosicao -> [Peca] -> [Picture]
desenhaPecas1 (c,l) [] = []
desenhaPecas1 (c,l) (h:t) = (translate c l $ png "blocoindestrutivel.png"):desenhaPeca (c+50,l) t

                       

--- fim ideias



desenhaPeca :: NewPosicao -> [Peca] -> [Picture]
desenhaPeca (c,l) [] = []
desenhaPeca (c,l) (h:t) | (h == Bloco Indestrutivel) = (translate c l $ png "blocoindestrutivel.png"):desenhaPeca (c+50,l) t
                        | (h == Vazia) = (translate c l $ png "blocosfundo.png"):desenhaPeca (c+50,l) t
                        | otherwise = (translate c l $ png "blocodestrutivel.png"):desenhaPeca (c+50,l) t


---- Desenhar Vidas

desenhaVidas ::  Jogador -> Picture
desenhaVidas (Jogador (l,c) dir v las ch) | (v == 0) = Translate (-200) (-200) $ png "vidas0.png"
                                          | (v == 1) = Translate (-200) (-200) $ png "vidas1.png" 
                                          | (v == 2) = Translate (-200) (-200) $ png "vidas2.png" 
                                          | (v == 3) = Translate (-200) (-200) $ png "vidas3.png"

desenhaVidasJog1 :: [Jogador] -> Picture
desenhaVidasJog1 jog = desenhaVidas (myIndex jog 0) 

desenhaVidasJog2 :: [Jogador] -> Picture
desenhaVidasJog2 jog = desenhaVidas (myIndex jog 1) 

desenhaVidasJog3 :: [Jogador] -> Picture
desenhaVidasJog3 jog = desenhaVidas (myIndex jog 2)

desenhaVidasJog4 :: [Jogador] -> Picture
desenhaVidasJog4 jog = desenhaVidas (myIndex jog 3)                                          


---- Desenhar Numero de Disparos Disponiveis

desenhaLaserJog :: Jogador -> Picture
desenhaLaserJog (Jogador (l,c) dir v las ch) | (las == 0) = Translate (-200) (-200) $ png "laser0.png" 
                                             | (las == 1) = Translate (-200) (-200) $ png "laser1.png"
                                             | (las == 2) = Translate (-200) (-200) $ png "laser2.png"
                                             | (las == 3) = Translate (-200) (-200) $ png "laser3.png" 

desenhaLaserJog1 :: [Jogador] -> Picture
desenhaLaserJog1 jog = desenhaLaserJog (myIndex jog 0)

desenhaLaserJog2 :: [Jogador] -> Picture
desenhaLaserJog2 jog = desenhaLaserJog (myIndex jog 1)

desenhaLaserJog3 :: [Jogador] -> Picture
desenhaLaserJog3 jog = desenhaLaserJog (myIndex jog 2)

desenhaLaserJog4 :: [Jogador] -> Picture
desenhaLaserJog4 jog = desenhaLaserJog (myIndex jog 3)                                              


------ Desenhar Numero de Choques

desenhaChoqueJog :: Jogador -> Picture
desenhaChoqueJog (Jogador (l,c) dir v las ch) | (ch == 0) = Translate (-200) (-200) $ png "choque0.png" 
                                              | (ch == 1) = Translate (-200) (-200) $ png "choque1.png"
                                              | (ch == 2) = Translate (-200) (-200) $ png "choque2.png"
                                              | (ch == 3) = Translate (-200) (-200) $ png "choque3.png" 

desenhaChoqueJog1 :: [Jogador] -> Picture
desenhaChoqueJog1 jog = desenhaChoqueJog (myIndex jog 0)

desenhaChoqueJog2 :: [Jogador] -> Picture
desenhaChoqueJog2 jog = desenhaChoqueJog (myIndex jog 1)

desenhaChoqueJog3 :: [Jogador] -> Picture
desenhaChoqueJog3 jog = desenhaChoqueJog (myIndex jog 2)

desenhaChoqueJog4 :: [Jogador] -> Picture
desenhaChoqueJog4 jog = desenhaChoqueJog (myIndex jog 3)



fr :: Int
fr = 3

dm :: Display
dm = InWindow "Neon Tanks" (800, 600) (0, 0)

main :: IO ()
main =  play dm  -- janela onde irá correr o jogo
            (black)  -- côr do fundo da janela
            fr  -- frame rate
            (estadoInicial  (-100,0) [(Jogador (1,1) C 3 3 3), (Jogador (1,12) C 3 3 3), (Jogador (12,1) C 3 3 3), (Jogador (12,12) C 3 3 3)] [fundo,paineis] [tank1,tank2,tank3,tank4,bala,laser,choque])                                                -- estado inicial
            desenhaEstado  -- desenha o estado do jogo
            reageEvento  -- reage a um evento
            reageTempo  -- reage ao passar do tempo
