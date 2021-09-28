-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g103 where

import LI11819
import Tarefa0_2018li1g103
import Tarefa1_2018li1g103
import Data.List

-- * Testes
--
-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56]
         where t1 = (0,Movimenta C, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))    
               t2 = (0,Movimenta C, (Estado (mapaInicial (10,10)) [(Jogador (1,1) E 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 1 (3,2) B)] ))
               t3 = (0,Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (1,1) D 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 2 (3,2) B)] ))
               t4 = (1,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) E 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (7,7) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoChoque 2 4)] ))
               t5 = (0,Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (1,1) D 1 1 1), (Jogador (1,3) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 2 (4,3) B)] ))
               t6 = (0,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (1,1) B 1 1 1), (Jogador (3,1) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 2 (4,4) B)] ))
               t7 = (0,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,4) E 1 1 1), (Jogador (1,2) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoChoque 2 4), (DisparoCanhao 2 (4,7) B)] ))
               t8 = (0,Movimenta C, (Estado (mapaInicial (10,10)) [(Jogador (3,1) C 1 1 1), (Jogador (1,1) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 3 (4,5) E),(DisparoChoque 2 4), (DisparoCanhao 2 (4,7) B)] ))
               t9 = (1,Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (7,7) D 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t10 = (1,Movimenta C, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (7,7) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) D)] ))
               t11 = (0,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (7,1) B 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t12 = (0,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) E 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t13 = (0,Movimenta C, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t14 = (0,Dispara Laser, (Estado (mapaInicial (99,99)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (3,2) B)] )) 
               t15 = (0,Dispara Choque, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 0), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoChoque 0 2)] )) 
               t16 = (0,Dispara Canhao, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 0 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) D)] ))
               t17 = (0,Dispara Laser, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 0 2 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (3,2) B)] ))
               t18 = (0,Dispara Choque, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 0 1 0), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoChoque 0 2)] )) 
               t19 = (2,Dispara Canhao, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 0 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 2 (3,2) E)] ))
               t20 = (1,Dispara Choque, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 0 1), (Jogador (1,3) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoChoque 0 3)] ))
               t21 = (2,Dispara Choque, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 0 1), (Jogador (1,3) B 1 1 1), (Jogador (3,1) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoChoque 0 3)] ))
               t22 = (3,Dispara Choque, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 0 0), (Jogador (1,3) B 1 1 1), (Jogador (3,1) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoChoque 0 3)] ))
               t23 = (0,Dispara Laser, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 0 0), (Jogador (1,3) B 1 1 1), (Jogador (3,1) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,3) C)] ))
               t24 = (1,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) C 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) D)] )) 
               t25 = (2,Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) C 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 1 (3,2) C)] ))
               t26 = (3,Movimenta C, (Estado (mapaInicial (50,50)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 2 (3,2) E)] ))
               t27 = (0,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (1,1) D 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 3 (3,2) B)] ))
               t28 = (1,Movimenta D, (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoChoque 0 2)] )) 
               t29 = (2,Movimenta D, (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B), (DisparoLaser 2 (4,4) E), (DisparoChoque 1 2), (DisparoCanhao 3 (5,5) C)] ))
               t30 = (3,Movimenta D, (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t31 = (0,Movimenta B, (Estado (mapaInicial (25,25)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] )) 
               t32 = (1,Movimenta B, (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]) [(Jogador (1,2) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] )) 
               t33 = (2,Movimenta B, (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]) [(Jogador (1,1) C 1 1 1), (Jogador (4,6) B 0 1 1), (Jogador (6,6) B 0 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] )) 
               t34 = (3,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 0), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] )) 
               t35 = (0,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 0 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 0 1 0)] [(DisparoCanhao 0 (3,2) B)] )) 
               t36 = (1,Movimenta E, (Estado (mapaInicial (75,75)) [(Jogador (1,1) C 1 0 1), (Jogador (4,4) B 0 0 0), (Jogador (6,6) B 0 1 1), (Jogador (2,6) B 1 1 0)] [(DisparoCanhao 0 (3,2) B)] )) 
               t37 = (2,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] )) 
               t38 = (3,Movimenta E, (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (7,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t39 = (3,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 0 1 1), (Jogador (4,4) B 0 1 1), (Jogador (6,6) B 0 1 1), (Jogador (2,6) B 0 1 1)] [(DisparoCanhao 0 (3,2) B), (DisparoChoque 1 2), (DisparoLaser 3 (4,7) C), (DisparoCanhao 2 (1,6) E)] )) 
               t40 = (1,Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (1,7) D 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t41 = (1,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (7,1) B 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] )) 
               t42 = (1,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (7,1) E 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t43 = (1,Movimenta B, (Estado (mapaInicial (6,6)) [(Jogador (1,1) D 1 1 1), (Jogador (3,1) B 1 1 1), (Jogador (1,3) B 1 1 1), (Jogador (3,3) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] )) 
               t44 = (0,Dispara Laser, (Estado (mapaInicial (10,10)) [(Jogador (1,1) D 1 1 0), (Jogador (1,3) B 1 1 1), (Jogador (3,1) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,3) D)] ))
               t45 = (0,Dispara Laser, (Estado (mapaInicial (10,10)) [(Jogador (1,1) E 1 1 0), (Jogador (1,3) B 1 1 1), (Jogador (3,1) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,3) E)] )) 
               t46 = (0,Dispara Canhao, (Estado (mapaInicial (10,10)) [(Jogador (1,1) D 1 0 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) D)] ))
               t47 = (0,Dispara Canhao, (Estado (mapaInicial (10,10)) [(Jogador (1,3) E 1 0 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) E)] )) 
               t48 = (0,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (1,1) B 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t49 = (0,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) D 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t50 = (3,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (3,3) B 1 1 1), (Jogador (5,5) B 1 1 1), (Jogador (7,7) E 1 1 1)] [(DisparoCanhao 0 (3,2) B)] ))
               t51 = (0,Dispara Laser, (Estado (mapaInicial (10,10)) [(Jogador (1,1) B 1 1 0), (Jogador (1,3) B 1 1 1), (Jogador (3,1) B 1 1 1), (Jogador (2,6) B 1 1 1)] [(DisparoLaser 0 (2,3) B)] ))
               t52 = (3,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) E 2 1 1)] [(DisparoCanhao 0 (3,2) B), (DisparoChoque 1 2), (DisparoLaser 3 (4,7) C), (DisparoCanhao 2 (1,6) E)] ))
               t53 = (3,Movimenta C, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) B 2 1 1)] [(DisparoCanhao 0 (3,2) B), (DisparoChoque 1 2), (DisparoLaser 3 (4,7) C), (DisparoCanhao 2 (1,6) E)] ))
               t54 = (3,Movimenta E, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) C 2 1 1)] [(DisparoCanhao 0 (3,2) B), (DisparoChoque 1 2), (DisparoLaser 3 (4,7) C), (DisparoCanhao 2 (1,6) E)] ))
               t55 = (3,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (4,4) B 1 1 1), (Jogador (6,6) B 1 1 1), (Jogador (2,6) C 2 1 1)] [(DisparoCanhao 0 (3,2) B), (DisparoChoque 1 2), (DisparoLaser 3 (4,7) C), (DisparoCanhao 2 (1,6) E)] ))
               t56 = (2,Movimenta B, (Estado (mapaInicial (10,10)) [(Jogador (1,1) C 1 1 1), (Jogador (3,3) B 1 1 1), (Jogador (5,5) B 1 1 1), (Jogador (7,5) E 0 1 1)] [] ))
                       
-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

jogada n (Movimenta d) (Estado mapa jog disp) | (vidaBool (encontraIndiceLista n jog)) && (temChoqueLista disp) = (Estado mapa (atualizaIndiceLista n (alterPos d mapa jog (encontraIndiceLista n jog)) jog) disp)
                                                   | (vidaBool (encontraIndiceLista n jog)) && (jogEmChoque' n (jogChoque disp) jog)==False = (Estado mapa (atualizaIndiceLista n (alterPos d mapa jog (encontraIndiceLista n jog)) jog) disp)
                                                   | (vidaBool (encontraIndiceLista n jog)) && d /= (direcaoJogador (encontraIndiceLista n jog)) = (Estado mapa (atualizaIndiceLista n (rodaJ d (encontraIndiceLista n jog)) jog) disp)
                                                   | otherwise = (Estado mapa jog disp)

jogada n (Dispara a) (Estado mapa jog disp) | vidaBool (encontraIndiceLista n jog) && temDisparo Laser (encontraIndiceLista n jog) && a==Laser = (Estado mapa (atualizaIndiceLista n (alterDisp a (encontraIndiceLista n jog)) jog) (insereDisp (dispDIr n a (encontraIndiceLista n jog)) disp))
                                                 | vidaBool (encontraIndiceLista n jog) && temDisparo Choque (encontraIndiceLista n jog) && a==Choque = (Estado mapa (atualizaIndiceLista n (alterDisp a (encontraIndiceLista n jog)) jog) (insereDisp (dispDIr n a (encontraIndiceLista n jog)) disp))
                                                 | vidaBool (encontraIndiceLista n jog) && a==Canhao = (Estado mapa (atualizaIndiceLista n (alterDisp a (encontraIndiceLista n jog)) jog) (insereDisp (dispDIr n a (encontraIndiceLista n jog)) disp))
                                                 | otherwise = (Estado mapa jog disp)

-- | Efetua uma jogada.

-- * Funções relativas ao choque

jogadaBot :: Int -- ^ O identificador do 'Jogador' que efetua a jogada
          -> Maybe Jogada -- ^ A 'Jogada' a efetuar
          -> Estado -- ^ O 'Estado' anterior
          -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada

jogadaBot n (Just (Movimenta d)) (Estado mapa jog disp) | (vidaBool (encontraIndiceLista n jog)) && (temChoqueLista disp) = (Estado mapa (atualizaIndiceLista n (alterPos d mapa jog (encontraIndiceLista n jog)) jog) disp)
                                                        | (vidaBool (encontraIndiceLista n jog)) && (jogEmChoque' n (jogChoque disp) jog)==False = (Estado mapa (atualizaIndiceLista n (alterPos d mapa jog (encontraIndiceLista n jog)) jog) disp)
                                                        | (vidaBool (encontraIndiceLista n jog)) && d /= (direcaoJogador (encontraIndiceLista n jog)) = (Estado mapa (atualizaIndiceLista n (rodaJ d (encontraIndiceLista n jog)) jog) disp)
                                                        | otherwise = (Estado mapa jog disp)

jogadaBot n (Just (Dispara a)) (Estado mapa jog disp) | vidaBool (encontraIndiceLista n jog) && temDisparo Laser (encontraIndiceLista n jog) && a==Laser = (Estado mapa (atualizaIndiceLista n (alterDisp a (encontraIndiceLista n jog)) jog) (insereDisp (dispDIr n a (encontraIndiceLista n jog)) disp))
                                                      | vidaBool (encontraIndiceLista n jog) && temDisparo Choque (encontraIndiceLista n jog) && a==Choque = (Estado mapa (atualizaIndiceLista n (alterDisp a (encontraIndiceLista n jog)) jog) (insereDisp (dispDIr n a (encontraIndiceLista n jog)) disp))
                                                      | vidaBool (encontraIndiceLista n jog) && a==Canhao = (Estado mapa (atualizaIndiceLista n (alterDisp a (encontraIndiceLista n jog)) jog) (insereDisp (dispDIr n a (encontraIndiceLista n jog)) disp))
                                                      | otherwise = (Estado mapa jog disp)                                                         

jogadaBot _ Nothing e = e

-- | Calcula a distância entre dois jogadores em linhas e colunas.
posChoque :: Posicao -- ^ 'Posição' de um dos 'Jogador'es.
          -> Posicao -- ^ 'Posição' do outro 'Jogador'.
          -> (Int,Int) -- ^ Diferença de 'Posição'.
posChoque (l,c) (l1,c1)  = (abs(l1-l),abs(c-c1))

-- | Verifica se a posição de um jogador está afetada pelo choque.
jogEmChoque' :: Int -- ^ Indice de um 'Jogador'.
             -> [Int] -- ^ Lista de indices de 'Jogador'es com 'DisparoChoque' ativo.
             -> [Jogador] -- ^ Lista de 'Jogador'es.
             -> Bool -- ^ É verdadeiro quando o 'Jogador' não está na área 'DisparoChoque'.
jogEmChoque' n [] _ = False
jogEmChoque' n (x:xs) l | n/=x = fst (posChoque (posicaoJogador (encontraIndiceLista x l)) (posicaoJogador(encontraIndiceLista n l))) <=3 && snd (posChoque (posicaoJogador (encontraIndiceLista x l)) (posicaoJogador(encontraIndiceLista n l))) <= 3
                        | otherwise = jogEmChoque' n xs l

{- 'posChoque'
   'posicaoJogador'
   'encontraIndiceLista' -}

-- | Verifica se Existe um DisparoChoque e guarda o Indice do Jogador Responsável numa Lista.
jogChoque :: [Disparo] -- ^ Lista de 'Disparo's. 
          -> [Int] -- ^ Indice do 'Jogador' que disparou.
jogChoque [] = []
jogChoque ((DisparoChoque n tik):t) = n:jogChoque t
jogChoque (h:t) = jogChoque t

-- | Verifica se Existem Choques numa Lista de Disparos.
temChoqueLista :: [Disparo] -- ^ Lista de 'Disparo's.
               -> Bool -- ^ Se Existir um 'DisparoChoque' a função é falsa.
temChoqueLista [] = True
temChoqueLista ((DisparoCanhao n p d):t) = temChoqueLista t
temChoqueLista ((DisparoLaser n p d):t) = temChoqueLista t
temChoqueLista ((DisparoChoque n tik):t) = False

-- * Funções de Movimento

-- | Altera a Posição dos Jogadores.
--
-- __NB:__ Foram utilizadas como funções auxiliares 'podeMoverC', 'podeMoverB', 'podeMoverE' e 'podeMoverD'
alterPos :: Direcao -- ^ 'Direcao' para a qual o 'Jogador' se pretende movimentar.
         -> Mapa -- ^ 'Mapa' do jogo.
         -> [Jogador] -- ^ Lista dos 'Jogador'es.
         -> Jogador -- 'Jogador' que se pretende movimentar.
         -> Jogador -- 'Jogador' resultante de aplicar a alteração de 'Posicao'.
alterPos dir mapa jog (Jogador (l,c) d v las ch)   | dir == C && podeMoverC d jog mapa (l,c) = Jogador (l-1,c) d v las ch
                                                   | dir == B && podeMoverB d jog mapa (l,c) = Jogador (l+1,c) d v las ch
                                                   | dir == E && podeMoverE d jog mapa (l,c) = Jogador (l,c-1) d v las ch
                                                   | dir == D && podeMoverD d jog mapa (l,c) = Jogador (l,c+1) d v las ch
                                                   | dir == D = Jogador (l,c) D v las ch
                                                   | dir == B = Jogador (l,c) B v las ch
                                                   | dir == E = Jogador (l,c) E v las ch
                                                   | dir == C = Jogador (l,c) C v las ch
                                                   | otherwise = (Jogador (l,c) d v las ch)

-- | Roda um 'Jogador'.

rodaJ :: Direcao -- ^ 'Direcao' para a qual pretende Rodar. 
      -> Jogador -- ^ 'Jogador' a ser rodado.
      -> Jogador -- ^ 'Jogador' resultante do processo de rodagem.
rodaJ dir (Jogador (l,c) t v las ch) | dir == D = (Jogador (l,c) D v las ch) 
                                     | dir == B = (Jogador (l,c) B v las ch)
                                     | dir == E = (Jogador (l,c) E v las ch)
                                     | dir == C = (Jogador (l,c) C v las ch)
                                     | otherwise = (Jogador (l,c) t v las ch)

-- ** Funções que restringem o movimento



-- | Verifica se o jogador tem vidas
vidaBool :: Jogador -- ^ 'Jogador' pretendido.
         -> Bool -- ^ Verdadeiro se tiver vidas, Falso caso contrario.
vidaBool Jogador{vidasJogador = t} | t==0 = False
                                   | otherwise = True


-- | Verifice se existe um Bloco numa dada posição.

--
-- __NB:__ Foi usada a função auxiliar 'encontraPosicaoMatriz'

eBloco :: Posicao -- ^ 'Posicao' a verificar.
       -> Mapa -- ^ 'Mapa' do jogo.
       -> Bool -- ^ É verdadeiro quando a 'Peca' é 'Vazia'.
eBloco (l,c) mapa | encontraPosicaoMatriz (l,c) mapa /= Vazia = False
                  | otherwise = True


-- | Verifica se um jogador se pode mover numa dada direção.

podeMoverD :: Direcao -- ^ 'Direcao' desejada 
           -> [Jogador] -- ^ Lista 'Jogador'es
           -> Mapa -- ^ 'Mapa' do Jogo
           -> Posicao -- ^ 'Posicao' Jogador
           -> Bool -- ^ Verdadeiro ou Falso

podeMoverD d jog mapa (l,c) = temJog (l,c+2) jog && temJog (l-1,c+2) jog && temJog (l+1,c+2) jog && eBloco (l+1,c+2) mapa && eBloco (l,c+2) mapa && d == D

-- | Verifica se um jogador se pode mover para a Esquerda.

podeMoverE :: Direcao -- ^ 'Direcao' desejada .
           -> [Jogador] -- ^ Lista 'Jogador'es.
           -> Mapa -- ^ 'Mapa' do Jogo.
           -> Posicao -- ^ 'Posicao' Jogador.
           -> Bool -- ^ Verdadeiro ou Falso.
podeMoverE d jog mapa (l,c) = temJog (l,c-2) jog && temJog (l-1,c-2) jog && temJog (l+1,c-2) jog && eBloco (l+1,c-1) mapa && eBloco (l,c-1) mapa && d == E

-- | Verifica se um jogador se pode mover para Cima.

podeMoverC :: Direcao -- ^ 'Direcao' desejada 
           -> [Jogador] -- ^ Lista 'Jogador'es.
           -> Mapa -- ^ 'Mapa' do Jogo.
           -> Posicao -- ^ 'Posicao' Jogador.
           -> Bool -- ^ Verdadeiro ou Falso.
podeMoverC d jog mapa (l,c) = temJog (l-2,c+1) jog && temJog (l-2,c-1) jog && temJog (l-2,c) jog && eBloco (l-1,c) mapa && eBloco (l-1,c+1) mapa && d == C

-- | Verifica se um jogador se pode mover para Baixo.

podeMoverB :: Direcao -- ^ 'Direcao' desejada .
           -> [Jogador] -- ^ Lista 'Jogador'es.
           -> Mapa -- ^ 'Mapa' do Jogo.
           -> Posicao -- ^ 'Posicao' Jogador.
           -> Bool -- ^ Verdadeiro ou Falso.
podeMoverB d jog mapa (l,c) = temJog (l+2,c) jog && temJog (l+2,c-1) jog && temJog (l+2,c+1) jog && eBloco (l+2,c) mapa && eBloco (l+2,c+1) mapa && d == B


-- | Verifica se existe algum jogador numa poisição
--
-- __NB:__ Foi usada a função auxiliar 'posicaoJogador'


temJog :: Posicao -- ^ 'Posicao' no 'Mapa' do Jogo
       -> [Jogador] -- ^ Lista de 'Jogador'es
       -> Bool -- Verdadeiro se não tiver, Falso se tiver 
temJog (l,c) [] = True
temJog (l,c) (h:t) | (l,c) == (posicaoJogador (h)) && (vidasJogador h)>0 = False
                   | otherwise = temJog (l,c) t


-- * Funções sobre Disparos

-- | Insere o disparo na lista de disparos 

insereDisp :: Disparo -- ^ 'Disparo' desejado 
           -> [Disparo] -- ^ Lista de 'Disparo's
           -> [Disparo] -- ^ Lista de 'Disparo's modificada
insereDisp d [] = [d]
insereDisp d (h:t) = (d:h:t)


-- | Atualiza a posicao do disparo

dispDIr :: Int -- ^ Indice do 'Jogador' 
        -> Arma -- ^ 'Arma' desejada
        -> Jogador -- ^ Indicar Jogador
        -> Disparo -- ^ 'Disparo' modificado

dispDIr n Canhao (Jogador (l,c) d v las ch) | d==C = (DisparoCanhao n (l-1,c) d)
                                            | d==B = (DisparoCanhao n (l+1,c) d)
                                            | d==D = (DisparoCanhao n (l,c+1) d)
                                            | d==E = (DisparoCanhao n (l,c-1) d)

dispDIr n Laser (Jogador (l,c) d v las ch) | d==C = (DisparoLaser n (l-1,c) d)
                                           | d==B = (DisparoLaser n (l+1,c) d)
                                           | d==D = (DisparoLaser n (l,c+1) d)
                                           | d==E = (DisparoLaser n (l,c-1) d)

dispDIr n Choque (Jogador (l,c) d v las ch) = (DisparoChoque n 5)


-- | Altera a quantidade de disparos disponiveis

alterDisp :: Arma -- ^ 'Arma' desejada
          -> Jogador -- ^ 'Jogador' pretendido
          -> Jogador -- ^ 'Jogador' modificado
alterDisp a (Jogador (l,c) d v las ch) | a==Choque =  (Jogador (l,c) d v las (ch-1))
                                       | a==Laser =  (Jogador (l,c) d v (las-1) ch)
                                       | otherwise = (Jogador (l,c) d v las ch)
-- | Verifica se um Jogador tem balas.
temDisparo :: Arma -- ^ 'Arma' que o 'Jogador' tentou disparar.
           -> Jogador -- ^ 'Jogador' que disparou.
           -> Bool -- ^ Verdadeiro quando o 'Jogador' tem balas.
temDisparo Choque (Jogador (l,c) d v las ch) | ch<=0 = False
                                             | otherwise = True

temDisparo Laser (Jogador (l,c) d v las ch) | las<=0 = False
                                            | otherwise = True

