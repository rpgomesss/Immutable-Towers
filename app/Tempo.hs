module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3
import Tarefa2 ( ganhouJogo, terminouJogo )
import Tarefa1

-- | Função que reage ao passar do tempo, atualizando o estado do jogo apenas se o estado do menu for Jogando.
reageTempo :: Tempo -> ImmutableTowers -> IO ImmutableTowers
reageTempo t i@ImmutableTowers {jogoIT = jogoAtual,menu = Jogando _} 
    | terminouJogo jogoAtualizado =
        if ganhouJogo jogoAtualizado
        then return $ i {menu = Final Vitoria}
        else return $ i {menu = Final Derrota}
    | otherwise = return $ i {jogoIT = if validaJogo jogoAtualizado then jogoAtualizado else jogoAtual}
  where
    jogoAtualizado = atualizaJogo t jogoAtual
reageTempo _ i = return $ i 

