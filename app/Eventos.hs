module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import System.Exit (exitSuccess)
import LI12425
import Tarefa1



reageEventos :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventos key i@ImmutableTowers {menu = MenuInicial _} = reageEventosMenuInicial key i
reageEventos key i@ImmutableTowers {menu = MenuMapa _} = reageEventosMenuMapa key i
reageEventos key i@ImmutableTowers {menu = MenuJogo _} = reageEventosMenuPausa key i
reageEventos key i@ImmutableTowers {menu = Jogando _} = reageEventosJogando key i
reageEventos key i@ImmutableTowers {menu = Final _} = reageEventosFinal key i

reageEventosMenuInicial :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosMenuInicial (EventKey (SpecialKey KeyDown) Down _ _) i@ImmutableTowers {menu = MenuInicial Jogar} = return $ i {menu = MenuInicial Sair}
reageEventosMenuInicial (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {menu = MenuInicial Jogar} = return $ i {menu = MenuMapa Mapa1}
reageEventosMenuInicial (EventKey (SpecialKey KeyUp) Down _ _) i@ImmutableTowers {menu = MenuInicial Sair} = return $ i {menu = MenuInicial Jogar}
reageEventosMenuInicial (EventKey (SpecialKey KeyEnter) Down _ _) ImmutableTowers {menu = MenuInicial Sair} = exitSuccess
reageEventosMenuInicial _ i =  return $ i

reageEventosMenuMapa :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosMenuMapa (EventKey (SpecialKey KeyRight) Down _ _) i@ImmutableTowers {menu = MenuMapa Mapa1} = return $ i {menu = MenuMapa Mapa2}
reageEventosMenuMapa (EventKey (SpecialKey KeyLeft) Down _ _) i@ImmutableTowers {menu = MenuMapa Mapa2} = return $ i {menu = MenuMapa Mapa1}
reageEventosMenuMapa (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {menu = MenuMapa Mapa1} = return $ i {menu = Jogando (LojaFechada Normal)}
reageEventosMenuMapa _ i = return $ i

reageEventosJogando :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosJogando (EventKey (Char 'b') Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada Normal)} = return $ i {menu = Jogando (LojaAberta TorreFogo)}
reageEventosJogando (EventKey (Char 'b') Down _ _) i@ImmutableTowers {menu = Jogando (LojaAberta TorreFogo)} = return $ i {menu = Jogando (LojaFechada Normal)}
reageEventosJogando (EventKey (SpecialKey KeyDown) Down _ _) i@ImmutableTowers {menu = Jogando (LojaAberta TorreFogo)} = return $ i {menu = Jogando (LojaAberta TorreGelo)}
reageEventosJogando (EventKey (SpecialKey KeyDown) Down _ _) i@ImmutableTowers {menu = Jogando (LojaAberta TorreGelo)} = return $ i {menu = Jogando (LojaAberta TorreResina)}
reageEventosJogando (EventKey (SpecialKey KeyUp) Down _ _) i@ImmutableTowers {menu = Jogando (LojaAberta TorreResina)} = return $ i {menu = Jogando (LojaAberta TorreGelo)}
reageEventosJogando (EventKey (SpecialKey KeyUp) Down _ _) i@ImmutableTowers {menu = Jogando (LojaAberta TorreGelo)} = return $ i {menu = Jogando (LojaAberta TorreFogo)}

reageEventosJogando (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {jogoIT = j , menu = Jogando (LojaAberta TorreFogo)} = if validaCreditosFogo (baseJogo j) then return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (6,6)))} else return $ i {menu = Jogando (LojaFechada Normal)}
reageEventosJogando (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {jogoIT = j , menu = Jogando (LojaAberta TorreGelo)} = if validaCreditosGelo (baseJogo j) then return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (6,6)))} else return $ i {menu = Jogando (LojaFechada Normal)}
reageEventosJogando (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {jogoIT = j , menu = Jogando (LojaAberta TorreResina)} = if validaCreditosResina (baseJogo j) then return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (6,6)))} else return $ i {menu = Jogando (LojaFechada Normal)}

reageEventosJogando (EventKey (SpecialKey KeyLeft) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x-1,y)))}
reageEventosJogando (EventKey (SpecialKey KeyRight) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x+1,y)))}
reageEventosJogando (EventKey (SpecialKey KeyUp) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x,y-1)))}
reageEventosJogando (EventKey (SpecialKey KeyDown) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x,y+1)))}

reageEventosJogando (EventKey (SpecialKey KeyLeft) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x-1,y)))}
reageEventosJogando (EventKey (SpecialKey KeyRight) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x+1,y)))}
reageEventosJogando (EventKey (SpecialKey KeyUp) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x,y-1)))}
reageEventosJogando (EventKey (SpecialKey KeyDown) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x,y+1)))}

reageEventosJogando (EventKey (SpecialKey KeyLeft) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x-1,y)))}
reageEventosJogando (EventKey (SpecialKey KeyRight) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x+1,y)))}
reageEventosJogando (EventKey (SpecialKey KeyUp) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x,y-1)))}
reageEventosJogando (EventKey (SpecialKey KeyDown) Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x,y)))} = return $ i {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x,y+1)))}

reageEventosJogando (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {jogoIT = jogoAtual, menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo pos))} = return $ i {jogoIT = transformaTorre TorreFogo pos jogoAtual, menu = Jogando (LojaFechada Normal)}
reageEventosJogando (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {jogoIT = jogoAtual, menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo pos))} = return $ i {jogoIT = transformaTorre TorreGelo pos jogoAtual, menu = Jogando (LojaFechada Normal)}
reageEventosJogando (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {jogoIT = jogoAtual, menu = Jogando (LojaFechada (PosicionandoTorre TorreResina pos))} = return $ i {jogoIT = transformaTorre TorreResina pos jogoAtual , menu = Jogando (LojaFechada Normal)}

reageEventosJogando (EventKey (Char 'p') Down _ _) i@ImmutableTowers {menu = Jogando (LojaFechada Normal)} = return $ i {menu = MenuJogo Continuar}
reageEventosJogando _ i = return $ i



reageEventosFinal :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosFinal (EventKey (SpecialKey KeyEnter) Down _ _) ImmutableTowers {menu = Final Vitoria} = exitSuccess
reageEventosFinal (EventKey (SpecialKey KeyEnter) Down _ _) ImmutableTowers {menu = Final Derrota} = exitSuccess
reageEventosFinal _ i = return $ i

reageEventosMenuPausa :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosMenuPausa (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {menu = MenuJogo Continuar} = return $ i {menu = Jogando (LojaFechada Normal)}
reageEventosMenuPausa (EventKey (SpecialKey KeyDown) Down _ _) i@ImmutableTowers {menu = MenuJogo Continuar} = return $ i {menu = MenuJogo SairDoJogo}
reageEventosMenuPausa (EventKey (SpecialKey KeyUp) Down _ _) i@ImmutableTowers {menu = MenuJogo SairDoJogo} = return $ i {menu = MenuJogo Continuar}
reageEventosMenuPausa (EventKey (SpecialKey KeyEnter) Down _ _) i@ImmutableTowers {menu = MenuJogo SairDoJogo} = return $ i {menu = MenuInicial Jogar}
reageEventosMenuPausa _ i = return i

transformaTorre :: OpcaoTorres -> Posicao -> Jogo -> Jogo
transformaTorre TorreFogo posicao j@Jogo {mapaJogo = mapa, torresJogo = torres, baseJogo = b@Base{creditosBase = creditos}} =
    let novaTorre = Torre {posicaoTorre = posicao, danoTorre = 2, alcanceTorre = 2.75, rajadaTorre = 2, cicloTorre = 3, tempoTorre = 4, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}}
        jogoAtualizado = j {torresJogo = novaTorre : torres, baseJogo = b {creditosBase = creditos - 90}}
    in if validaTorresSobrepostas' jogoAtualizado && ehRelva posicao mapa then jogoAtualizado else j

transformaTorre TorreGelo posicao j@Jogo {mapaJogo = mapa,torresJogo = torres, baseJogo = b@Base{creditosBase = creditos}} =
    let novaTorre = Torre {posicaoTorre = posicao, danoTorre = 1.75, alcanceTorre = 2.5, rajadaTorre = 2, cicloTorre = 2, tempoTorre = 2, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 5}}
        jogoAtualizado = j {torresJogo = novaTorre : torres, baseJogo = b {creditosBase = creditos - 80}}
    in if validaTorresSobrepostas' jogoAtualizado && ehRelva posicao mapa then jogoAtualizado else j

transformaTorre TorreResina posicao j@Jogo {mapaJogo = mapa,torresJogo = torres, baseJogo = b@Base{creditosBase = creditos}} =
    let novaTorre = Torre {posicaoTorre = posicao, danoTorre = 1.5, alcanceTorre = 2, rajadaTorre = 2, cicloTorre = 3, tempoTorre = 2, projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}}
        jogoAtualizado = j {torresJogo = novaTorre : torres, baseJogo = b {creditosBase = creditos - 70}}
    in if validaTorresSobrepostas' jogoAtualizado && ehRelva posicao mapa then jogoAtualizado else j

validaTorresSobrepostas' :: Jogo -> Bool
validaTorresSobrepostas' Jogo {torresJogo = []} = True
validaTorresSobrepostas' j@Jogo {torresJogo = (torre : torresRestantes)} =
    torreValida torre torresRestantes && validaTorresSobrepostas' j {torresJogo = torresRestantes}
  where
    torreValida :: Torre -> [Torre] -> Bool
    torreValida _ [] = True
    torreValida t@(Torre {posicaoTorre = pos1}) (Torre {posicaoTorre = pos2} : torres) = distancia pos1 pos2 > 1 && torreValida t torres
        
validaCreditosFogo :: Base -> Bool --verifica se tem creditos suficientes para comprar a torre de fogo
validaCreditosFogo (Base {creditosBase = creditos})
 |creditos >= 100 = True
 |otherwise = False

validaCreditosGelo :: Base -> Bool --verifica se tem creditos suficientes para comprar a torre de gelo
validaCreditosGelo (Base {creditosBase = creditos})
 |creditos >= 80 = True
 |otherwise = False

validaCreditosResina :: Base -> Bool --verifica se tem creditos suficientes para comprar a torre de resina
validaCreditosResina (Base {creditosBase = creditos})
 |creditos >= 70 = True
 |otherwise = False




-- Função que verifica se a posição é válida (terreno relva)
ehRelva :: Posicao -> Mapa -> Bool
ehRelva (x, y) mapa =
    let linha = floor y
        coluna = floor x
    in linha >= 0 && coluna >= 0 && linha < length mapa && coluna < length (head mapa) && (mapa !! linha !! coluna == Relva)



  