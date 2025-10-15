module ImmutableTowers where
import LI12425
import Graphics.Gloss


data ImmutableTowers = ImmutableTowers 
                       { jogoIT :: Jogo,            -- O estado atual do jogo
                         menu :: Menu,              -- O estado atual do menu       
                         imagensTerreno :: [Picture],
                         imagensInimigos :: [Picture],
                         imagensElementos :: [Picture],
                         imagensMenu :: [Picture],
                         imagensFinal :: [Picture]
                         } 


data OpcaoMenuInicial = Jogar | Sair

data OpcaoMenuMapa = Mapa1 | Mapa2

data OpcaoMenuJogo = Continuar | SairDoJogo

data OpcaoMenuLoja = LojaAberta OpcaoTorres | LojaFechada OpcaoLojaFechada

data OpcaoLojaFechada = PosicionandoTorre OpcaoTorres Posicao | Normal

data OpcaoTorres = TorreFogo | TorreGelo | TorreResina

data OpcaoFinal = Vitoria | Derrota 

data Menu = MenuInicial OpcaoMenuInicial
          | MenuMapa OpcaoMenuMapa
          | MenuJogo OpcaoMenuJogo
          | Jogando OpcaoMenuLoja
          | Final OpcaoFinal