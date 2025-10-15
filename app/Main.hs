module Main where

import LI12425
import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import Graphics.Gloss.Interface.IO.Game (playIO)


janela :: Display
janela = FullScreen

fundo :: Color
fundo = white

fr :: Int
fr = 60

main :: IO ()
main = do
  imagensTerreno <- loadImagensTerreno
  imagensElementos <- loadImagensElementos
  imagensInimigos <- loadImagensInimigos
  imagensMenu <- loadImagensMenu 
  imagensFinal <- loadImagensFinal
  playIO janela fundo fr (it imagensTerreno imagensElementos imagensInimigos imagensMenu imagensFinal) desenha reageEventos reageTempo

  where
    -- Função que inicializa o estado com as imagens
    it imagensTerreno imagensElementos imagensInimigos imagensMenu imagensFinal = ImmutableTowers 
      { jogoIT = jogoInicial,

        menu = MenuInicial Jogar,
        imagensTerreno = imagensTerreno,
        imagensElementos = imagensElementos,
        imagensInimigos = imagensInimigos, 
        imagensMenu = imagensMenu,
        imagensFinal = imagensFinal
      }

jogoInicial :: Jogo
jogoInicial = Jogo
    { baseJogo = Base {vidaBase = 100, posicaoBase = (0,11), creditosBase = 200}
    , portaisJogo = [Portal {posicaoPortal = (0,2), ondasPortal = [Onda 
        { inimigosOnda = [inimigo01,inimigo01,inimigo01,inimigo01,inimigo01,inimigo01,inimigo01,inimigo01,inimigo01,inimigo01,inimigo01,inimigo01],
          cicloOnda = 5.0,            
          tempoOnda = 5.0,            
          entradaOnda = 0         
        }]}]
    , torresJogo = []
    , mapaJogo = mapa01
    , inimigosJogo = []
    , lojaJogo = [(90, Torre {posicaoTorre = (6,6), danoTorre = 2, alcanceTorre = 2.75, rajadaTorre = 2, cicloTorre = 3, tempoTorre = 4, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}}), --fogo
                  (80, Torre {posicaoTorre = (6,6), danoTorre = 1.75, alcanceTorre = 2.5, rajadaTorre = 2, cicloTorre = 2, tempoTorre = 2, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 5}}), --gelo
                  (70, Torre {posicaoTorre = (6,6), danoTorre = 1.5, alcanceTorre = 2, rajadaTorre = 2, cicloTorre = 3, tempoTorre = 2, projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}}) --resina
                 ]
    }
    where 
       inimigo01 :: Inimigo
       inimigo01 = Inimigo 
                          { posicaoInimigo = (0,2),
                            direcaoInimigo = Este,
                            vidaInimigo = 25,       
                            velocidadeInimigo = 0.7,
                            ataqueInimigo = 20,     
                            butimInimigo = 15,      
                            projeteisInimigo = [] }

mapa01 :: Mapa
mapa01 =
 [ [r, a, r, r, r, r, r, a, r, r, r, r, r, r],
   [r, a, r, r, r, r, r, a, r, r, r, r, r, r],
   [t, t, t, t, t, r, r, a, a, a, a, a, a, a],
   [r, a, r, r, t, r, r, r, t, t, t, t, t, r],
   [r, a, r, r, t, r, r, r, t, r, r, r, t, r],
   [r, a, r, r, t, r, r, r, t, r, r, r, t, r],
   [r, a, r, r, t, r, r, r, t, r, r, r, t, r],
   [r, a, r, r, t, t, t, t, t, r, r, r, t, r],
   [r, a, r, r, r, r, r, r, r, r, r, r, t, r],
   [r, a, a, a, a, a, a, a, a, r, r, r, t, r],
   [r, r, r, r, r, r, r, r, a, r, r, r, t, r],
   [t, t, t, t, t, t, t, t, t, t, t, t, t, r],
   [r, r, r, r, r, r, r, r, a, r, r, r, r, r],
   [r, r, r, r, r, r, r, r, a, r, r, r, r, r]
 ]
  where
    t = Terra
    r = Relva
    a = Agua