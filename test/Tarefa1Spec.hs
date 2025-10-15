module Tarefa1Spec (testesTarefa1) where
  
import Test.HUnit
import LI12425
import Tarefa1


testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ "testa a funcao principal validaJogo e nela estao implementadas todas as funcoes da tarefa1" ~: True ~=? validaJogo jogoInicial
      ]


jogoInicial :: Jogo
jogoInicial = Jogo
    { baseJogo = Base {vidaBase = 5, posicaoBase = (0,11), creditosBase = 10000}
    , portaisJogo = [Portal {posicaoPortal = (0,2), ondasPortal = [Onda 
        { inimigosOnda = [Inimigo 
                          { posicaoInimigo = (0,2),
                            direcaoInimigo = Este,
                            vidaInimigo = 50,        -- Vida inicial do inimigo
                            velocidadeInimigo = 0.75, -- Velocidade do inimigo
                            ataqueInimigo = 10,     -- Dano que o inimigo causa na base
                            butimInimigo = 5,       -- Créditos ganhos ao derrotar o inimigo
                            projeteisInimigo = []   -- Sem efeitos secundários no início
                          }],
          cicloOnda = 5.0,            -- Tempo entre a entrada de inimigos
          tempoOnda = 5.0,            -- Tempo até o próximo inimigo
          entradaOnda = 0         -- Tempo até a entrada da onda
        }]}]
    , torresJogo = []
    , mapaJogo = mapa01
    , inimigosJogo = []
    , lojaJogo = [(100, Torre {posicaoTorre = (0,0), danoTorre = 10, alcanceTorre = 20, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 1, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}}), --fogo
                  (80, Torre {posicaoTorre = (0,0), danoTorre = 7, alcanceTorre = 20, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 1, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4}}), --gelo
                  (70, Torre {posicaoTorre = (0,0), danoTorre = 5, alcanceTorre = 20, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 1, projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}}) --resina
                 ]
    }

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

