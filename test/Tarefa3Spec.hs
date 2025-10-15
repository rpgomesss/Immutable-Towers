module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import Tarefa3
import LI12425


testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ "Testa a funcao principal atualizaJogo" ~: jogoEsperado ~=? atualizaJogo 1 jogoInicial,

         "Nenhum inimigo no alcance" ~: [] ~=? detetaInimigosNoAlcance [] [] mapa01,

         "Inimigos detectados corretamente" ~: [(torre1, [inimigo1])] ~=? detetaInimigosNoAlcance [torre1] [inimigo1] mapa01,

         "Torres sem inimigos no alcance" ~: ([], []) ~=? torresAtacam 1 mapa01 [] [],
         "Torres atacam corretamente" ~: ([torre1Atualizada],inimigoAtualizado) ~=? torresAtacam 1 mapa01 [torre1] [inimigo1],

         "Base intacta com inimigos vivos" ~: ([inimigo1], base) ~=? atualizaEstadoInimigosEBase [inimigo1] base,
         "Inimigos atingem a base" ~: ([],baseAtualizada) ~=? atualizaEstadoInimigosEBase [inimigo2] base,

         "Inimigo sem efeitos" ~: inimigo1 ~=? aplicaEfeitos 1 inimigo1,
         "Inimigo com efeito de fogo" ~: inimigo4 ~=? aplicaEfeitos 1 inimigo3,

         "Movimento em terreno valido" ~: inimigo6 ~=? moveInimigo 1 mapa01 inimigo5,
         "Movimento em terreno invalido" ~:  inimigo1 ~=? moveInimigo 1 mapa01 inimigo1,

         "Nenhum portal para atualizar" ~: ([], [inimigo1]) ~=? atualizaPortais 1 [] [inimigo1],
         "Portal gera inimigos corretamente" ~: ([portalAtualizado],[]) ~=? atualizaPortais 1 [portal] [],

         "Inimigos detectados corretamente" ~: [(torre1, [inimigo1])] ~=? detetaInimigosNoAlcance [torre1] [inimigo1] mapa01

      ]
      

baseAtualizada :: Base
baseAtualizada = Base {posicaoBase = (0,11), vidaBase = 90, creditosBase = 200}

base :: Base
base = Base {posicaoBase = (0,11), vidaBase = 100, creditosBase = 200}

inimigoAtualizado :: [Inimigo]
inimigoAtualizado = [Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Norte, vidaInimigo = 90.0, velocidadeInimigo = 2.0, ataqueInimigo = 10.0, butimInimigo = 20, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}]}]

torre1Atualizada :: Torre
torre1Atualizada = Torre { posicaoTorre = (0, 0), alcanceTorre = 10, danoTorre = 10, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 1, projetilTorre = Projetil Fogo (Finita 2) }

portalAtualizado :: Portal
portalAtualizado = Portal {posicaoPortal = (0,2), ondasPortal = [Onda 
                                                          { inimigosOnda = [Inimigo 
                                                                              { posicaoInimigo = (0,2),
                                                                                direcaoInimigo = Este,
                                                                                vidaInimigo = 50,
                                                                                velocidadeInimigo = 0.75,
                                                                                ataqueInimigo = 10,     
                                                                                butimInimigo = 5,       
                                                                                projeteisInimigo = []   
                                                                              }
                                                                           ],
                                                           cicloOnda = 5.0,            
                                                           tempoOnda = 4.0,            
                                                           entradaOnda = 0         
                                                         }]}

portal :: Portal
portal = Portal {posicaoPortal = (0,2), ondasPortal = [Onda 
                                                          { inimigosOnda = [Inimigo 
                                                                              { posicaoInimigo = (0,2),
                                                                                direcaoInimigo = Este,
                                                                                vidaInimigo = 50,
                                                                                velocidadeInimigo = 0.75,
                                                                                ataqueInimigo = 10,     
                                                                                butimInimigo = 5,       
                                                                                projeteisInimigo = []   
                                                                              }
                                                                           ],
                                                           cicloOnda = 5.0,            
                                                           tempoOnda = 5.0,            
                                                           entradaOnda = 0         
                                                         }]}

torre1 :: Torre
torre1 = Torre { posicaoTorre = (0, 0), alcanceTorre = 10, danoTorre = 10, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 2) }

inimigo4 :: Inimigo
inimigo4 = Inimigo { posicaoInimigo = (3, 4), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 98, projeteisInimigo = [Projetil Fogo (Finita 1)]}

inimigo3 :: Inimigo
inimigo3 = Inimigo { posicaoInimigo = (3, 4), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [Projetil Fogo (Finita 2)]}

inimigo2 :: Inimigo
inimigo2 = Inimigo { posicaoInimigo = (0, 11), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

inimigo6 :: Inimigo
inimigo6 = Inimigo { posicaoInimigo = (2, 2), direcaoInimigo = Este, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

inimigo5 :: Inimigo
inimigo5 = Inimigo { posicaoInimigo = (0, 2), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

inimigo1 :: Inimigo
inimigo1 = Inimigo { posicaoInimigo = (3, 4), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

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

jogoEsperado :: Jogo
jogoEsperado = Jogo 
    { baseJogo = Base {vidaBase = 5.0, posicaoBase = (0.0,11.0), creditosBase = 10000},
      portaisJogo = [Portal {posicaoPortal = (0.0,2.0), ondasPortal = [Onda 
        { inimigosOnda = [Inimigo
                         { posicaoInimigo = (0.0,2.0),  
                           direcaoInimigo = Este, 
                           vidaInimigo = 50.0, 
                           velocidadeInimigo = 0.75, 
                           ataqueInimigo = 10.0, 
                           butimInimigo = 5, 
                           projeteisInimigo = []
                         }], 
          cicloOnda = 5.0, 
          tempoOnda = 4.0, 
          entradaOnda = 0.0
        }]}], 
      torresJogo = [], 
      mapaJogo = mapa01, 
      inimigosJogo = [], 
      lojaJogo = [(100,Torre {posicaoTorre = (0.0,0.0), danoTorre = 10.0, alcanceTorre = 20.0, rajadaTorre = 1, cicloTorre = 1.0, tempoTorre = 1.0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}}),
                  (80,Torre {posicaoTorre = (0.0,0.0), danoTorre = 7.0, alcanceTorre = 20.0, rajadaTorre = 1, cicloTorre = 1.0, tempoTorre = 1.0, projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0}}),
                  (70,Torre {posicaoTorre = (0.0,0.0), danoTorre = 5.0, alcanceTorre = 20.0, rajadaTorre = 1, cicloTorre = 1.0, tempoTorre = 1.0, projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}})
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
