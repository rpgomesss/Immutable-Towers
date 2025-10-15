module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import LI12425
import Tarefa2

{-testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ "basic example test" ~: (2 :: Int) ~=? 1 + 1,
        "another basic example" ~: True ~=? not False
      ]-}

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ --testa a inimigosNoAlcance
        "Todos os inimigos dentro do alcance" ~: [inimigo1, inimigo2] ~=? inimigosNoAlcance torre1 [inimigo1, inimigo2],
    
        "Alguns inimigos fora do alcance" ~: inimigosNoAlcance torre1 [inimigo1, inimigo3] ~=? [inimigo1],
    
        "Nenhum inimigo no alcance" ~: [] ~=? inimigosNoAlcance torre1 [inimigo3],
        
        --testa a atingeInimigo
        "Testa se o dano e o projetil sao aplicados corretamente" ~: inimigo4 { vidaInimigo = 90, projeteisInimigo = [Projetil Fogo (Finita 3)] } ~=?  atingeInimigo torre2 inimigo4,
        
        --testa de as sinergias estao a funcionar corretamente
        "Fogo e Gelo anulam-se" ~: [] ~=? sinergias (Projetil Fogo (Finita 3)) [Projetil Gelo (Finita 2)],
    
        "Fogo dobra a duração com Resina" ~: [Projetil Fogo (Finita 6)] ~=? sinergias (Projetil Fogo (Finita 3)) [Projetil Resina Infinita],
    
        "Projetil adicionado quando nao ha interaçao com outro tipo de projetil" ~: [Projetil Gelo (Finita 2), Projetil Resina Infinita] ~=? sinergias (Projetil Gelo (Finita 2)) [Projetil Resina Infinita],

        --testa a funcao ativaInimigos
        "Ativar primeiro inimigo da onda" ~: (portal3, [inimigo5]) ~=? ativaInimigo portal1 [inimigo5],
    
        "Sem inimigos na onda" ~: (portal3, []) ~=? ativaInimigo portal3 [],

        --termminar e ganhar jogo
        "terminouJogo e o jogador perdeu" ~: True ~=? terminouJogo jogoPerdeu,
        "terminouJogo e o jogo ainda esta ativo" ~: False ~=? terminouJogo jogoAtivo,
        "ganhouJogo e o jogador venceu" ~: True ~=? ganhouJogo jogoVenceu,
        "ganhouJogo com inimigos ainda ativos" ~: False ~=? ganhouJogo jogoAtivo,
        "perdeuJogo e base com vida 0" ~: True ~=? perdeuJogo jogoPerdeu,
        "perdeuJogo e base com vida positiva" ~: False ~=? perdeuJogo jogoAtivo
      ]

portal :: Portal
portal = Portal {ondasPortal = [Onda {inimigosOnda = [inimigoAtivado]}]}

inimigoAtivado :: Inimigo
inimigoAtivado = Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, projeteisInimigo = []}

jogoPerdeu :: Jogo 
jogoVenceu :: Jogo
jogoAtivo :: Jogo

jogoPerdeu = Jogo {baseJogo = Base {vidaBase = 0}, portaisJogo = [], inimigosJogo = []}
jogoVenceu = Jogo {baseJogo = Base {vidaBase = 10}, portaisJogo = [], inimigosJogo = []}
jogoAtivo = Jogo {baseJogo = Base {vidaBase = 10}, portaisJogo = [portal], inimigosJogo = [inimigo1]}

torre1 :: Torre
torre1 = Torre { posicaoTorre = (0, 0), alcanceTorre = 5.0, danoTorre = 10, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 2) }

torre2 :: Torre
torre2 = Torre { posicaoTorre = (0, 0), alcanceTorre = 5.0, danoTorre = 10, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 3) }


inimigo1 :: Inimigo
inimigo1 = Inimigo { posicaoInimigo = (3, 4), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

inimigo2 :: Inimigo
inimigo2 = Inimigo { posicaoInimigo = (0, 4), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

inimigo3 :: Inimigo
inimigo3 = Inimigo { posicaoInimigo = (6, 8), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

inimigo4 :: Inimigo
inimigo4 = Inimigo { posicaoInimigo = (1, 1), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

inimigo5 :: Inimigo
inimigo5 = Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Norte, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 20, vidaInimigo = 100, projeteisInimigo = [] }

onda1 :: Onda
onda1 = Onda { cicloOnda = 1, tempoOnda = 2, entradaOnda = 3, inimigosOnda = [] }

portal1 :: Portal
portal1 = Portal { posicaoPortal = (0,0), ondasPortal = [onda1] }

portal3 :: Portal
portal3 = Portal { posicaoPortal = (0,0), ondasPortal = [] }