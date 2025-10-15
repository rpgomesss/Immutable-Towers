{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Rui Pedro Ferreira de Jesus Gomes <a111273@alunos.uminho.pt>
              Ricardo Machado Rodrigues <a111225@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

-- | A distancia entre a posição do inimigo e a posição da torre terá que ser menor ou igual ao alcance desta 
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance t@Torre {posicaoTorre = (xTorre,yTorre), alcanceTorre = alcance } (i@Inimigo {posicaoInimigo = (xInimigo,yInimigo)}:inimigos) 
    | sqrt((xTorre - xInimigo)**2 + (yTorre - yInimigo)**2) <= alcance = i : inimigosNoAlcance t inimigos -- |usamos ** em vez de ^ uma vez que este ultimo só funciona com Integral 
    | otherwise = inimigosNoAlcance t inimigos

-- | Aplica o dano de uma torre a um inimigo, atualizando a vida do inimigo e adicionando o projétil da torre aos efeitos ativos no inimigo.
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = projetil} i@Inimigo {vidaInimigo = vida, projeteisInimigo = efeitosprojeteis} =
    i {vidaInimigo = vida - dano, projeteisInimigo = sinergias projetil efeitosprojeteis}

-- | Ajusta a lista de projéteis aplicados a um inimigo, levando em conta sinergias e interações entre diferentes tipos de projéteis.
sinergias :: Projetil -> [Projetil] -> [Projetil]
sinergias proj@(Projetil {tipoProjetil = tipo, duracaoProjetil = duracao}) projeteis
  | tipo == Fogo && Gelo `elem` tipos =
      removeProjetilTipo Gelo (removeProjetilTipo Fogo projeteis)
  | tipo == Gelo && Fogo `elem` tipos =
      removeProjetilTipo Fogo (removeProjetilTipo Gelo projeteis)
  | tipo == Fogo && Resina `elem` tipos =
      Projetil {tipoProjetil = Fogo, duracaoProjetil = dobraDuracao duracao} : removeProjetilTipo Resina projeteis
  | tipo == Resina && Fogo `elem` tipos =
      Projetil {tipoProjetil = Fogo, duracaoProjetil = dobraDuracao (duracaoDo Fogo projeteis)} : removeProjetilTipo Resina projeteis
  | tipo `elem` tipos =
      ajustaDuracaoLista proj projeteis
  | otherwise =
      proj : projeteis
  where
    tipos = map tipoProjetil projeteis

-- | Dobra a duração de um projétil, considerando se a duração é finita ou infinita.
dobraDuracao :: Duracao -> Duracao
dobraDuracao (Finita d) = Finita (2 * d)
dobraDuracao Infinita   = Infinita

-- | Remove projéteis de um tipo específico de uma lista de projéteis.
removeProjetilTipo :: TipoProjetil -> [Projetil] -> [Projetil]
removeProjetilTipo tipo = filter (\p -> tipoProjetil p /= tipo)

-- | Ajusta a duração de um projétil na lista de projéteis de um inimigo.
ajustaDuracaoLista :: Projetil -> [Projetil] -> [Projetil]
ajustaDuracaoLista proj [] = [proj]
ajustaDuracaoLista proj@(Projetil {tipoProjetil = tipo1, duracaoProjetil = duracaoAdicional}) (p@Projetil {tipoProjetil = tipo2, duracaoProjetil = duracaoOriginal} : ps)
  | tipo1 == tipo2 =
      Projetil {tipoProjetil = tipo2, duracaoProjetil = ajustaDuracao duracaoOriginal duracaoAdicional} : ps
  | otherwise =
      p : ajustaDuracaoLista proj ps

-- | Obtém a duração de um projétil de um tipo específico em uma lista de projéteis.
duracaoDo :: TipoProjetil -> [Projetil] -> Duracao
duracaoDo tipo = foldr (\p acc -> if tipoProjetil p == tipo then duracaoProjetil p else acc) (Finita 0)

-- | Ajusta a duração combinando duas durações.
ajustaDuracao :: Duracao -> Duracao -> Duracao
ajustaDuracao Infinita _ = Infinita
ajustaDuracao _ Infinita = Infinita
ajustaDuracao (Finita d1) (Finita d2) = Finita (d1 + d2)
          

-- | Função que dado um portal que tem contido diferentes ondas irá mover o seu proximo inimigo inativo e colocá-lo na lista de inimigos ativos
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo p@Portal {ondasPortal = []} inimativos =
    (p, inimativos) -- |Caso base: sem ondas no portal.
ativaInimigo p@Portal {ondasPortal = o@Onda {inimigosOnda = iniminativos} : ondas} inimativos
    | not (null iniminativos) =
        -- |Remove o primeiro inimigo de iniminativos e o adiciona à lista de inimigos ativos.
        let inimigoAtivado = head iniminativos
            restantesInimigos = tail iniminativos
            ondaAtualizada = o {inimigosOnda = restantesInimigos}
            portalAtualizado = p {ondasPortal = ondaAtualizada : ondas}
        in (portalAtualizado, inimigoAtivado : inimativos)
    | otherwise =
        -- |Caso não haja mais inimigos na onda, avança para a próxima.
        (p {ondasPortal = ondas}, inimativos)

-- | O jogo só termina caso o jogador tenha ganho ou perdido 
terminouJogo :: Jogo -> Bool 
terminouJogo jogo = perdeuJogo jogo || ganhouJogo jogo  

-- | O jogador ganho o jogo no caso da vida da sua base for positiva e já não houver inimigos ativos ou inativos
ganhouJogo :: Jogo -> Bool
ganhouJogo Jogo {baseJogo = Base {vidaBase = vidabase}, portaisJogo = portais, inimigosJogo = inimativos } =
      vidabase > 0 && all verificaInimigosInativos portais && null inimativos

-- | Função auxiliar que dado um portal verifica se ainda contem inimigos por mover (inativos) 
verificaInimigosInativos :: Portal -> Bool
verificaInimigosInativos Portal {ondasPortal = ondas} = all verificaOndas ondas 
                where 
                    -- | verifica se uma onda contem inimigos restantes
                    verificaOndas :: Onda -> Bool 
                    verificaOndas Onda {inimigosOnda = iniminativos} = null iniminativos



-- | O jogador perde o jogo no caso da vida da sua base chegar a um valor nulo (ou negativo)
perdeuJogo :: Jogo -> Bool
perdeuJogo Jogo {baseJogo = Base {vidaBase = vidabase}} = vidabase <= 0