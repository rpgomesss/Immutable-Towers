{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Rui Pedro Ferreira de Jesus Gomes <a111273@alunos.uminho.pt>
              Ricardo Machado Rodrigues <a111225@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where
import Tarefa2 
import LI12425

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo deltaT jogo@Jogo {mapaJogo = mapa, baseJogo = base, portaisJogo = portais, inimigosJogo = inimigos, torresJogo = torres} =
    let
        -- Atualiza os portais e gera novos inimigos
        (portaisAtualizados, novosInimigosPortal) = atualizaPortais deltaT portais inimigos

        -- Atualiza os inimigos existentes (movimento e aplicação de efeitos)
        inimigosAtualizados = map (aplicaEfeitos deltaT. moveInimigo deltaT mapa) novosInimigosPortal
        
        -- Atualiza torres e aplica danos nos inimigos
        (torresAtualizadas, inimigosPosAtaque) = torresAtacam deltaT mapa torres inimigosAtualizados

        -- Atualiza a base e remove inimigos que morreram ou atingiram a base
        (inimigosFinais, baseAtualizada) = atualizaEstadoInimigosEBase inimigosPosAtaque base
    in
        -- Retorna o novo estado do jogo com todos os componentes atualizados
        jogo
            { mapaJogo = mapa,
              baseJogo = baseAtualizada,
              portaisJogo = portaisAtualizados,
              inimigosJogo = inimigosFinais, 
              torresJogo = torresAtualizadas
            }

--3.3.1 COMPORTAMENTO DAS TORRES

-- |Detetar inimigos dentro do seu alcance.

detetaInimigosNoAlcance :: [Torre] -> [Inimigo] -> Mapa -> [(Torre, [Inimigo])]
detetaInimigosNoAlcance  [] _ _ = []  -- |Caso base sem torres
detetaInimigosNoAlcance (torre:torres) inimigos mapa =
  let
    -- |Encontra os inimigos no alcance dessa torre
    inimigosNoAlcanceTorre = inimigosNoAlcance torre inimigos
  in
    -- |Retorna a torre com os inimigos no alcance e processa as torres restantes
    (torre, inimigosNoAlcanceTorre) : detetaInimigosNoAlcance torres inimigos mapa

-- |Escolher e disparar automaticamente projeteis contra  os inimigos detetados.

torresAtacam :: Tempo -> Mapa -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
torresAtacam tempo mapa torres inimigos =
    let
        -- |Detecta inimigos no alcance para cada torre
        torresComInimigos = detetaInimigosNoAlcance torres inimigos mapa

        -- |Processa cada torre e atualiza inimigos
        processaTorre (torre@Torre {tempoTorre = tempoAtual, rajadaTorre = rajada, cicloTorre = ciclo}, inimigosAlcance) (torresAtualizados, inimigosAtualizados)
            | tempoAtual > 0 = (torre {tempoTorre = tempoAtual - tempo} : torresAtualizados, inimigosAtualizados)
            | otherwise =
                let
                    -- |Inimigos que serão atingidos pela torre (limite da rajada)
                    inimigosAtingidos = take rajada inimigosAlcance

                    -- |Atualiza os inimigos atingidos com dano e sinergias
                    inimigosAtualizadosComDano = map (atingeInimigo torre) inimigosAtingidos

                    -- |Atualiza a lista completa de inimigos
                    novosInimigos =
                        inimigosAtualizadosComDano ++
                        filter (`notElemInimigos` inimigosAtingidos) inimigosAtualizados
                    -- |Atualiza a torre (tempo de recarga)
                    torreAtualizada = torre {tempoTorre = ciclo}
                in
                    (torreAtualizada : torresAtualizados, novosInimigos)
    in
        -- |Processa todas as torres e acumula os resultados
        foldr processaTorre ([], inimigos) torresComInimigos

-- |Verifica se um inimigo não está na lista com base na posição
notElemInimigos :: Inimigo -> [Inimigo] -> Bool
notElemInimigos inimigo lista = not (any (\i -> posicaoInimigo i == posicaoInimigo inimigo) lista)


-- |Função que atualiza os inimigos e a base do jogador
atualizaEstadoInimigosEBase :: [Inimigo] -> Base -> ([Inimigo], Base)
atualizaEstadoInimigosEBase inimigos base =
    foldl processaInimigo ([], base) inimigos
  where
    processaInimigo :: ([Inimigo], Base) -> Inimigo -> ([Inimigo], Base)
    processaInimigo (inimigosRestantes, b@Base {vidaBase = vidaBase, creditosBase = creditosBase}) inimigo@Inimigo {vidaInimigo = vidaInimigo, ataqueInimigo = ataque, butimInimigo = creditosInimigo}
        | vidaInimigo <= 0 = (inimigosRestantes, b {creditosBase = creditosBase + creditosInimigo}) -- |Se o inimigo morrer não é atualizado na lista de inimigos ativos e os seus creditos vão para a base
        | atingiuBase inimigo b    = (inimigosRestantes, b {vidaBase = vidaBase - ataque}) -- |Se o inimigo chegar á base este desaparecerá e a base irá perder vida
        | otherwise                = (inimigo : inimigosRestantes, b) -- |Caso contrário é adicionado á lista de inimigos ativos 
    -- |Função auxiliar que verifica se um inimigo chegou relativamente perto da base
    atingiuBase :: Inimigo -> Base -> Bool
    atingiuBase Inimigo {posicaoInimigo = (x, y)} Base {posicaoBase = (bx, by)} =
        -- |Verifica se o inimigo está na posição da base (tolerância ajustável)
        abs (x - bx) < 1 && abs (y - by) < 1

-- |Função que aplica os efeitos presentes na lista de projeteis de um inimigo
aplicaEfeitos :: Tempo -> Inimigo -> Inimigo
aplicaEfeitos deltaT inimigo@Inimigo {vidaInimigo = vidaInimigo, projeteisInimigo = efeitos} =
    inimigo {projeteisInimigo = novosEfeitos, vidaInimigo = novaVida}
  where
    -- |Aplicação dos efeitos dos projéteis no inimigo
    (novaVida, novosEfeitos) = foldr aplicaEfeito (vidaInimigo, []) efeitos
      -- |Função auxiliar para aplicar um efeito específico
    aplicaEfeito :: Projetil -> (Float, [Projetil]) -> (Float, [Projetil])
    aplicaEfeito proj@Projetil {tipoProjetil = tipo, duracaoProjetil = duracao} (vida, efeitosAtualizados)
        | duracaoRestante <= 0 = (vida,efeitosAtualizados) -- |Remove efeito expirado
        | otherwise = case tipo of
            Fogo   -> (vida - danoPorSegundo * deltaT, projAtualizado : efeitosAtualizados)
            _      -> (vida, projAtualizado : efeitosAtualizados)
      where
        -- |Calcula a duração restante do projétil
        duracaoRestante = case duracao of
            Finita t -> t - deltaT
            Infinita -> 1  -- |Sempre ativo
        -- |Atualiza a duração do projétil
        projAtualizado = proj {duracaoProjetil = case duracao of
                                                    Finita t -> Finita (t - deltaT)
                                                    Infinita -> Infinita}

    -- |Dano por segundo para projéteis de Fogo
    danoPorSegundo = 2
    
-- |Função que verifica todas as novas direções possíveis para um inimigo e escolhe a primeira válida
moveInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
moveInimigo tempo mapa inimigo@Inimigo {posicaoInimigo = (x, y), velocidadeInimigo = vel, direcaoInimigo = dir, projeteisInimigo = projeteis} =
    let
        -- |Lista de direções possíveis, excluindo a direção oposta e a direção atual
        direcoes = filter (\d -> d /= direcaoOposta dir) [dir,Norte, Sul, Este, Oeste]
        -- |Calcula a nova posição para cada direção devolvendo um par (Direcao, Posicao)
        novasPosicoes = [(novaDirecao, calculaNovaPosicao novaDirecao) | novaDirecao <- direcoes]
        -- |Filtra apenas posições válidas (presentes em Terra)
        posicoesValidas = [(d, p) | (d, p) <- novasPosicoes, terrenoTerra p mapa]
    in case posicoesValidas of
        -- |Move para a primeira posição válida encontrada
        ((novaDirecao, novaPosicao) : _) -> inimigo {posicaoInimigo = novaPosicao, direcaoInimigo = novaDirecao}
        -- |Não há posições válidas, o inimigo não se move
        [] -> inimigo
  where
    -- |Função que calcula uma nova posição em função do tempo e
    calculaNovaPosicao :: Direcao -> Posicao
    calculaNovaPosicao Norte = (x, y + velPosProjeteis projeteis vel * tempo)
    calculaNovaPosicao Sul   = (x, y - velPosProjeteis projeteis vel * tempo)
    calculaNovaPosicao Este  = (x + velPosProjeteis projeteis vel * tempo, y)
    calculaNovaPosicao Oeste = (x - velPosProjeteis projeteis vel * tempo, y)

    velPosProjeteis :: [Projetil] -> Float -> Float
    velPosProjeteis projeteisInimigo velocidade
      | elem Gelo projeteisAtivos = 0
      | elem Resina projeteisAtivos = velocidade * 0.5
      | otherwise = vel
     where
        projeteisAtivos = map tipoProjetil projeteisInimigo
    -- |Função que determina a direção oposta
    direcaoOposta :: Direcao -> Direcao
    direcaoOposta Norte = Sul
    direcaoOposta Sul   = Norte
    direcaoOposta Este  = Oeste
    direcaoOposta Oeste = Este

-- |Função que verifica se uma posição (em coordenadas da tela) está em Terra no mapa
terrenoTerra :: Posicao -> Mapa -> Bool
terrenoTerra (x, y) mapa =
    let 
        -- |Converte as coordenadas da tela em índices inteiros
        ix = floor x
        iy = floor y
    in
        -- |Verifica se os índices estão dentro dos limites do mapa
        iy >= 0 && iy < length mapa && -- Índice Y dentro do limite do mapa
        ix >= 0 && ix < length (mapa !! iy) && -- Índice X dentro da linha do mapa
        (mapa !! iy !! ix) == Terra -- Verifica se o terreno na posição é Terra

 -- 3) Comportamento dos portais                            

--- ex 3.3.3
-- |Atualiza todos os portais e lança os inimigos correspondentes.
-- |Recebe o tempo decorrido, uma lista de portais e a lista de inimigos atualmente ativos.
-- |Retorna a lista atualizada de portais e a nova lista de inimigos ativos.
atualizaPortais :: Tempo -> [Portal] -> [Inimigo] -> ([Portal], [Inimigo])
atualizaPortais _ [] inimigosAtivos = ([], inimigosAtivos)  -- Caso base: sem portais, nada a atualizar.
atualizaPortais t (portal:restoPortais) inimigosAtivos =
    -- |Atualiza o primeiro portal e divide a atualização dos restantes portais.
    (portalAtualizado : portaisAtualizados, novosInimigos ++ inimigosAtualizados)
  where
    -- |Atualiza o estado do portal atual e obtém os novos inimigos gerados.
    (portalAtualizado, novosInimigos) = atualizaPortal t portal
    -- |Chama recursivamente a função para atualizar os portais restantes.
    (portaisAtualizados, inimigosAtualizados) = atualizaPortais t restoPortais inimigosAtivos

-- |Atualiza um portal e processa as suas ondas de inimigos.
-- |Recebe o tempo decorrido e o portal a ser atualizado.
-- |Retorna o portal atualizado e a lista de inimigos lançados.
atualizaPortal :: Tempo -> Portal -> (Portal, [Inimigo])
atualizaPortal t portal =
    case ondasPortal portal of
        [] -> (portal, [])  -- |Se o portal não tem ondas, nada a fazer.
        (onda : ondasRestantes) ->
            -- |Atualiza a onda atual e obtém os novos inimigos gerados.
            (portal { ondasPortal = novasOndas }, novosInimigos)
          where
            -- |Processa a primeira onda do portal.
            (novaOnda, novosInimigos) = atualizaOnda t onda
            -- |Determina se a onda atual ainda tem inimigos ou se deve ser removida.
            novasOndas = if length (inimigosOnda novaOnda) == 0
                         then ondasRestantes  -- |Remove a onda se não há inimigos restantes.
                         else novaOnda : ondasRestantes  -- |Mantém a onda atualizada.

-- |Processa uma onda, lançando inimigos se necessário.
-- |Recebe o tempo decorrido e a onda a ser processada.
-- |Retorna a onda atualizada e a lista de novos inimigos lançados.
atualizaOnda :: Tempo -> Onda -> (Onda, [Inimigo])
atualizaOnda t onda
  -- |Se a onda ainda está aguardando para começar, reduz o tempo de entrada.
  | entradaOnda onda > 0 = (onda { entradaOnda = entradaOnda onda - t }, [])
  -- |Se a onda já começou, mas está no período de espera entre lançamentos, reduz o tempo de espera.
  | tempoOnda onda > 0   = (onda { tempoOnda = tempoOnda onda - t }, [])
  -- |Caso contrário, a onda está pronta para lançar inimigos.
  | otherwise =
      case inimigosOnda onda of
          [] -> (onda, [])  -- |Se não há mais inimigos na onda, nada a fazer.
          (novoInimigo : inimigosRestantes) ->
              -- |Lança o próximo inimigo e reinicia o tempo de espera para o próximo.
              (onda { inimigosOnda = inimigosRestantes, tempoOnda = cicloOnda onda }, [novoInimigo])