{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Rui Pedro Ferreira de Jesus Gomes <a111273@alunos.uminho.pt>
              Ricardo Machado Rodrigues <a111225@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

{- A funcao 'validaJogo' recebe um Jogo e valida o mesmo returnado True ou False

@
validaJogo jogo = validaTudoRelativoPortais jogo && validaTudoRelativoInimigos jogo && validaTudoRelativoTorres jogo && validaTudoRelativoBase jogo

@

-}

validaJogo :: Jogo -> Bool
validaJogo jogo = validaTudoRelativoPortais jogo && validaTudoRelativoInimigos jogo && validaTudoRelativoTorres jogo && validaTudoRelativoBase jogo

{-
validaJogo j@Jogo { baseJogo = b@Base {vidaBase = vida, posicaoBase = posicao, creditosBase = creditos}, 
                    portaisJogo = p@Portal {posicaoPortal = (x,y), ondasPortal = ondas}, 
                    torresJogo = torres, 
                    mapaJogo = [terrenos], 
                    inimigosJogo = [inimigos],
                    lojaJogo = [(creditos,torre)] 
                  } =
-}



--EXERCICIO 1 (RELATIVAMENTE A PORTAIS)



{-| A funcao 'validaTudoRelativoPortais' recebe um Jogo e retorna True se todas as funcoes relativas a Portais forem verdadeiras, caso contrario retorna False.

@
validaTudoRelativoPortais (Jogo {portaisJogo = portais, mapaJogo = mapa, torresJogo = torres, baseJogo = base}) = 
 validaPortais portais && validaPosicaoPortal' mapa portais && validaPosicaoPortal'' portais torres base && validaOndasAtivas portais
@

-}

validaTudoRelativoPortais :: Jogo -> Bool
validaTudoRelativoPortais (Jogo {portaisJogo = portais, mapaJogo = mapa, torresJogo = torres, baseJogo = base}) = 
 validaPortais portais && validaPosicaoPortal' mapa portais && existeCaminhoBase mapa portais base && validaPosicaoPortal'' portais torres base && validaOndasAtivas portais 


{- |A funcao 'validaPortais' recebe uma lista de Portais e verifica se o numero de portais é maior ou igual a 1 e retorna True em caso afirmativo.

@
validaPortais p = length p >= 1
@

-}

--(a) verifica se existe pelo menos um portal
validaPortais :: [Portal] -> Bool
validaPortais p = length p >= 1 -- verifica se o numero de portais é maior ou igual a 1 e retorna True em caso afirmativo

{- |A funcao 'validaPosicaoPortal'' recebe um Mapa e uma lista de Portais e verifica se os portais estao posicionados sobre terra.

@
validaPosicaoPortal' _ [] = True -- Se não houver portais a verificar, retorna True
validaPosicaoPortal' mapa (Portal {posicaoPortal = (x, y)} : portaisRestantes)
  | xValido && yValido && mapa !! linha !! coluna == Terra = validaPosicaoPortal' mapa portaisRestantes
  | otherwise = False
  where
    linha = floor y 
    coluna = floor x
    xValido = coluna >= 0 && coluna < length (head mapa) 
    yValido = linha >= 0 && linha < length mapa 
@

-}

--(b) verifica se os portais estao posicionados sobre terra
validaPosicaoPortal' :: Mapa -> [Portal] -> Bool
validaPosicaoPortal' _ [] = True -- Se não houver portais a verificar, retorna True
validaPosicaoPortal' mapa (Portal {posicaoPortal = (x, y)} : portaisRestantes)
  | xValido && yValido && mapa !! linha !! coluna == Terra = validaPosicaoPortal' mapa portaisRestantes
  | otherwise = False
  where
    linha = floor y -- Convertendo a coordenada y para índice
    coluna = floor x -- Convertendo a coordenada x para índice
    xValido = coluna >= 0 && coluna < length (head mapa) -- Verifica se a coluna está dentro dos limites
    yValido = linha >= 0 && linha < length mapa -- Verifica se a linha está dentro dos limites

--(c) verifica se existe pelo menos um caminho de terra ligando qualquer portal à base.

{-|A funcao 'existeCaminho' recebe um mapa, uma lista de portais e uma base e verifica se existe um caminho de terra entre os portais e a base.

@
existeCaminhoBase _ [] _ = False
existeCaminhoBase mapa portais Base {posicaoBase = posBase} = any (\Portal {posicaoPortal = posPortal} -> existeCaminho mapa posPortal posBase) portais
@

-}

existeCaminhoBase :: Mapa -> [Portal] -> Base -> Bool
existeCaminhoBase _ [] _ = False
existeCaminhoBase mapa portais Base {posicaoBase = posBase} = any (\Portal {posicaoPortal = posPortal} -> existeCaminho mapa posPortal posBase) portais

{-|A funcao 'existeCaminho' recebe um mapa e duas posicoes e verifica se há um caminho de terra que liga as duas posições no mapa.

@
existeCaminho mapa inicio fim = encontraCaminho [inicio] []
  where
    encontraCaminho :: [Posicao] -> [Posicao] -> Bool
    encontraCaminho [] _ = False
    encontraCaminho (atual:resto) visitados
        | distancia atual fim < 0.5 = True
        | atual `elem` visitados = encontraCaminho resto visitados 
        | otherwise =
            let novosVisitados = atual : visitados
                vizinhosValidos = filter (`notElem` visitados) (vizinhos mapa atual)
            in encontraCaminho (resto ++ vizinhosValidos) novosVisitados
@

-}

existeCaminho :: Mapa -> Posicao -> Posicao -> Bool
existeCaminho mapa inicio fim = encontraCaminho [inicio] []
  where
    encontraCaminho :: [Posicao] -> [Posicao] -> Bool
    encontraCaminho [] _ = False
    encontraCaminho (atual:resto) visitados
        | distancia atual fim < 0.5 = True
        | atual `elem` visitados = encontraCaminho resto visitados 
        | otherwise =
            let novosVisitados = atual : visitados
                vizinhosValidos = filter (`notElem` visitados) (vizinhos mapa atual)
            in encontraCaminho (resto ++ vizinhosValidos) novosVisitados

{-|A funcao 'vizinhos' recebe um mapa e uma posicao e calcula os vizinhos válidos de uma posição no mapa.

@
vizinhos mapa (x, y) = filter (validaPosicao mapa) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
@

-}

vizinhos :: Mapa -> Posicao -> [Posicao]
vizinhos mapa (x, y) = filter (validaPosicao mapa) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

{-|A funcao 'validaPosicao' recebe um mapa e uma posicao e verifica se uma posição é válida e contém terra no mapa.

@
validaPosicao mapa (x, y) =
    x >= 0 && y >= 0 &&
    y < fromIntegral (length mapa) &&
    x < fromIntegral (length (head mapa)) &&
    terrenoNaPosicao mapa (x, y) == Terra
@

-}

validaPosicao :: Mapa -> Posicao -> Bool
validaPosicao mapa (x, y) =
    x >= 0 && y >= 0 &&
    y < fromIntegral (length mapa) &&
    x < fromIntegral (length (head mapa)) &&
    terrenoNaPosicao mapa (x, y) == Terra

{-|A funcao 'terrenoNaPosicao' recebe um mapa e uma posicao e obtém o terreno em uma posição no mapa.

@
terrenoNaPosicao mapa (x, y)
    | ix < 0 || iy < 0 || iy >= length mapa || ix >= length (head mapa) =
        error "Posição fora dos limites do mapa"
    | otherwise = mapa !! iy !! ix
  where
    ix = floor x
    iy = floor y
@

-}

terrenoNaPosicao :: Mapa -> Posicao -> Terreno
terrenoNaPosicao mapa (x, y)
    | ix < 0 || iy < 0 || iy >= length mapa || ix >= length (head mapa) =
        error "Posição fora dos limites do mapa"
    | otherwise = mapa !! iy !! ix
  where
    ix = floor x
    iy = floor y

{-|A funcao 'distancia' calcula a distância entre duas posições.

@
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)**2 + (y2 - y1)**2)
@

-}

distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)**2 + (y2 - y1)**2)

{- |A funcao 'validaPosicaoPortal''' recebe uma Base, uma lista de Torres e uma lista de Portais e verifica se os portais estao sobrepostos a torres ou bases.

@
validaPosicaoPortal'' :: [Portal] -> [Torre] -> Base -> Bool
validaPosicaoPortal'' [] _  _ = True
validaPosicaoPortal'' (portal : portaisRestantes) torres base = validaPosicaoPortalAux portal torres base && validaPosicaoPortal'' portaisRestantes torres base
@

-}

--(d) verifica se os portais estao sobrepostos a torres ou bases
validaPosicaoPortal'' :: [Portal] -> [Torre] -> Base -> Bool
validaPosicaoPortal'' [] _  _ = True
validaPosicaoPortal'' (portal : portaisRestantes) torres base = validaPosicaoPortalAux portal torres base && validaPosicaoPortal'' portaisRestantes torres base

{- |A funcao 'validaPosicaoPortalAux' recebe um Portal, uma lista de Torres e uma Base e verifica se o portal nao esta sobreposto a torres ou bases.

@
validaPosicaoPortalAux :: Portal -> [Torre] -> Base -> Bool
validaPosicaoPortalAux _ [] _ = True
validaPosicaoPortalAux p@(Portal {posicaoPortal = (x1,y1)}) (Torre {posicaoTorre = (x2,y2)} : torresRestantes) b@(Base {posicaoBase = (x3,y3)})
 |distancia posPortal posTorre > 0 && distancia posPortal posBase > 0  = validaPosicaoPortalAux p torresRestantes b
 |otherwise = False
@

-}

validaPosicaoPortalAux :: Portal -> [Torre] -> Base -> Bool
validaPosicaoPortalAux _ [] _ = True
validaPosicaoPortalAux p@(Portal {posicaoPortal = posPortal}) (Torre {posicaoTorre = posTorre} : torresRestantes) b@(Base {posicaoBase = posBase})
 |distancia posPortal posTorre > 0 && distancia posPortal posBase > 0 = validaPosicaoPortalAux p torresRestantes b
 |otherwise = False

{- |A funcao 'validaOndasAtivas' recebe uma lista de Portais e verifica se o numero de ondas por portal é maior ou igual a 1 e retorna True em caso afirmativo.

@
validaOndasAtivas [] = True
validaOndasAtivas (Portal {ondasPortal = ondas} : ps)
 |length ondas <= 1 = validaOndasAtivas ps
 |otherwise = False
@

-}

--(e) verifica se há, no máximo, uma onda ativa por portal
validaOndasAtivas :: [Portal] -> Bool
validaOndasAtivas [] = True -- se nao houver portais a verificar retorna True
validaOndasAtivas (Portal {ondasPortal = ondas} : ps) -- verifica recursivamente se o numero de ondas ativas por portal é no maximo 1 e retorna True em caso afirmativo
 |length ondas <= 1 = validaOndasAtivas ps
 |otherwise = False



--EXERCICIO 2 (RELATIVAMENTE A INIMIGOS)



{-| A funcao 'validaTudoRelativoInimigos' recebe um Jogo e retorna True se todas as funcoes relativas a Inimigos forem verdadeiras, caso contrario retorna False.

@
validaTudoRelativoInimigos (Jogo {inimigosJogo = inimigos, mapaJogo = mapa, portaisJogo = portais, torresJogo = torres}) =
  validaInimigos inimigos portais && validaPosicaoInimigos' inimigos mapa && validaPosicaoInimigos'' inimigos torres && validaVelocidadeInimigos inimigos && validaProjeteisInimigos' inimigos && validaProjeteisInimigos'' inimigos
@

-}

validaTudoRelativoInimigos :: Jogo -> Bool
validaTudoRelativoInimigos (Jogo {inimigosJogo = inimigos, mapaJogo = mapa, portaisJogo = portais, torresJogo = torres}) =
  validaInimigos portais && validaPosicaoInimigos' inimigos mapa && validaPosicaoInimigos'' inimigos torres && validaVelocidadeInimigos inimigos && validaProjeteisInimigos' inimigos && validaProjeteisInimigos'' inimigos

{-| A funcao 'validaInimigos' recebe uma lista de Portais e verifica se todos os inimigos por lancar tem a posicao do respetivo portal, nivel de vida positivo, e lista de projeteis ativos vazia.

@
validaInimigos' :: [Portal] -> Bool
validaInimigos' portais = all validaOndas portais
  where
    validaOndas :: Portal -> Bool
    validaOndas portal@Portal {ondasPortal = ondas} = all (validaInimigosOnda (posicaoPortal portal)) ondas

    validaInimigosOnda :: Posicao -> Onda -> Bool
    validaInimigosOnda posicaoPortal Onda {inimigosOnda = inimigos} = all (validaInimigoInativo posicaoPortal) inimigos

    validaInimigoInativo :: Posicao -> Inimigo -> Bool
    validaInimigoInativo posicaoPortal Inimigo {posicaoInimigo = posicaoInimigo, vidaInimigo = vida, projeteisInimigo = projeteis} =
        distancia posicaoPortal posicaoInimigo == 0 && vida > 0 && null projeteis


-}

--(a) Todos os inimigos por lancar tem a posicao do respetivo portal, nivel de vida positivo, e lista de projeteis ativos vazia.


validaInimigos :: [Portal] -> Bool
validaInimigos portais = all validaOndas portais
  where
    -- Verifica se todas as ondas de um portal são válidas
    validaOndas :: Portal -> Bool
    validaOndas portal@Portal {ondasPortal = ondas} = all (validaInimigosOnda (posicaoPortal portal)) ondas

    -- Verifica se todos os inimigos de uma onda são válidos
    validaInimigosOnda :: Posicao -> Onda -> Bool
    validaInimigosOnda posicaoPortal Onda {inimigosOnda = inimigos} = all (validaInimigoInativo posicaoPortal) inimigos

    -- Verifica se um inimigo está na posição inicial, tem vida > 0 e não possui projéteis
    validaInimigoInativo :: Posicao -> Inimigo -> Bool
    validaInimigoInativo posicaoPortal Inimigo {posicaoInimigo = posicaoInimigo, vidaInimigo = vida, projeteisInimigo = projeteis} =
        distancia posicaoPortal posicaoInimigo == 0 && vida > 0 && null projeteis


{-| A funcao 'validaPosicaoInimigos'' recebe uma lista de Inimigos e um Mapa e verifica se todos os inimigos em jogo encontram-se sobre terra.

@
validaPosicaoInimigos' [] _ = True
validaPosicaoInimigos' (Inimigo {posicaoInimigo = (x,y)} : inimigosRestantes) mapa
 |mapa !! floor y !! floor x == Terra = validaPosicaoInimigos' inimigosRestantes mapa
 |otherwise = False
@

-}

--(b) Todos os inimigos em jogo encontram-se sobre terra.
validaPosicaoInimigos' :: [Inimigo] -> Mapa -> Bool
validaPosicaoInimigos' [] _ = True
validaPosicaoInimigos' (Inimigo {posicaoInimigo = (x,y)} : inimigosRestantes) mapa
 |mapa !! floor y !! floor x == Terra = validaPosicaoInimigos' inimigosRestantes mapa
 |otherwise = False

{-| A funcao 'validaPosicaoInimigos''' recebe uma lista de Inimigos e uma lista de Torres e verifica se os inimigos nao estao sobrepostos a torres.

@
validaPosicaoInimigos'' :: [Inimigo] -> [Torre] -> Bool
validaPosicaoInimigos'' [] _ = True
validaPosicaoInimigos'' (inimigo : inimigosRestantes) torres = validaPosicaoInimigosAux inimigo torres && validaPosicaoInimigos'' inimigosRestantes torres
@

-}

--(c) Nao podem estar sobrepostos a torres.
validaPosicaoInimigos'' :: [Inimigo] -> [Torre] -> Bool
validaPosicaoInimigos'' [] _ = True
validaPosicaoInimigos'' (inimigo : inimigosRestantes) torres = validaPosicaoInimigosAux inimigo torres && validaPosicaoInimigos'' inimigosRestantes torres

{-| A funcao 'validaPosicaoInimigosAux' recebe um Inimigo e uma lista de Torres e verifica se o Inimigo nao esta sobreposto a torres.

@
validaPosicaoInimigosAux :: Inimigo -> [Torre] -> Bool
validaPosicaoInimigosAux _ [] = True
validaPosicaoInimigosAux i@(Inimigo {posicaoInimigo = posInimigo}) (Torre {posicaoTorre = posTorre} : torresRestantes)
 |distancia posInimigo posTorre > 0 = validaPosicaoInimigosAux i torresRestantes
 |otherwise = False
@

-}

validaPosicaoInimigosAux :: Inimigo -> [Torre] -> Bool
validaPosicaoInimigosAux _ [] = True
validaPosicaoInimigosAux i@(Inimigo {posicaoInimigo = posInimigo}) (Torre {posicaoTorre = posTorre} : torresRestantes)
 |distancia posInimigo posTorre > 0 = validaPosicaoInimigosAux i torresRestantes
 |otherwise = False

{-| A funcao 'validaVelocidadeInimigos' recebe uma lista de Inimigos e verifica se a velocidade dos inimigos nao e negativa

@
validaVelocidadeInimigos [] = True
validaVelocidadeInimigos (Inimigo {velocidadeInimigo = velocidade} : inimigosRestantes)
 |velocidade >= 0 = validaVelocidadeInimigos inimigosRestantes
 |otherwise = False
@

-}

--(d) Velocidade nao pode ser negativa.
validaVelocidadeInimigos :: [Inimigo] -> Bool
validaVelocidadeInimigos [] = True
validaVelocidadeInimigos (Inimigo {velocidadeInimigo = velocidade} : inimigosRestantes)
 |velocidade >= 0 = validaVelocidadeInimigos inimigosRestantes
 |otherwise = False

{-| A funcao 'validaProjeteisInimigos'' recebe uma lista de Inimigos e verifica se nao pode conter mais do que um projetil do mesmo tipo.

@
validaProjeteisInimigos :: [Inimigo] -> Bool
validaProjeteisInimigos [] = True
validaProjeteisInimigos (Inimigo {projeteisInimigo = projeteis} : inimigosRestantes)
 |semProjeteisRepetidos (map tipoProjetil projeteis) = validaProjeteisInimigos inimigosRestantes
 |otherwise = False
   where semProjeteisRepetidos :: Eq a => [a] -> Bool
         semProjeteisRepetidos [] = True
         semProjeteisRepetidos (p:ps)
          |p `elem` ps = False
          |otherwise = semProjeteisRepetidos ps
@

-}

--(e)
--i. Nao pode conter mais do que um projetil do mesmo tipo.
validaProjeteisInimigos' :: [Inimigo] -> Bool
validaProjeteisInimigos' [] = True
validaProjeteisInimigos' (Inimigo {projeteisInimigo = projeteis} : inimigosRestantes)
 |semProjeteisRepetidos (map tipoProjetil projeteis) = validaProjeteisInimigos' inimigosRestantes
 |otherwise = False
   where semProjeteisRepetidos :: Eq a => [a] -> Bool
         semProjeteisRepetidos [] = True
         semProjeteisRepetidos (p:ps)
          |p `elem` ps = False
          |otherwise = semProjeteisRepetidos ps

{-| A funcao 'validaProjeteisInimigos''' recebe uma lista de Inimigos e verifica se nao pode conter, simultaneamente, projeteis do tipo Fogo e Resina, nem Fogo e Gelo.

@
validaProjeteisInimigos'' :: [Inimigo] -> Bool
validaProjeteisInimigos'' [] = True
validaProjeteisInimigos'' (Inimigo {projeteisInimigo = projeteis} : inimigosRestantes)
 |Fogo `elem` tipoDeProjeteis && Resina `elem` tipoDeProjeteis = False
 |Fogo `elem` tipoDeProjeteis && Gelo `elem` tipoDeProjeteis = False
 |otherwise = validaProjeteisInimigos'' inimigosRestantes
  where tipoDeProjeteis = map tipoProjetil projeteis
@

-}

--ii. Nao pode conter, simultaneamente, projeteis do tipo Fogo e Resina, nem Fogo e Gelo.
validaProjeteisInimigos'' :: [Inimigo] -> Bool
validaProjeteisInimigos'' [] = True
validaProjeteisInimigos'' (Inimigo {projeteisInimigo = projeteis} : inimigosRestantes)
 |Fogo `elem` tipoDeProjeteis && Resina `elem` tipoDeProjeteis = False
 |Fogo `elem` tipoDeProjeteis && Gelo `elem` tipoDeProjeteis = False
 |otherwise = validaProjeteisInimigos'' inimigosRestantes
   where tipoDeProjeteis = map tipoProjetil projeteis



--EXERCICIO 3 (RELATIVAMENTE A TORRES)



{-| A funcao 'validaTudoRelativoTorres' recebe um Jogo e retorna True se todas as funcoes relativas a Torres forem verdadeiras, caso contrario retorna False.

@
validaTudoRelativoTorres (Jogo {torresJogo = torres, mapaJogo = mapa}) =
  validaTorres torres mapa && validaAlcanceTorres torres && validaRajadaTorres torres && validaCicloTorres torres && validaTorresSobrepostas torres
@

-}


validaTudoRelativoTorres :: Jogo -> Bool
validaTudoRelativoTorres (Jogo {torresJogo = torres, mapaJogo = mapa}) =
  validaTorres torres mapa && validaAlcanceTorres torres && validaRajadaTorres torres && validaCicloTorres torres && validaTorresSobrepostas torres

{-| A funcao 'validaTorres' recebe uma lista de Torres e um Mapa e verifica se todas as torres estao posicionadas sobre relva.

@
validaTorres [] _ = True
validaTorres (Torre {posicaoTorre = (x,y)} : torresRestantes) mapa
 |mapa !! floor y !! floor x == Terra = validaTorres torresRestantes mapa
 |otherwise = False
@

-}

--(a) Estao posicionadas sobre relva.
validaTorres :: [Torre] -> Mapa -> Bool
validaTorres [] _ = True
validaTorres (Torre {posicaoTorre = (x,y)} : torresRestantes) mapa
 |mapa !! floor y !! floor x == Relva = validaTorres torresRestantes mapa
 |otherwise = False

{-| A funcao 'validaAlcanceTorres' recebe uma lista de Torres e verifica se o alcance das torres e um valor positivo.

@
validaAlcanceTorres [] = True
validaAlcanceTorres (Torre {alcanceTorre = alcance} : torresRestantes)
 |alcance > 0 = validaAlcanceTorres torresRestantes
 |otherwise = False
@

-}

--(b) O seu alcance e um valor positivo.
validaAlcanceTorres :: [Torre] -> Bool
validaAlcanceTorres [] = True
validaAlcanceTorres (Torre {alcanceTorre = alcance} : torresRestantes)
 |alcance > 0 = validaAlcanceTorres torresRestantes
 |otherwise = False

{-| A funcao 'validaRajadaTorres' recebe uma lista de Torres e verifica se a rajada das torres e um valor positivo.

@
validaRajadaTorres [] = True
validaRajadaTorres (Torre {rajadaTorre = rajada} : torresRestantes)
 |rajada > 0 = validaRajadaTorres torresRestantes
 |otherwise = False
@

-}

--(c) A rajada e um valor positivo.
validaRajadaTorres :: [Torre] -> Bool
validaRajadaTorres [] = True
validaRajadaTorres (Torre {rajadaTorre = rajada} : torresRestantes)
 |rajada > 0 = validaRajadaTorres torresRestantes
 |otherwise = False

{-| A funcao 'validaCicloTorres' recebe uma lista de Torres e verifica se o ciclo das torres e um valor nao negativo.

@
validaCicloTorres [] = True
validaCicloTorres (Torre {cicloTorre = ciclo} : torresRestantes)
 |ciclo >=0 = validaCicloTorres torresRestantes
 |otherwise = False
@

-}

--(d) O ciclo e um valor nao negativo.
validaCicloTorres :: [Torre] -> Bool
validaCicloTorres [] = True
validaCicloTorres (Torre {cicloTorre = ciclo} : torresRestantes)
 |ciclo >=0 = validaCicloTorres torresRestantes
 |otherwise = False

{-| A funcao 'validaTorresSobrepostas' recebe uma lista de Torres e verifica se as torres nao estao sobrepostas.

@
validaTorresSobrepostas [] = True
validaTorresSobrepostas (torre : torresRestantes) = torreValida torre torresRestantes && validaTorresSobrepostas torresRestantes
 where torreValida :: Torre -> [Torre] -> Bool
       torreValida _ [] = True
       torreValida t@(Torre {posicaoTorre = pos1}) (Torre {posicaoTorre = pos2} : torres)
        |distancia pos1 pos2 > 0 = torreValida t torres
        |otherwise = False


-}

--(e) Nao podem estar sobrepostas.
validaTorresSobrepostas :: [Torre] -> Bool
validaTorresSobrepostas [] = True
validaTorresSobrepostas (torre : torresRestantes) = torreValida torre torresRestantes && validaTorresSobrepostas torresRestantes
 where torreValida :: Torre -> [Torre] -> Bool
       torreValida _ [] = True
       torreValida t@(Torre {posicaoTorre = pos1}) (Torre {posicaoTorre = pos2} : torres)
        |distancia pos1 pos2 > 0 = torreValida t torres
        |otherwise = False



--EXERCICIO 4 (RELATIVAMENTE A BASE)



{-| A funcao 'validaTudoRelativoBase' recebe um Jogo e retorna True se todas as funcoes relativas a Base forem verdadeiras, caso contrario retorna False.

@
validaTudoRelativoBase (Jogo {baseJogo = base, mapaJogo = mapa, torresJogo = torres, portaisJogo = portais}) =
  validaPosicaoBase base mapa && validaCreditosBase base && validaBaseSobreposta base torres portais
@

-}

validaTudoRelativoBase :: Jogo -> Bool
validaTudoRelativoBase (Jogo {baseJogo = base, mapaJogo = mapa, torresJogo = torres, portaisJogo = portais}) =
  validaPosicaoBase base mapa && validaCreditosBase base && validaBaseSobreposta base torres portais

{-| A funcao 'validaPosicaoBase' recebe uma Base e um Mapa e verifica se a base esta colocada sobre terra.

@
validaPosicaoBase :: Base -> Mapa -> Bool
validaPosicaoBase (Base {posicaoBase = (x,y)}) mapa
 |mapa !! floor y !! floor x == Terra = True
 |otherwise = False


-}

--(a) Esta colocada sobre terra.
validaPosicaoBase :: Base -> Mapa -> Bool
validaPosicaoBase (Base {posicaoBase = (x,y)}) mapa
 |mapa !! floor y !! floor x == Terra = True
 |otherwise = False

{-| A funcao 'validaCreditosBase' recebe uma Base e verifica se a base nao tem creditos negativo.

@
validaCreditosBase (Base {creditosBase = creditos})
 |creditos >= 0 = True
 |otherwise = False
@

-}

--(b) Nao tem credito negativo.
validaCreditosBase :: Base -> Bool
validaCreditosBase (Base {creditosBase = creditos})
 |creditos >= 0 = True
 |otherwise = False

{-| A funcao 'validaBaseSobreposta' recebe uma Base, uma lista de Torres e uma lista de Portais e verifica se a base nao esta sobreposta a uma torre ou portal, com ajuda das funcoes auxiliares 'validaBaseSobrepostaATorres' e 'validaBaseSobrepostaAPortais'.

@
validaBaseSobreposta _ [] [] = True
validaBaseSobreposta base torres portais = validaBaseSobrepostaATorres base torres && validaBaseSobrepostaAPortais base portais
@

-}

--(c) Nao pode estar sobreposta a uma torre ou portal.
validaBaseSobreposta :: Base -> [Torre] -> [Portal] -> Bool
validaBaseSobreposta _ [] [] = True
validaBaseSobreposta base torres portais = validaBaseSobrepostaATorres base torres && validaBaseSobrepostaAPortais base portais

{-| A funcao 'validaBaseSobrepostaATorres' recebe uma Base e uma lista de Torres e verifica se a base nao esta sobreposta a uma torre.

@
validaBaseSobrepostaATorres :: Base -> [Torre] -> Bool
validaBaseSobrepostaATorres _ [] = True
validaBaseSobrepostaATorres b@(Base {posicaoBase = posicaoBase}) (Torre {posicaoTorre = posicaoTorre} : torresRestantes)
 |distancia posicaoBase posicaoTorre > 0 = validaBaseSobrepostaATorres b torresRestantes
 |otherwise = False

@

-}

validaBaseSobrepostaATorres :: Base -> [Torre] -> Bool
validaBaseSobrepostaATorres _ [] = True
validaBaseSobrepostaATorres b@(Base {posicaoBase = posicaoBase}) (Torre {posicaoTorre = posicaoTorre} : torresRestantes)
 |distancia posicaoBase posicaoTorre > 0 = validaBaseSobrepostaATorres b torresRestantes
 |otherwise = False

{-| A funcao 'validaBaseSobrepostaAPortais' recebe uma Base e uma lista de Portais e verifica se a base nao esta sobreposta a um portal.

@
validaBaseSobrepostaAPortais :: Base -> [Portal] -> Bool
validaBaseSobrepostaAPortais _ [] = True
validaBaseSobrepostaAPortais b@(Base {posicaoBase = posicaoBase}) (Portal {posicaoPortal = posicaoPortal} : portaisRestantes)
 |distancia posicaoBase posicaoPortal > 0 = validaBaseSobrepostaAPortais b portaisRestantes
 |otherwise = False

@

-}

validaBaseSobrepostaAPortais :: Base -> [Portal] -> Bool
validaBaseSobrepostaAPortais _ [] = True
validaBaseSobrepostaAPortais b@(Base {posicaoBase = posicaoBase}) (Portal {posicaoPortal = posicaoPortal} : portaisRestantes)
 |distancia posicaoBase posicaoPortal > 0 = validaBaseSobrepostaAPortais b portaisRestantes
 |otherwise = False