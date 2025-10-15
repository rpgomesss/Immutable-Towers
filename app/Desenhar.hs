module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425


-- | Função principal de renderização, desenha o estado atual do jogo.
desenha :: ImmutableTowers -> IO Picture
-- | Renderiza o menu inicial com a opção "Jogar" selecionada.
desenha ImmutableTowers {menu = MenuInicial Jogar, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate 0 250 $ Scale 0.55 0.55 $ (imagensMenu !! 8),
          Translate (-250) (-150) $ Scale 0.1 0.1 $ (imagensMenu !! 5), -- Imagem para a seta
          Translate (0) (-150) $ Scale 1.6 1.6 $ (imagensMenu !! 6), -- Imagem para "Start"
          Translate (0) (-300) $ Scale 1.5 1.5 $ (imagensMenu !! 7) -- Imagem para "Exit"
        ]
desenha ImmutableTowers {menu = MenuJogo Continuar, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (-250) (100) $ Scale 0.1 0.1 $ (imagensMenu !! 5), -- Imagem para a seta
          Translate (0) (100) $ Scale 1.5 1.5 $ (imagensMenu !! 10), -- Imagem para "Pause"
          Translate (0) (-50) $ Scale 1.5 1.5 $ (imagensMenu !! 11) -- Imagem para "Menu"
        ]
desenha ImmutableTowers {menu = MenuJogo SairDoJogo, imagensMenu = imagensMenu} =
   return $  Pictures
        [ Translate (-250) (-50) $ Scale 0.1 0.1 $ (imagensMenu !! 5), -- Imagem para a seta
          Translate (0) (100) $ Scale 1.5 1.5 $ (imagensMenu !! 10), -- Imagem para "Pause"
          Translate (0) (-50) $ Scale 1.5 1.5 $ (imagensMenu !! 11) -- Imagem para "Menu"
        ]
-- | Renderiza o menu inicial com a opção "Sair" selecionada.
desenha ImmutableTowers {menu = MenuInicial Sair, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate 0 250 $ Scale 0.55 0.55 $ (imagensMenu !! 8),
          Translate (-250) (-300) $ Scale 0.1 0.1 $ (imagensMenu !! 5), -- Imagem para a seta
          Translate (0) (-150) $ Scale 1.6 1.6 $ (imagensMenu !! 6), -- Imagem para "Start"
          Translate (0) (-300) $ Scale 1.5 1.5 $ (imagensMenu !! 7) -- Imagem para "Exit"
        ]
-- | Renderiza o menu de seleção de mapas com a opção "Mapa1" selecionada.
desenha ImmutableTowers {menu = MenuMapa Mapa1, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (400) (-100) $ Scale 0.5 0.5 $ (imagensMenu !! 4), -- Imagem do cadeado
          Translate (-420) (-175) $ Scale 0.5 0.5 $ (imagensMenu !! 0),
          Translate (-800) (250) $ Scale 0.15 0.15 $ (imagensMenu !! 5),
          Translate (-550) 400 $ Text "Escolha o mapa",
          Translate (-650) 200 $ Text "Mapa 1",
          Translate (200) (200) $ Text "Mapa 2"
        ]
-- | Renderiza o menu de seleção de mapas com a opção "Mapa2" selecionada.
desenha ImmutableTowers {menu = MenuMapa Mapa2, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (400) (-100) $ Scale 0.5 0.5 $ (imagensMenu !! 4), -- Imagem do cadeado
          Translate (-420) (-175) $ Scale 0.5 0.5 $ (imagensMenu !! 0),
          Translate (50) (250) $ Scale 0.15 0.15 $ (imagensMenu !! 5),
          Translate (-550) 400 $ Text "Escolha o mapa",
          Translate (-650) 200 $ Text "Mapa 1",
          Translate (200) (200) $ Text "Mapa 2"
        ]
-- | Renderiza o estado do jogo com a loja fechada e o modo "Normal".
desenha ImmutableTowers {menu = Jogando (LojaFechada Normal), jogoIT = jogo, imagensTerreno = imagensTerreno, imagensElementos = imagensElementos, imagensInimigos = imagensInimigos , imagensMenu = imagensMenu} =
   return $  Pictures
        [ Translate (490) (-200) $ Scale 0.3 0.3 $ Text "Loja Bloqueada",
          Translate (390) (-275) $ Scale 0.3 0.3 $ Text "Clique B para desbloquear",
          Translate (630) 0 $ Scale 0.5 0.5 $ (imagensMenu !! 4), -- Imagem do cadeado
          desenhaMapa (mapaJogo jogo) imagensTerreno,   -- Desenha o mapa
          desenhaBase (baseJogo jogo) (imagensElementos !! 0), -- Desenha a base (imagem no índice 8)
          desenhaPortais (portaisJogo jogo) (imagensElementos !! 1),
          desenhaTorre (torresJogo jogo) imagensMenu,
          Translate 425 250 $ Scale 0.35 0.35 $ Text (textoCreditos (baseJogo jogo)),
          Translate 775 265 $ Scale 3 3 $ (imagensElementos !! 2),
          desenhaInimigos (inimigosJogo jogo) imagensInimigos,
          Translate (910) 550 $ Scale 0.08 0.08 (imagensMenu !! 9),
          Translate (905) 543 $ Scale 0.12 0.12 $ Text "P"
        ]
-- | Renderiza o estado do jogo com a loja aberta e a torre de fogo selecionada.
desenha ImmutableTowers {menu = Jogando (LojaAberta TorreFogo), jogoIT = jogo, imagensTerreno = imagensTerreno, imagensElementos = imagensElementos, imagensInimigos = imagensInimigos, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (450) (300) $ Scale 0.1 0.1 $ (imagensMenu !! 5),
          Translate (450) (500) $ Scale 0.3 0.3 $ Text "Escolha uma torre",
          Translate (615) (300) $ Scale 0.8 0.8 $ (imagensMenu !! 1),
          Translate (635) (0) $ Scale 1 1 $ (imagensMenu !! 2),
          Translate (650) (-300) $ Scale 0.9 0.9 $ (imagensMenu !! 3), 
          desenhaMapa (mapaJogo jogo) imagensTerreno,   -- Desenha o mapa
          desenhaBase (baseJogo jogo) (imagensElementos !! 0), -- Desenha a base (imagem no índice 8)
          desenhaPortais (portaisJogo jogo) (imagensElementos !! 1),
          desenhaTorre (torresJogo jogo) imagensMenu,
          Translate 450 450 $ Scale 0.25 0.25 $ Text (textoCreditos (baseJogo jogo)),
          Translate 700 460 $ Scale 2.5 2.5 $ (imagensElementos !! 2),
          Translate (475) (150) $ Scale 0.25 0.25 $ Text (desenhaCustoTorre TorreFogo (lojaJogo jogo)),
          desenhaInimigos (inimigosJogo jogo) imagensInimigos
        ]
-- | Renderiza o estado do jogo com a loja aberta e a torre de gelo selecionada.
desenha ImmutableTowers {menu = Jogando (LojaAberta TorreGelo), jogoIT = jogo, imagensTerreno = imagensTerreno, imagensElementos = imagensElementos, imagensInimigos = imagensInimigos, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (450) (0) $ Scale 0.1 0.1 $ (imagensMenu !! 5),
          Translate (450) (500) $ Scale 0.3 0.3 $ Text "Escolha uma torre",
          Translate (630) (300) $ Scale 0.8 0.8 $ (imagensMenu !! 1),
          Translate (615) (0) $ Scale 1 1 $ (imagensMenu !! 2),
          Translate (650) (-300) $ Scale 0.9 0.9 $ (imagensMenu !! 3), 
          desenhaMapa (mapaJogo jogo) imagensTerreno,   -- Desenha o mapa
          desenhaBase (baseJogo jogo) (imagensElementos !! 0), -- Desenha a base (imagem no índice 8)
          desenhaPortais (portaisJogo jogo) (imagensElementos !! 1),
          desenhaTorre (torresJogo jogo) imagensMenu,
          Translate 450 450 $ Scale 0.25 0.25 $ Text (textoCreditos (baseJogo jogo)),
          Translate 700 460 $ Scale 2.5 2.5 $ (imagensElementos !! 2),
          Translate (475) (-150) $ Scale 0.25 0.25 $ Text (desenhaCustoTorre TorreGelo (lojaJogo jogo)),
          desenhaInimigos (inimigosJogo jogo) imagensInimigos
        ]
-- | Renderiza o estado do jogo com a loja aberta e a torre de resina selecionada.
desenha ImmutableTowers {menu = Jogando (LojaAberta TorreResina), jogoIT = jogo, imagensTerreno = imagensTerreno, imagensElementos = imagensElementos, imagensInimigos = imagensInimigos, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (450) (-300) $ Scale 0.1 0.1 $ (imagensMenu !! 5),
          Translate (450) (500) $ Scale 0.3 0.3 $ Text "Escolha uma torre",
          Translate (630) (300) $ Scale 0.8 0.8 $ (imagensMenu !! 1),
          Translate (635) (0) $ Scale 1 1 $ (imagensMenu !! 2),
          Translate (630) (-300) $ Scale 0.9 0.9 $ (imagensMenu !! 3), 
          desenhaMapa (mapaJogo jogo) imagensTerreno,   -- Desenha o mapa
          desenhaBase (baseJogo jogo) (imagensElementos !! 0), -- Desenha a base (imagem no índice 8)
          desenhaPortais (portaisJogo jogo) (imagensElementos !! 1),
          desenhaTorre (torresJogo jogo) imagensMenu,
          Translate 450 450 $ Scale 0.25 0.25 $ Text (textoCreditos (baseJogo jogo)),
          Translate 700 460 $ Scale 2.5 2.5 $ (imagensElementos !! 2),
          Translate (475) (-450) $ Scale 0.25 0.25 $ Text (desenhaCustoTorre TorreResina (lojaJogo jogo)),
          desenhaInimigos (inimigosJogo jogo) imagensInimigos
        ]
-- | Renderiza a tela de vitória do jogo.
desenha ImmutableTowers {menu = Final Vitoria, imagensMenu = imagensMenu, imagensFinal = imagensFinal} =
    return $ Pictures 
        [
          Translate (-250) (-300) $ Scale 0.15 0.15 $ (imagensMenu !! 5), 
          Translate (0) (-300) $ Scale 2 2 $ (imagensMenu !! 7), 
          Translate (0) (150) $ Scale 0.75 0.75 $ (imagensFinal !! 0)
        ]
-- | Renderiza a tela de derrota do jogo.
desenha ImmutableTowers {menu = Final Derrota, imagensMenu = imagensMenu, imagensFinal = imagensFinal} =
    return $ Pictures 
        [
          Translate (-250) (-300) $ Scale 0.15 0.15 $ (imagensMenu !! 5), 
          Translate (0) (-300) $ Scale 2 2 $ (imagensMenu !! 7), 
          Translate (0) (150) $ Scale 1.25 1.25 $ (imagensFinal !! 1)
        ]
-- | Renderiza o estado do jogo quando a loja está fechada e o jogador está a posicionar uma torre de fogo.
desenha ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreFogo (x,y))), jogoIT = jogo, imagensTerreno = imagensTerreno, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (490) (-200) $ Scale 0.3 0.3 $ Text "Loja Bloqueada",
          Translate (390) (-275) $ Scale 0.3 0.3 $ Text "Clique B para desbloquear",
          Translate (630) 0 $ Scale 0.5 0.5 $ (imagensMenu !! 4), -- Imagem do cadeado
          desenhaMapa' (mapaJogo jogo) (imagensTerreno),
          Translate (x*80-910) ((-y)*80+550) $ Scale 0.55 0.55 $ (imagensMenu !! 1),
          desenhaTorre (torresJogo jogo) imagensMenu
        ]
-- | Renderiza o estado do jogo quando a loja está fechada e o jogador está a posicionar uma torre de gelo.
desenha ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreGelo (x,y))), jogoIT = jogo, imagensTerreno = imagensTerreno, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (490) (-200) $ Scale 0.3 0.3 $ Text "Loja Bloqueada",
          Translate (390) (-275) $ Scale 0.3 0.3 $ Text "Clique B para desbloquear",
          Translate (630) 0 $ Scale 0.5 0.5 $ (imagensMenu !! 4), -- Imagem do cadeado
          desenhaMapa' (mapaJogo jogo) (imagensTerreno),
          Translate (x*80-910) ((-y)*80+550) $ Scale 0.55 0.55 $ (imagensMenu !! 2),
          desenhaTorre (torresJogo jogo) imagensMenu
        ]
-- | Renderiza o estado do jogo quando a loja está fechada e o jogador está a posicionar uma torre de resina.
desenha ImmutableTowers {menu = Jogando (LojaFechada (PosicionandoTorre TorreResina (x,y))), jogoIT = jogo, imagensTerreno = imagensTerreno, imagensMenu = imagensMenu} =
    return $ Pictures
        [ Translate (490) (-200) $ Scale 0.3 0.3 $ Text "Loja Bloqueada",
          Translate (390) (-275) $ Scale 0.3 0.3 $ Text "Clique B para desbloquear",
          Translate (630) 0 $ Scale 0.5 0.5 $ (imagensMenu !! 4), -- Imagem do cadeado
          desenhaMapa' (mapaJogo jogo) (imagensTerreno),
          Translate (x*80-910) ((-y)*80+550) $ Scale 0.55 0.55 $ (imagensMenu !! 3),
          desenhaTorre (torresJogo jogo) imagensMenu
        ]


-- |Função que retorna o custo da torre selecionada e desenha o texto correspondente.
desenhaCustoTorre :: OpcaoTorres -> Loja -> String
desenhaCustoTorre opcaoTorres loja =
    let custo = buscaCusto opcaoTorres loja
    in ("Custo: " ++ show custo ++ " Creditos")
   where
    -- |Mapeamento entre OpcaoTorres e o tipo de projétil associado.
    opcaoParaTipoProjetil :: OpcaoTorres -> TipoProjetil
    opcaoParaTipoProjetil TorreFogo = Fogo
    opcaoParaTipoProjetil TorreGelo = Gelo
    opcaoParaTipoProjetil TorreResina = Resina

    -- |Busca o custo associado a uma opção de torre na loja.
    buscaCusto :: OpcaoTorres -> Loja -> Creditos
    buscaCusto _ [] = 0
    buscaCusto opcao ((creditos, Torre {projetilTorre = p}):resto)
        | opcaoParaTipoProjetil opcao == tipoProjetil p = creditos
        | otherwise = buscaCusto opcao resto


-- |Retorna o texto dos créditos da base.
textoCreditos :: Base -> String
textoCreditos base =
    "Creditos: " ++ show (creditosBase base)

-- | Renderiza todas as torres no mapa de acordo com o tipo de projetil.
desenhaTorre :: [Torre] -> [Picture] -> Picture
desenhaTorre torres imagensMenu =
    Pictures (map desenhaTorre' torres)
  where
    desenhaTorre' :: Torre -> Picture
    desenhaTorre' Torre {posicaoTorre = (x, y), projetilTorre = Projetil {tipoProjetil = tipo}} =
        Translate (x*80-910) ((-y)*80+550)$
        Scale 0.55 0.55 $
        case tipo of
            Fogo -> imagensMenu !! 1
            Gelo -> imagensMenu !! 2
            Resina -> imagensMenu !! 3

-- | Renderiza todos os portais presentes no jogo.
desenhaPortais :: [Portal] -> Picture -> Picture
desenhaPortais portais imagemPortal = Pictures $ map desenhaPortal portais
  where
    -- |Desenha cada portal individualmente
    desenhaPortal :: Portal -> Picture
    desenhaPortal Portal {posicaoPortal = (x, y)} =
        Translate (x * tamanhoTile-895) ((-y) * tamanhoTile+470) $
        Scale escalaPortal escalaPortal imagemPortal
    
    tamanhoTile = 32.0  -- |Tamanho do tile em pixels
    escalaPortal = 0.4  -- |Ajuste da escala para redimensionar a imagem do portal

-- | Renderiza a base do jogador no mapa.
desenhaBase :: Base -> Picture -> Picture
desenhaBase Base {posicaoBase = (x, y)} imagemBase =
    Translate (x * tamanhoTile - 895) ((-y) * tamanhoTile + 35) $
    Scale escalaBase escalaBase imagemBase
  where
    tamanhoTile = 32.0 -- Tamanho do tile em pixels
    escalaBase = 1.85   -- Ajuste o valor para redimensionar a imagem da base

-- | Renderiza o mapa completo do jogo.
desenhaMapa :: Mapa -> [Picture] -> Picture
desenhaMapa mapa imagens = Translate (-900) 530 $ Scale escalax escalay $ Pictures $ concatMap desenhaLinha (zip [0..] mapa)
  where
    escalay = 2.5
    escalax = 2.5
    tamanhoTile = 32.0 -- |Tamanho do tile em pixels

    desenhaLinha :: (Int, [Terreno]) -> [Picture]
    desenhaLinha (y, linha) = map (desenhaTerreno y) (zip [0..] linha)

    desenhaTerreno :: Int -> (Int, Terreno) -> Picture
    desenhaTerreno y (x, terreno) = Translate (fromIntegral x * tamanhoTile) (fromIntegral (-y) * tamanhoTile) (imagemTerreno terreno x y mapa imagens)

-- | Renderiza apenas os blocos de relva do mapa.
desenhaMapa' :: Mapa -> [Picture] -> Picture
desenhaMapa' mapa imagens = Translate (-900) 530 $ Scale escalax escalay $ Pictures $ concatMap desenhaLinha (zip [0..] mapa)
  where
    escalay = 2.5
    escalax = 2.5
    tamanhoTile = 32.0 -- |Tamanho do tile em pixels

    desenhaLinha :: (Int, [Terreno]) -> [Picture]
    desenhaLinha (y, linha) = map (desenhaTerreno y) (zip [0..] linha)

    desenhaTerreno :: Int -> (Int, Terreno) -> Picture
    desenhaTerreno y (x, terreno) = Translate (fromIntegral x * tamanhoTile) (fromIntegral (-y) * tamanhoTile) (imagemTerreno' terreno x y mapa imagens)

-- | Renderiza os inimigos no mapa.
desenhaInimigos :: [Inimigo] -> [Picture] -> Picture
desenhaInimigos inimigos imagensInimigos =
    Pictures $ map desenhaInimigo inimigos
  where
    desenhaInimigo :: Inimigo -> Picture
    desenhaInimigo Inimigo {posicaoInimigo = (x, y), direcaoInimigo = dir} =
        Translate (x * tamanhoTile -940) ((-y) * tamanhoTile + 560)  $
        Scale escalaInimigo escalaInimigo $
        imagemDirecao dir
    
    tamanhoTile = 75-- |Tamanho do tile em pixels
    escalaInimigo = 2.0 -- |Escala para ajustar o tamanho do inimigo
    
    imagemDirecao :: Direcao -> Picture
    imagemDirecao Norte  = imagensInimigos !! 0 -- |Imagem do inimigo virado para cima
    imagemDirecao Sul    = imagensInimigos !! 1 -- |Imagem do inimigo virado para baixo
    imagemDirecao Oeste  = imagensInimigos !! 2 -- |Imagem do inimigo virado para a esquerda
    imagemDirecao Este   = imagensInimigos !! 3 -- |Imagem do inimigo virado para a direita

imagemTerreno' :: Terreno -> Int -> Int -> Mapa -> [Picture] -> Picture
imagemTerreno' Relva _ _ _ imagens = imagens !! 0
imagemTerreno' _ _ _ _ _ = blank

-- | Mapeia o tipo de terreno para a imagem correspondente.
imagemTerreno :: Terreno -> Int -> Int -> Mapa -> [Picture] -> Picture
imagemTerreno Relva _ _ _ imagens = imagens !! 0
imagemTerreno Terra _ _ _ imagens = imagens !! 1
imagemTerreno Agua x y mapa imagens =
    case (vizinhoEsquerda, vizinhoDireita, vizinhoCima, vizinhoBaixo) of
        (_, True, _, True) -> imagens !! 5 -- |Curva Superior Esquerda
        (True, _, _, True) -> imagens !! 4 -- |Curva Superior Direita 
        (_, True, True, _) -> imagens !! 7 -- |Curva Inferior Esquerda 
        (True, _, True, _) -> imagens !! 6 -- |Curva Inferior Direita 
        (True, _, _, _) -> imagens !! 2 -- |Água Horizontal
        (_, True, _, _) -> imagens !! 2 -- |Água Horizontal
        (_, _, True, _) -> imagens !! 3 -- |Água Vertical
        (_, _, _, True) -> imagens !! 3 -- |Água Vertical
        _ -> imagens !! 3 -- |Fallback padrão
  where
     -- |Verificação para as curvas de água considerando até dois blocos de distância
    vizinhoEsquerda  = verificaVizinho x y (-2) 0
    vizinhoDireita   = verificaVizinho x y 2 0
    vizinhoCima      = verificaVizinho x y 0 (-2)
    vizinhoBaixo     = verificaVizinho x y 0 2

   -- |Verificação para vizinhos até dois blocos de distância
    verificaVizinho :: Int -> Int -> Int -> Int -> Bool
    verificaVizinho xOrigem yOrigem dx dy =
        any (\dist -> verificaPosicao (xOrigem + dx * dist) (yOrigem + dy * dist)) [1, 2]
        
    -- |Função auxiliar que verifica se a posição é válida e contém Água
    verificaPosicao :: Int -> Int -> Bool
    verificaPosicao nx ny =
        nx >= 0 && ny >= 0 &&
        ny < length mapa && nx < length (head mapa) &&
        (mapa !! ny !! nx == Agua)



loadImagensTerreno :: IO [Picture]
loadImagensTerreno = do
  -- |Imagens do terreno
    relva <- loadBMP "IMAGENSFINAIS/relva.bmp"
    terra <- loadBMP "IMAGENSFINAIS/terra.bmp"
    aguaH  <- loadBMP "IMAGENSFINAIS/aguaH.bmp"
    aguaV  <- loadBMP "IMAGENSFINAIS/aguaV.bmp"
    aguaSD <- loadBMP "IMAGENSFINAIS/aguaSD.bmp"
    aguaSE <- loadBMP "IMAGENSFINAIS/aguaSE.bmp"
    aguaID <- loadBMP "IMAGENSFINAIS/aguaID.bmp"
    aguaIE <- loadBMP "IMAGENSFINAIS/aguaIE.bmp"
    return [relva, terra, aguaH,aguaV, aguaSD, aguaSE, aguaID, aguaIE]

loadImagensElementos :: IO [Picture]
loadImagensElementos = do
  -- |Imagens dos elementos do jogo
    base   <- loadBMP "IMAGENSFINAIS/base.bmp"
    portal <- loadBMP "IMAGENSFINAIS/portal.bmp"
    credito <- loadBMP "IMAGENSFINAIS/credito.bmp"
    return [base, portal,credito]

loadImagensInimigos :: IO [Picture]
loadImagensInimigos = do
  -- |Imagens de todas as posicoes do inimigo
    inimigoBaixo    <- loadBMP "IMAGENSFINAIS/inimigoBaixo.bmp"
    inimigoCima     <- loadBMP "IMAGENSFINAIS/inimigoCima.bmp"
    inimigoEsquerda <- loadBMP "IMAGENSFINAIS/inimigoEsquerda.bmp"
    inimigoDireita  <- loadBMP "IMAGENSFINAIS/inimigoDireita.bmp"
    return [inimigoBaixo, inimigoCima, inimigoEsquerda, inimigoDireita]

loadImagensMenu :: IO [Picture]
loadImagensMenu = do
  -- |Imagem do primeiro mapa do jogo
    mapa1 <- loadBMP "IMAGENSFINAIS/mapa1.bmp"
  -- |Imagem da torre de fogo
    torreFogo <- loadBMP "IMAGENSFINAIS/torreFogo.bmp"
  -- |Imagem da torre de gelo
    torreGelo <- loadBMP "IMAGENSFINAIS/torreGelo.bmp"
  -- |Imagem da torre de resina
    torreResina <- loadBMP "IMAGENSFINAIS/torreResina.bmp"
  -- |Imagem do cadeado para o estado de loja fechada
    cadeado <- loadBMP "IMAGENSFINAIS/cadeado.bmp"
  -- |Imagem da seta de selecao usada nos varios menus do jogo
    seta <- loadBMP "IMAGENSFINAIS/seta.bmp"
  -- |Imagem do botao de play do menu inicial
    start <- loadBMP "IMAGENSFINAIS/jogar.bmp"
  -- |Imagem do botao de exit do menu inicial
    exit  <- loadBMP "IMAGENSFINAIS/sair.bmp"
  -- |Imagem do titulo do jogo presente no menu inicial
    titulo <- loadBMP "IMAGENSFINAIS/titulo.bmp"
  -- |Imagem da engrenagem
    engrenagem <- loadBMP "IMAGENSFINAIS/engrenagem.bmp"
  -- |Imagem do botao pause
    pause <- loadBMP "IMAGENSFINAIS/pause.bmp"
  -- |Imagem do botao menu
    menu <- loadBMP "IMAGENSFINAIS/menu.bmp"
    return [mapa1, torreFogo, torreGelo, torreResina, cadeado, seta, start, exit, titulo, engrenagem, pause, menu]

loadImagensFinal :: IO [Picture]
loadImagensFinal = do 
  -- |Imagem de vitoria
    vitoria <- loadBMP "IMAGENSFINAIS/victory.bmp"
  -- |Imagem de derrota
    derrota <- loadBMP "IMAGENSFINAIS/defeat.bmp"
    return [vitoria,derrota]