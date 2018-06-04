module TiledMap exposing (..)

import Tileset exposing (Tileset)
import Tiled.Decode as Tiled
import Element exposing (Element)
import Dict


drawLevel : List (List Element) -> Element
drawLevel listOfElements =
    List.map
        (\row ->
            Element.flow Element.right row
        )
        listOfElements
        |> List.foldr Element.above Element.empty


overwriteList : List Int -> List Int -> List Int
overwriteList original dominant =
    List.map2
        (\a ->
            \b ->
                if b == 0 then
                    a
                else
                    b
        )
        original
        dominant



-- case ( original, dominant ) of
--     ( [], [] ) ->
--         []
--     ( [], _ ) ->
--         []
--     ( _, [] ) ->
--         []
--     ( h1 :: t1, h2 :: t2 ) ->
--         (if h2 == 0 then
--             h1
--          else
--             h2
--         )
--             :: overwriteList t1 t2
-- We want to only take the most recent.. and ignore 0s


overwriteLayers : List (List Int) -> List (List Int) -> List (List Int)
overwriteLayers original dominant =
    List.map2 (\a -> \b -> overwriteList a b) original dominant



-- case ( original, dominant ) of
--     ( [], [] ) ->
--         []
--     ( [], _ ) ->
--         []
--     ( _, [] ) ->
--         []
--     ( h1 :: t1, h2 :: t2 ) ->
--         (overwriteList h1 h2) :: overwriteLayers t1 t2


nullLayer : Tiled.Level -> List (List Int)
nullLayer level =
    List.repeat
        level.height
        (List.repeat level.width 0)


overlayedSpriteInts : Tiled.Level -> List (List Int)
overlayedSpriteInts level =
    List.foldr overwriteLayers
        (nullLayer level)
        [ (spriteNumbers 0 level)
        , (spriteNumbers 1 level)
        , (spriteNumbers 2 level)
        , (spriteNumbers 3 level)
        , (spriteNumbers 4 level)
        ]


gameMap : Tiled.Level -> Element
gameMap level =
    mapSprites
        (overlayedSpriteInts level)
        |> List.map (\row -> List.foldr Element.beside Element.empty row)
        |> List.foldr Element.above Element.empty



-- List.map drawLevel
-- [ (spriteElements 0 level)
-- -- , (spriteElements 1 level)
-- -- , (spriteElements 2 level)
-- -- , (spriteElements 3 level)
-- -- , (spriteElements 4 level)
-- ]
-- |> Element.layers


spriteElements : Int -> Tileset -> Tiled.Level -> List (List Element.Element)
spriteElements levelNumber tileset level =
    spriteNumbers levelNumber level
        |> List.map
            (\row ->
                List.map (\int -> Tileset.cropImage tileset int) row
            )


spriteNumbers : Int -> Tiled.Level -> List (List Int)
spriteNumbers levelNumber level =
    layoutLayerTileNumbers <|
        nLayer levelNumber level.layers


getTileLayerData : Tiled.Layer -> Tiled.TileLayerData
getTileLayerData layer =
    case layer of
        Tiled.TileLayer a ->
            a

        _ ->
            nullTileLayerData


headWithDefault : List Tiled.Layer -> Tiled.Layer
headWithDefault layers =
    List.head layers
        |> Maybe.withDefault (Tiled.TileLayer nullTileLayerData)


nLayer : Int -> List Tiled.Layer -> Tiled.TileLayerData
nLayer layerNumber layers =
    let
        head =
            headWithDefault layers

        tail =
            List.tail layers |> Maybe.withDefault []
    in
        case layerNumber of
            0 ->
                firstLayer layers

            _ ->
                nLayer (layerNumber - 1) tail


firstLayer : List Tiled.Layer -> Tiled.TileLayerData
firstLayer layerList =
    List.filter
        (\a ->
            case a of
                Tiled.TileLayer _ ->
                    True

                _ ->
                    False
        )
        layerList
        |> headWithDefault
        -- |> List.head
        -- |> Maybe.withDefault (Tiled.TileLayer nullTileLayerData)
        |> getTileLayerData


mapSprites : Tileset -> List (List Int) -> List (List Element)
mapSprites tileset =
    List.map (\row -> List.map (Tileset.cropImage tileset) row)



-- mapSprites : Int -> Tiled.Level -> List (List Element)
-- mapSprites levelNumber level =
--     spriteNumbers levelNumber level
--         |> List.map (\row -> List.map (Tileset.cropImage) row)
--     -- layerToElements <| (nLayer 3 level.layers)


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


layoutLayerTileNumbers : Tiled.TileLayerData -> List (List Int)
layoutLayerTileNumbers layer =
    let
        width =
            layer.width

        layerdata =
            layer.data
    in
        split width layerdata


nullTileLayerData : Tiled.TileLayerData
nullTileLayerData =
    { data = []
    , height = 0
    , name = "No Layer"
    , opacity = 1
    , visible = True
    , width = 0
    , x = 0
    , y = 0
    , properties = Dict.fromList []
    }
