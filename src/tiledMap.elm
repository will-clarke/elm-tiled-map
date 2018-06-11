module TiledMap exposing (gameMap, cropImage, overlayedSpriteInts, spritePixelPosition, spritePosition)

import Tiled.Decode as Tiled
import Element exposing (Element)
import Dict
import Debug
import Dict


gameMap : Tiled.Level -> Element
gameMap level =
    mapSprites
        level
        (overlayedSpriteInts level)
        |> List.map (\row -> List.foldr Element.beside Element.empty row)
        |> List.foldr Element.above Element.empty


overlayedSpriteInts : Tiled.Level -> List (List Int)
overlayedSpriteInts level =
    List.foldr overwriteLayers
        (nullLayer level)
        (level.layers
            |> List.indexedMap (,)
            |> List.filter (\( index, layer ) -> isTileLayer layer)
            |> List.map (\( index, layer ) -> (spriteNumbers index level))
        )


isTileLayer : Tiled.Layer -> Bool
isTileLayer layer =
    case layer of
        Tiled.TileLayer _ ->
            True

        _ ->
            False


nullLayer : Tiled.Level -> List (List Int)
nullLayer level =
    List.repeat
        level.height
        (List.repeat level.width 0)


overwriteLayers : List (List Int) -> List (List Int) -> List (List Int)
overwriteLayers original dominant =
    List.map2 (\a -> \b -> overwriteList a b) original dominant


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


spriteElements : Int -> Tiled.Level -> List (List Element.Element)
spriteElements levelNumber level =
    spriteNumbers levelNumber level
        |> List.map
            (\row ->
                List.map (\int -> cropImage level int) row
            )


spriteNumbers : Int -> Tiled.Level -> List (List Int)
spriteNumbers levelNumber level =
    nLayer levelNumber level.layers
        |> layoutLayerTileNumbers


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
        |> getTileLayerData


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


mapSprites : Tiled.Level -> List (List Int) -> List (List Element)
mapSprites level =
    List.map (\row -> List.map (cropImage level) row)


layoutLayerTileNumbers : Tiled.TileLayerData -> List (List Int)
layoutLayerTileNumbers layer =
    let
        width =
            layer.width

        layerdata =
            layer.data
    in
        split width layerdata


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


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


unwrap : Maybe a -> a
unwrap wrapped =
    case wrapped of
        Just a ->
            a

        Nothing ->
            Debug.crash "Tried to unwrap a None"


spritePixelPosition : Tiled.Level -> Int -> ( Int, Int )
spritePixelPosition level tileNumber =
    let
        embeddedTileData =
            firstTileSetData level

        ( x, y ) =
            spritePosition level tileNumber
    in
        ( x * embeddedTileData.tilewidth
        , y * embeddedTileData.tileheight
        )


spritePosition : Tiled.Level -> Int -> ( Int, Int )
spritePosition level tileNumber =
    let
        embeddedTileData =
            firstTileSetData level

        x =
            if embeddedTileData.columns == 0 then
                0
            else
                (tileNumber - 1) % embeddedTileData.columns

        y =
            (tileNumber - 1) // embeddedTileData.columns
    in
        ( x, y )


nullEmbeddedTileData : Tiled.EmbeddedTileData
nullEmbeddedTileData =
    { columns = 0
    , firstgid = 0
    , image = ""
    , imageheight = 0
    , imagewidth = 0
    , margin = 0
    , name = ""
    , spacing = 0
    , tilecount = 0
    , tileheight = 0
    , tilewidth = 0
    , transparentcolor = ""
    , tiles = Dict.empty
    , properties = Dict.empty
    }


firstTileSetData : Tiled.Level -> Tiled.EmbeddedTileData
firstTileSetData level =
    case
        List.head level.tilesets
    of
        Just a ->
            case a of
                Tiled.TilesetEmbedded a ->
                    a

                _ ->
                    nullEmbeddedTileData

        Nothing ->
            nullEmbeddedTileData



-- Tiled.TilesetEmbedded a ->
--     a
-- _ ->
--     Debug.crash
--         ("We need an embeddedTileData object in the .json file. "
--             ++ toString level
--             ++ " See https://discourse.mapeditor.org/t/how-do-i-embed-the-tilesets/1761"
--         )


cropImage : Tiled.Level -> Int -> Element.Element
cropImage level tileNumber =
    let
        embeddedTileData =
            firstTileSetData level

        ( x, y ) =
            spritePixelPosition level tileNumber
    in
        Element.croppedImage
            ( x, y )
            level.tileheight
            level.tilewidth
            embeddedTileData.image
