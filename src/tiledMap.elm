module TiledMap exposing (gameMap, cropImage)

import Tiled.Decode as Tiled
import Element exposing (Element)
import Dict
import Debug


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
        [ (spriteNumbers 0 level)
        , (spriteNumbers 1 level)
        , (spriteNumbers 2 level)
        , (spriteNumbers 3 level)
        , (spriteNumbers 4 level)
        ]


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
    layoutLayerTileNumbers <|
        nLayer levelNumber level.layers


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


cropImage : Tiled.Level -> Int -> Element.Element
cropImage level tileNumber =
    let
        embeddedTileData : Tiled.EmbeddedTileData
        embeddedTileData =
            case
                (List.head level.tilesets
                    |> unwrap
                )
            of
                Tiled.TilesetEmbedded a ->
                    a

                _ ->
                    Debug.crash "We need an embeddedTileData object in the .json file.\n See https://discourse.mapeditor.org/t/how-do-i-embed-the-tilesets/1761"

        xTileCount =
            embeddedTileData.columns

        xTileNumber =
            if xTileCount == 0 then
                0
            else
                tileNumber % xTileCount

        yTileNumber =
            tileNumber // xTileCount

        xTilePxStart =
            (xTileNumber * embeddedTileData.tilewidth) - embeddedTileData.tilewidth

        yTilePxStart =
            (yTileNumber * embeddedTileData.tileheight)
    in
        Element.croppedImage
            ( xTilePxStart, yTilePxStart )
            level.tileheight
            level.tilewidth
            embeddedTileData.image
