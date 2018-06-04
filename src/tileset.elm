module Tileset exposing (Tileset, cropImage)

import Element


type alias Tileset =
    { path : String
    , widthPx : Int
    , heightPx : Int
    , tileHeightPx : Int
    , tileWidthPx : Int
    }


tileCountWidth : Tileset -> Int
tileCountWidth tileset =
    tileset.widthPx // tileset.tileWidthPx


tileCountHeight : Tileset -> Int
tileCountHeight tileset =
    tileset.heightPx // tileset.tileHeightPx


cropImage : Tileset -> Int -> Element.Element
cropImage tileset tileNumber =
    let
        xTileCount =
            tileCountWidth tileset

        xTileNumber =
            if xTileCount == 0 then
                0
            else
                tileNumber % xTileCount

        yTileNumber =
            tileNumber // xTileCount

        xTilePxStart =
            (xTileNumber * tileset.tileWidthPx) - tileset.tileWidthPx

        yTilePxStart =
            (yTileNumber * tileset.tileHeightPx)
    in
        Element.croppedImage
            ( xTilePxStart, yTilePxStart )
            tileset.tileHeightPx
            tileset.tileWidthPx
            tileset.path
