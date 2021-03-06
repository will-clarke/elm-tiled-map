# Tiled Map

[View this on package.elm-lang.org](http://package.elm-lang.org/packages/will-clarke/elm-tiled-map/latest)

This package allows the (super inefficient!) rendering of Tiled Maps made with the [Tiled](https://www.mapeditor.org/) / `.tmx` editor.

### Example

Full tilemap:

![](http://i.wclarke.net/2018-06-13--e222j.png)

Original sprite:

![](https://github.com/will-clarke/elm-tiled-map/raw/master/example/assets/image.png)

See the [example](https://github.com/will-clarke/elm-tiled-map/tree/master/example/) directory to see how this was done.

### How to create the tileset
You'll need to:
- Edit the map as you'd like with the [Tiled](https://www.mapeditor.org/) editor
- Embed the tileset into the tilemap [(documentation here)](https://discourse.mapeditor.org/t/how-do-i-embed-the-tilesets/1761)
- Export it to JSON.

### Caveats

- Only currently supports one tileset
- No transparrent tiles allowed (they won't be stacked)
- It creates really horrible HTML / inefficient tilemaps
