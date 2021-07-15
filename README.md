# wmts

The pakcage provides a set of functions for the generation of "slippy" web map
tiles from `raster` layers.

The package [tiler](https://docs.ropensci.org/tiler/) already adresses this
need by wrapping the `gdal2tiles` script written in python. The motivation of
this package is two fold

- to provide a "purer" R means of generating tiles
- to provide a more R based means of controlling the tile size, colour scheme
etc

Addressing these should hopefully result in a more manageable and
understandable tool for general R users.
  
Current performance in generating tiles is no where near that of `gdal2tiles` and thus the tiler package, but as
demonstrated in the vignette the more obvious control can be useful.



