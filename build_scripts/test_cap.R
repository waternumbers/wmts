rm(list=ls())
ftmp <- function(url,layers,tmplt){
    out <- tmplt #private$capabilities_template
    str <- ""
    for(ii in layers){
        str <- paste(
            str,
            "<Layer>",
            paste0("<ows:Title>",ii,"</ows:Title>"),
            paste0("<ows:Identifier>",ii,"</ows:Identifier>"),
            "<ows:BoundingBox>",
            "<ows:LowerCorner>-20037508.3427892 -20037508.3427892</ows:LowerCorner>",
            "<ows:UpperCorner>-20037508.3427892 20037508.3427892</ows:UpperCorner>",
            "</ows:BoundingBox>",
            '<Style isDefault="true">',
            '<ows:Title>default</ows:Title>',
            '<ows:Identifier/>',
            '</Style>',
            '<Format>image/png</Format>',
            '<TileMatrixSetLink>',
            '<TileMatrixSet>WorldWebMercatorQuad</TileMatrixSet>',
            '</TileMatrixSetLink>',
            paste0('<ResourceURL format="image/png" resourceType="simpleProfileTile" template="',url,'/',ii,'/{TileMatrix}/{TileCol}/{TileRow}.png"/>'),
            paste0('<ResourceURL format="image/png" resourceType="tile" template="',url,'/',ii,'/{TileMatrix}/{TileCol}/{TileRow}.png"/>'),
            '</Layer>',
            collapse="\n")
    }
    gsub("LAYER_INFO",str,out)
}

tmplt <- paste0(readLines("./build_scripts/template.xml"),collape="\n")
writeLines( ftmp("http://waternumbers.io",c("add","divide"),tmplt), "test.xml")
