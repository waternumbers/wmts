## a simple script to see if we can imporve the spped of wmts
rm(list=ls())
devtools::load_all("../")
#library(wmts)
outdir <- "./tmp"
dir.create(outdir)
fn <- system.file("extdata","glofas.nc",package="wmts",mustWork=TRUE)
rst <- raster::raster(fn)
brk <- c(-Inf,0.000000e+00,2.656250e-01,2.453125e+00,2.239063e+05,Inf)
crp <- colorRampPalette(c("lightblue", "blue"),alpha=TRUE)
pal <- genPalette(brk,crp)

profvis::profvis({
ofn <- genTiles(rst,pal,outdir)
})

plotPalette(pal,file.path(outdir,"legend.svg"))
unlink(outdir,recursive=TRUE))
