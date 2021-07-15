## a simple script to see if we can imporve the spped of wmts
rm(list=ls())
devtools::load_all("../")
#library(wmts)
outdir <- "./tmp"
unlink(outdir,recursive=TRUE)
dir.create(outdir)
fn <- system.file("extdata","glofas.nc",package="wmts",mustWork=TRUE)
rst <- raster::raster(fn)
brk <- c(-Inf,0.000000e+00,2.656250e-01,2.453125e+00,2.239063e+05,Inf)
crp <- colorRampPalette(c("lightblue", "blue"),alpha=TRUE)
pal <- genPalette(brk,crp)

system.time({
ofn <- genTiles(rst,pal,outdir)
})
system.time({
ofn <- genTiles(rst,pal,outdir,maxMemory=2e8)
})

plotPalette(pal,file.path(outdir,"legend.svg"))
unlink(outdir,recursive=TRUE)







rm(list=ls())
devtools::load_all("../")
fn <- system.file("extdata","glofas.nc",package="wmts",mustWork=TRUE)
rst <- rast(fn)
maxZ <- 3
w <- 512
h <- 512
nr <- (2^maxZ)*h
nc <- (2^maxZ)*w
## reproject p into format for tiles
trst <- raster::raster(nrows=nr,ncol=nc,xmn=-20037508,xmx=20037508,ymn=-20037508,ymx=20037508,crs=sp::CRS("EPSG:3857"))

pnt <- rasterToPoints(trst)
spnt <- SpatialPoints(pnt,proj4string=crs(trst))
slpnt <- spTransform(spnt,"EPSG:4326")
lpnt <- geom(slpnt)
max(diff(sort(unique(lpnt[,"x"]))))
max(diff(sort(unique(lpnt[,"y"]))))


tr <- rast(nrows=nr, ncols=nc,xmin=-180, xmax=180, ymin=-85.051129,ymax=85.051129, crs="EPSG:3857")

tmp <- project(rst,tr)
