## simple script to test the output of a plumber server
rm(list=ls())
graphics.off()

devtools::load_all("../")

fn <- system.file("extdata","glofas.nc",package="wmts",mustWork=TRUE)
fcolour <- function(x){
  brk <- c(-Inf,0.000000e+00,2.656250e-01,2.453125e+00,2.239063e+05,Inf)
  crp <- colorRampPalette(c("lightblue", "blue"),alpha=TRUE)
  col <- c("#00000000",crp(length(brk)-2),NA)
  col[ cut(as.numeric(x),brk) ]
}

## won't have legend or static path
serveTiles(fn,fcolour)

