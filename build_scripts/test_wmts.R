## simple script to test the output of a plumber server
rm(list=ls())
graphics.off()

devtools::load_all("../")


fn <- system.file("extdata","glofas.nc",package="wmts",mustWork=TRUE)
test <- wmts$new(fn)
#test$overload(NULL,"fLegend")
#test$overload(NULL,"fColour")
#test$overload(NULL,"fPlot")
#test$overload(NULL,"fExtract")
## tm <- test$TileMatrix()
## test$legend(tm)
## test$feature(tm,80,20)
## test$feature(tm,80,20,TRUE)
## tl <- test$tile(tm,0,0,0)

s <- wmtsServer$new(test)$start()

