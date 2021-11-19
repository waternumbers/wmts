## how does it handle edge cases?
## set different methods for reprojection
## can we limit to only looping values within domain of rst?


#' Function to generate tiles for 0.1 degree data
#' @param rst a SpatRaster or file name to open as a Spatraster to generate tiles from
#' @param fcol function to convert raster values into an RGBA matrix - see details
#' @param outdir directory into which the tiles are written
#' @param minZ lowest zoom level to generate
#' @param maxZ maximum zoom level to generate
#' @param w width of tiles in pixels
#' @param h height of tiles in pixels
#' @param lv values used to generage a legend. If \code{NULL} to legend is generated.
#' @param ... passed to terra project or terra resample
#'
#' @details This is naive generater.
#' Defaults work well for plotting 0.1 degree data in a limited number of tiles.
#' fcol should take a vector of n values and return an n hex colour values which should include transparency.
#' 
#' @export
genTiles <- function(rst,fcol,outdir=".",minZ=0,maxZ=3,w=512,h=w,lv=NULL, ...){

    if(!("SpatRaster" %in% class(rst))){
        rst <- terra::rast(rst)
    }

    ## generate legend
    if(!is.null(lv)){
        tmp <- scaleImage(fcol,lv)
        writeLines(tmp,file.path(outdir,"legend.svg"))
    }
    
    ## loop zoom levels
    for(Z in minZ:maxZ){
        dir.create(file.path(outdir,Z),showWarnings=FALSE,recursive=TRUE)
        for(X in 0:(2^Z -1)){
            dir.create(file.path(outdir,Z,X),showWarnings=FALSE)
            for(Y in 0:(2^Z - 1)){
                genFlag <- TRUE
                if( Z>minZ ){
                    ## work out if parent file exists
                    ## if it doesn't then don't generate
                    ZZ <- Z-1
                    XX <- X %/% 2
                    YY <- Y %/% 2
                    genFlag <- file.exists(file.path(outdir,ZZ,XX,paste0(YY,".png")))
                }
                if(genFlag){
                    tmp <- singleTile(rst,fcol,Z,X,Y,w,h,...)
                    if(!is.null(tmp)){
                        writeBin(tmp,file.path(outdir,Z,X,paste0(Y,".png")))
                    }
                }
            }
        }
    }
    list.files(outdir,recursive=TRUE)
}
