#' R6 Class for wmts
#' @export
wmts <- R6Class(
    "wmts",
    public = list(      
        #' @description Creates a wmts object from the file name passed to the object
        #'
        #' @param dataFile Name of the file to be used as the wmts
        #' @param use_time logical should TileMatrix value be taken from the times rather then layer names
        #'
        #' @return invisible(self) suitable for chaining
        #'
        initialize = function(dataFile, use_time=FALSE){
            private$dataFile <- normalizePath(dataFile)
            private$use_time <- as.logical(use_time)
            private$readRast()
            self$overload(NA,"fExtract")
            self$overload(NA,"fPlot")
            self$overload(NA,"fColour")
            self$overload(NA,"fLegend")
            
            invisible(self)
        },
        #' @description Return the list of available TileMatrix sets
        TileMatrix = function(){
            private$isChanged()
            private$tileMatrix
        },
        #' @description Returns a single tile as a raster object
        #'
        #' @param TileMatrix the TileMatrix (SpatRaster layer) to return from
        #' @param Z zoom level: 0,1,2,3,4,...
        #' @param X row of image grid
        #' @param Y column of image grid
        #' @param w width in pixels
        #' @param h height in pixels
        #'
        #' @return a grDevices raster object
        #'
        #' @details This works by defining a SpatRaster of the required area then projecting the SpatRaster layer onto this. The numeric values of the resulting raster are converted to colouts using fcolour and returned as a raster (as defined in grDevices). Plotting or saving this to an image of the correct dimensions is handled in genTiles or serveWMTS. 
        tile = function(TileMatrix,Z,X,Y,w=256,h=w){
            private$isChanged()
            ## TODO checks on TileMatrix,Z,X, & Y,w & h
            private$fTile(TileMatrix,Z,X,Y,w,h)
        },
        #' @description Returns a legend for a TileMatrix
        #'
        #' @param TileMatrix the layer of rst to generate the legend for
        #'
        #' @return a plot containing the legend
        legend = function(TileMatrix){
            private$isChanged()
            ##TODO check on TileMatrix
            private$fLegend(TileMatrix)
        },
        #' return data for a given set of coordinates
        #'
        #' @param TileMatrix the layer of rst to generate tile from
        #' @param lon Longitdue of point of interest
        #' @param lat Latitude of the point of interest
        #' @param plot should a plot be returned
        #'
        #' @details returns the output of fExtract or fPlot
        feature = function(TileMatrix,lon,lat,plot=FALSE){
            private$isChanged()
            ## TODO check on TileMatrix and other inputs
            pnt <- terra::vect( matrix(c(lon,lat),1,2), crs="EPSG:4326" )
            pnt <- terra::project(pnt,terra::crs(private$rst))
            out <- private$fExtract(pnt,TileMatrix)
            if(plot){
                out <- private$fPlot(out,TileMatrix)
            }
            out
        },
        #' @description Shows, or sets the cache values
        #'
        #' @param p the path
        #' @param x logical if to clear path on file change
        #' @param create logical, should path be created. Default is FALSE
        #'
        #' @details If both p and x are \code{NULL} returns the current values as a list. If only one is \code{NULL} then it is left unaltered. The non-NULL values is set to the value given, or if \code{NA} the default value
        #'
        #' @return Either the current value or invisible(self)
        #'
        cache = function(p=NULL, x=NULL,create=FALSE){
            if( is.null(p) &is.null(x) ){
                return( list(clearCache = private$clearCache,
                             cachePath = private$cachePath) )
            }
            if(!is.null(p)){
                p <- as.character(p[1])
                if( is.na(p) ){ p <- NA } ## default value
                else{
                    p <- file.path(p)
                    if(!dir.exists(p) & create){ dir.create(p,recursive=TRUE) }
                    if( !dir.exists(p) ){ stop("path for cache does not exist") }
                }
                private$cachePath <- p
            }
            if(!is.null(x)){
                x <- as.logical(x[1])
                if( is.na(x) ){ x <- FALSE } ## default value
                private$clearCache <- x
            }
            invisible(self)
        },
        #' @description Shows or Sets one over the overloadable function
        #'
        #' @param f see details
        #' @param name name of function to overload
        #'
        #' @details If \code{f=NULL} the current value of the names function is returned.
        #' If \code{f=NA} then the default function is set. Otherwise if f is a function it set to be used.
        #' The function being overloaded should take the following inputs and outputs
        #'
        #' fExtract
        #' Input: a SpatVector (containing only the point for extraction) and the TileMatrix name
        #' Output: a object that be serialised to json for a GetFeatureInfo response
        #'
        #' fPlot
        #' Input: The output of fExtract and the TileMatrix name
        #' Output: A plot of the extracted data
        #'
        #' fColour
        #' Input: a vector or matrix of numerical values and the TileMatrix name
        #' Output: a raster object of colour codes the same size as the input matrix
        #'
        #' fLegend
        #' Input: the TileMatrix name
        #' Output: A plot to use as the legend
        #'
        #' @return Either the current value or invisible(self)
        overload = function(f=NULL,name=c("fExtract","fPlot","fColour","fLegend")){
            name <- match.arg(name)
            if( is.null(f) ){ return( private[[name]] ) }
            if( is.na(f) ){ private[[name]] <- private[[paste0(name,"_default")]] }
            else{
                if(is.function(f)){ private[[name]] <- f }
                else{ stop("The value of f is not valid") }
            }
            invisible(self)
        }
    ),
    private = list(
        ## #################################################
        ## Storage objects
        ## #################################################
        dataFile = NULL, ## name of data file
        use_time = FALSE,## should times be used
        rst = NULL, ## storage for SpatRaster object
        snapshot = NULL, ## snapshot of folder status for testing file changes
        cachePath = NA, ## path of root folder of cache
        clearCache = FALSE, ## should cache be cleared if file changes
        tileMatrix = NULL, ## list of valid tile layers
        validZ = c(0,4), ## storage of valid zoom levels
        ## #################################################
        ## Functions
        ## #################################################
        readRast = function(){ ## read in the SpatRaster object
            private$rst <- terra::rast(private$dataFile)
            if(private$use_time){
                names(rst) <- format(terra::time(rst),"%Y-%m-%dT%H:%M:%SZ",tz="GMT")
            }
            nm <- names(private$rst)
            private$tileMatrix <- nm[is.character(nm)]
            private$snapshot <- utils::fileSnapshot(dirname(private$dataFile))
            invisible(NULL)
        },
        isChanged = function(){ ## function to test if dataFile has changed
            tmp <- utils::fileSnapshot(dirname(private$dataFile))
            if( basename(private$dataFile) %in% utils::changedFiles(private$snapshot,tmp)$changed ){
                private$readRast()
                if(private$clearCache){
                    ##TODO check this removes files in folder but not folder
                    unlink(file.path(private$cachePath,"*"))
                }
                ## TODO remove when tested
                cat("reloading")
            }
            invisible(NULL)
        },
        fTile = function(TileMatrix,Z,X,Y,w,h,...){
            
            ## check is tile is already generated
            if(!is.na(private$cachePath)){
                fn <- file.path(private$cachePath,TileMatrix,Z,X,paste0(Y,".png"))
                if(file.exists(fn)){ return( png::readPNG(fn) ) }
            }
            
            ## get wkt string for the desired projection
            wkt <- terra::crs(terra::rast(crs="EPSG:3857"))
            resample_flag <- terra::crs(private$rst)==wkt

            ## project/sample raster to correct shape
            step <- (20037508*2)/(2^Z)
            p <- terra::rast(nrows=h, ncols=w, nlyrs=1,
                             xmin=-20037508 + X*step,
                             xmax=-20037508 + (X+1)*step,
                             ymin=20037508 - (Y+1)*step,
                             ymax=20037508 - Y*step,
                             crs = "EPSG:3857"
                             )
            if( resample_flag ){
                pp <- terra::resample(private$rst[[TileMatrix]],p,...)
            }else{                   
                pp <- terra::project(private$rst[[TileMatrix]],p,...)
            }
            
            ## convert to numeric vector (in standard R order..
            v <- terra::as.matrix(pp,wide=TRUE)
            out <-  private$fColour(as.vector(v),TileMatrix) / 255
            out <- array(out,c(nrow(v),ncol(v),4))

            ## write out if cache and file does not exist
            if(!is.na(private$cachePath)){ png::writePNG(out,fn) }
            
            return(out)
        },
        ## #############################################
        ## overloadable functions
        ## #############################################        
        fExtract_default = function(pnt,TileMatrix){ terra::extract(private$rst[[TileMatrix]],pnt)[TileMatrix] }, ## default extraction function
        fExtract = NULL, ## function for plotting extracted data - can be overwritten
        fPlot_default= function(d,TileMatrix){barplot(unlist(d),names.arg=names(d))}, ## default function for plotting  extracted data
        fPlot = NULL,  ## function for plotting extracted data - can be overwritten
        fColour_default = function(x,TileMatrix){ # Default function for computing colours
            mn <- as.numeric(terra::global(private$rst[[TileMatrix]],min,na.rm=TRUE))
            mx <- as.numeric(terra::global(private$rst[[TileMatrix]],max,na.rm=TRUE))
            cp <- colorRamp(c("lightblue", "blue"),alpha=TRUE)
            out <- cp((x-mn)/(mx-mn))
            out[is.na(x),] <- 0 ## set NA to transparent
            out
        },
        fColour = NULL, ## function for computing colours - can be overwritten
        fLegend_default = function(TileMatrix){
            
            mn <- as.numeric(terra::global(private$rst[[TileMatrix]],min,na.rm=TRUE))
            mx <- as.numeric(terra::global(private$rst[[TileMatrix]],max,na.rm=TRUE))
            n <- 1000; scale <- (n-1)/(mx-mn); s <- seq(mn,mx,length=n-1)
            lut <- private$fColour(s,TileMatrix)
            lut <- rgb(lut[,1:3],alpha=lut[,4], maxColorValue=255)
            plot(c(0,10), c(mn,mx), type='n', bty='n', xaxt='n', xlab='',
                 yaxt='n', ylab='')
            axis(2, s[seq(1,n-1,length=10)], las=0.2)
            for (i in 1:(n-1)) {
                y = (i-1)/scale + mn
                rect(0,y,10,y+1/scale, col=lut[i], border=NA)
            }
        },
        fLegend =  NULL
    )
)

  
