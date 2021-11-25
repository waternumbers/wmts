#' Start a serving serving tiles from a raster data set with time attribute
#' 
#' @param dataFile file containing spatial data to be read by \code{terra::rast}
#' @param fcol function translating raster values to a colour (in hex format) including transparency
#' @param flegend function for generating a legend as an svg string (see details)
#' @param fextract function to extract the data for feature requests (see details)
#' @param fplot function to plot data extracted for feature requests (see details)
#' @param cachePath path to directory for storing cached files
#' @param staticPath path for serving static assets from
#' @param fextract function to extract the data for feature requests (see details)
#' @param fplot function to plot data extracted for feature requests (see details)
#' @param ferror function to process errors and return messages (see details)
#' @param port port to serve data on
#' @param clearCache logical, should cache be cleared if file changes
#' @param resolution pixel size of tiles
#' @param minZ minimum zoom level supported
#' @param maxZ maximum zoom level supported
#' 
#' @details For a decription of fcol see \code{scaleImage}. flegend should return the legend image as an svg string. If set to NULL then the legend route is removed. fextract shoudl return one or more values for the given point. If a json format GetFeature request is made then this is serialised to the output. In a svg format GetFeature request is made then the output of fextract is passed to fplot. If fplot is NULL then the svg GetFeature request is not available. If fextract is NULL then no GetFeature requests are available. ferror generates the error message from the standard R err object. If set to NULL the default plumber routing is used.
#' @export
serveTiles <- function(dataFile,fcol,
                       flegend = function(rst,fcol,TileMatrix){ scaleImage(fcol,seq(as.numeric(terra::global(rst[[TileMatrix]],min,na.rm=TRUE)),as.numeric(terra::global(rst[[TileMatrix]],max,na.rm=TRUE)),length=10)) },
                       fextract = function(rst,pnt,TileMatrix){ terra::extract(rst[[TileMatrix]],pnt)[TileMatrix] },
                       fplot = function(d){barplot(unlist(d),names.arg=names(d))},
                       ferror = function(err){err$message},
                       cachePath=NULL,staticPath=NULL,
                       port = NULL,
                       clearCache = FALSE,
                       resolution = 256,
                       minZ = 0,
                       maxZ = 4
                       ){
    
    
    
    ## #####################################################
    ## helper functions
    readRast <- function(){
        rst <<- terra::rast(dataFile)
        if("POSIXt" %in% class(terra::time(rst))){
            names(rst) <- format(terra::time(rst),"%Y-%m-%dT%H:%M:%SZ",tz="GMT")
        }
        invisible(NULL)
    }
    
    
    isChanged <- function(){
        tmp <- utils::fileSnapshot(dirname(dataFile))
        if( basename(dataFile) %in% utils::changedFiles(snapshot,tmp)$changed ){
            snapshot <<- tmp
            rst <<- readRast()
            if(clearCache){
                ##TODO check this removes files in folder but not folder
                unlink(file.path(cachePath,"*"))
            }
            cat("reloading")
        }
        invisible(NULL)
    }
    
    ## #####################################################
    ## route functions
    
    ## extract data as json for get feature requests
    json_handler <- function(TileMatrix,lon,lat){
        ## check if changed
        isChanged()

        pnt <- terra::vect( matrix(c(lon,lat),1,2), crs="EPSG:4326" )
        pnt <- terra::project(pnt,terra::crs(rst))
        
        out <- fextract(rst,pnt,TileMatrix)
    }
    
    ## plot data for get feature requests
    plot_handler <- function(TileMatrix,lon,lat){
        ## check if changed
        isChanged()
        
        pnt <- terra::vect( matrix(c(lon,lat),1,2), crs="EPSG:4326" )
        pnt <- terra::project(pnt,terra::crs(rst))
        
        d <- fextract(rst,pnt,TileMatrix)
        if(any(is.finite(unlist(d)))){
            fplot(d)
        }else{
            NULL
        }
    }
    
    ## return tile as raw vector
    tile_handler <- function(res,TileMatrix,Z,X,Y){
        ## check if changed
        isChanged()
        browser()
        ## check time is valid TileMatrix
        if(!(TileMatrix %in% names(rst))){
            res$status <- 404 # Unauthorized
            return(list(error= paste0("TileMatrix ", TileMatrix, " is not present")))
        }

        ## check zoom is valid
        if( Z<minZ | Z>maxZ ){
            res$status <- 404 # Unauthorized
            return(list(error= paste0("Only zooms between ",minZ," and ",maxZ," permitted")))
        }
        
        hasFile <- FALSE
        if(flgs[["hasCache"]]){
            fn <- file.path(cachePath,TileMatrix,Z,X,paste0(Y,".png"))
            hasFile <- file.exists(fn)
        }
        if(hasFile){
            out <- readBin(fn,'raw',n = file.info(fn)$size)
        }else{
            out <- singleTile(rst[[TileMatrix]],fcol,Z,X,Y,w=resolution)
            if(flgs[["hasCache"]]){
                writeBin(out,fn)
            }
        }
        out
    }
    
    ## legend handler
    legend_handler <- function(TileMatrix){
        
        hasFile <- FALSE
        if(flgs[["hasCache"]]){
            fn <- file.path(cachePath,TileMatrix,"legend.svg")
            hasFile <- file.exists(fn)
        }
        if(hasFile){
            out <- readLines(fn)
        }else{
            out <- flegend(rst,fcol,TileMatrix)
            if(flgs[["hasCache"]]){ writeLines(out,fn) }
        }
        out
    }

    ## error handler
    error_handler <- function(req, res, err){
        res$body <- ferror(err)
        res$status <- 400
        res
    }

    ## json capabilites handler
    capabilities_handler_json <- function(){
        
        list(TileMatrix = names(rst),
             projection = "ESPG:3857",
             hasLegend = flgs[["hasLegend"]],
             hasStaticFiles = flgs[["hasStatic"]],
             GetFeature = names(flgs[["hasGetFeature"]])[flgs[["hasGetFeature"]]],
             resolution = resolution,
             zoom = list(minZ = minZ,maxZ = maxZ)
             )
    }

    ## #############################################################################
    ## processing inputs to function
    ## set flags
    flgs <- list(hasCache = !is.null(cachePath),
                 hasStatic = !is.null(staticPath),
                 hasLegend = is.function(flegend),
                 hasGetFeature = c(json =is.function(fextract), svg=is.function(fplot)),
                 hasError = is.function(ferror)
                 )
    
    ## open data file and take snapshot
    dataFile <- normalizePath(dataFile)
    rst <- NULL # initialise
    readRast() # creates rst object and set TileMatrix names
    snapshot <- utils::fileSnapshot(dirname(dataFile))

    
    
    ## deal with cache
    if(flgs[["hasCache"]]){
        cachePath <- normalizePath(cachePath)
        if(!dir.exists(cachePath)){ dir.create(cachePath,recursive=TRUE) }
    }
    clearCache <- clearCache & flgs[["hasCache"]]

    ## ############################################################################
    ## routes
    ## ############################################################################
    root <- plumber::pr()
    root %>%
        plumber::pr_filter("CORS", function(req,res){
            res$setHeader("Access-Control-Allow-Origin", "*")
            if (req$REQUEST_METHOD == "OPTIONS") {
                res$setHeader("Access-Control-Allow-Methods","*")
                res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
                res$status <- 200
                return(list())
            } else {
                plumber::forward()
            }
        }) %>%
        plumber::pr_get(path = "/GetCapabilities.json",
                        handler = capabilities_handler_json,
                        serializer = plumber::serializer_unboxed_json(),
                        comments = "Returns the capabilities of the server"
                        ) %>%
        plumber::pr_get(path = "/<TileMatrix>/<Z:int>/<X:int>/<Y:int>.png",
                        handler = tile_handler,
                        serializer = plumber::serializer_content_type("image/png"),
                        comments = "Returns tile as png image"
                        ) %>%
        plumber::pr_static("/__docs__","./__docs__/")

    ## Add GetFeature if required
    if( flgs[["hasGetFeature"]]["json"] ){
        root %>%
            plumber::pr_get(path = "/<TileMatrix>/<lon:double>/<lat:double>.json",
                            handler = json_handler,
                            serializer = plumber::serializer_unboxed_json(),
                            comments = "Data for given longitude and latitude"
                            )
        if( flgs[["hasGetFeature"]]["svg"] ){
            root %>%
                plumber::pr_get(path = "/<TileMatrix>/<lon:double>/<lat:double>.svg",
                                handler = plot_handler,
                                serializer = plumber::serializer_svg(width=10,height=8),
                                comments = "SVG plot of data for given longitude and latitude"
                                )
        }
    }

    ## add static
    if(flgs[["hasLegend"]]){
        root %>%
            plumber::pr_get(path = "/<TileMatrix>/legend.svg",
                            handler = legend_handler,
                            serializer = plumber::serializer_content_type("image/svg+xml"),
                            comments = "Returns legend as svg")
    }
    ## add static
    if(flgs[["hasStatic"]]){
        root %>% plumber::pr_static("/", normalizePath(staticPath) )
    }
    ## add no standard error handling
    if( flgs[["hasError"]] ){
        root %>%
            plumber::pr_set_error(error_handler)
    }

    ## And run it!
    root %>% plumber::pr_run(port=port)
}
