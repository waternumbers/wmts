#' Start a serving serving tiles from a raster data set with time attribute
#' 
#' @param dataFile file containing spatial data to be read by \code{terra::rast}
#' @param fcol function for returning colours
#' @param lv values for drawing in legend
#' @param cachePath path to directory for storing cached files
#' @param staticPath path for serving static assets from
#' @param fplot function to plot a timeseries of data (see details)
#' @param ferror function to process errors and return messages (see details)
#' @param port port to serve data on
#' @param clearCache logical, should cache be cleared if file changes
#' 
#' @details For a decription of fcol and lv see \code{scaleImage}. fplot shoudl take as its first input a POSIXct vector and as for its second a numeric vector of the same length and return a plot. ferror creates the response body contents from the standard R err object.
#' @export
serveTiles <- function(dataFile,fcol,lv=NULL,cachePath=NULL,staticPath=NULL,
                       fplot=function(t,v){plot(t,v)},
                       ferror=function(err){err$message},
                       port=NULL,
                       clearCache=FALSE){

    ## set flags
    flgs <- c(hasCache = !is.null(cachePath),
              hasStatic = !is.null(staticPath),
              hasLegend = !is.null(lv))
    
    ## open data file and take snapshot
    dataFile <- normalizePath(dataFile)
    rst <- terra::rast(normalizePath(dataFile))
    snapshot <- utils::fileSnapshot(dirname(dataFile))

    ## deal with cache
    if(flgs["cachePath"]){
        cachePath <- normalizePath(cachePath)
        if(!dir.exists(cachePath)){ dir.create(cachePath,recursive=TRUE) }
    }
    clearCache <- clearCache & flgs["cachePath"]
    
    ## #####################################################
    ## helper functions
    
    isChanged <- function(){
        tmp <- utils::fileSnapshot(dirname(dataFile))
        if( basename(dataFile) %in% utils::changedFiles(snapshot,tmp)$changed ){
            snapshot <<- tmp
            rst <<- terra::rast(dataFile)
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
    
    ## provide times
    times_handler <- function(){
        ## check for changes
        isChanged()
        sort( format(terra::time(rst),"%Y-%m-%dT%H:%M:%SZ",tz="GMT") )
    }
    
    ## extract data as json - time series
    json_handler <- function(lon,lat){
        ## check if changed
        isChanged()

        pnt <- terra::vect( matrix(c(lon,lat),1,2), crs="EPSG:3857" )
        pnt <- terra::project(pnt,terra::crs(rst))
        
        ts <- format(terra::time(rst),"%Y-%m-%dT%H:%M:%SZ",tz="GMT")
        tv <- unlist( terra::extract(rst,pnt) )
        idx <- !is.na(ts)
        out <- data.frame(dateTime = unname(ts[idx]),
                          value = unname(tv[idx])                      
                          )
        out[order(out$dateTime),]
    }

    
    ## plot data
    plot_handler <- function(lon,lat){
        ## check if changed
        isChanged()

        pnt <- terra::vect( matrix(c(lon,lat),1,2), crs="EPSG:3857" )
        pnt <- terra::project(pnt,terra::crs(rst))
        
        ts <- terra::time(rst)
        tv <- unlist(terra::extract(rst,pnt))
        idx <- !is.na(as.character(ts)) ## for some reason is.na returns FALSE for NA values!
        fplot(ts[idx],tv[idx])
    }

    ## return tile as raw vector
    tile_handler <- function(res,time,X,Y,Z){
        ## check if changed
        isChanged()

        ## check time is valid
        ts <- format(terra::time(rst),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
        if(!(time %in% ts)){
            stop( sprintf("Time stamp %s not valid or missing",time) )
        }else{
            ts <- which(ts==time)
        }
        
        hasFile <- FALSE
        if(flgs["hasCache"]){
            fn <- file.path(cachePath,time,X,Y,paste0(Z,".png"))
            hasFile <- file.exists(fn)
        }
        if(hasFile){
            out <- readBin(fn,'raw',n = file.info(fn)$size)
        }else{
            out <- singleTile(rst[[ts]],fcol,Z,X,Y)
            if(flgs["hasCache"]){
                writeBin(out,fn)
            }
        }
        out
    }

    ## legend handler
    legend_handler <- function(res){
        hasFile <- FALSE
        if(flgs["hasCache"]){
            fn <- file.path(cachePath,"legend.svg")
            hasFile <- file.exists(fn)
        }
        if(hasFile){
            out <- readLines(fn)
        }else{
            out <- scaleImage(fcol,lv)
            if(flgs["hasCache"]){ writeLines(out,fn) }
        }
        out
    }

    ## error handler
    error_handler <- function(req, res, err){
        res$body <- ferror(err)
        res$status <- 400
        res
    }

    


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
        plumber::pr_get(path = "/times",
               handler = times_handler,
               serializer = plumber::serializer_unboxed_json(),
               comments = "Times for which tiles are available"
               ) %>%
        plumber::pr_get(path = "/json/<lon:double>/<lat:double>",
               handler = json_handler,
               serializer = plumber::serializer_unboxed_json(),
               comments = "Precipitation for the given location in mm"
               ) %>%
        plumber::pr_get(path = "/plot/<lon:double>/<lat:double>",
               handler = plot_handler,
               serializer = plumber::serializer_svg(width=10,height=8),
               comments = "Plot of precipitation for the given location"
               ) %>%
        plumber::pr_get(path = "/tiles/<time>/<X:int>/<Y:int>/<Z:int>",
               handler = tile_handler,
               serializer = plumber::serializer_content_type("image/png"),
               comments = "Returns tile as png image"
               ) %>%
        plumber::pr_static("/__docs__","./__docs__/") %>%
        plumber::pr_set_error(error_handler)

    if(flgs["hasLegend"]){
        root %>%
            plumber::pr_get(path = "/legend",
                   handler = legend_handler,
                   comments = "Returns legend as svg"
                   )
    }
    if(flgs["hasStatic"]){
        root %>% plumber::pr_static("/", normalizePath(staticPath) )
    }

    root %>% plumber::pr_run(port=port)
}
