#' R6 Class for wmts
#' @export
wmtsServer <- R6Class(
    "wmtsServer",
    public = list(      
        #' @description Creates a wmtsServer object from a wmts object
        #'
        #' @param wmts a wmts object
        #' @param port=NULL port to serve the data on
        #'
        #' @return invisible(self) suitable for chaining
        #'
        initialize = function(wmts,capabilities_template){
            if(!("wmts" %in% class(wmts))){
                stop("Please provide a wmts object")
            }
            private$wmts <- wmts
            private$capabilities_template <- paste(readLines(capabilities_template),collapse="\n")
            self$overload(NA,"fError")
            self$overload(NA,"fCORS")
            self$overload(NA,"fGenerateCapabilities")
            invisible(self)
        },
        #' @description Shows, or sets the staticPath value
        #'
        #' @param x see details
        #'
        #' @details If \code{x=NULL} the current value is returned. If \code{x=NA} then no path is set and caching does not occur (the default setup).
        #' If x can be interpreded as a character vector then the path is set (subject to it existing)
        #'
        #' @return the value taken by cachePath at the end of the call
        static = function(x=NULL){
            if( !is.null(x) ){ return( private$staticPath ) }
            x <- as.character(x[1])
            if( is.na(x) ){ x <- NA } ## default value
            else{
                x <- file.path(x)
                if( !dir.exists(x) ){ stop("path for static files does not exist") }
            }
            private$staticPath <- x
            invisible(self)
        },
        #' @description Start the server
        #'
        #' @param ... passed to plumber::pr_run
        #'
        start = function(...){
            root <- plumber::pr()
            root %>%
                plumber::pr_filter("CORS", private$fCORS) %>%
                plumber::pr_get(path = "/GetLayers",
                                handler = private$layers_handler,
                                serializer = plumber::serializer_unboxed_json(),
                                comments = "Returns the capabilities of the server (json)"
                                ) %>%
                plumber::pr_get(path = "/GetCapabilities",
                                handler = private$capabilities_handler,
                                serializer = plumber::serializer_content_type(
                                                          "application/xml",as.character),
##                                                          xml2::as_xml_document),
                                comments = "Returns the capabilities of the server (xml)"
                                ) %>%
                plumber::pr_get(path = "/<Layer>/<Z:int>/<X:int>/<Y:int>.png",
                        handler = private$tile_handler,
                        serializer = plumber::serializer_content_type("image/png",
                                                                      png::writePNG),
                        comments = "Returns tile as png image"
                        ) %>%
                plumber::pr_get(path = "/<Layer>/<Z:int>/<X:int>/<Y:int>.jpeg",
                        handler = private$tile_handler,
                        serializer = plumber::serializer_content_type("image/png",
                                                                      jpeg::writeJPEG),
                        comments = "Returns tile as jpeg image"
                        ) %>%
                plumber::pr_get(path = "/<Layer>/<lon:double>/<lat:double>.json",
                                handler = private$feature_handler,
                                serializer = plumber::serializer_unboxed_json(),
                                comments = "Data for given longitude and latitude"
                                ) %>%
                plumber::pr_get(path = "/<Layer>/<lon:double>/<lat:double>.png",
                                handler = private$feature_plot_handler,
                                serializer = plumber::serializer_png(),
                                comments = "Plotted data for given longitude and latitude"
                                ) %>%
                plumber::pr_static("/__docs__","./__docs__/") %>%
                plumber::pr_run(...)
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
        #' fError
        #' Input: an R error
        #' Output: Message to display
        #'
        #' @return Either the current value or invisible(self)
        overload = function(f=NULL,name=c("fError","fCORS","fGenerateCapabilities")){
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
        ## ###############################
        ## variable storage
        ## ###############################
        wmts = NULL,
        staticPath = NA,
        capabilities_template = NA,
        ## ###############################
        ## routing functions
        ## ###############################
        capabilities_handler = function(req){
##            browser()
            private$fGenerateCapabilities(paste0("http://",req$HTTP_HOST),private$wmts$Layer())
        },
        layers_handler = function(){
            list(Layer = private$wmts$Layer(),
                 projection = "ESPG:3857")
        },
        tile_handler = function(Layer,Z,X,Y){
            private$wmts$tile(Layer,Z,X,Y)
        },
        feature_handler = function(Layer,lon,lat){
            private$wmts$feature(Layer,lon,lat)
        },
        feature_plot_handler = function(Layer,lon,lat){
            private$wmts$feature(Layer,lon,lat,plot=TRUE)
        },
        ## ###############################
        ## overloadable functions
        ## ###############################
        fError_default = function(err){err$message}, ## default function for error handling
        fError = NULL,  ## function for error handling - can be overwritten
        fCORS_default = function(req,res){
            res$setHeader("Access-Control-Allow-Origin", "*")
            if (req$REQUEST_METHOD == "OPTIONS") {
                res$setHeader("Access-Control-Allow-Methods","*")
                res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
                res$status <- 200
                return(list())
            } else {
                plumber::forward()
            }
        },
        fCORS=NULL,
        fGenerateCapabilities_default = function(url,layers){
            out <- private$capabilities_template
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
        },
        fGenerateCapabilities=NULL
    )
)
    
