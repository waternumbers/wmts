#' Function for Generating a legend from fcol
#'
#' @param fcol function to convert raster values into an hex colour scheme
#' @param lv values to label on the legend
#' @param nb  number of values to generate between the legend values
#' @param w width of plot
#' @param h height of plot
#'
#' @return String of class svg from svglite containing the svg image.
#' 
#' @details fcol should take a vector of n values and return an n hex colour values which should include transparency.
#'
#' @export
scaleImage <- function(fcol,lv,nb=0,w=5,h=10){


    lv <- sort(lv)
    if(length(lv)<2){ stop("lv must have a length of at least 2") }

    ## work out names and colours
    lbl <- paste(lv)
    pcol <- max(lv[1],-1e306)
    plbl <- c("",lbl[1])
    for(ii in 2:length(lv)){
        pcol <- c(pcol, seq(utils::tail(pcol,1),min(lv[ii],1e306),length=nb+2)[-1])
        plbl <- c(plbl,rep("",nb),lbl[ii])
    }
    pcol <- fcol(pcol)

    
    ## if first colour is transparent or NA
    flg <- TRUE
    while(flg){
        if(is.na(pcol[1]) | substr(pcol[1],8,9) == "00"){
            pcol <- pcol[-1]
            plbl <- plbl[-1]
        }else{
            flg <- FALSE
        }
        if(length(pcol)==0){ stop("No valid colours to plot") }
    }

    
    ## last highest value if color is NA or transparent
    flg <- TRUE
    while(flg){
        if(is.na(pcol[length(pcol)]) | substr(pcol[length(pcol)],8,9) == "00"){
            pcol <- utils::tail(pcol,-1)
            plbl <- utils::tail(plbl,-1)
        }else{
            flg <- FALSE
        }
        if(length(pcol)==0){ stop("No valid colours to plot") }
    }

    ## remove Inf or -Inf from label
    plbl <- gsub("[-Inf|Inf]","",plbl)

    nrct <- length(pcol) ## number of rectangles

    ## limits of the rectangles
    xl <- 1; xr <- 2 ## left and right limits of rectangles
    yb <- 1; yt <- 2 ## bottom and top limits of the rectanges

    ## start plot
    svglite::stringSVG({
        graphics::par(mar=c(0.05,0.05,0.05,5))
        plot(NA,type="n",ann=FALSE,xlim=c(xl,xr),ylim=c(yb,yt),
             xaxt="n",yaxt="n",bty="n")
        graphics::rect(
                      xl,
                      utils::head(seq(yb,yt,(yt-yb)/nrct),-1),
                      xr,
                      utils::tail(seq(yb,yt,(yt-yb)/nrct),-1),
                      col=pcol
                  )
        graphics::mtext(plbl,side=4,at=utils::head(seq(yb,yt,(yt-yb)/nrct),-1),las=2,cex=1.7)
    },width=w,height=h)
}
