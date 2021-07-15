#' Function for Generating a legend from a palette
#'
#' @param pal the Palette to plot
#' @param fileName location of the svg image legend
#'
#' @export
plotPalette <- function(pal,fileName="legend.svg"){

    fileName <- as.character(fileName)

    ## tidy up the palette
    pcol <- pal$col
    plbl <- paste(pal$brk)

    ## if first colour is transparent so drop
    if(substr(pcol[1],8,9) == "00"){
        pcol <- pcol[-1]; plbl <- plbl[-1]
    }

    ## last highest value if color is NA
    if(is.na(utils::tail(pcol,1))){
        pcol <- utils::head(pcol,-1)
        plbl <- utils::head(plbl,-1)
    }

    ## remove Inf or -Inf from label
    plbl <- gsub("[-Inf|Inf]","",plbl)

    nrct <- length(pcol) ## number of rectangles

    ## limits of the rectangles
    xl <- 1; xr <- 2 ## left and right limits of rectangles
    yb <- 1; yt <- 2 ## bottom and top limits of the rectanges

    ## start plot
    svglite::svglite(fileName,width=5,height=10)
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
    grDevices::dev.off()
    fileName
}
