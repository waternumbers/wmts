#' Generate a colour palette for the wmts
#'
#' @param brk list of break values between classes, including the upper and lower limits
#' @param crp a colorRampPalette used for generating the colors for each class
#' @param lowerTransparent logical - should the lowest class be transparent
#'
#' @details Generates a data frame of breaks and Hex colour codes. Values above the upper limit are treated as having colour NA
#' @export
genPalette <- function(brk,crp,lowerTransparent=TRUE){
    brk <- sort(unique(brk))

    if( lowerTransparent ){
        col <- c("#00000000",crp(length(brk)-2),NA)
    }else{
        col <- c(crp(length(brk)-1),NA)
    }
    
    data.frame(brk = as.numeric(brk),
               col = col)
    
}
