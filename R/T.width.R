#' Determine Width of Caps On Error Bars
#' 
#'  
#' T.width returns a fraction of the range of the current plot
#' 
#' This is meant to be useful for determining the width of the T's at the end 
#' of error bars.
#
#' @param fraction 
#'   A number between zero and one.
#' @param axis
#'   A number specifying which axis to return the fraction of, 1 for x or 
#'   2 for y
#
#' @return
#'   Returns a numeric vector with the requested fraction of the current plot's 
#'   actual x-axis limits from \code{\link[graphics]{par}}("usr"). 
#'   
#' @importFrom graphics par
#' @export
T.width <- 
function(fraction=0.01,axis=1){
   if(axis==2){
      t <- fraction*diff(par("usr")[3:4])/2
   }else t <- fraction*diff(par("usr")[1:2])/2
   return(t)
}
