#' Draw Edge Around A Plot.
#' 
#' 
#' Enclose a plot with lines at the axes; useful for the barplot function.
#'
#' @param ... 
#'   arguments to be passed to \code{\link[graphics]{abline}}()-- col, lwd, 
#'   lty, etc.
#
#' @return
#'   Returns a named numeric vector with left, right, bottom and top limits of 
#'   the current plot; same as returned by par("usr"), but with named elements
#'   
#' @importFrom graphics par axis
#' @export
enclose.plot <-
function(...){
   lims <- structure(par("usr"),names=c("left","right","bottom","top"))
   axis(1,lims[1:2],tcl=0,labels=FALSE,...)
   axis(3,lims[1:2],tcl=0,labels=FALSE,...)
   axis(2,lims[3:4],tcl=0,labels=FALSE,...)
   axis(4,lims[3:4],tcl=0,labels=FALSE,...)
   invisible(lims)
   
}
#-------------------------------------------------------------------------------

