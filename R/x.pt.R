#' Get the Value Associated With A Fraction of X- or Y-Axis
#'
#' Returns the coordinate of a point that is a given fraction between the actual
#' axis limits of the current plot, in the units of that axis' data.
#' 
#' @param fraction
#'    Numeric values between zero and one specifying distance from the left (x)
#'    or bottom (y) limit of the current plot relative to its range.
#' 
#' @return 
#'    Returns numeric value of specified location in the units of the axis.
#'    
#' @importFrom graphics par
#' @export
x.pt <-
function(fraction){
   return(par("usr")[1] + fraction*diff(par("usr")[1:2]))
}
#' @rdname x.pt
#' @export
y.pt <- function(fraction){
  return(par("usr")[3] + fraction*diff(par("usr")[3:4]))
}
