#' Plot a Circle or Regular Polygon
#' 
#' Plot a circle or regular polygon of a given radius at a given location.
#' 
#' @param x
#'   A numeric vector of length 1 or 2:
#'   \itemize{
#'      \item If length(x)==1: origin is assumed to be c(0,0); x-coordinate of 
#'         a radial segment.
#'      \item If length(x)==2: x-coordinates of origin (x[1]) and radius 
#'        endpoint.
#'   }
#' @param y 
#'    Optional numeric vector of length 1 or 2:
#'   \itemize{
#'      \item If omitted: assume y-coordinates of zero.
#'      \item If length(y)==1: origin is assumed to be c(0,0); y-coordinate of 
#'        a radial segment.
#'      \item If length(y)==2: y-coordinates of origin (x[1]) and radius 
#'        endpoint.
#'   }
#' @param add 
#'    Logical value indicating whether circle should be added to an
#'    existing plot (default) or to a new plot.
#' @param segs 
#'    Integer; number of segments used to approximate a circle.  A small number
#'    will produce a regular polygon inscribed within the radius, with a vertex
#'    at x, y
#' @param how 
#'    Character string specifying how x and y relate to the circle geometry.  
#'    Currently only radius is implemented.
#' @param ... 
#'    Other named arguments passed to \code{\link[graphics]{lines}} or 
#'    \code{\link[graphics]{plot}} (such as lwd, lty, col, etc.)
#'    
#' @return
#' Returns a data.frame with columns x and y specifying coordinates of 
#' the vertices.
#' 
#' @seealso 
#'   \code{\link[graphics]{par}} and \code{\link[graphics]{plot.default}} give
#'   more detailed descriptions of the ... plotting parameters.
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @import graphics
#' @export
circle <- 
function(x, y=0, add=T, segs=100, how="radius", ...){
   if(how=="radius" & length(x)==1 & length(y)==1){
      x <- c(0, x)
      y <- c(0, y)
   }
   if(length(x)!=2 | length(y)!=2){
      stop("x and y arguments must either both be length 1 or both 2")
   }
   r <- sqrt(diff(x)^2 + diff(y)^2)
   #theta0 <- atan(diff(y)/diff(x))
   theta0 <- acos(diff(x)/r)
   pi <- 2*asin(1)
   theta <- theta0 + seq(0, 2*pi, len=segs+1)
   x <- x[1] + r*cos(theta)
   y <- y[1] + r*sin(theta)
   if(add){
      lines(x, y, ...)
   }else{
      plot(x, y, type="l", ...)
   }
   invisible(data.frame(x=x, y=y))
}
