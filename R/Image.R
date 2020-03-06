#' An Alternative Image Plotting Function
#' 
#' Image is a modified version of the \code{\link[graphics]{image}} function 
#' that behaves more like Matlab imagesc.
#' 
#' 
#' Image behaves like \code{\link[graphics]{image}}, with the following 
#' differences:
#' \itemize{
#'   \item The z argument may be a data.frame with numeric columns; it will be
#'     automatically converted to a matrix.
#'   \item The data matrix z is transposed, so that columns run along the x-axis 
#'     and rows along the y-axis.
#'   \item The direction of the y-axis is reversed, so that zero is at the top.
#'   \item If omitted, the x and y arguments are assumed from the size of z, 
#'      i.e.,
#'      
#'          \code{   x <- 0.5 + (0:ncol(z))}
#'          
#'          \code{   y <- 0.5 + (0:nrow(z))}
#'          
#'       If supplied, the x and y arguments must be explicitly named.
#'   \item If a zlim argument is supplied, values outside the zlim range are 
#'     replaced with the appropriate limit (floor or ceiling values to zlim).
#'   \item You may annotate the axes using the xaxis and yaxis list arguments 
#' }
#'   
#' This behavior should be familiar to users of Matlab's imagesc function.
#'
#' @param z
#'   A numeric matrix or data.frame containing image data.
#' @param xaxis,yaxis 
#'   Lists of arguments to the \code{\link[graphics]{axis}}. All arguments 
#'   should be explicitly named.  The "side" argument may be omitted. 
#'   An example:
#'   
#'   \code{   xaxis=list(side=1, at=1:4, labels=LETTERS[1:4], cex.axis=0.8)}
#' @param NA.color
#'   Color for missing values.  Set to NA for transparency (so the color 
#'   specified by \code{\link[graphics]{par}}(bg="") will show through.  Note
#'   that this is accomplished by plotting a rectangle over the whole plot of 
#'   the specified color, then replotting the image, so if you are adding the 
#'   image to an existing plot, you may want to set this argument to NA
#' @param ... 
#'   Other arguments passed to \code{\link[graphics]{image}}.
#'
#' @seealso \code{\link[graphics]{image}}, \code{\link[graphics]{par}}.
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
#' @importFrom utils data 
#' @import graphics
#' 
Image <-
function(z,xaxis,yaxis,NA.color="gray",...){
   arglist <- list(...)
   if(is.data.frame(z)) z <- as.matrix(z)
   if(!any(names(arglist)=="x")) arglist$x <- 0.5 + (0:ncol(z))
   if(!any(names(arglist)=="y")) arglist$y <- 0.5 + (0:nrow(z))
   # reverse the order of the yaxis unless the ylim axis was passed in
   if(!any(names(arglist)=="ylim")) arglist$ylim <- rev(range(arglist$y))
   # plot the axes separately if xaxis or yaxis arguments passed in
   if(!missing(yaxis)) arglist$yaxt <- "n"
   if(!missing(xaxis)) arglist$xaxt <- "n"
   if(is.array(z))
      z <- matrix(z,nrow=nrow(z),ncol=ncol(z))
   # transpose z
   arglist$z <- t(z)
   # do not label the x- and y-axes by default
   if(!"xlab"%in%names(arglist)){
      if(is.null(names(dimnames(data))[2])) arglist$xlab <- ""
      else arglist$xlab <- names(dimnames(data))[2]
   }
   if(!"ylab"%in%names(arglist)){
      if(is.null(names(dimnames(data))[1])) arglist$ylab <- ""
      else arglist$ylab <- names(dimnames(data))[1]
   }
   # if a zlim argument was supplied, floor and ceiling the z matrix
   if(any(names(arglist)=="zlim")){
      arglist$z[arglist$z<arglist$zlim[1]] <- arglist$zlim[1] 
      arglist$z[arglist$z>arglist$zlim[2]] <- arglist$zlim[2] 
   }
   # plot the image
   do.call(image,args=arglist)
   if(any(is.na(z) & !is.na(NA.color))){
      # if there are missing values and a color is specified, draw bg rectangle
      rect.args <- as.list(structure(names=c("xleft","xright","ybottom","ytop"),
         par("usr")))
      do.call(rect,c(rect.args,list(col=NA.color,border=NA)))
      # ... then replot the image
      arglist$add <- TRUE
      do.call(image,args=arglist)
   }
   # add the x- and y-axis annotations if xaxis or yaxis arguments passed in
   if(!missing(xaxis)){
      # permit the x-axis to be plotted on the top if explicitly requested
      if(!any(names(xaxis)=="side") || xaxis$side!=3) 
         xaxis$side <- 1 # ...otherwise, put it on the bottom
      if(any(names(arglist)=="cex.axis") & !any(names(xaxis)=="cex.axis"))
         xaxis$cex.axis <- arglist$cex.axis
      do.call(axis,args=xaxis)
   }
   if(!missing(yaxis)){
      # permit the y-axis to be plotted on the right if explicitly requested
      if(!any(names(yaxis)=="side") || yaxis$side!=4) 
         yaxis$side <- 2 # ...otherwise, put it on the left
      if(any(names(arglist)=="cex.axis") & !any(names(yaxis)=="cex.axis"))
         yaxis$cex.axis <- arglist$cex.axis
      do.call(axis,args=yaxis)
   }
   # enclose in a box
   enclose.plot(lwd=0.1)
}
