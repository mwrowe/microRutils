#' Generate and Plot a 2D-Histogram as a Heatmap
#' 
#' plot2Dhist generates a two-dimensional histogram image.
#' 
#' This function calculates a histogram of the join distribution of two 
#' variables, and plots the resulting matrix as a heatmap with the first 
#' variable along the x axis an the second along the y, with values increasing
#' from the lower left corner.  The axes are presented in the units of the 
#' input data (i.e., not bin numbers, but their values.)
#'
#' @param xdata,ydata
#'    Numeric vectors or matrices of the same size, with data to be
#'    plotted as a 2D-histogram.  Data may also be passed in with xdata only, 
#'    as a two-column array, data.frame or matrix with numeric values, but in
#'    this case, \strong{ALL SUBSEQUENT ARGUMENTS MUST BE NAMED.}
#' @param breaks,xbreaks,ybreaks
#'    Same as breaks argument to \code{\link[graphics]{hist}}() function.  If
#'    breaks is supplied, xbreaks and ybreaks are ignored and the same breaks
#'    are applied to each axis.  Otherwise, xbreaks and ybreaks may be 
#'    specified separately.  If omitted, breaks are determined for each 
#'    variable as per the hist() function.
#' @param base
#'    Number specifying base for the z-axis.  Default of 1 plots the 
#'    intensities in each bin, untransformed.  If a value greater than one is
#'    supplied, the z-axis is transformed as log(intensities+1, base).
#' @param col
#'    Vector of colors specifying colormap to use.  See, for example,
#'    \code{\link{hot}}.
#' @param xlab,ylab
#'    Labels for the x- and y-axes.  If omitted, the names of the 
#'    variables used for the xdata and ydata arguments will be used.
#' @param plot.zero
#'    Value to plot zero count bins. Use NA to make those cells with 
#'    zero counts behave as NA's (i.e. having no color; see help for 
#'    \code{\link[graphics]{image}}).
#' @param ...
#'    Additional arguments passed to \code{\link[graphics]{image}}
#'    
#' @return
#'    Returns a list object with named elements:
#'    \itemize{
#'      \item \strong{xbreaks, ybreaks} 
#'         The breaks specifying binning of x- and y-data.
#'      \item \strong{xmids, ymids} Midpoints of the bins.
#'      \item \strong{intensities} Matrix with number of counts in each bin.
#'      \item \strong{zdata} Matrix of z-axis data as plotted (i.e.,  
#'         log transformed if base!=1).
#'      \item \strong{xdata, ydata, base} Same as input arguments.
#'    }
#
#' @seealso \code{\link[graphics]{hist}}, \code{\link[graphics]{image}}.
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
#' @importFrom methods is
#' @importFrom grDevices rainbow
#' @import graphics
plot2Dhist <- 
function(xdata, ydata, breaks=NULL, xbreaks=NULL, ybreaks=NULL, base=1, col,
         xlab=NULL, ylab=NULL, plot.zero=0, ...){
   if(is(xdata)[1]%in%c("matrix","array","data.frame") && dim(xdata)[2]==2){
      # assume that if no breaks argument was found, 2nd arg is really breaks
      if(missing(breaks) & !missing(ydata) && is.numeric(ydata)) breaks <- ydata 
      # if not passed in, use column names for the axes
      if(is.null(xlab)) xlab <- colnames(xdata)[1]
      if(is.null(ylab)) ylab <- colnames(xdata)[2]
      ydata <- xdata[,2]
      xdata <- xdata[,1]
   }else{
      # if not passed in, use variable names for the axes
      if(is.null(xlab)) 
         xlab <- deparse(substitute(xdata))
      if(is.null(ylab)) 
         ylab <- deparse(substitute(ydata))
   }
   if(missing(col)) col=rainbow(100)
   # determine the binning for the x- and y-data
   if(!is.null(breaks)){
      xbreaks <- breaks
      ybreaks <- breaks
   }
   # if breaks specified as a vector, remove out of range values
   if(length(xbreaks)>1){
      xdata[xdata<=xbreaks[1]] <- NA
      xdata[xdata>=xbreaks[length(xbreaks)]] <- NA
   }
   if(length(ybreaks)>1){
      ydata[ydata<=ybreaks[1]] <- NA
      ydata[ydata>=ybreaks[length(ybreaks)]] <- NA
   }
   # histogram each variable's data (do not actually plot)
   if(is.null(xbreaks)) xhist <- hist(xdata,plot=F)
   else xhist <- hist(xdata,breaks=xbreaks,plot=F)
   if(is.null(ybreaks)) yhist <- hist(ydata,plot=F)
   else yhist <- hist(ydata,breaks=ybreaks,plot=F)
   # bin x- and y-data
   xbin <- cut(xdata,breaks=xhist$breaks)
   ybin <- cut(ydata,breaks=yhist$breaks)
   hist2d <- table(xbin,ybin) 
    if(base==1) to.plot <- hist2d
   else to.plot <- log(hist2d+1,base)
   #EJM set zero bin values to plot.zero
   to.plot[which(to.plot==0)]<- plot.zero
   image(xhist$mids,yhist$mids,to.plot,col=col,xlab=xlab,ylab=ylab,...)
   box("plot")
   hist2D <- list(xbreaks=xhist$breaks,xmids=xhist$mids,
      ybreaks=yhist$breaks,ymids=yhist$mids,intensities=hist2d,
      zdata=to.plot,xdata=xdata,ydata=ydata,base=base)
} 
