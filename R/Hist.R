#' An Alternative Histogram Function
#' 
#' Hist generates a histogram with better (IMNSHO) defaults than the 
#' \code{\link[graphics]{hist}}() function.
#'
#' The behavior of Hist() is very similar to \code{\link[graphics]{hist}}(),
#' but with the following differences:
#' \itemize{
#'   \item Values may be outside the range of the breaks specified (bound 
#'     argument determines how they are handled).
#'   \item By default, the bars are gray.
#'   \item The axes extend to the limits of the plot, and are not displaced 
#'     from the plot.
#'   \item Permits you to plot the log counts via the ylog argument.
#'   \item  The optional "add" argument, if true, prevents labels or axes from 
#'     being drawn.  To make this work, you should pass 
#'     \code{breaks=this.hist$breaks} from the previous call in order to use 
#'     the same bins.
#'   \item If the optional argument horiz=TRUE, the bars will be plotted 
#'     horizontally.  The y-axis will then be treated as the x- and vice versa
#'     with regards to labeling, limits, etc. (i.e., xlabel will be used to 
#'     label the y-axis).  Note that the names of the list returned are also 
#'     "flipped", so that this.hist$ylim is really the y-axis limits, etc.
#' }
#'
#' @param x
#'   Numeric vector of values to be histogramed.
#' @param ylog
#'   Logical or numeric, controlling whether y-axis is logged.  If TRUE,
#'   \code{log(counts+1, 10)} is plotted; use integer to specify the base.  
#'   Default is FALSE (linear y-axis).
#' @param xlab,ylab
#'   Character values; labels for the x- and y-axes
#' @param bound
#'   Logical value.  If TRUE, values outside the range of breaks are added to 
#'   the extreme bins; otherwise, they are excluded.  Ignored if 
#'   length(breaks)==1.
#' @param ... 
#'    Other plotting arguments passed to \code{\link[graphics]{barplot}} or
#'    \code{\link[graphics]{axis}} as appropriate (see also
#'    \code{\link[graphics]{plot.default}}).
#' @inheritParams graphics::hist 
#'
#' @return Returns a histogram object, like what is returned by the 
#'    \code{\link[graphics]{hist}} function, but with additional elements:
#'    \itemize{
#'      \item \strong{xbin} (or \strong{ybin} if horiz=T): 
#'        A function that accepts a single argument. If numeric, the values are 
#'        converted to bin number.  This allows you to add more lines, points, 
#'        etc to the plot, by passing x.bin(x) instead of  x to the plotting 
#'        function.
#'      \item \strong{xlim}: x-axis limits of plot ("true" limits, in bins; to 
#'        get the limits in terms of the input data, use: 
#'        \code{range(this.hist$breaks))}.
#'      \item \strong{ylim}: y-axis limits of the plot.
#'      \item \strong{xname, yname}: character string with the x- and y-labels.
#'      \item \strong{logcounts}: The logged counts + 1, in the base specified 
#'        (only if ylog argument is not FALSE).
#'      \item \strong{logbase}: The base used for logcounts.
#'    }
#
#' @examples
#' # plot normally distributed random numbers, with y-axis logged
#' this.hist <- Hist(rnorm(10000), 40, ylog=TRUE)
#' # vertical lines at +/-2
#' abline(v=this.hist$xbin(c(-2, 2)), col="red", lty=3, lwd=2)
#' # plot it again, without logging the y-axis
#' this.hist <- Hist(rnorm(10000), 40)
#' # for comparison, look at the standard histogram function
#' plot(this.hist)
#' 
#' @seealso \code{\link[graphics]{plot.default}}, \code{\link[graphics]{par}}, 
#'   \code{\link[graphics]{axis}} for more details about plotting options, as
#'   well as \code{\link[graphics]{barplot}} and \code{\link[graphics]{hist}},
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
#' @import graphics
#' @import stats
Hist <-
function(x, breaks="Sturges", ylog=F, xlab=NULL, ylab="Counts", bound=T, ...){
   these.args <- list(...)
   if(is.null(xlab)){
      # get the name of the variable being histogrammed (must be done first)
      xlab <- deparse(substitute(x))
   }
   # if vector of bin edges supplied, remove values outside their range
   if(length(breaks)>1){
      if(bound){
         # force all values to be in range of breaks (pile-up at edges)
         x[x<min(breaks)] <- min(breaks)+ 1e-9
         x[x>max(breaks)] <- max(breaks)- 1e-9
      }else{
         # ignore values outside of range of breaks
         x <- x[x>min(breaks) & x<max(breaks)]
      }
   }
   # histogram the data, without plotting it
   this.hist <- hist(x=x,breaks=breaks,plot=F)
   # figure out the range of the data
   if("xlim"%in%names(these.args)){
      # use the xlim argument passed in; remove it from the arglist
      xlims <- these.args$xlim
      these.args[["xlim"]] <- NULL
   }else xlims <- range(this.hist$breaks)
   xvals <- this.hist$mids
   yvals <- this.hist$counts
   if(ylog>0){
      # convert y-axis to log(counts+1)
      if(is.logical(ylog)) ylog <- 10  # base 10 by default if unspecified
      yvals <- log(yvals+1,ylog)
      if(ylab>"") ylab <- paste("log",ylog,"(",ylab," + 1)",sep="")
      this.hist$logcounts <- yvals
   }
   # figure out the range and ticks of the y-axis
   ytix <- pretty(c(0,yvals),5)
   if(max(ytix)<max(yvals))
      ytix <- c(ytix,max(ytix)+diff(ytix[1:2]))
   # permit the user to override the automatic ylims setting
   if("ylim"%in%names(these.args)){
      ylims <- these.args$ylim
      if(ylog>0) ylims <- log(ylims+1,ylog)
      these.args[["ylim"]] <- NULL
   }else ylims <- range(ytix)
   # create a function that maps x-values to the axis
   x.bin <- 1:length(xvals)-0.5
   xmap <- lm(x.bin~xvals)$coeff
   func.str <- paste("function(x) return( x *",xmap["xvals"],"+",
      xmap["(Intercept)"],")")
   assign("x.bin",eval(parse(text=func.str)))
   environment(x.bin) <- .GlobalEnv
   xbinlims <- x.bin(xlims) # range of the plot in x-bin space
   # make the plot
   if(("add"%in%names(these.args) && these.args$add)){
      # do not draw axes or label the plot
      plot.args <- list(height=yvals,space=0,xlim=xbinlims,ylim=ylims,xlab="",
         ylab="",xaxt="n",yaxt="n")
      for(Li in names(these.args)) plot.args[Li] <- these.args[Li]
      if("horiz"%in%names(plot.args) && plot.args$horiz==T){
         # swap the x- and y-axis arguments
         names(plot.args) <- sub("^x","Y",names(plot.args))
         names(plot.args) <- sub("^y","x",names(plot.args))
         names(plot.args) <- sub("^Y","y",names(plot.args))
      }
      bplot <- do.call(barplot,plot.args)
   }else{
      plot.args <- list(height=yvals,space=0,xlim=xbinlims,ylim=ylims,xlab=xlab,
         ylab=ylab)
      for(Li in names(these.args)) plot.args[Li] <- these.args[Li]
      if("horiz"%in%names(plot.args) && plot.args$horiz==T){
         # swap the x- and y-axis arguments
         names(plot.args) <- sub("^x","Y",names(plot.args))
         names(plot.args) <- sub("^y","x",names(plot.args))
         names(plot.args) <- sub("^Y","y",names(plot.args))
         this.side <- 2
      }else this.side <- 1
      bplot <- do.call(barplot,plot.args)
      # manually draw a box around the plot
      if(this.side==1){
         abline(h=ylims)
         abline(v=par("usr")[1:2])    
      }else{
         abline(v=ylims)
         abline(h=par("usr")[3:4])      
      }
      if(!("xaxt"%in%names(these.args) && these.args$xaxt=="n")){
         # figure out where the x-ticks should go
         xticlab <- pretty(xvals,5)
         xtix <- x.bin(xticlab)
         # annotate the x-axis, passing along any axis-specific arguments
         axis.args <- c(list(side=this.side,at=xtix,labels=xticlab),
            these.args[regexpr("(las|.axis$)",names(these.args))>0])
         do.call(axis,axis.args)
      }
   }
   # return a histogram object, including the coordinate mapping function
   if("horiz"%in%names(plot.args) && plot.args$horiz==T){
      this.hist$yname <- xlab
      this.hist$xname <- ylab
      this.hist$ybin <- x.bin
      this.hist$ylim <- xbinlims
      this.hist$xlim <- ylims 
   }else{
      this.hist$xname <- xlab
      this.hist$yname <- ylab
      this.hist$xbin <- x.bin
      this.hist$xlim <- xbinlims
      this.hist$ylim <- ylims  
   }
   if(ylog>0){
      this.hist$logcounts <- yvals
      this.hist$logbase <- ylog
   }else{
      this.hist$logcounts <- NULL
      this.hist$logbase <- F
   }
   invisible(this.hist)
}
