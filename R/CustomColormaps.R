#' Custom Colormaps For Heat Maps 
#' 
#' A set of functions that to define various attractive color scales
#' 
#' Many of these functions (\code{orange.blue}, \code{yellow.blue},
#' \code{red.green}, \code{purple.orange}, \code{aqua.brown} and
#' \code{purple.green}) interpolate between three colors, where the function
#' name contains the start and stop colors, and the "thru" argument is used to
#' define a midpoint (typically black or white).  Many of these were defined to
#' accomodate people with color blindness.
#' 
#' Colormap functions that do not follow this convention are:
#' \itemize{
#'   \item \strong{grayscale} maps shades of grey from black to white.
#'   \item \strong{hot} mimics black-body radiation, passing from black to white
#'      through red, orange and yellow (like iron being heated up).
#'   \item \strong{hot2} is similar to hot, but has a more subdued shard of red
#'     as the first value after black. This is useful for de-emphasizing single
#'     data points in a heatmap of a 2D-histogram.
#'   \item \strong{jet} interpolates from dark blue to dark red through shades
#'      of royal blue, cyan, green, yellow and orange.
#' }
#' 
#' @param levels 
#'   Integer, number of levels in the color map
#' @param thru 
#'   A color, either named or as \code{\link[grDevices]{rgb}}() output, that
#'   will serve as midpoint for color maps that are symmetric about a central
#'   value.
#' @inheritParams grDevices::colorRamp
#' 
#' @return 
#'   A character vector with elements of 7 or 9 characters, "#" followed by the 
#'   red, blue, green and optionally alpha values in hexadecimal (after 
#'   rescaling to 0 ... 255). The optional alpha values range from 0 (fully
#'   transparent) to 255 (opaque).
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' 
#' @aliases yellow.blue grayscale jet purple.orange aqua.brown purple.green
#'    hot hot2 orange.blue
#' 
#' @importFrom grDevices colorRampPalette rainbow rgb 
#' @export
#-------------------------------------------------------------------------------
red.green <- 
function(levels=100,space="rgb",thru="black",...){
   cmap <- colorRampPalette(colors=c("green",thru,"red"),space=space,...)
   cullers <- cmap(levels)
   return(cullers)
}
#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
yellow.blue <- 
function(levels=100,space="rgb",...){
   cmap <- colorRampPalette(colors=c("royalblue1","black","yellow"),
      space=space,...)
   return(cmap(levels))
}
#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
grayscale <- 
function(levels=100,space="rgb",...){
   cmap <- colorRampPalette(colors=c("white","black"),space=space,...)
   return(cmap(levels))
}
#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
jet <- 
function(levels=100,space="rgb",...){
   colors <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", 
      "#FF7F00", "red", "#7F0000")
   dots <- list(...)
   if("alpha"%in%names(dots)){
      cmap <- colorRampPalette(colors=colors,...)
   }else{
      cmap <- colorRampPalette(colors=colors,space=space,...)
   }
   return(cmap(levels))
}
#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
purple.orange <-
function(levels=100,space="rgb",...){
   red <- c(179,241,254,247,216,153,84)
   green <- c(88,163,224,247,218,142,39)
   blue <- c(6,64,182,247,235,195,136)
   colors <- rgb(red,green,blue,maxColorValue=255)
   cmap <- colorRampPalette(colors=colors,space=space,...)
   return(cmap(levels))
}
#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
aqua.brown <-
function(levels=100,space="rgb",...){
   red <- c(1,90,199,245,246,216,140)
   green <- c(102,180,234,245,232,179,81)
   blue <- c(94,172,229,245,195,101,10)
   colors <- rgb(red,green,blue,maxColorValue=255)
   cmap <- colorRampPalette(colors=colors,space=space,...)
   return(cmap(levels))
}

#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
purple.green <-
function(levels=100,space="rgb",...){
   red <- c(118,175,231,247,217,127,27)
   green <- c(42,141,212,247,240,191,120)
   blue <- c(131,195,232,247,211,123,55)
   colors <- rgb(red,green,blue,maxColorValue=255)
   cmap <- colorRampPalette(colors=colors,space=space,...)
   return(cmap(levels))
}
#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
hot <-
   function(levels=100,space="rgb",...){
   colors <- c("darkred","red","orange","yellow","white")
   cmap <- colorRampPalette(colors=colors,space=space,...)
   return(c("black",cmap(levels)))
}
#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
hot2 <-
   function(levels=100,space="rgb",...){ #bias=0.5,
   colors <- c("black","darkred","red","orange","yellow","white")
   cmap <- colorRampPalette(colors=colors,space=space,...) #bias=bias,...)
   return(c(cmap(levels)))
}
#-------------------------------------------------------------------------------
#' @rdname red.green
#' @export
orange.blue <- 
   function(levels=100,space="rgb",thru="black",...){
   cmap <- colorRampPalette(colors=c("royalblue1",thru,"orange"),
      space=space,...)
   return(cmap(levels))
}
