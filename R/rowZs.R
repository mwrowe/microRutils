#' rowZs: Fast Calculation of Z-Scores by Row
#' 
#' rowZs() calculates Z-scores across rows of a matrix using fast row methods.
#' 
#' This function is meant to help find samples or groups of samples with
#' extreme values, typically relative to a control group (vs)
#'   
#' @param X 
#'   Numeric matrix, or object that can be coerced to matrix.
#' @param vs
#'   Indices (numeric or character) of the columns of X to be used in the
#'   calculation of the mean or median and sd or mad of each row.  If omitted, 
#'   defaults to all columns of X.
#' @param robust
#'   Logical. If TRUE, calculate Z-scores as (x-median)/mad; otherwise,
#'   as (x - mean) / sd.
#' @param parts
#'   Logical; if TRUE, return the mean/median and sd/\code{\link[stats]{mad}} of 
#'   each row  as named attributes, CALCULATED ACROSS THE COLUMNS SPECIFIED BY 
#'   vs.
#' @param na.rm
#'   Logical; should NA's be omitted?
#' @param min.sd
#'   Optional numeric value, *quantile* of the non-zero sd's (or mad's) to 
#'   replace very low (~zero) sd or mad values with.  A typical value  might be 
#'   in the range 0 to 0.1.  Meant to prevent very large or unrealistic
#'   Z-scores when the sd is small by chance or because of pile-up at a signal 
#'   limit (e.g., saturation).
#'   
#' @return
#'    Returns a matrix with the same dimensions as X with z-scores for each
#'    element of X calculated from the row means/medians and sds/mads.  If 
#'    parts==TRUE, will have named attributes: "vs.mean" and "vs.sd" or
#'    "vs.median" and "vs.mad", plus "vs".
#'    
#' @seealso \code{\link[stats]{mad}}.
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @importFrom stats quantile
#' @export
# Created 10NOV2014
rowZs <- 
function(X, vs, robust=F, parts=F, na.rm=T, min.sd){
   if(missing(vs)) vs <- 1:ncol(X)
   if(robust){
      medians <- Biobase::rowMedians(X[,vs], na.rm=na.rm)
      mads <- 1.4826*Biobase::rowMedians(abs(X[,vs] - medians), na.rm=na.rm)
      if(!missing(min.sd)){
         min.sd <- quantile(mads[which(mads>0)], min.sd, na.rm=na.rm)
         mads[which(mads < min.sd)] <- min.sd
      }
      Z <- (X - medians)/mads
      if(parts){
         attr(Z,"vs.median") <- structure(names=rownames(X), medians)
         attr(Z,"vs.mad") <- structure(names=rownames(X), mads)
      }
   }else{
      means <- rowMeans(X[,vs], na.rm=na.rm)
      SDs <- rowSds(X[,vs], na.rm=na.rm)
      if(!missing(min.sd)){
         min.sd <- quantile(SDs[which(SDs>0)], min.sd, na.rm=na.rm)
         SDs[which(SDs < min.sd)] <- min.sd
      }
      Z <- (X - means)/SDs
      if(parts){
         attr(Z,"vs.mean") <- structure(names=rownames(X), means)
         attr(Z,"vs.sd") <- structure(names=rownames(X), SDs)
      }
   }
   if(parts & !missing(vs)) attr(Z,"vs") <- vs
   return(Z)
}
#-------------------------------------------------------------------------------
rowSds <- 
#' rowSds: Fast Calculation of Standard Deviations of Rows
#'
#' rowSds(x) calculates standard deviatons across rows, faster than 
#' \code{apply(x, 1, sd)}
#'   
#' @param  x
#'   A numeric matrix-like object
#' @param na.rm
#'   Logical; should NA's be omitted prior to calculations?
#'   
#' @return 
#'   Returns a numeric vector with length equal to number of rows of x.
#  
#' @export 
function(x, na.rm=TRUE){
   sqr <- function(x) x * x
   n <- rowSums(!is.na(x))
   n[n <= 1] <- NA
   means <- rowMeans(x, na.rm=na.rm)
   SDs <- rowSums(sqr(x - means), na.rm=na.rm)/(n - 1)
   return(sqrt(SDs))
}
#-------------------------------------------------------------------------------
rowMads <- 
#' rowMads: Fast Calculation of MADs by Row.
#' 
#'   rowMads(X) returns median absolute deviation (MAD) of each row of matrix x.
#'   
#' @param X 
#'   Numeric matrix, or object that can be coerced to matrix.
#' @param na.rm
#'   Logical; should NA's be omitted?
#' @param constant
#'   Scale factor; numeric constant for asymptotically normal consistency.
#'   
#' @seealso \code{\link[stats]{mad}}.
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
function(X, na.rm=T, constant=1.4826){
   constant * Biobase::rowMedians(
      abs(X - Biobase::rowMedians(X, na.rm=na.rm)), na.rm=na.rm)
}

