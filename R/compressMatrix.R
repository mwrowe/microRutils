#' Resize a Matrix By Combining Adjacent Elements
#' 
#' compressMatrix() resizes a matrix by averaging groups of adjacent
#' rows/columns (typically averaging or summing).
#'
#' @param X 
#'   A matrix or object that can be coerced to such (array, data.frame), 
#'   typically numeric, though other types are possible depending on the 
#'   FUN argument.
#' @param howmuch
#'   One or two element integer vector specifying compression factor
#'   in each dimension.  If length(howmuch)==1, both dimensions will be
#'   reduced by the same factor.
#' @param FUN
#'   Function to apply for combining values.
#' @param na.rm
#'   Logical; if TRUE, remove missing values from X prior to calling FUN
#' @param ... 
#'   Additional arguments passed to FUN
#' 
#' @return
#'   Returns a matrix with dimensions dim(X) / howmuch, where each element is
#'   average of howmuch[1] neighboring rows and howmuch[2] neighboring
#'   columns.  If the original dimensions of X are not evenly divisible
#'   by howmuch, additional rows and/or columns of missing values will be 
#'   appended.  
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
compressMatrix <- 
function(X, howmuch=4, FUN=mean, na.rm=T, ...){
   X <- as.matrix(X)
   if(length(howmuch)==1) howmuch[2] <- howmuch
   # figure out whether columns/rows need to be padded
   to.add <- (howmuch - dim(X)%%howmuch[1])%%howmuch
   # pad with rows of NA's if needed, reshape and calculate averages of rows
   X <- as.array(rbind(X, matrix(NA, to.add[1], dim(X)[2])))
   dim(X) <- c(howmuch[1], dim(X)[1]/howmuch[1], dim(X)[2])
   X <- apply(X, 2:3, FUN, na.rm=na.rm, ...)
   # repeat the above for the columns, first transposing the matrix
   X <- t(as.array(cbind(X, matrix(NA, dim(X)[1], to.add[2]))))
   dim(X) <- c(howmuch[2], dim(X)[1]/howmuch[2], dim(X)[2])
   X <- t(apply(X, 2:3, FUN, na.rm=na.rm, ...))
   return(X)
}
