#' Formats p-values for readability.
#' 
#' 
#' p.print() formats p-values for readability, with values floored at a lower
#' limit.
#
#' @param p
#'   A numeric vector of values between zero and one.
#' @param llim
#'   A numeric value between zero and one (exclusive) specifying the lowest
#'   p-value to report.  For example, if llim=0.001, values below this will
#'   be printed as "<0.001".
#' @param ... 
#'   Other argments passed to \code{\link[base]{format}}().
#
#' @return
#'   Returns a character vector of p-values.  Values >= than 0.1 are reported 
#'   with two significant figures; values <0.1 are reported with one significant
#'   figure.
#
#' @seealso \code{\link[base]{format}}.
#' 
#' @export
p.print <-
function(p, llim, ...){
   p.txt <- rep(NA,length(p)) 
   for(pndx in which(!is.na(p))) if(p[pndx]>=0.095){
      p.txt[pndx] <- format(round(p[pndx],2),nsmall=2)
   }else p.txt[pndx] <- format(signif(p[pndx],1),...)
   if(!missing(llim)) 
      p.txt[which(p<llim)] <- paste("<",sep="",format(llim,...))
   if(is.matrix(p) | is.array(p)) dim(p.txt) <- dim(p)
   return(p.txt)
} 
