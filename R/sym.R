#' sym: Get A Numeric Range That Is Symmetric Around Zero
#'
#' sym returns a symmetric numeric interval, centered about zero.
#' 
#' This function is useful for setting symmetric limits on graphs (among other
#' things.)  
#' 
#' @param lims 
#'        A numeric value or vector.  If its length is greater than one, the
#'        quantile specified by the qtile argument will be calculated for 
#'        the absolute values of lims. 
#' @param qtile
#'        A numeric value in the range 0-1 (inclusive).  If lims contains
#'        multiple values, this quantile of the distributions of abs(lims)
#'        will be used to define the interval.  The default value of 1 will 
#'        return max(abs(lims)).
#'        
#' @return 
#'    Returns a symmetric interval about zero, from -lims to lims, or if lims
#'    contains more than one value, a symmetric interval about zero that 
#'    contains the fraction of the data specified by qtile.
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
sym <- 
function(lims=1, qtile=1){
   lims <- abs(lims)
   if(length(lims)>1){
      lims <- stats::quantile(lims[TRUE], qtile, na.rm=T)
   }   
   c(-1,1)*lims
}
