#' nonUniq: Find Replicated Elements In A Vector
#'
#' nonUniq(x) returns items in x that occur more than once (indices or values)
#'
#' @param x
#'        vector or matrix. Missing values are ignored.
#' @param value
#'        logical; if TRUE, return the unique elements in x that occur more
#'        than once.  If FALSE (default), return their indices.
#' @param na.rm
#'        logical; if TRUE, ignore missing values (NA's)
#'        
#' @return
#'    If value==F, an integer vector with positions (indices) of non-unique 
#'    elements of x; otherwise, a vector with the unique elements of x that
#'    occur more than once.  Thus:
#'       all(nonUniq(x, value=T)==unique(x[nonUniq(x)])) is TRUE   
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
nonUniq <- 
function(x,value=F,na.rm=T){
   if(na.rm) uniqs <- table(x) else uniqs <- table(x,exclude=NULL)
   nonUniqs <- which(x%in%names(uniqs)[which(uniqs>1)])
   if(value & length(x)>0) nonUniqs <- unique(x[nonUniqs])
   return(nonUniqs)
}
