#' numberReplicates: Number Recurring Elements
#'
#' numberReplicates numbers instances of each non-unique value in the order 
#' found
#' numberReplicates
#'   
#' @param x
#'        a vector that includes non-unique elements
#' @param number.NAs
#'        logical; if TRUE, missing values will be numbered; otherwise,
#'        return an NA for each missing value
#'        
#' @return
#'    returns a numeric vector where replicates of each unique value in x are 
#'    numbered from 1 to the number of replicates found in the order in which 
#'    they are found in x.  The replicate numbers returned correspond to the 
#'    elements of x as it was passed in.
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
numberReplicates <-
# CREATED: 18SEP2014
function(x, number.NAs=FALSE){
   # keep track of original order; sort x
   x <- data.frame(x=x[T],order=1:length(x),repnum=NA)
   x <- x[order(x$x),]
   # assign replicate numbers for each unique value of x
   not.na <- which(!is.na(x$x))
   if(length(not.na)>0){
      x[not.na,"repnum"] <- 
         unlist(tapply(rep(1,length(not.na)),x[not.na,"x"],cumsum))
   }
   # deal with missing values
   na.ndxs <- which(is.na(x$x))
   if(number.NAs & length(na.ndxs)>0){
      x[na.ndxs,"repnum"] <- 1:length(na.ndxs)
   }
   # go back ot the original order of x
   x <- x[order(x$order),]
   x$repnum
}


