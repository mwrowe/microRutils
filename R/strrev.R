#' strrev: Reverse Character Order
#'
#' strrev() reverses the order of characters within each element of a character 
#'    vector
#' @param strs
#'        A vector, matrix or array of character values, or other data type
#'        that can be coerced to type character.  Note that if strs is numeric,
#'        trailing zeros will not be preserved.
#'        
#' @return
#'     A vector, matrix or array of character values with the same dimensions
#'     and other attributes (i.e., names) as the input argument, but with the
#'     the order of characters in the individual elements reversed.
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
strrev <-
function(strs){
   if(any(class(strs)%in%c("list","data.frame"))) stop(
      "strs argument must be vector, matrix or array, not list or data.frame") 
   revstrs <- unlist(
      lapply(lapply(strsplit(strs, ""), rev), paste, collapse=""))
   if(any(is.na(strs))) revstrs[is.na(strs)] <- NA
   strs[TRUE] <- revstrs
   return(strs)
}

