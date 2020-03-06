#' getLapplyIndex: Access Index of List Element Within lapply()
#'
#' Get name/index of current list element from within FUN argument of lapply()
#'   
#' \itemize{
#'   \item This is meant to be called from within the FUN argument passed to 
#'     lapply(); it cannot be used as the FUN argument itself.
#'   \item It will also return the index along the current dimension DIM if 
#'     called from a function within the apply() function, though without a 
#'     name.
#'   \item This function enables the user to change the processing that occurs 
#'   within FUN according to which element is being operated on.
#' }
#'   
#' @return
#'    Returns an integer value indicating the current index of the list passed 
#'    as the X argument to lapply().  If X is named, the name of the current
#'    element will be the name of the index.
#'    
#' @examples
#'   x <- list(a=1:10, b=1:10, c=1:10)
#'   FUNS <- list(a=mean, b=prod, c=max)
#'   FUN <- function(x){
#'      ndx <- getLapplyIndex()
#'      y <- FUNS[[names(ndx)]](x)
#'      y
#'   }
#'   lapply(x, FUN)
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
getLapplyIndex <- 
function(){
   ndx <- get("i", parent.frame(2))
   names(ndx) <- names(get("X", parent.frame(2)))[[ndx]]
   ndx
}
