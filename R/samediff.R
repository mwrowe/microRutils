#' samediff: Find Elements in Common, and Unique To, Two Lists
#'
#' samediff returns a list with elements of a and b in common, and unique to 
#' each, or numbers of unique elements in such a list.
#'
#' @param a
#'        a vector or other object that can be treated as a vector OR a 
#'        two-element list of vectors; if a is a list, all subsequent
#'        arguments must be named and b will be ignored.  If 
#'        use.arg.names==TRUE, the names of the list elements will be used to 
#'        label the output
#' @param b
#'        a second vector or other object that can be treated as a vector unless
#'        a is a two-element list, in which case b is ignored.
#' @param sort
#'        logical; determines whether the elements of A and B will be returned
#'        in alphabetical order
#' @param na.rm
#'        logical; should missing values be removed from A and B?
#' @param use.arg.names
#'        if true, the names of the variables passed in as argumentsa and b to
#'        this function will be substituted for the default names "a.only" and 
#'        "b.only" in the list returned.  Default FALSE.
#' @param Ns
#'        logical; if TRUE, return a vector of the numbers of item in common and 
#'        unique to a and b
#'        
#' @return
#'   \itemize{
#'     \item A list, with three named character strings:
#'       \itemize{
#'         \item "in.both": intersection of a and b
#'         \item "a.only": (or the name of a, if use.arg.names==T): elements 
#'           unique to a
#'         \item "b.only": (or the name of b, if use.arg.names==T): elements 
#'           unique to b
#'        }
#'     \item ...OR a named numeric vector with the numbers of items in common, 
#'        unique to a and unique to b, with the same names as above
#'    }
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
samediff <-
function(a,b,sort=T,na.rm=F,use.arg.names=F,Ns=FALSE){
   if(is.list(a)){
      if(length(a)==2){
         tmp <- a
         a <- tmp[[1]]
         b <- tmp[[2]]
         tmp <- names(tmp)
      }else stop("if 'a' argument is a list, it must be length 2")
   }else tmp <- c()
   if(na.rm){
      a <- a[!is.na(a)]
      b <- b[!is.na(b)]
   }
   x <- list(in.both=intersect(a,b),a.only=setdiff(a,b),b.only=setdiff(b,a))
   if(sort) x <- lapply(x,sort)
   if(use.arg.names){
      if(length(tmp)>0){
         names(x)[2:3] <- tmp
      }else{
         names(x)[2] <- deparse(substitute(a))
         names(x)[3] <- deparse(substitute(b))
      }
   }
   if(Ns){
      return(unlist(lapply(x,length)))
   } else{
      return(x)
   }
}
