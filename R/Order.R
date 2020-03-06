#' Order: Order Items Using Columns Of A Data.Frame As Keys
#'
#' Order augments the base order() function to work with data.frames, lists, 
#' etc.
#'
#'    This function is just a wrapper for \code{link{order}} that permits it to 
#'    be called with more complex objects.  For example, if you wanted to sort a 
#'    data.frame x based on the contents of its first three columns...
#'
#'    > Order(x)
#'
#'    ... would give the same result as:
#'
#'    > order(x[, 1], x[, 2], x[, 3])
#' 
#' @param sort.on
#'        a data.frame, list, matrix, 2D-array, table or vector.  If sort.on
#'        is a list, all elements must be vectors of the same length.  All other
#'        classes will be coerced to list.  The first element in the list will
#'        used for sorting; additional elements (if supplied) will be used to 
#'        break ties.
#' @param ... 
#'        additional vector arguments, all of the same length (which must be
#'        equal to the number of rows of sort.on).  This argument permits Order to
#'        be called in the same manner as the base order() function.
#' @param na.last
#'        for controlling the treatment of NAs. If TRUE, missing values in 
#'        the data are put last; if FALSE, they are put first; if NA, they are 
#'        removed. 
#' @param decreasing
#'        logical. Should the sort order be increasing or decreasing?
#'        
#' @return
#'    a vector of integers specifying the permutation that will result in 
#'    ordering of the elements of sort on
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
Order <- 
function(sort.on,...,na.last=T,decreasing=F){
   if(class(sort.on)[1]=="table"){
      # convert table objects to array
      sort.on <- array(sort.on,dim=dim(sort.on),dimnames=dimnames(sort.on))
   }
   if(class(sort.on)[1]%in%c("array","matrix")){
      # convert array or matrix argument to data.frame
      if(length(dim(sort.on))!=2){
         stop("Arrays or tables with more than two dimensions are not allowed.")
      }
      sort.on <- as.data.frame(sort.on)
   }
   # convert data.frame or vector argument to list
   if(class(sort.on)[1]=="data.frame") sort.on <- as.list(sort.on)
   # make sure it works with vector arguments as well
   vectors <- c("integer","numeric","character","logical","complex")
   if(class(sort.on)[1]%in%vectors){
      sort.on <- list(sort.on)
   }
   if(class(sort.on)[1]!="list"){
      stop("sort.on must be a vector, matrix, array, data.frame, table or list.")
   }
   if(!missing(...)){
      # this permits the function be called the normal way (with vectors)
      more.args <- list(...) 
      sort.on[length(sort.on)+(1:length(more.args))] <- more.args
   }
   # remove any names that might be mistaken for arguments by order()
   names(sort.on) <- NULL
   # do some argument checking
   lengths <- unlist(lapply(sort.on,length))
   if(!all(lengths==lengths[1])){
      stop("All arguments must be the same length.")
   }
   # append the other arguments to order()
   sort.on$na.last <- na.last
   sort.on$decreasing <- decreasing
   ndxs <- do.call(get("order",baseenv()),args=sort.on)
   return(ndxs)
}
#-------------------------------------------------------------------------------
#' pvalOrder: Order Values By Levels, Then Numeric Value
#'
#' Order a set of p-values first on significance level, then on their values.
#'
#' @param pvals
#'        numeric vector of values between zero and one
#' @param p.cut
#'        numeric vector of significance thresholds
#'        
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
pvalOrder <- 
function(pvals, p.cut=0.05){
   signif <- cut(pvals, sort(unique(c(-Inf, p.cut, Inf))))
   ndxs <- Order(cbind(signif, pvals))
}
