#' rbindSafe: Row-wise Concatenation of Objects With Potentially Different 
#'   Columns
#'
#' rbindSafe() concatenates rows of arguments after ensuring same columns are 
#' present
#' 
#' rbindSafe finds the union of (column) names of the objects.  It puts
#' the columns in common in the same order and applies \code{\link{rbind}},
#' then adds the columns unique to one object or the other, with missing values
#' for the object where the column is missing. 
#'
#' @param ... 
#'    Two or more objects such as matrices, data.frames, arrays or vectors 
#'    to concatenate along the first dimension (adding rows).
#'        
#' @return 
#'    Returns an object of the same class as the inputs (coerced to the same
#'    class if they differ) with additional rows, and the union of the columns.
#'    
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
rbindSafe <- 
function(...){
   to.bind <- list(...)
   if(length(to.bind)==1) return(to.bind[[1]])
   x <- to.bind[[1]]
   dups <- c()
   for(Ai in 2:length(to.bind)){
      y <- to.bind[[Ai]]
      if(length(x)==0){
         return(y)
      }else if(length(y)==0){
         return(x)
      }
      # deal with vector arguments
      if(length(dim(x))<2){
         x <- array(x,
            dim=c(1,length(x)),dimnames=list(names(to.bind)[1],names(x)))
      }
      if(length(dim(y))<2){
         y <- array(y,
            dim=c(1,length(y)),dimnames=list(names(to.bind)[Ai],names(y)))
      }
      if(is.null(colnames(x)) | is.null(colnames(y))){
         stop("all arguments must have named columns.")
      }
      dups <- c(dups,intersect(rownames(x),rownames(y)))
      cnames <- samediff(colnames(x),colnames(y))
      if(length(cnames$a.only)>0){
         y <- cbind(y,array(NA,dim=c(nrow(y),length(cnames$a.only)),
            dimnames=list(rownames(y),cnames$a.only)))
      }
      if(length(cnames$b.only)>0){
         x <- cbind(x,array(NA,dim=c(nrow(x),length(cnames$b.only)),
            dimnames=list(rownames(x),cnames$b.only)))
      }
      x <- rbind(x,y[,colnames(x),drop=F])
   }
   if(length(dups)>0 & !all(dups%in%as.char(1:100000))){
      n.dups <- length(dups)
      dups <- paste0(dups[1:pmin(10,n.dups)],collapse="\n     ")
      warning(paste0(n.dups," duplicate rownames found:\n     ",dups,
         ifelse(n.dups>10,"\n     ...",""),"\n"))
   }
   x
}
#-------------------------------------------------------------------------------
#' cbindSafe: Column-wise Concatenation of Objects With Potentially Different 
#'   Rows
#'
#' cbindSafe() concatenates columns of arguments after ensuring same rows are 
#' present.
#' 
#' cbindSafe finds the union of (row) names of the objects.  It puts
#' the rows in common in the same order and applies \code{\link{cbind}},
#' then adds the rows unique to one object or the other, with missing values
#' for the object where the row is missing. 
#'
#' @param ... 
#'        two or more objects such as matrices, data.frames, arrays or vectors 
#'        to concatenate along the second dimension (adding columns)
#'        
#' @return 
#'    Returns an object of the same class as the inputs (coerced to the same
#'    class if they differ) with additional columns, and the union of the rows.
#'    
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
cbindSafe <- 
function(...){
   to.bind <- list(...)
   if(length(to.bind)==1) return(to.bind[[1]])
   x <- to.bind[[1]]
   dups <- c()
   for(Ai in 2:length(to.bind)){
      y <- to.bind[[Ai]]
      if(length(x)==0){
         return(y)
      }else if(length(y)==0){
         return(x)
      }
      # deal with vector arguments
      if(length(dim(x))<2){
         x <- array(x,
            dim=c(length(x),1),dimnames=list(names(x),names(to.bind)[1]))
      }
      if(length(dim(y))<2){
         y <- array(y,
            dim=c(length(y),1),dimnames=list(names(y),names(to.bind)[Ai]))
      }
      if(is.null(rownames(x)) | is.null(rownames(y))){
         stop("all arguments must have named rows.")
      }
      dups <- c(dups,intersect(colnames(x),colnames(y)))
      rnames <- samediff(rownames(x),rownames(y))
      if(length(rnames$a.only)>0){
         newrows <- array(NA,c(length(rnames$a.only),ncol(y)),
            list(rnames$a.only,colnames(y)))
         y <- rbind(y,newrows)
      }
      if(length(rnames$b.only)>0){
         newrows <- array(NA,c(length(rnames$b.only),ncol(x)),
            list(rnames$b.only,colnames(x)))
         x <- rbind(x,newrows)
      }
      x <- cbind(x,y[rownames(x),,drop=F])
   }
   if(length(dups)>0 & !all(dups%in%as.char(1:100000))){
      n.dups <- length(dups)
      dups <- paste0(dups[1:pmin(10,n.dups)],collapse="\n     ")
      warning(paste0(n.dups," duplicate colnames found:\n     ",dups,
            ifelse(n.dups>10,"\n     ...",""),"\n"))
   }
   x
}

