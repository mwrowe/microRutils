#' rename: Rename Elements, Rows or Columns, Etc.
#'
#' rename is a generic function for renaming elements, rows or columns
#'
#' @param x
#'        a vector or list with named elements, or a data.frame, matrix or array 
#'        with named rows, columns, pages, etc.
#' @param oldnames
#'        character vector with names of the elements, rows or columns to be
#'        renamed.  All names must be unique.  Partial matching is NOT 
#'        supported.  An error will be generated if any oldnames are not found 
#'        in x, or if they may specify more than one element (for example, if 
#'        both a row and column share the same name, and which.dim was not 
#'        specified.) The elements to be renamed may be in any order, and may 
#'        come from multiple dimensions (e.g., a mix of row and column names).
#' @param newnames
#'        character vector of unique replacement names for \emph{corresponding} 
#'        elements of oldnames.  Must be the same length as oldnames.
#' @param which.dim
#'        integer or character string specifying which dimension of x where 
#'        names should be replaced.  Ignored if x is a vector or list.  If 
#'        omitted, all names returned by dimnames(x) will be considered 
#'        candidates for replacement, but will only be replaced if a unique 
#'        match is found.
#' @param sanity
#'        logical; if true, a table summarizing the replacements is printed
#'        to the screen.
#'        
#' @return
#'     Returns the object x with elements, rows or columns renamed as specified.  
#'     Note that you must reassign the variable to this value if you want the 
#'     new names to take effect.
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
rename <- 
function(x,oldnames,newnames,which.dim,sanity=T){
   # check the oldnames and newnames arguments for validity
   if(length(oldnames)!=length(newnames))
      stop("Arguments oldnames and newnames must be the same length.")
   if(length(unique(oldnames))!=length(oldnames))
      stop("Elements of oldnames argument must be unique.")
   if(length(unique(newnames))!=length(newnames))
      stop("Elements of newnames argument must be unique.")
   # create table to keep track of which elements are renamed
   name.table <- data.frame(DIM=NA,INDEX=NA,OLD.NAME=oldnames,TO="  ->  ",
      NEW.NAME=newnames)
   # handle vectors and lists differently from data.frames and arrays
   if(class(x)%in%c("data.frame","matrix","array")){
      if(missing(which.dim)){
         # see whether unique matches can be found among all the dimensions
         x.names <- dimnames(x)
         for(Ni in 1:length(oldnames)){
            N.matched <- sum(unlist(x.names)==oldnames[Ni],na.rm=T)
            if(N.matched==0)
               stop(paste('No element named "',oldnames[Ni],'" was found.',sep=""))
            if(N.matched>1){
               stop(paste('Multiple elements named "',oldnames[Ni],
                  '" were found; specify which.dim.',sep=""))   
            }
         }
         # make a copy of the original names, to permit swapping  
         renamed <- x.names 
         # loop through the dimensions and apply replacements to renamed
         for(Di in 1:length(dim(x))){
            ndxs <- match(oldnames,x.names[[Di]])
            # missing values indicate no match in this dimension (this is okay)
            if(all(is.na(ndxs))){
               next
            }
            # replace in renamed where matches were found in x.names
            renamed[[Di]][ndxs[!is.na(ndxs)]] <- newnames[!is.na(ndxs)]
            name.table[!is.na(ndxs),c("DIM","INDEX")] <- 
               data.frame(Di,ndxs[!is.na(ndxs)])
         }
         # copy renamed into x after renaming is complete
         dimnames(x) <- renamed
         # make sure all elements are still named uniquely
         for(Di in 1:length(dim(x))){
            if(any(table(dimnames(x)[[Di]])>1))
               stop("Element names would no longer be unique.")
         }
      }else{
         # replace names in the specified dimension
         x.names <- dimnames(x)[[which.dim]]
         # check that oldnames specify exactly one element each
         for(name in oldnames){
            N.matched <- sum(x.names==name,na.rm=T)
            if(N.matched==0) 
               stop(paste('No element named "',name,'" was found.',sep=""))
            if(N.matched>1)
               stop(paste('Multiple elements named "',name,'" were found.'),sep="")
         }
         # find the elements with oldnames and replace them with newnames
         ndxs <- match(oldnames,x.names)
         x.names[ndxs] <- newnames
         dimnames(x)[[which.dim]] <- x.names
         # update the name.table
         name.table$DIM <- which.dim
         name.table$INDEX <- ndxs
         # make sure all elements are still named uniquely
         if(any(table(dimnames(x)[[which.dim]])>1))
            stop("Element names would no longer be unique.")
      }
   }else if(is.vector(x)){
      x.names <- names(x)
      # check that oldnames specify exactly one element each
      for(name in oldnames){
         N.matched <- sum(x.names==name,na.rm=T)
         if(N.matched==0) 
            stop(paste('No element named "',name,'" was found.',sep=""))
         if(N.matched>1)
            stop(paste('Multiple elements named "',name,'" were found.'),sep="")
      }
      # find the elements with oldnames and replace them with newnames
      ndxs <- match(oldnames,x.names)
      x.names[ndxs] <- newnames
      names(x) <- x.names
      # update the name.table
      name.table$DIM <- 1
      name.table$INDEX <- ndxs
      # make sure all elements are still named uniquely
      if(any(table(names(x))>1))
         stop("Element names would no longer be unique.")
   }else{
      stop(paste(class(x),"objects are not supported."))
   }
   if(sanity){
      name.table <- name.table[order(name.table$DIM,name.table$INDEX),]
      rownames(name.table) <- NULL
      print(name.table)
   }
   # return the renamed object
   renamed <- x
   return(renamed)
}
