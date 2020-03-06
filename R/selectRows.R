#' selectRows: Row Selection of A Data.Frame
#'
#' selectRows returns row indices of a data.frame matching column-value criteria
#' 
#' @details 
#' The criteria are applied in order, so this will run faster if the most
#' exclusive criteria are applied first.  However, the reorder option orders
#' the indices according to the order of the criteria annotations
#'   
#' @param meta
#'        a data.frame from which to select rows
#' @param criteria
#'        a list of named vectors, where the names correspond to columns
#'        of meta, and the elements are values of those columns to include.  NA
#'        is a permissible value.  Logically, the values within each element of 
#'        criteria act as an OR; values between elements are treated as an AND.
#' @param rownames
#'        logical; if TRUE, rownames are returned rather than numeric 
#'        indices
#' @param not
#'        character vector specifying names of criteria where only rows of meta
#'        that do NOT include the elements specified in the criteria list
#' @param reorder
#'        logical; if TRUE, reorder the row indices by the values of the
#'        columns specified by criteria, applied in order
#'        
#' @return
#'     Returns a vector of numeric row indices (rownames==FALSE) or character
#'     rownames of meta that matched the criteria specified
#'   
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
selectRows <- 
function(meta,criteria,rownames=TRUE,not=c(),reorder=FALSE){
   ndxs <- 1:nrow(meta)
   # loop through the criteria list, applying successive criteria
   for(Ci in names(criteria)){
      if(length(ndxs)==0) break
      if(Ci%in%not){
         ndxs <- ndxs[which(!meta[ndxs,Ci]%in%criteria[[Ci]])]
         
      }else{
         ndxs <- ndxs[which(meta[ndxs,Ci]%in%criteria[[Ci]])]
      }
   }
   if(rownames & length(ndxs)>0){
      ndxs <- rownames(meta)[ndxs]
   }
   if(reorder){
      ndxs <- ndxs[Order(meta[ndxs,names(criteria)])]
   }
   return(ndxs)
}
