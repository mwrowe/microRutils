#' percent: Format Numeric Values As (Character) Percentages
#'
#' percent() reformats a (numeric) fraction as a (character) percentage
#'
#' @param fractions
#'        numeric vector, matrix or array, where one corresponds to 
#'        100%.  Missing values are permitted.  Use lapply() to format numeric
#'        columns of a data.frame.
#' @param digits
#'        number of digits to the right of the decimal place to preserve.
#'        If missing, defaults to the minimum number of digits such that all
#'        results greater than zero will be represented as greater than zero.
#' @param justify
#'        logical, should the results be right justified by padding with 
#'        spaces? 
#'        
#' @return
#'    returns a character vector with same dimensions as fractions, where values
#'    are represented as percentages, including "%" symbol.  Infinite values
#'    are returned as "Inf" or "-Inf"; missing values as NA's (unquoted).
#'     
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
percent <- 
function(fractions, digits, justify=F){
   if(!is.numeric(fractions)){
      return("fractions argument must be numeric.")
   }
   if(missing(digits)){
      # determine minimum number of digits needed
      digits <- 
         pmax(2,max(-floor(log10(fractions[which(fractions>0)])),na.rm=T)) - 2
   }
   pct <- paste(sep="",
      format(round(100*fractions,digits),nsmall=digits,sci=F,just="right"),"%")
   if(!justify){  # remove leading spaces added by format()
      pct <- gsub(" ","",pct)
   }
   if(any(is.na(fractions))){  # set missing values to NA (instead of "NA%")
      pct[is.na(fractions)] <- NA
   }
   infs <- which(is.infinite(fractions)) # deal with infinite values (no "%")
   if(length(infs)>0){
      pct[infs] <- fractions[infs]
   }
   if(!is.null(dim(fractions))){  # return with same dimensions as fractions
      dim(pct) <- dim(fractions)
   }
   if(!is.null(dimnames(fractions))){  # apply the same names as fractions
      dimnames(pct) <- dimnames(fractions)
   }else if(!is.null(names(fractions))){
      names(pct) <- names(fractions)
   }
   return(pct)
}

