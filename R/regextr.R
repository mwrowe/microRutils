#' regextr: Substring Extraction By Regular Expression
#'
#' Extract substring(s) that match a regular expression in a character vector
#'
#' @param pattern
#'        character string specifying regular expression to be matched
#' @param x
#'        string or character vector of strings to be searched
#' @param L.trim
#'        integer; number of characters to strip from the start of the
#'        matched substring
#' @param R.trim
#'        integer; number of characters to strip from the end of the
#'        matched substring
#' @param ... other arguments passed to regexpr()
#'        
#' @return
#'    matched: a character vector the same length as x containing substrings of
#'       that match the pattern.  Only the first match found is returned.  If no
#'       match is found, an empty string ("") is returned in that element.
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
regextr <-
function(pattern, x, L.trim=0, R.trim=0, ...){
   if(length(x)>1){
      # if a vector is passed in, call this function recursively
      matched <- rep("",length(x))
      for(Si in 1:length(x)) matched[Si] <- regextr(pattern,x[Si],...)
   }else{
      # match the pattern and extract the matching substring
      matched <- regexpr(pattern,x,...)
      if(is.na(matched)){
         matched <- "" 
      }else if(matched<0){
         matched <- ""
      }else{
         matched <- substr(x,matched,matched+attr(matched,"match.length")-1)
      }
   }
   # if requested, trim the matches
   if(L.trim>0){
      N.chars <- nchar(matched)
      trim.me <- which(N.chars>L.trim)
      matched[trim.me] <- substr(matched[trim.me],L.trim+1,N.chars[trim.me])
      matched[setdiff(1:length(x),trim.me)] <- ""
   }
   if(R.trim>0){
      N.chars <- nchar(matched)
      trim.me <- which(N.chars>R.trim)
      matched[trim.me] <- substr(matched[trim.me],1,N.chars[trim.me]-R.trim)
      matched[setdiff(1:length(x),trim.me)] <- ""
   }
   return(matched)
}
