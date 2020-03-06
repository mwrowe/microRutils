#' cString: Create a c() Expression From Character Vector
#'
#' Convert a character vector to comma-separated list of quoted strings
#'
#' This is a handy function for creating a c("element1", "element2", "etc") 
#' expression from a character vector that can them be pasted into your code.
#' Note that it returns a single element character vector not an expression 
#' object.
#'
#' @section Side effects:
#'    The output string is printed to standard out using cat()
#'
#' @param char.vector
#'        a character vector containing one or more elements
#' @param wrap
#'        logical or numeric value indicating whether the string should be
#'        wrapped at a given width.  If TRUE, wrap at 0.9*getOption("width");
#'        if FALSE do not wrap; if numeric, wrap at that number of characters.
#'        The output will be broken after the comma separators.
#' @param var.name
#'        character string to be prepended to the output; typically the
#'        name of a variable to which the output will be assigned.
#' @param prefix
#'        character string that will be inserted between the var.name and the
#'        commas-separated quoted list; default is ' <- c('.
#' @param suffix
#'        character string to append after the commas-separated list; default
#'        is ')'.  If prefix is set to "" (empty string), suffix will 
#'        automatically be set to that as well.
#' @param indent
#'        integer indicating number of spaces by which to indent all lines
#'        except the first; default is 3.
#' @param sep
#'        character string indicating the separator between elements; defaults
#'        to '", "' (don't forget the double quotes)
#'        
#' @return
#'    a character value containing the input strings as a comma-separated
#'    quoted list, returned invisibly.
#'    
#' @examples
#'  animals <- c("dog", "cat", "moose", "antelope", "aardvark", "goose", "rat",
#'   "elephant", "turkey", "cow", "platypus", "human", "crocodile", "rooster")
#'  cString(animals, wrap=80, var.name="zoo")
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
cString <-
function(char.vector, wrap=TRUE, var.name="", prefix=" <- c(", indent=3, 
         suffix=')', sep='", "'){
   if(is.logical(wrap) & wrap==T) wrap <- floor(0.9*getOption("width"))
   if(prefix=="") suffix <- ""
   c.string <- paste(sep="", var.name, prefix, '"', 
                     paste(char.vector, collapse=sep), '"', suffix)
   if(wrap>0){
      split.up <- vector()
      lines <- 0
      while(nchar(c.string)>0){
         lines <- lines + 1
         split.at <- ifelse(lines==1, wrap-2, wrap-2-indent)
         splits <- gregexpr('", "', c.string)[[1]]
         if(nchar(c.string)<split.at | all(splits<0)){
            split.up[lines] <- c.string
            c.string <- ""
         }else{
            if(!any(splits<split.at)){
               at <- splits[1] + 1
            }else at <- splits[max(which(splits<split.at))] + 1
            split.up[lines] <- substr(c.string, 1, at)
            c.string <- substring(c.string, at+1)
         }
      }
      c.string <- split.up
      cat(c.string, sep=paste0("\n", paste(rep(" ",indent), collapse="")))
   }else cat(c.string,sep="","\n")
   return(invisible(c.string))
}
#===============================================================================
#' orString: Generate An OR Regular Expression Substring
#'
#' orString concatenates strings for use in regular expression, like "(a|b|c)"
#'
#' @param chars
#'        a character vector of strings to be concatenated
#'        
#' @return
#'    a character string, of the form "(a|b|c)",  where a, b and c are the
#'    elements of chars, concatentated
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
orString <-
function(chars) paste0("(", paste0(chars, collapse="|"), ")")
#===============================================================================
