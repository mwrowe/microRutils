#' asciiToChar: conversion between letters and ASCII codes 
#' 
#' asciiToChar converts ASCII codes to vector of single characters and vice 
#' versa
#' 
#' @param ascii.codes
#'        An integer vector specifying individual characters by ASCII code, with 
#'        values in the range 1:255 OR a vector of individual characters
#' @return
#'   Returns a character vector where each element is a single character 
#'   specified by the corresponding ASCII code in ascii.codes OR a numeric
#'   vector of ASCII codes corresponding to the characters in ascii.codes
#'
#' @details
#'  \itemize{
#'    \item Useful for generating character sequences
#'    \item Missing values (NA) are ignored: asciiToChar(NA) -> NA
#'    \item This may be slow when converting characters to codes(?)
#'   }
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export 
asciiToChar <-
function(ascii.codes){
   if(is.numeric(ascii.codes)){
      ascii.codes[ascii.codes<1 | ascii.codes>255] <- NA
      chars <- rep(NA,length(ascii.codes))
      chars[!is.na(ascii.codes)] <- unlist(strsplit(
         rawToChar(as.raw(ascii.codes[!is.na(ascii.codes)])),split=""))
   }else if(is.character(ascii.codes)){
      if(any(nchar(ascii.codes))>1) 
         stop("ascii.codes may only contain single characters")
      chars <- rep(NA,length(ascii.codes))
      for(Ci in which(!is.na(ascii.codes))){
         chars[Ci] <- as.numeric(charToRaw(ascii.codes[Ci]))
      }
   }else if(all(is.na(ascii.codes))){
      char <- rep(NA,length(ascii.codes))
   }else stop("ascii.codes must be integers or single characters.")
   return(chars)
}
#-------------------------------------------------------------------------------
#' as.char: An Alias For as.character()
#'
#'as.char() is an alias for \code{\link{as.character}}().
#'
#' @inheritParams base::as.character
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
as.char <- function(x, ...) return(as.character(x, ...))
#-------------------------------------------------------------------------------
#' is.char: An Alias For is.character()
#' 
#' is.char() is an alias for \code{\link{is.character}}().
#'
#' @inheritParams base::is.character
#' 
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#' @export
is.char <- function(x) return(is.character(x))
#-------------------------------------------------------------------------------
