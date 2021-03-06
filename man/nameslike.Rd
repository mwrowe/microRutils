% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nameslike.R
\name{nameslike}
\alias{nameslike}
\title{nameslike: Find Named Elements, Rows or Columns Using A Regular Expression}
\usage{
nameslike(pattern, x, DIM = 0, ignore.case = T)
}
\arguments{
\item{pattern}{character string containing a regular expression to match}

\item{x}{an object with named elements along at least one dimension; may be
a vector, list, matrix, array, data.frame, table, etc.}

\item{DIM}{integer indicating dimension to search for names that match the 
pattern.  Switches which function gets called to get the names:
\itemize{
  \item 0: names (default); works on vectors, lists and data.frame 
     columns
  \item 1: rownames; for rows of data.frames, matrices or arrays
  \item 2: colnames; for columns of data.frames, matrices or arrays
  \item >2: dimnames; for any case where the dimnames function works, 
    such as matrices or arrays.  May be used to access higher order
    dimensions.
  \item NA: dimnames; return results from all dimensions
}}

\item{ignore.case}{logical; should case be ignored?  Search is case-insensitive by 
default.}
}
\value{
character string, or list of character strings if is.na(DIM), 
   containing names that match the specified pattern.  Returns an empty 
   vector if no match is found or dimension is unnamed.  Returns NULL if the  
   specified dimension does not exist.
}
\description{
nameslike() returns element names of x that match a regular expression
}
\examples{
hec <- datasets::HairEyeColor
cat("\nEXAMPLE: HairEyeColor is a table object in the datasets package:",
    "\n\n> dimnames(hec)\n\n")
print(dimnames(hec))
for(DIM in c(0:4,NA)){
   cat('\n> nameslike("bl", HairEyeColor, DIM=', DIM, sep="", "):\n")
   print(nameslike("bl", hec, DIM=DIM))
}

}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}
