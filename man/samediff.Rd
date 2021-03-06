% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samediff.R
\name{samediff}
\alias{samediff}
\title{samediff: Find Elements in Common, and Unique To, Two Lists}
\usage{
samediff(a, b, sort = T, na.rm = F, use.arg.names = F, Ns = FALSE)
}
\arguments{
\item{a}{a vector or other object that can be treated as a vector OR a 
two-element list of vectors; if a is a list, all subsequent
arguments must be named and b will be ignored.  If 
use.arg.names==TRUE, the names of the list elements will be used to 
label the output}

\item{b}{a second vector or other object that can be treated as a vector unless
a is a two-element list, in which case b is ignored.}

\item{sort}{logical; determines whether the elements of A and B will be returned
in alphabetical order}

\item{na.rm}{logical; should missing values be removed from A and B?}

\item{use.arg.names}{if true, the names of the variables passed in as argumentsa and b to
this function will be substituted for the default names "a.only" and 
"b.only" in the list returned.  Default FALSE.}

\item{Ns}{logical; if TRUE, return a vector of the numbers of item in common and 
unique to a and b}
}
\value{
\itemize{
    \item A list, with three named character strings:
      \itemize{
        \item "in.both": intersection of a and b
        \item "a.only": (or the name of a, if use.arg.names==T): elements 
          unique to a
        \item "b.only": (or the name of b, if use.arg.names==T): elements 
          unique to b
       }
    \item ...OR a named numeric vector with the numbers of items in common, 
       unique to a and unique to b, with the same names as above
   }
}
\description{
samediff returns a list with elements of a and b in common, and unique to 
each, or numbers of unique elements in such a list.
}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}
