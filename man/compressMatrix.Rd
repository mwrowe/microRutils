% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compressMatrix.R
\name{compressMatrix}
\alias{compressMatrix}
\title{Resize a Matrix By Combining Adjacent Elements}
\usage{
compressMatrix(X, howmuch = 4, FUN = mean, na.rm = T, ...)
}
\arguments{
\item{X}{A matrix or object that can be coerced to such (array, data.frame), 
typically numeric, though other types are possible depending on the 
FUN argument.}

\item{howmuch}{One or two element integer vector specifying compression factor
in each dimension.  If length(howmuch)==1, both dimensions will be
reduced by the same factor.}

\item{FUN}{Function to apply for combining values.}

\item{na.rm}{Logical; if TRUE, remove missing values from X prior to calling FUN}

\item{...}{Additional arguments passed to FUN}
}
\value{
Returns a matrix with dimensions dim(X) / howmuch, where each element is
  average of howmuch[1] neighboring rows and howmuch[2] neighboring
  columns.  If the original dimensions of X are not evenly divisible
  by howmuch, additional rows and/or columns of missing values will be 
  appended.
}
\description{
compressMatrix() resizes a matrix by averaging groups of adjacent
rows/columns (typically averaging or summing).
}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}
