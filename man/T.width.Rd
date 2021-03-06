% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/T.width.R
\name{T.width}
\alias{T.width}
\title{Determine Width of Caps On Error Bars}
\usage{
T.width(fraction = 0.01, axis = 1)
}
\arguments{
\item{fraction}{A number between zero and one.}

\item{axis}{A number specifying which axis to return the fraction of, 1 for x or 
2 for y}
}
\value{
Returns a numeric vector with the requested fraction of the current plot's 
  actual x-axis limits from \code{\link[graphics]{par}}("usr").
}
\description{
T.width returns a fraction of the range of the current plot
}
\details{
This is meant to be useful for determining the width of the T's at the end 
of error bars.
}
