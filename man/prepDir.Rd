% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepDir.R
\name{prepDir}
\alias{prepDir}
\title{prepDir: Create A New Directory}
\usage{
prepDir(outdir, recurs = T)
}
\arguments{
\item{outdir}{character value with path of new directory to create}

\item{recurs}{logical; should parent directories be created?}
}
\description{
prepDir() checks whether a directory exists and tries to creates it if not
}
\details{
This function will check whether the specified directory exists.  If it does
not, it will try to create it; it will fail with an error if it can not.  If
it already exists, it will return a value of TRUE.
}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}
