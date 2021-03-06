% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regextr.R
\name{regextr}
\alias{regextr}
\title{regextr: Substring Extraction By Regular Expression}
\usage{
regextr(pattern, x, L.trim = 0, R.trim = 0, ...)
}
\arguments{
\item{pattern}{character string specifying regular expression to be matched}

\item{x}{string or character vector of strings to be searched}

\item{L.trim}{integer; number of characters to strip from the start of the
matched substring}

\item{R.trim}{integer; number of characters to strip from the end of the
matched substring}

\item{...}{other arguments passed to regexpr()}
}
\value{
matched: a character vector the same length as x containing substrings of
      that match the pattern.  Only the first match found is returned.  If no
      match is found, an empty string ("") is returned in that element.
}
\description{
Extract substring(s) that match a regular expression in a character vector
}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}
