% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/names.R
\name{extract_name}
\alias{extract_name}
\title{Extracts parts from character vectors of names using simple heuristics}
\usage{
extract_name(x, which = NULL)
}
\arguments{
\item{x}{a character vector}

\item{which}{the part of the name to be extracted. "last" extracts family names, "first" first names.}
}
\description{
Extracts parts from character vectors of names using simple heuristics
}
\examples{
extract_name("Dagobert Duck", "last")
extract_name("Dr. Martin Luther King", "first")
}

