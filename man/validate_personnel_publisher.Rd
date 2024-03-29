% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_templates.R
\name{validate_personnel_publisher}
\alias{validate_personnel_publisher}
\title{Check for one publisher in the personnel template}
\usage{
validate_personnel_publisher(x)
}
\arguments{
\item{x}{(list) The data and metadata object returned by 
\code{template_arguments()}.}
}
\value{
\item{character}{Description of validation issues, or NULL if no issues
    were found}
    \item{x}{(list) A potentially modified \code{x} if relevant issues were
    found}
}
\description{
Check for one publisher in the personnel template
}
\details{
Only one publisher is allowed.
}
\keyword{internal}
