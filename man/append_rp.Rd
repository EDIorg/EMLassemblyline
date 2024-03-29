% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_eml.R
\name{append_rp}
\alias{append_rp}
\title{Assign ResponsibleParty IDs and append recurrences to the rp data frame}
\usage{
append_rp(responsible.party, annotations.template)
}
\arguments{
\item{responsible.party}{(emld list) A ResponsibleParty EML node in the emld list format.}

\item{annotations.template}{(data.frame) The annotations template created within 
\code{annotate_eml()}.}
}
\value{
\item{responsible.party}{(emld list) Annotated responsible party node}
    \item{rp}{(data.frame) the rp object}
}
\description{
This is a helper function for \code{annotate_eml()}
}
\keyword{internal}
