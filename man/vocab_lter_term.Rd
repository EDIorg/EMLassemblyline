% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{vocab_lter_term}
\alias{vocab_lter_term}
\title{Search for an LTER Controlled Vocabulary term}
\usage{
vocab_lter_term(x, messages = FALSE, interactive = FALSE)
}
\arguments{
\item{x}{(character) A term to search for.}

\item{messages}{(logical) Display diagnostic messages, e.g. alternative spelling options.}

\item{interactive}{(logical) Query user to select from alternative terms and returns back
selection.}
}
\value{
Logical value (TRUE/FALSE) indicating whether the searched term could
    be found. 
    
    If messages = TRUE, then alternative spellings and near misses 
    are displayed. 
    
    If interactive mode = TRUE, then a user selected term is returned.
}
\description{
Search for a term in the LTER Controlled Vocabulary (Long Term 
    Ecological Research Network).
}
\keyword{internal}
