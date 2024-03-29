% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eml2eal.R
\name{is_char}
\alias{is_char}
\title{Is attribute a character?}
\usage{
is_char(nodeset)
}
\arguments{
\item{nodeset}{(xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}}
}
\value{
(logical) TRUE if nodeset has \code{./measurementScale/nominal/nonNumericDomain/textDomain} or \code{./measurementScale/ordinal/nonNumericDomain/textDomain}
}
\description{
Is attribute a character?
}
\keyword{internal}
