% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_templates.R
\name{validate_table_attribute_name_order}
\alias{validate_table_attribute_name_order}
\title{Check all column names are listed in the same order as in the data}
\usage{
validate_table_attribute_name_order(template.name, data.name, x)
}
\arguments{
\item{template.name}{(character) The table attributes template to apply this function 
to. Full file name is required.}

\item{data.name}{(character) The data table to apply this function to. Full file name 
is required.}

\item{x}{(list) The data and metadata object returned by 
\code{template_arguments()}.}
}
\value{
\item{character}{Description of validation issues}
    \item{NULL}{If no issues were found}
}
\description{
Check all column names are listed in the same order as in the data
}
\details{
This function will not run unless all attributes listed in 
    \code{template.name} are also listed in \code{data.name}.
}
\keyword{internal}
