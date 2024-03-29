% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_templates.R
\name{validate_categorical_variable_column_names}
\alias{validate_categorical_variable_column_names}
\title{Check column names of categorical variables template}
\usage{
validate_categorical_variable_column_names(file.name, x)
}
\arguments{
\item{file.name}{(character) The categorical variables template to apply this function 
to. Full file name is required.}

\item{x}{(named list) The data and metadata object returned by 
\code{template_arguments()}.}
}
\value{
(error or NULL) An error of issues if any were found, otherwise NULL.
}
\description{
Check column names of categorical variables template
}
\keyword{internal}
