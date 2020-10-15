#' Create provenance template
#'
#' @description  
#'     Use this function to create the provenance template for listing data entities (i.e. datasets, articles, etc.) that serve as data sources (inputs) to the derived dataset metadata is being created for.
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param empty
#'     (logical) Whether to write an empty template file. Default is \code{TRUE}.
#' @param write.file
#'     (logical; optional) Whether to write the template file. Default is \code{TRUE}.
#' @param return.obj
#'     (logical; optional) Whether to return the provenance template as a data frame. Default is \code{FALSE}.
#'     
#' @return
#' \item{provenance.txt}{The tab delimited provenance template written to \code{path} with the columns:
#' \itemize{
#'     \item{dataPackageID - Data package identifier. Supplying a valid packageID and systemID is all that is needed to create a complete provenance record.}
#'     \item{systemID - System (i.e. data repository) identifier. Currently supported systems are: EDI (Environmental Data Initiative).}
#'     \item{url - URL linking to an online source (i.e. data, paper, etc.). Required when a source can't be defined by a packageID and systemID.}
#'     \item{onlineDescription - Description of the data source. Required when a source can't be defined by a packageID and systemID.}
#'     \item{title - The source title. Required when a source can't be defined by a packageID and systemID.}
#'     \item{givenName - A creator or contacts given name. Required when a source can't be defined by a packageID and systemID.}
#'     \item{middleInitial - A creator or contacts middle initial. Required when a source can't be defined by a packageID and systemID.}
#'     \item{surName - A creator or contacts middle initial. Required when a source can't be defined by a packageID and systemID.}
#'     \item{role - "creator" and "contact" of the data source. Required when a source can't be defined by a packageID and systemID. Add both the creator and contact as separate rows within the template, where the information in each row is duplicated except for the givenName, middleInitial, surName (or organizationName), and role fields.}
#'     \item{organizationName - Name of organization the creator or contact belongs to. Required when a source can't be defined by a packageID and systemID.}
#'     \item{email - Email of the creator or contact. Required when a source can't be defined by a packageID and systemID.}
#' }
#' This table has a "long" (i.e. name-value) format accomodating multiple contacts and creators within a single provenance record.
#' }
#'     
#' @details 
#'     An existing provenance template will not be overwritten by a call to \code{template_provenance()}.
#'
#' @examples 
#' # Set working directory
#' setwd(tempdir())
#' 
#' # The most common use case is import of a blank template and manual completion
#' template_provenance(path = ".")
#' 
#' # Clean up
#' unlink('.', recursive = TRUE)
#'
#' @export
#'
template_provenance <- function(
  path = NULL, empty = TRUE, write.file = TRUE, return.obj = FALSE) {
  
  message("Templating provenance ...")
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'template_provenance',
    fun.args = as.list(environment()))
  
  # Write to file -------------------------------------------------------------
  
  if (!is.null(path)) {
    invisible(
      file.copy(
        from = system.file(
          "/templates/provenance.txt", 
          package = "EMLassemblyline"),
        to = path, 
        overwrite = FALSE))
  }

  # Return values -------------------------------------------------------------
  
  message("Done.")
  
  if (isTRUE(return.obj)){
    return(
      data.table::fread(
        system.file(
          "/templates/provenance.txt", 
          package = "EMLassemblyline")))
  }
  
}
