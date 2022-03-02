#' Describe categorical variables associated with a raster file
#' 
#' @description Describes the categorical variable represented in a raster file. Use if any raster files have numberType of "categorical" in raster_attributes.txt.
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#' @param empty
#'     (logical) Whether template should be empty. If FALSE, raster_attrbutes.txt must exist and have at least one categorical type specified.
#' @param return.obj
#'     (logical) Whether to return table as an R object.
#'
#'
#' @return
#' \item{raster_catvars.txt}{Columns:
#'     \itemize{
#'     \item{filename: Filename of a spatial raster file}
#'     \item{code: Categorical variable}
#'     \item{definition: Definition of categorical variable}
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For raster files containing categorical variables as classified in the raster attributes template
#' template_raster_variables(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects")
#' }
#' @export
#' 
template_raster_variables <- function(
  path = NULL,
  data.path = path,
  write.file = TRUE,
  empty = FALSE,
  return.obj = FALSE) {
  
  message('Templating raster categorical variables ...')
 
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'template_raster_variables',
    fun.args = as.list(environment()))
  
  # Read raster attribute template
  
  if (!file.exists(paste0(path, '/raster_attributes.txt')) & isFALSE(empty)) {
    
    message('raster_attributes.txt does not exist')
    
  } else {
    
    # read table
    if (file.exists(paste0(path, '/raster_attributes.txt'))) {
      r <- data.table::fread(
        paste0(path, '/raster_attributes.txt'),
        colClasses = "character")
    } else {
      r <-  NULL
    }
    

    if(!is.null(r) & !"categorical" %in% r$numberType & isFALSE(empty)) {
      
      message('No categorical variables declared. Declare files with categorical variables by designating numberType as "categorical".')
      
    } else {
      
      if (!file.exists(paste0(path, '/raster_attributes.txt')) | !"categorical" %in% r$numberType) {
        output <- data.frame(
          filename = "",
          code = "",
          definition = "",
          stringsAsFactors = F)
        
        message("raster_attributes.txt does not exist or no categorical variables found.\nWriting empty raster_catvars.txt.")
      } else {
        
        cats <- subset(r, numberType == 'categorical')
        
        output <- data.frame(
          filename = cats$filename,
          code = rep("", nrow(cats)),
          definition = rep("", nrow(cats)),
          stringsAsFactors = F)
      }
      

# Write table to a file --------------------------------------------------------------
      if (!is.null(path)){
        if (isTRUE(write.file)) {
          
          suppressWarnings(
            utils::write.table(
              output,
              paste0(path, "/", "raster_catvars.txt"),
              sep = "\t",
              row.names = F,
              quote = F,
              fileEncoding = "UTF-8"))
          }
        }
      
# Return values -----------------------------------------------------------

      message("Done.")
      
      if (isTRUE(return.obj)){
        
          return(output)
          
      }
    }
  }
}