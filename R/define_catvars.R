#' Import template for categorical variables
#'
#' @description  
#'     Import template for defining categorical variables used in data a
#'     table. This function automatically extracts and returns categorical 
#'     codes, the definitions of which are supplied by the user. 
#'     \href{https://ediorg.github.io/EMLassemblyline/articles/edit_metadata_templates.html}{Instructions for editing the template.}
#'
#' @usage 
#'     define_catvars(
#'       path,
#'       data.path = path,
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the directory containing table attributes 
#'     template(s), and where the categorical variable template(s) will be
#'     written.
#' @param data.path
#'     (character) Path to the directory containing the data table with 
#'     categorical variables.
#' @param write.file
#'     (logical; optional) Whether to write the categorical variable template
#'     to \code{path}.
#' @param x
#'     (named list; optional) Alternative input to \code{EMLassemblyline} 
#'     functions. Use \code{template_arguments()} to create \code{x}.
#'
#' @return 
#'     \itemize{
#'         \item{\strong{catvars_*.txt} The categorical variable 
#'         template. A tab delimited table.}
#'         \item{If using \code{x}, then the categorical variable template is 
#'         added to \strong{/x/templates}.}
#'     }
#'     
#' @details 
#'     \code{define_catvars()} knows which variables are \code{categorical} 
#'     based on their listing in the \code{class} column of the 
#'     attributes.txt template. 
#'     
#'     An existing categorical variables template will not be overwritten 
#'     by subsequent calls to \code{define_catvars()}.
#'
#' @export
#'

define_catvars <- function(path, data.path = path, write.file = TRUE, 
                           x = NULL) {
  
  message('Creating categorical variable template.')
  
  # Send deprecation notice ---------------------------------------------------
  
  .Deprecated(
    new = 'template_categorical_variables',
    package = 'EMLassemblyline',
    old = 'define_catvars'
  )
  
  # Validate arguments and import data ----------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  # When not using x, inputs are expected from path and data.path. 
  # When using x, only data.path is used. Ignored are path and write.file.
  
  if (is.null(x) & missing(path)){
    stop('Input argument "path" is missing.')
  } else if (!is.null(x) & missing(path)){
    path <- NULL
    if (missing(data.path)){
      stop('Input argument "data.path" is missing.')
    }
  }
  
  # Pass remaining arguments to validate_arguments().
  
  validate_arguments(
    fun.name = 'define_catvars',
    fun.args = as.list(environment())
  )
  
  # If not using x ...
  
  if (is.null(x)){
    
    # Get attribute file names and data file names

    files <- list.files(path)
    use_i <- stringr::str_detect(string = files,
                                 pattern = "^attributes")
    
    attribute_files <- files[use_i]
    table_names_base <- stringr::str_sub(string = attribute_files,
                                         start = 12,
                                         end = nchar(attribute_files)-4)
    data_files <- list.files(data.path)
    use_i <- stringr::str_detect(string = data_files,
                                 pattern = stringr::str_c("^", table_names_base, collapse = "|"))
    table_names <- data_files[use_i]
    data_files <- table_names

    # Read templates and data.table into list
    
    x <- template_arguments(
      path = path,
      data.path = data.path,
      data.table = data_files
    )
    
    x <- x$x
    
    data_read_2_x <- TRUE
    
  }

  # Extract categorical variables and write to file ---------------------------
  
  table_names <- names(x$data.table)
  
  fname_table_catvars <- paste0(
    'catvars_',
    substr(names(x$data.table), 1, (nchar(names(x$data.table))-4)),
    '.txt'
  )
  
  attribute_files <- names(x$template)[
    stringr::str_detect(
      names(x$template),
      "attributes_(?!dataset).*.txt"
    )
  ]
  
  files <- names(x$template)
  
  for (i in 1:length(attribute_files)){
    
    use_i <- stringr::str_detect(
      string = files,
      pattern = fname_table_catvars[i]
    )
    
    if (sum(use_i) > 0){
      
      message(paste(files[use_i], "already exists! Skipping this one."))
      
      catvars <- NULL
      
    } else {
      
      # Read attributes_datatablename.txt

      df_attributes <- x$template[[attribute_files[i]]]$content
      
      # Build catvars table
      
      catvars_I <- which(df_attributes$class %in% "categorical")
      
      # Read data table
      
      df_table <- x$data.table[[table_names[i]]]$content

      # If there are no catvars then skip to the next file
      
      if (length(catvars_I) > 0){

        rows <- 0
        for (j in 1:length(catvars_I)){
          factor_names <- unique(
            eval(
              parse(
                text = paste(
                  "df_table",
                  "$",
                  df_attributes$attributeName[catvars_I[j]],
                  sep = ""))))
          
          rows <- length(factor_names) + rows
          
        }
        
        catvars <- data.frame(attributeName = character(rows),
                              code = character(rows),
                              definition = character(rows),
                              stringsAsFactors = F)
        
        row <- 1
        for (j in 1:length(catvars_I)){
          
          factor_names <- unique(
            eval(
              parse(
                text = paste(
                  "df_table",
                  "$",
                  df_attributes$attributeName[catvars_I[j]],
                  sep = ""))))
          
          catvars$attributeName[row:(length(factor_names)+row-1)] <-
            df_attributes$attributeName[catvars_I[j]]
          
          catvars$code[row:(length(factor_names)+row-1)] <- factor_names
          
          row <- row + length(factor_names)
          
        }
        
        # Remove rows with empty codes
        
        use_i <- catvars$code == ""
        if (sum(use_i, na.rm = T) > 0){
          use_i <- match("", catvars$code)
          index <- seq(length(catvars$code))
          use_i <- index %in% use_i
          catvars <- catvars[!use_i, ]
        }

        # Write template to file

        if (isTRUE(write.file) & exists('data_read_2_x')){
          
          message(paste("Writing", fname_table_catvars[i]))
          suppressWarnings(utils::write.table(catvars,
                                              paste(path,
                                                    "/",
                                                    fname_table_catvars[i],
                                                    sep = ""),
                                              sep = "\t",
                                              row.names = F,
                                              quote = F,
                                              fileEncoding = "UTF-8"))
        
        # Add template to x
          
        } else if (!exists('data_read_2_x')){
          
          value <- stringr::str_detect(
            names(x$template),
            fname_table_catvars[i]
          )
          
          if (!any(value)){
            
            message(
              paste0(
                "Adding ",
                fname_table_catvars[i],
                ' to x'
              )
            )
            
            missing_template <- list(
              content = catvars
            )
            
            missing_template <- list(
              missing_template
            )
            
            names(missing_template) <- fname_table_catvars[i]
            
            x$template <- c(
              x$template, 
              missing_template
            )

          }
          
        }

      }
      
    }

  }

  message("Done.")
  
  # Return
  
  if (!exists('data_read_2_x')){
    
    x
    
  }

}
