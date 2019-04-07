#' Import template(s) for categorical variable definitions
#'
#' @description  
#'     Import template(s) for defining categorical variable codes used in data
#'     tables. This function automatically extracts and returns categorical 
#'     codes, the definitions of which must be supplied by a human. 
#'     Instructions for filling out the template are at
#'     \url{https://clnsmth.github.io/EMLassemblyline/articles/instructions.html}.
#'
#' @usage define_catvars(path, data.path = path, x = NULL, write.file = TRUE)
#'
#' @param path 
#'     (character) Path to where the template(s) will be imported.
#' @param data.path
#'     (character) Path to where the data files are stored.
#' @param x
#'     (named list) Alternative input to all `EMLassemblyline` functions (i.e. 
#'     rather than supplying the files themselves). Use 
#'     `read_files()` to create this list, then use `import_templates()` to 
#'     populate additional content.
#' @param write.file
#'     (logical) Write `catvars` file to `path`.
#'
#' @return 
#'     \itemize{
#'         \item{`Status messages` describing the catvars creation status}
#'         \item{`catvars file` A tab delimited file written to `path`
#'         containing codes to be defined. Each template is appended with the 
#'         name of the data table from which the codes were extracted 
#'         (`catvars_*.txt`)}
#'     }
#'     
#' @details 
#'     `define_catvars` knows which variables are `categorical` based on their 
#'     listing under the `class` column in the `attributes_*.txt` file(s). 
#'     
#'     Existing template(s) will not be overwritten by subsequent calls to 
#'     `define_catvars`.
#'
#' @export
#'

define_catvars <- function(path, data.path = path, x = NULL, 
                           write.file = TRUE) {
  
  message('Creating templates for categorical variables')
  
  # Import content from x -----------------------------------------------------

  # If x exists ...

  if (!is.null(x)){
    
    # Validate input arguments
    
    if (!missing(path)){
      stop('Input argument "path" is not allowed when using "x".')
    }
    
    if (!missing(data.path)){
      stop('Input argument "data.path" is not allowed when using "x".')
    }
    
    if (isTRUE(write.file)){
      stop('Input argument "write.file" must be FALSE when using "x".')
    }
    
    # Add arguments
    
    path <- x$template[[1]]$path
    
    # Validate x content ...

  }
  
  # Import content from path and data.path ------------------------------------
  
  # If x doesn't exist ...
  
  if (is.null(x)){
    
    # Validate input arguments ...
    
    if (missing(path)){
      stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
    }
    
    # Detect operating system
    
    os <- EDIutils::detect_os()
    
    # Get attribute file names and data file names

    files <- list.files(path)
    use_i <- stringr::str_detect(string = files,
                                 pattern = "^attributes")
    if (sum(use_i) == 0){
      stop('There are no attributes.txt files in your dataset working directory. Please fix this.')
    }
    attribute_files <- files[use_i]
    table_names_base <- stringr::str_sub(string = attribute_files,
                                         start = 12,
                                         end = nchar(attribute_files)-4)
    data_files <- list.files(data.path)
    use_i <- stringr::str_detect(string = data_files,
                                 pattern = stringr::str_c("^", table_names_base, collapse = "|"))
    table_names <- data_files[use_i]
    data_files <- table_names
    
    # Send warning if data table name is repeated more than once
    
    if (length(unique(tools::file_path_sans_ext(data_files))) != length(data_files)){
      stop('Duplicate data file names exist in this directory. Please remove duplicates, even if they are a different file type.')
    }
    
    # Validate fields of data.files
    
    EDIutils::validate_fields(path = data.path, data.files = data_files)
    
    # Set file names to be written
    
    fname_table_catvars <- stringr::str_c("catvars_", table_names_base, ".txt")
    
    # Detect field delimiters of data files
    
    delim_guess <- EDIutils::detect_delimeter(path = data.path, data.files = data_files, os)
    
    # Read templates and data.table into list
    
    x <- read_files(
      path = path,
      data.path = data.path,
      data.table = data_files
    )
    
    data_read_2_x <- TRUE
    
  }
  
  # Loop through data tables --------------------------------------------------
  
  table_names <- names(x$data.table)
  
  fname_table_catvars <- paste0(
    'catvars_',
    substr(names(x$data.table), 1, (nchar(names(x$data.table))-4)),
    '.txt'
  )
  
  attribute_files <- names(x$template)[
    stringr::str_detect(
      names(x$template),
      'attributes_[:graph:]*.txt$'
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

        if (isTRUE(write.file)){
          
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
              content = catvars,
              path = path
            )
            
            missing_template <- list(
              missing_template
            )
            
            names(missing_template) <- fname_table_catvars[i]
            
            x$template <- c(
              x$template, 
              missing_template
            )

          } else {
            
            message(
              paste0(
                fname_table_catvars[i],
                " already exists!"
              )
            )
            
          }
          
        }

      } else {
        
        message("No categorical variables found.")
        
        catvars <- NULL
        
      }
      
    }

  }
  
  if (!isTRUE(write.file) & exists('data_read_2_x')){
    
    message('write.file = FALSE, no templates were written')
    
  }

  message("Done.")
  
  # Return
  
  if (!exists('data_read_2_x')){
    x
  }

}

