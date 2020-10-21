#' Validate arguments of EMLassemblyline functions
#'
#' @description
#'     Validate input arguments to `EMLassemblyline` functions.
#'
#' @param fun.name
#'     (character) Function name passed to `validate_x` with 
#'     `as.character(match.call()[[1]])`.
#' @param fun.args
#'     (named list) Function arguments and values passed to `validate_x` with 
#'     `as.list(environment())`.
#'     
#' @details
#'     Validation checks are function specific.    
#'
validate_arguments <- function(fun.name, fun.args){
  
  # Parameterize --------------------------------------------------------------
  
  use_i <- sapply(fun.args, function(X) identical(X, quote(expr=)))
  fun.args[use_i] <- list(NULL)
  
  # Call from annotate_eml() --------------------------------------------------
  
  if (fun.name == 'annotate_eml'){
    
    # annotations
    
    if (is.null(fun.args$annotations)) {
      stop('Input argument "annotations" is missing.', call. = FALSE)
    }
    
    if (is.character(fun.args$annotations)) {
      if (!file.exists(fun.args$annotations)) {
        stop(
          paste0(
            "The file specified by the input argument 'annotations' doesn't ",
            "exist."
          ), 
          call. = FALSE
        )
      } 
    } else if (!is.data.frame(fun.args$annotations)) {
      stop(
        "Input argument 'annotations' is not one of the two supported types.", 
        call. = FALSE
      )
    }
    
    # eml.in
    
    if (is.null(fun.args$eml.in)) {
      stop('Input argument "eml.in" is missing.', call. = FALSE)
    }
    
    if (is.character(fun.args$eml.in)) {
      if (!file.exists(fun.args$eml.in)) {
        stop(
          paste0(
            "The file specified by the input argument 'eml.in' doesn't ",
            "exist."
          ), 
          call. = FALSE
        )
      } else {
        eml <- EML::read_eml(fun.args$eml.in)
        if (!EML::eml_validate(eml)) {
          stop(
            "Input argument 'eml.in' is not a valid EML record.", 
            call. = FALSE
          )
        }
      }
    } else if (!any(class(fun.args$eml.in) %in% c("emld", "list"))) {
      stop(
        "Input argument 'eml.in' is not one of the two supported types.", 
        call. = FALSE
      )
    }
    
    # eml.out
    
    if (is.character(fun.args$eml.in) & is.null(fun.args$eml.out)) {
      stop(
        "Input argument 'eml.out' is missing.",
        call. = FALSE
      )
    }
    
    if (!is.null(fun.args$eml.out) & is.character(fun.args$eml.out)) {
      if (!dir.exists(dirname(fun.args$eml.out))) {
        stop(
          "Input argument 'eml.out' is a non-existant path.", 
          call. = FALSE
        )
      }
    }
    
  }
  
  # Call from define_catvars() ------------------------------------------------
  
  if (fun.name == 'define_catvars'){
    
    # If not using x ...
    
    if (is.null(fun.args$x)){
      
      # Get attribute file names and data file names
      
      files <- list.files(fun.args$path)
      use_i <- stringr::str_detect(string = files,
                                   pattern = "^attributes")
      if (sum(use_i) == 0){
        stop('There are no attributes.txt files in your dataset working directory. Please fix this.')
      }
      
      attribute_files <- files[use_i]
      table_names_base <- stringr::str_sub(string = attribute_files,
                                           start = 12,
                                           end = nchar(attribute_files)-4)
      data_files <- list.files(fun.args$data.path)
      use_i <- stringr::str_detect(string = data_files,
                                   pattern = stringr::str_c("^", table_names_base, collapse = "|"))
      table_names <- data_files[use_i]
      data_files <- table_names
      
      # Send warning if data table name is repeated more than once
      
      if (length(unique(tools::file_path_sans_ext(data_files))) != length(data_files)){
        stop('Duplicate data file names exist in this directory. Please remove duplicates, even if they are a different file type.')
      }
    
    # If using x ...
      
    } else if (!is.null(fun.args$x)){
      
      # Get attribute file names and data file names
      
      files <- names(fun.args$x$template)
      use_i <- stringr::str_detect(string = files,
                                   pattern = "^attributes")
      if (sum(use_i) == 0){
        stop('There are no attributes.txt files in your dataset working directory. Please fix this.')
      }
      
      attribute_files <- files[use_i]
      
      table_names_base <- stringr::str_sub(string = attribute_files,
                                           start = 12,
                                           end = nchar(attribute_files)-4)
      
      data_files <- names(fun.args$x$data.table)
      
      use_i <- stringr::str_detect(string = data_files,
                                   pattern = stringr::str_c("^", table_names_base, collapse = "|"))
      
      table_names <- data_files[use_i]
      data_files <- table_names
      
      # Send warning if data table name is repeated more than once
      
      if (length(unique(tools::file_path_sans_ext(data_files))) != length(data_files)){
        stop('Duplicate data file names exist in this directory. Please remove duplicates, even if they are a different file type.')
      }
      
    }

  }
  
  # Call from extract_geocoverage() -------------------------------------------
  
  if (fun.name == 'extract_geocoverage'){
    
    # Handle deprecated arguments
    
    if (!is.null(fun.args$data.file)){
      fun.args$data.table <- fun.args$data.file
    }

    # data.table
    
    if (is.null(fun.args$data.table)){
      stop('Input argument "data.table" is missing! Specify the data file containing the geographic coordinates.')
    }
    
    # lat.col
    
    if (is.null(fun.args$lat.col)){
      stop('Input argument "lat.col" is missing! Specify latitude column name.')
    }
    
    # lon.col
    
    if (is.null(fun.args$lon.col)){
      stop('Input argument "lon.col" is missing! Specify longitude column name.')
    }
    
    # site.col
    
    if (is.null(fun.args$site.col)){
      stop('Input argument "site.col" is missing! Specify site column name.')
    }

  }
  
  # Call from import_templates() ----------------------------------------------
  
  if (fun.name == 'import_templates'){
    
    # Handle deprecated arguments
    
    if (!is.null(fun.args$data.files)){
      fun.args$data.table <- fun.args$data.files
    }

    # license
    
    if (is.null(fun.args$license)){
      stop('Input argument "license" is missing')
    }
    
    license.low <- tolower(fun.args$license)
    
    if (!stringr::str_detect(license.low, "^cc0$|^ccby$")){
      stop('Invalid value entered for the "license" argument. Please choose "CC0" or "CCBY".')
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      
      # Validate table names
      
      data_files <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$data.table
        )
      )

    }
    
  }
  
  # Call from make_eml() ------------------------------------------------------
  
  if (fun.name == 'make_eml'){
    
    # Handle deprecated arguments
    
    if (!is.null(fun.args$affiliation)){
      fun.args$user.domain <- fun.args$affiliation
    }
    
    if (!is.null(fun.args$data.files)){
      fun.args$data.table <- fun.args$data.files
    }
    
    if (!is.null(fun.args$data.files.description)){
      fun.args$data.table.description <- fun.args$data.files.description
    }
    
    if (!is.null(fun.args$data.files.quote.character)){
      fun.args$data.table.quote.character <- fun.args$data.files.quote.character
    }
    
    if (!is.null(fun.args$data.files.url)){
      fun.args$data.url <- fun.args$data.files.url
    }
    
    if (!is.null(fun.args$zip.dir)){
      fun.args$other.entity <- fun.args$zip.dir
    }
    
    if (!is.null(fun.args$zip.dir.description)){
      fun.args$other.entity.description <- fun.args$zip.dir.description
    }
    
    # path
    
    if (is.null(fun.args$x)) {
      EDIutils::validate_path(fun.args$path)
    }
    
    # data.path
    
    EDIutils::validate_path(fun.args$data.path)  
    
    # eml.path
    
    if (isTRUE(fun.args$write.file)){
      EDIutils::validate_path(fun.args$eml.path)
    }
    
    # dataset.title
    
    if (is.null(fun.args$dataset.title)) {
      warning('A dataset title is required.', call. = FALSE)
    }

    # data.table
    
    if (!is.null(fun.args$data.table)) {
      table_names <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$data.table))
    }
    
    # data.table.description
    
    if (!is.null(fun.args$data.table)) {
      if (is.null(fun.args$data.table.description)) {
        warning('Data table descriptions are recommended.', call. = FALSE)
      }
      if (length(fun.args$data.table) > 
          length(fun.args$data.table.description)) {
        warning(
          'One or more data table descriptions are missing.', call. = FALSE)
      }
    }
    
    # data.table.name
    
    if (!is.null(fun.args$data.table.name)) {
      if (all(fun.args$data.table %in% fun.args$data.table.name)) {
        warning(
          "Data table names are missing. A short name for each data table is ",
          "recommended. Defaulting to data table file names.", call. = FALSE)
      }
    }
    
    # data.table.name - If uneven, default missing to data.table file name
    
    if (!is.null(fun.args$data.table) & !is.null(fun.args$data.table.name)) {
      if (length(fun.args$data.table.name) < length(fun.args$data.table)) {
        warning(
          "One or more data table names are missing. Defaulting to data table ",
          "file names", call. = FALSE)
      }
    }
    
    # data.table.quote.character
    
    if (!is.null(fun.args$data.table)) {
      if (!is.null(fun.args$data.table.quote.character)) {
        if (length(fun.args$data.table.quote.character) < 
            length(fun.args$data.table)) {
          warning(
            "One or more data table quote characters are missing.",
            call. = FALSE)
        }
      }
    }
    
    # data.table.url
    
    if (!is.null(fun.args$data.table.url)) {
      if (length(fun.args$data.table.url) < length(fun.args$data.table)) {
        warning("One or more data table URLs are missing.", call. = FALSE)
      }
    }
    
    # geographic.coordinates and geographic.description
    
    if (!is.null(fun.args$geographic.coordinates) & 
        is.null(fun.args$geographic.description)) {
      warning('Geographic description is missing.', call. = FALSE)
    }
    
    if (is.null(fun.args$geographic.coordinates) & 
        !is.null(fun.args$geographic.description)){
      warning('Geographic coordinates are missing.', call. = FALSE)
    }

    # maintenance.description
    
    if (is.null(fun.args$maintenance.description)) {
      warning(
        paste0(
          'A maintenance description is recommended. Describe the collection ',
          'status of this dataset (e.g. "Ongoing collection with quarterly ', 
          'updates", or "Completed collection, updates to these data are not ',
          'expected.")'), call. = FALSE)
    }
    
    # other.entity
    
    if (!is.null(fun.args$other.entity)) {
      other_entity_names <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$other.entity))
    }

    # other.entity.description
    
    if (!is.null(fun.args$other.entity)) {
      if (is.null(fun.args$other.entity.description)) {
        warning('Other entity descriptions are recommended.', call. = FALSE)
      }
      if (length(fun.args$other.entity) > 
          length(fun.args$other.entity.description)) {
        warning(
          'One or more other entity descriptions are missing.', call. = FALSE)
      }
    }
    
    # other.entity.name
    
    if (!is.null(fun.args$other.entity.name)) {
      if (all(fun.args$other.entity %in% fun.args$other.entity.name)) {
        warning(
          "Other entity names are missing. A short name for each other entity is ",
          "recommended. Defaulting to other entity file names.", call. = FALSE)
      }
    }
    
    # other.entity.name - If uneven, default missing to other.entity file name
    
    if (!is.null(fun.args$other.entity) & !is.null(fun.args$other.entity.name)) {
      if (length(fun.args$other.entity.name) < length(fun.args$other.entity)) {
        warning(
          "One or more other entity names are missing. Defaulting to other entity ",
          "file names", call. = FALSE)
      }
    }
    
    # other.entity.url
    
    if (!is.null(fun.args$other.entity.url)) {
      if (length(fun.args$other.entity.url) < length(fun.args$other.entity)) {
        warning("One or more other entity URLs are missing.", call. = FALSE)
      }
    }
    
    # package.id
    
    if (!is.null(fun.args$package.id) & !is.null(fun.args$user.domain)) {
      if (all(tolower(fun.args$user.domain) %in% c("edi", "lter"))) {
        if (!isTRUE(stringr::str_detect(
          fun.args$package.id, '[:alpha:]\\.[:digit:]+\\.[:digit:]'))) {
          warning(
            "Warning: 'package.id' is not valid for EDI or LTER. Expected is ",
            "the form 'edi.xxx.x' (EDI) or 'knb-lter-ccc.xxx.x' (LTER).", 
            call. = FALSE)
        }
      }
    }
    
    # provenance - Warn about unrecognized identifiers
    
    if (!is.null(fun.args$provenance)){
      use_i <- stringr::str_detect(
        fun.args$provenance, 
        "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")
      if (!all(use_i)){
        warning(
          "Unable to generate provenance metadata for unrecognized identifier:\n",
          paste(fun.args$provenance[!use_i], collapse = ", "), call. = FALSE)
      }
    }

    # temporal.coverage
    
    if (is.null(fun.args$temporal.coverage)) {
      warning('Temporal coverage is recommended', call. = FALSE)
    }
    if (length(fun.args$temporal.coverage) != 2) {
      warning('Temporal coverage requires a begin and end date', call. = FALSE)
    }
    # TODO: Validate date format
  }
  
  # Call from template_annotations() ------------------------------------------
  
  if (fun.name == 'template_annotations'){
    
    # path
    
    if (is.null(fun.args$path)) {
      stop('Input argument "path" is missing.', call. = FALSE)
    }
    
    if (!is.null(fun.args$path)) {
      EDIutils::validate_path(fun.args$path)
    }
    
    if (file.exists(paste0(fun.args$path, "/annotations.txt"))) {
      stop(
        paste0(
          "annotations.txt already exists. To create a new annotations ",
          "template, remove this one from 'path'."
        ), 
        call. = FALSE
      )
    }
    
    # data.path
    
    if (!is.null(fun.args$data.path)) {
      EDIutils::validate_path(fun.args$data.path)
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      table_names <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$data.table
        )
      )
    }
    
    # other.entity
    
    if (!is.null(fun.args$other.entity)){
      table_names <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$other.entity
        )
      )
    }
    
    # default.annotations
    
    if (!is.null(fun.args$default.annotations)){
      
      if (!is.data.frame(fun.args$default.annotations)) {
        stop(
          "Input argument 'default.annotations' is not a data frame.", 
          call. = FALSE
        )
      }
      
      if (!all(c("element", "predicate_label", "predicate_uri", "object_label", 
          "object_uri") %in% colnames(fun.args$default.annotations))) {
        stop(
          paste0(
            "The 'default.annotations' data frame is missing one or more of ",
            "these columns: 'element', 'predicate_label', 'predicate_uri', ",
            "'object_label', 'object_uri'"
          ),
          call. = FALSE
        )
      }
      
    }
    
    # eml
    
    if (!is.null(fun.args$eml)) {
      if (!file.exists(paste0(fun.args$eml.path, "/", fun.args$eml))) {
        stop(
          paste0("The file ", fun.args$eml, " cannot be found at 'path'."),
          call. = FALSE
        )
      }
      
    }

  }
  
  # Call from template_arguments() ------------------------------------------------
  
  if (fun.name == 'template_arguments') {
    
    # path
    
    if (!is.null(fun.args$path)) {
      EDIutils::validate_path(fun.args$path)
      attr_tmp <- read_template_attributes()
      path_files <- list.files(fun.args$path)
      if (!length(path_files) == 0) {
        is_template <- rep(FALSE, length(path_files))
        for (i in 1:length(path_files)){
          is_template[i] <- any(
            stringr::str_detect(path_files[i], attr_tmp$regexpr))
        }
        if (!any(is_template)) {
          stop("No metadata templates found at 'path'.", call. = F)
        }
      } else {
        stop("No metadata templates found at 'path'.", call. = F)
      }
      check_duplicate_templates(fun.args$path)
    }
    
    # data.path
    
    if (!is.null(fun.args$data.path)){
      EDIutils::validate_path(fun.args$data.path)
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      output <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path,
          data.files = fun.args$data.table
        )
      )
    }
    
    # other.entity
    
    if (!is.null(fun.args$other.entity)){
      output <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path,
          data.files = fun.args$other.entity
        )
      )
    }
    
  }
  
  # Call from template_categorical_variables() -------------------------------------
  
  if (fun.name == 'template_categorical_variables'){
    
    # If not using x ...
    
    if (is.null(fun.args$x)){
      
      # Get attribute file names and data file names
      
      files <- list.files(fun.args$path)
      use_i <- stringr::str_detect(string = files,
                                   pattern = "^attributes")
      if (sum(use_i) == 0){
        stop('There are no attributes.txt files in your dataset working directory. Please fix this.', call. = F)
      }
      
      attribute_files <- files[use_i]
      table_names_base <- stringr::str_sub(string = attribute_files,
                                           start = 12,
                                           end = nchar(attribute_files)-4)
      data_files <- list.files(fun.args$data.path)
      use_i <- stringr::str_detect(string = data_files,
                                   pattern = stringr::str_c("^", table_names_base, collapse = "|"))
      table_names <- data_files[use_i]
      data_files <- table_names
      
      # Send warning if data table name is repeated more than once
      
      if (length(unique(tools::file_path_sans_ext(data_files))) != length(data_files)){
        stop('Duplicate data file names exist in this directory. Please remove duplicates, even if they are a different file type.', call. = F)
      }
      
      # If using x ...
      
    } else if (!is.null(fun.args$x)){
      
      # Get attribute file names and data file names
      
      files <- names(fun.args$x$template)
      use_i <- stringr::str_detect(string = files,
                                   pattern = "^attributes")
      if (sum(use_i) == 0){
        stop('There are no attributes.txt files in your dataset working directory. Please fix this.', call. = F)
      }
      
      attribute_files <- files[use_i]
      
      table_names_base <- stringr::str_sub(string = attribute_files,
                                           start = 12,
                                           end = nchar(attribute_files)-4)
      
      data_files <- names(fun.args$x$data.table)
      
      use_i <- stringr::str_detect(string = data_files,
                                   pattern = stringr::str_c("^", table_names_base, collapse = "|"))
      
      table_names <- data_files[use_i]
      data_files <- table_names
      
      # Send warning if data table name is repeated more than once
      
      if (length(unique(tools::file_path_sans_ext(data_files))) != length(data_files)){
        stop('Duplicate data file names exist in this directory. Please remove duplicates, even if they are a different file type.', call. = F)
      }
      
    }

  }
  
  # Call from template_core_metadata() ----------------------------------------
  
  if (fun.name == 'template_core_metadata'){
    
    # license
    
    if (is.null(fun.args$license)){
      stop('Input argument "license" is missing', call. = F)
    }
    
    license.low <- tolower(fun.args$license)
    
    if (!stringr::str_detect(license.low, "^cc0$|^ccby$")){
      stop('Invalid value entered for the "license" argument. Please choose "CC0" or "CCBY".', call. = F)
    }
    
    # file.type
    
    if ((fun.args$file.type != '.txt') & (fun.args$file.type != '.docx') & 
        (fun.args$file.type != '.md')){
      stop(paste0('"', fun.args$file.type, '" is not a valid entry to the "file.type" argument.'), call. = F)
    }
    
  }
  
  # Call from template_geographic_coverage() -------------------------------------
  
  if (fun.name == 'template_geographic_coverage'){

    if (!isTRUE(fun.args$empty)){
      
      # data.table
      
      if (is.null(fun.args$data.table)){
        stop('Input argument "data.table" is missing! Specify the data file containing the geographic coordinates.', call. = F)
      }
      
      # lat.col
      
      if (is.null(fun.args$lat.col)){
        stop('Input argument "lat.col" is missing! Specify latitude column name.', call. = F)
      }
      
      # lon.col
      
      if (is.null(fun.args$lon.col)){
        stop('Input argument "lon.col" is missing! Specify longitude column name.', call. = F)
      }
      
      # site.col
      
      if (is.null(fun.args$site.col)){
        stop('Input argument "site.col" is missing! Specify site column name.', call. = F)
      }
      
      if (is.null(fun.args$x)){
        
        # Validate file name
        
        data_file <- suppressWarnings(
          EDIutils::validate_file_names(
            path = fun.args$data.path, 
            data.files = fun.args$data.table
          )
        )
        
        # Read data table
        
        x <- template_arguments(
          data.path = fun.args$data.path,
          data.table = data_file
        )
        
        x <- x$x
        
        data_read_2_x <- NA_character_
        
      }
      
      df_table <- fun.args$x$data.table[[fun.args$data.table]]$content
      
      # Validate column names
      
      columns <- colnames(df_table)
      columns_in <- c(fun.args$lat.col, fun.args$lon.col, fun.args$site.col)
      use_i <- stringr::str_detect(string = columns,
                                   pattern = stringr::str_c("^", columns_in, "$", collapse = "|"))
      if (sum(use_i) > 0){
        use_i2 <- columns[use_i]
        use_i3 <- columns_in %in% use_i2
        if (sum(use_i) != 3){
          stop(paste("Invalid column names entered: ", paste(columns_in[!use_i3], collapse = ", "), sep = ""), call. = F)
        }
      }
    }

  }
  
  # Call from template_provenance() -------------------------------------------
  
  if (fun.name == 'template_provenance'){
    
    # path
    if (!is.null(fun.args$path)) {
      EDIutils::validate_path(fun.args$path)
    }
    
  }
  
  # Call from template_table_attributes() -------------------------------------
  
  if (fun.name == 'template_table_attributes'){
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      
      # Validate table names
      
      data_files <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$data.table))
      
      # attributeName - Names follow best practices
      
      r <- lapply(
        fun.args$data.table,
        function(k) {
          x <- template_arguments(
            data.path = fun.args$data.path,
            data.table = k)$x
          use_i <- stringr::str_detect(
            colnames(x$data.table[[k]]$content), "(%|[:blank:]|([:punct:]^_))")
          if (any(use_i)) {
            paste0(
              k, " has column names that are not composed of strictly ",
              "alphanumeric characters and underscores (recommended). This ",
              "best practice ensures the data may be read by most software ",
              "applications. Consider revising these columns: ", 
              paste(
                colnames(x$data.table[[k]]$content)[use_i], collapse = ", "))
          }
        })
      if (!is.null(unlist(r))) {
        warning(paste(unlist(r), collapse = "\n"), call. = FALSE)
      }

    }
    
  }
  
  # Call from template_taxonomic_coverage() -------------------------------------
  
  if (fun.name == 'template_taxonomic_coverage'){
    
    # taxa.table

    if (is.null(fun.args$taxa.table)){
      stop('Input argument "taxa.table" is missing.', call. = F)
    }
    
    if (is.null(fun.args$x)){
      
      data_files <- suppressWarnings(
        EDIutils::validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$taxa.table
        )
      )

    } else if (!is.null(fun.args$x)){
      
      if (!any(fun.args$taxa.table %in% names(fun.args$x$data.table))){
        stop('Input argument "taxa.table" is invalid.', call. = F)

      }
      
    }

    # taxa.col
    
    if (is.null(fun.args$taxa.col)){
      stop('Input argument "taxa.col" is missing.')
    }
    
    if (length(fun.args$taxa.col) != length(fun.args$taxa.table)){
      stop('Each "taxa.table" requires a corresponding "taxa.col".', call. = F)
    }
    
    if (is.null(fun.args$x)){
      
      x <- template_arguments(
        data.path = fun.args$data.path,
        data.table = data_files
      )
      
      for (i in seq_along(fun.args$taxa.table)){
        if (!isTRUE(fun.args$taxa.col[i] %in% colnames(x$x$data.table[[data_files[i]]]$content))){
          stop('Input argument "taxa.col" can not be found in "taxa.table".', call. = F)
        }
      }
      
    } else if (!is.null(fun.args$x)){
      
      for (i in seq_along(fun.args$taxa.table)){
        if (!isTRUE(fun.args$taxa.col[i] %in% colnames(fun.args$x$data.table[[fun.args$taxa.table[i]]]$content))){
          stop('Input argument "taxa.col" can not be found in "taxa.table"', call. = F)
        }
      }
      
    }
    
    # taxa.name.type
    
    if (is.null(fun.args$taxa.name.type)){
      
      stop('Input argument "taxa.name.type" is missing.', call. = F)
      
    } else {
      
      if (!any(tolower(fun.args$taxa.name.type) %in% c('scientific', 'common', 'both'))){
        
        stop('Input argument "taxa.name.type" must be "scientific", "common", or "both".', call. = F)
      }
      
    }
    
    # taxa.authority
    
    if (is.null(fun.args$taxa.authority)){
      stop('Input argument "taxa.authority" is missing.', call. = F)
    }
    
    authorities <- taxonomyCleanr::view_taxa_authorities()
    
    authorities <- authorities[authorities$resolve_sci_taxa == 'supported', ]
    
    use_i <- as.character(fun.args$taxa.authority) %in% as.character(authorities$id)
    
    if (sum(use_i) != length(use_i)){
      stop('Input argument "taxa.authority" contains unsupported authority IDs.', call. = F)
    }
  
  }
  
}
