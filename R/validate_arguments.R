#' Validate arguments of EMLassemblyline functions
#'
#' @description
#'     Validate input arguments to EMLassemblyline functions.
#'
#' @param fun.name
#'     (character) Name calling function.
#' @param fun.args
#'     (named list) Function arguments and their values.
#'     
#' @return 
#' \item{argument_issues}{Any issues found in the validation process and returned as a list of character strings to the users environment. This object can be saved for later use and viewed in a human readable form with \code{issues()}}.
#' 
#' @note Some arise where an arguments value is set to NULL 
#'     
#' @details
#'     \code{fun.name} defines which checks to call.
#'     
#'     \code{fun.args} values are modified when critical issues are found and are reassigned to the calling environment so the calling function may proceed without error.
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
        # FIXME: emld::eml_validate() has a new set of constraints causing 
        # validate_eml_content() to stop here. Some initial testing suggests 
        # deep inconsistency in emld's validation methods so loosening
        # this constraint for the time being in favor of more productive ends.
        # eml <- EML::read_eml(fun.args$eml.in)
        # if (!emld::eml_validate(eml)) {
        #   stop(
        #     "Input argument 'eml.in' is not a valid EML record.", 
        #     call. = FALSE
        #   )
        # }
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
  
  # Call from eml2eal() -------------------------------------------------------
  
  if (fun.name == "eml2eal"){
    if (!isTRUE(fun.args$empty)){
      # eml
      if (!file.exists(fun.args$eml)) {
        stop("Input argument 'eml' is to a non-existant object.", call. = F)
      }
      # file.type
      if (!(fun.args$file.type %in% c(".docx", ".txt", ".md"))) {
        stop("Input argument 'file.type' must be '.docx', '.txt', or '.md'.", 
             call. = F)
      }
      # path
      if (is.null(fun.args$path)) {
        stop("Input argument 'path' is missing.", call. = FALSE)
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
        validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$data.table
        )
      )

    }
    
  }
  
  # Call from make_eml() ------------------------------------------------------
  
  if (fun.name == 'make_eml'){
    
    # argument_issues object should be removed from the global environment each
    # time this function is called otherwise issues that have been fixed may 
    # appear outstanding
    if (exists("argument_issues", where = ".GlobalEnv")) {
      rm(argument_issues, envir = as.environment(".GlobalEnv"))
    }
    
    # Initialize object for collecting issue messages
    issues <- c()
    
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
      validate_path(fun.args$path)
    }

    # data.path

    validate_path(fun.args$data.path)

    # eml.path

    if (isTRUE(fun.args$write.file)){
      validate_path(fun.args$eml.path)
    }
    
    # dataset.title
    issues <- c(issues, validate_title(fun.args))

    # data.table
    
    if (!is.null(fun.args$data.table)) {
      table_names <- suppressWarnings(
        validate_file_names(
          path = fun.args$data.path,
          data.files = fun.args$data.table))
    }
    
    # data.table.description
    r <- validate_table_description(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args
    
    # data.table.name
    r <- validate_table_name(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args
    
    # data.table.quote.character
    r <- validate_table_quote_character(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args
    
    # data.table.url
    r <- validate_table_url(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args
    
    # geographic.coordinates and geographic.description
    r <- validate_geographic_coord_desc(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args

    # maintenance.description
    r <- validate_maintenance_description(fun.args)
    issues <- c(issues, r$issues)
    
    # other.entity
    if (!is.null(fun.args$other.entity)) {
      other_entity_names <- suppressWarnings(
        validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$other.entity))
    }

    # other.entity.name
    r <- validate_other_entity_name(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args
    
    # other.entity.description
    r <- validate_other_entity_description(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args
    
    # other.entity.url
    r <- validate_other_entity_url(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args

    # temporal.coverage
    r <- validate_temporal_coverage(fun.args)
    issues <- c(issues, r$issues)
    fun.args <- r$fun.args

    # Return
    if (!is.null(issues)) {
      list2env(
        list(argument_issues = issues),
        # if(is.null(options("eal.env"))) {
          .GlobalEnv
        # } else {
          # options("eal.env")[[1]]
        # }
      )
      warning(
        "Argument issues found. Use issues() to see them.",
        call. = FALSE)
    }
    
    for (i in seq_along(fun.args)) {
      assign(
        x = names(fun.args[i]), 
        value = fun.args[[i]], 
        envir = parent.frame())
    }
    
  }
  
  # Call from template_annotations() ------------------------------------------
  
  if (fun.name == 'template_annotations'){
    
    # path
    
    if (is.null(fun.args$path)) {
      stop('Input argument "path" is missing.', call. = FALSE)
    }
    
    if (!is.null(fun.args$path)) {
      validate_path(fun.args$path)
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
      validate_path(fun.args$data.path)
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      table_names <- suppressWarnings(
        validate_file_names(
          path = fun.args$data.path, 
          data.files = fun.args$data.table
        )
      )
    }
    
    # other.entity
    
    if (!is.null(fun.args$other.entity)){
      table_names <- suppressWarnings(
        validate_file_names(
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
      validate_path(fun.args$path)
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
      validate_path(fun.args$data.path)
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      output <- suppressWarnings(
        validate_file_names(
          path = fun.args$data.path,
          data.files = fun.args$data.table
        )
      )
    }
    
    # other.entity
    
    if (!is.null(fun.args$other.entity)){
      output <- suppressWarnings(
        validate_file_names(
          path = fun.args$data.path,
          data.files = fun.args$other.entity
        )
      )
    }
    
  }
  
  # Call from template_categorical_variables() -------------------------------------
  
  if (fun.name == 'template_categorical_variables'){

    if (is.null(fun.args$path)) {
      stop("Input argument 'path' is missing", call. = FALSE)
    }
    
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
          validate_file_names(
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
      validate_path(fun.args$path)
    }
    
  }
  
  

  # Call from template_spatial_attributes() ---------------------------------
  
  if (fun.name == 'template_spatial_attributes'){
    
    # path
    if (!is.null(fun.args$path)) {
      EDIutils::validate_path(fun.args$path)
    }
    
    # Validate file names
    
    data_files <- suppressWarnings(
      EDIutils::validate_file_names(
        path = fun.args$data.path, 
        data.files = fun.args$spatial.files))
    
  }
  

  # Call from template_raster_attributes() ----------------------------------

  if (fun.name == 'template_raster_attributes'){
    
    # path
    if (!is.null(fun.args$path)) {
      EDIutils::validate_path(fun.args$path)
    }
    
    # Validate file names
    
    data_files <- suppressWarnings(
      EDIutils::validate_file_names(
        path = fun.args$data.path, 
        data.files = fun.args$raster.files))
    
  }
  

  # Call from template_raster_variables() -----------------------------------

  if (fun.name == 'template_raster_variables'){
    
    # path
    if (!is.null(fun.args$path)) {
      EDIutils::validate_path(fun.args$path)
    }
  }
  

  # Call from template_shape_attributes() -----------------------------------

  if (fun.name == 'template_shape_attributes'){
    
    # path
    if (!is.null(fun.args$path)) {
      EDIutils::validate_path(fun.args$path)
    }
    
    # Validate file names
    
    data_files <- suppressWarnings(
      EDIutils::validate_file_names(
        path = fun.args$data.path, 
        data.files = fun.args$shape.files))
    
  }
  
  
  # Call from template_table_attributes() -------------------------------------
  
  if (fun.name == 'template_table_attributes'){
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      
      # Validate table names
      
      data_files <- suppressWarnings(
        validate_file_names(
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
    
    # When not returning an empty template, several arguments are required
    
    if (!isTRUE(fun.args$empty)) {
      
      # path
      
      if (isTRUE(fun.args$write.file)) {
        validate_path(fun.args$path)
      }
      
      # data.path
      
      validate_path(fun.args$data.path)
      
      # taxa.table
      
      if (is.null(fun.args$taxa.table)) {
        stop('Input argument "taxa.table" is missing.', call. = F)
      }
      
      fun.args$x <- template_arguments(
        data.path = fun.args$data.path,
        data.table = fun.args$taxa.table)$x
      
      if (!any(fun.args$taxa.table %in% names(fun.args$x$data.table))) {
        stop('Input argument "taxa.table" is invalid.', call. = F)
        
      }
      
      # taxa.col
      
      if (is.null(fun.args$taxa.col)){
        stop('Input argument "taxa.col" is missing.')
      }
      
      if (length(fun.args$taxa.col) != length(fun.args$taxa.table)){
        stop('Each "taxa.table" requires a corresponding "taxa.col".', call. = F)
      }
      
      for (i in seq_along(fun.args$taxa.table)){
        if (!isTRUE(fun.args$taxa.col[i] %in% colnames(fun.args$x$data.table[[i]]$content))) {
          stop('Input argument "taxa.col" can not be found in "taxa.table".', call. = F)
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
    
    # When returning an empty template, not much is required
    
    if (isTRUE(fun.args$empty)) {
      
      # path
      
      if (isTRUE(fun.args$write.file)) {
        validate_path(fun.args$path)
      }
      
    }
  
  }
  
}









#' Validate dataset title
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{character}{Description of issues}
#'     \item{NULL}{If no issues were found}
#'
validate_title <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # Is present
  r <- validate_title_presence(fun.args)
  required_issues <- c(required_issues, r)
  
  # Is of adequate length
  r <- validate_title_length(fun.args)
  optional_issues <- c(optional_issues, r)
  
  # A compiled report of issues helps the user fix them
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Dataset title (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"), 
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Dataset title (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"), 
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  
  return(issues)
  
}








#' Check for title presence
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{character}{Description of issues}
#'     \item{NULL}{If no issues were found}
#'
validate_title_presence <- function(fun.args) {
  if (is.null(fun.args$dataset.title)) {
    return("The dataset title is missing.")
  }
}








#' Check title length
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{character}{Description of issues}
#'     \item{NULL}{If no issues were found}
#'
validate_title_length <- function(fun.args) {
  title <- fun.args$dataset.title
  if (!is.null(title)) {
    if (length(stringr::str_split(title, " ")[[1]]) < 5) {
      return("The dataset title should be at least 5 words.")
    }
  }
}








#' Validate data table description
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_table_description <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # Each table has a description, otherwise the description defaults to the 
  # file name
  for (i in seq_along(fun.args$data.table)) {
    tbl <- fun.args$data.table[i]
    des <- fun.args$data.table.description[i]
    if (is.na(des) || is.null(des)) {
      optional_issues <- c(
        optional_issues,
        paste0("Missing description for ", tbl, ". Short descriptions of the ",
               "file contents help future users understand the data. ",
               "Defaulting to file name."))
      fun.args$data.table.description[i] <- tbl
    }
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Table descriptions (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Table descriptions (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}








#' Validate data table names
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_table_name <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # Each table has a name, otherwise the name defaults to the file name
  for (i in seq_along(fun.args$data.table)) {
    tbl <- fun.args$data.table[i]
    name <- fun.args$data.table.name[i]
    if (is.na(name) || is.null(name)) {
      optional_issues <- c(
        optional_issues,
        paste0("Missing name for ", tbl, ". Short descriptive names of the ",
               "files help users understand the data. Defaulting to file ",
               "name."))
      fun.args$data.table.name[i] <- tbl
    } else if (tbl == name) {
      optional_issues <- c(
        optional_issues,
        paste0("Missing name for ", tbl, ". Short descriptive names of the ",
               "files help users understand the data. Defaulting to file ",
               "name."))
    }
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Table names (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Table names (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  return(list(issues = issues, fun.args = fun.args))
}








#' Validate data table quote characters
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_table_quote_character <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # If quote characters are used, then each table has one otherwise a 
  # default " is added
  for (i in seq_along(fun.args$data.table)) {
    tbl <- fun.args$data.table[i]
    quo <- fun.args$data.table.quote.character[i]
    if (!is.null(quo)) {
      if (is.na(quo)) {
        required_issues <- c(
          required_issues,
          paste0("Missing quote character for ", tbl, ". A quote character ",
                 "should be specified for each table. Use '' if no quote ",
                 "character is used. Defaulting to '\"'"))
        fun.args$data.table.quote.character[i] <- '"'
      }
    }
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Table quote character (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Table quote character (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}








#' Validate data table url
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_table_url <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # If urls are used, then each table has one otherwise a default "" is added
  for (i in seq_along(fun.args$data.table)) {
    tbl <- fun.args$data.table[i]
    url <- fun.args$data.table.url[i]
    if (!is.null(url)) {
      if (is.na(url)) {
        required_issues <- c(
          required_issues,
          paste0("Missing URL for ", tbl, ". A URL should be specified for ",
                 'each table. Use "" if no URL is used. Defaulting to "".'))
        fun.args$data.table.url[i] <- ""
      }
    }
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Table URL (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Table URL (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}








#' Validate geographic coordinate and description pair
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_geographic_coord_desc <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # geographic.description is missing
  if (!is.null(fun.args$geographic.coordinates) & 
      is.null(fun.args$geographic.description)) {
    required_issues <- c(
      required_issues,
      "Geographic description is missing.")
    fun.args$geographic.coordinates <- NULL
  }
  
  # geographic.coordinates are missing
  if (is.null(fun.args$geographic.coordinates) & 
      !is.null(fun.args$geographic.description)){
    required_issues <- c(
      required_issues,
      "Geographic coordinates are missing.")
    fun.args$geographic.description <- NULL
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Geographic coverage (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Geographic coverage (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}







#' Validate maintenance description
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_maintenance_description <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  if (is.null(fun.args$maintenance.description)) {
    optional_issues <- c(
      optional_issues,
      paste0(
        'A maintenance description is recommended. Describe the collection ',
        'status of this dataset (e.g. "Ongoing collection with quarterly ', 
        'updates", or "Completed collection, updates to these data are not ',
        'expected.")'))
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Maintenance description (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Maintenance description (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}








#' Validate other entity names
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_other_entity_name <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # Each table has a name, otherwise the name defaults to the file name
  for (i in seq_along(fun.args$other.entity)) {
    othent <- fun.args$other.entity[i]
    name <- fun.args$other.entity.name[i]
    if (is.na(name) || is.null(name)) {
      optional_issues <- c(
        optional_issues,
        paste0("Missing name for ", othent, ". Short descriptive names of the ",
               "files help users understand the data. Defaulting to file ",
               "name."))
      fun.args$other.entity.name[i] <- othent
    } else if (othent == name) {
      optional_issues <- c(
        optional_issues,
        paste0("Missing name for ", othent, ". Short descriptive names of the ",
               "files help users understand the data. Defaulting to file ",
               "name."))
    }
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Other entity names (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Other entity names (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}








#' Validate other entity description
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_other_entity_description <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # Each table has a description, otherwise the description defaults to the 
  # file name
  for (i in seq_along(fun.args$other.entity)) {
    othent <- fun.args$other.entity[i]
    othent <- fun.args$other.entity.description[i]
    if (is.na(othent) || is.null(othent)) {
      optional_issues <- c(
        optional_issues,
        paste0("Missing description for ", othent, ". Short descriptions of the ",
               "file contents help future users understand the data. ",
               "Defaulting to file name."))
      fun.args$other.entity.description[i] <- othent
    }
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Other entity descriptions (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Other entity descriptions (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}








#' Validate other entity url
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_other_entity_url <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # If urls are used, then each table has one otherwise a default "" is added
  for (i in seq_along(fun.args$other.entity)) {
    othent <- fun.args$other.entity[i]
    url <- fun.args$other.entity.url[i]
    if (!is.null(url)) {
      if (is.na(url)) {
        required_issues <- c(
          required_issues,
          paste0("Missing URL for ", othent, ". A URL should be specified for ",
                 'each other entity. Use "" if no URL is used. Defaulting to "".'))
        fun.args$other.entity.url[i] <- ""
      }
    }
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Other entity URL (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Other entity (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}








#' Validate temporal coverage
#'
#' @param fun.args
#'     (named list) Function arguments and their values.
#'
#' @return
#'     \item{issues}{Description of issues}
#'     \item{fun.args}{Updated list of function arguments}
#'
validate_temporal_coverage <- function(fun.args) {
  
  # Each issue is logged as "required" or "optional"
  required_issues <- c()
  optional_issues <- c()
  
  # temporal.coverage is missing
  if (is.null(fun.args$temporal.coverage)) {
    optional_issues <- c(
      optional_issues,
      "Temporal coverage is missing.")
  }
  
  # temporal.coverage is incomplete
  if (!is.null(fun.args$temporal.coverage)) {
    if (length(fun.args$temporal.coverage) != 2){
      required_issues <- c(
        required_issues,
        "Temporal coverage requires a begin and end date.")
      fun.args$temporal.coverage <- NULL
    }
  }
  
  # Compile report
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Temporal coverage (Required):\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"),
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Temporal coverage (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"),
      "\n")
  }
  
  issues <- c(required_issues, optional_issues)
  
  return(list(issues = issues, fun.args = fun.args))
}
