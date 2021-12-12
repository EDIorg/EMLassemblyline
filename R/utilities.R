#' Get field delimiters of input files
#'
#' @description  
#'     Detect and return field delimiters of input files (tables).
#'
#' @usage 
#' detect_delimeter(
#'   path, 
#'   data.files, 
#'   os
#' )
#' 
#' @param path 
#'     (character) Path to files.
#' @param data.files
#'     (character) File names.
#' @param os
#'     (character) Operating system. Valid options are returned from  
#'     \code{detect_os}.
#' 
#' @return 
#'     (character) Field delimiters of input files.
#'     \item{"\\t"}{tab}
#'     \item{","}{comma}
#'     \item{";"}{semi-colon}
#'     \item{"|"}{pipe}
#'
#' @export
#'

detect_delimeter <- function(path, data.files, os){
  
  # Validate data tables
  
  data_files <- validate_file_names(path, data.files)
  
  # Detect field delimiters ---------------------------------------------------
  # Loop through each table using reader::get.delim() to return the field
  # delimiter. Note: reader::get.delim() performance seems to be operating 
  # system specific.
  
  delim_guess <- c()
  data_path <- c()
  
  for (i in seq_along(data_files)){
    
    # Initialize output vector
    
    data_path[i] <- paste0(path, '/', data_files[i])
    
    if (os == "mac"){
      
      # Detect delimiter for table in Mac OS
      
      delim_guess[i] <- suppressWarnings(
        try(
          reader::get.delim(
            data_path[i],
            n = 1,
            delims = c('\t', ',', ';', '|')
          ), 
          silent = T
        )
      )
      
    } else if (os == "win"){
      
      # Detect delimiter for table in Windows OS
      
      delim_guess[i] <- suppressWarnings(
        try(
          reader::get.delim(
            data_path[i],
            n = 1,
            delims = c('\t', ',', ';', '|')
          ), 
          silent = T
        )
      )
      
    } else if (os == 'lin'){
      
      # Detect delimiter for table in Linux OS
      
      delim_guess[i] <- suppressWarnings(
        try(
          reader::get.delim(
            data_path[i],
            n = 1,
            delims = c('\t', ',', ';', '|')
          ), 
          silent = T
        )
      )
      
    }
    
    # Infer field delimiter (if necessary) ------------------------------------
    
    # If the field delimiter can't be determined, then infer it from the file 
    # name.
    
    if (is.na(delim_guess[i])){
      delim_guess[i] <- delimiter_infer(data_path[i])
    }
    
    # Check delimiters and provide manual override ----------------------------
    
    delim_guess[i] <- detect_delimeter_2(
      data.file = data_files[i],
      delim.guess = delim_guess[i]
    )
    
  }
  
  # Return --------------------------------------------------------------------
  
  delim_guess
  
}






#' Infer field delimiter from file name
#' 
#' @param x
#'   (character) File name including path
#'
#' @return
#'   (character) Delimiter
#' 
delimiter_infer <- function(x){
  
  # FIXME: The following method needs improvement. 
  
  if (stringr::str_detect(x, '.csv$')){
    output <- ','
  } else if (stringr::str_detect(x, '.txt$')){
    output <- '\t'
  }
  
  warning(
    paste0(
      'Cannot detect field delimiter for ',
      x,
      ', assigning a value of "',
      output,
      '".'
    )
  )
  
  # Return
  
  output
  
}






#' Detect field delimiter 2
#' 
#' @description
#'     Secondary check on delimeter detection with manual override
#' 
#' @usage detect_delimeter_2(data.file, delim.guess)
#' 
#' @param data.file
#'     (character) Data file name.
#' @param delim.guess
#'     (character) Delimiter guessed from `detect_delimeter`.
#' 
#' @return
#'     If ambiguity exists, a manual overide option is presented.
#'     

detect_delimeter_2 <- function(data.file, delim.guess){
  
  # Get file extension
  
  file_ext <- substr(
    data.file,
    nchar(data.file)-3,
    nchar(data.file)
  )
  
  # Apply logic
  
  if (is.null(delim.guess) |
      ((delim.guess == ",") & (file_ext == ".txt")) |
      ((delim.guess == "\t") & (file_ext == ".csv")) |
      ((delim.guess == "|") & (file_ext == ".csv"))){
    
    # Send option for manual override
    
    message(
      paste0(
        "I'm having trouble identifying the field delimeter of ",
        data.file, 
        ". Enter the field delimeter of this file.",
        ' Valid options are:  ,  \\t  ;  |'
      )
    )
    
    answer <- readline('ENTER here: ')
    
    # Process user input (add escape characters)
    
    if (answer == "\\t"){
      answer <- "\t"
    }
    
  } else {
    
    answer <- delim.guess
    
  }
  
  answer
  
}








#' Detect operating system
#'
#' @description  
#'     This function uses \code{Sys.info} to detect the user's operating system 
#'     and outputs an abbreviated character string to be used as inputs to OS
#'     specific function calls.
#'
#' @usage detect_os()
#' 
#' @return 
#'     \item{win}{Windows OS}
#'     \item{mac}{Mac OS}
#'
#' @export
#'

detect_os <- function(){
  sysinfo <- Sys.info()['sysname']
  if (sysinfo == 'Darwin'){
    os <- 'mac'
  } else if (sysinfo == 'Windows'){
    os <- 'win'
  } else {
    os <- 'lin'
  }
  os
}







#' Fix methods section of EML file
#'
#' @param eml (charcter) Full path to EML document.
#'
#' @return (.xml) The EML file with fixed methods section (see details). 
#'
#' @details This function is the second step in creating an EML methods section from an .md file. Some parsing constraints in \code{EML::write_eml()} require this fix.
#'
fix_methods <- function(eml) {
  eml2 <- xml2::read_xml(eml)
  nodes_2_modify <- xml2::xml_find_all(eml2, ".//*[starts-with(name(), 'markdown')]") # Nodes to modify are prefixed with 'markdown'
  eqn <- xml2::xml_find_all(nodes_2_modify, "//*[contains(text(), '$$')]") # LaTeX equations become <para>
  xml2::xml_set_name(eqn, "para")
  txtblocks <- xml2::xml_find_all(nodes_2_modify, "//*[starts-with(name(), 'markdown')]") # Text becomes <markdown>
  xml2::xml_set_name(txtblocks, "markdown")
  xml2::write_xml(x = eml2, file = eml)
}







#' Get end of line (EOL) character
#'
#' @description
#'     Get EOL character of input file(s).
#'
#' @usage get_eol(path, file.name, os)
#'
#' @param path
#'     (character) A path to the target file directory.
#' @param file.name
#'     (character) The target file name.
#' @param os
#'     (character) The operating system in which this function is called
#'     called. Valid options are generated from \code{detect_os}.
#'
#' @return
#'     A character string representation of the EOL character.
#'
#' @export
#'
get_eol <- function(path, file.name, os){
  
  # Detect end of line character ----------------------------------------------
  
  if (os == 'mac'){ # Macintosh OS
    
    command <- paste0(
      'od -c "',
      path,
      '/',
      file.name,
      '"'
    )
    
    output <- system(
      command,
      intern = T
    )
    
    use_i <- stringr::str_detect(
      output,
      '\\\\r  \\\\n'
    )
    
    if (sum(use_i) > 0){
      eol <- '\\r\\n'
    } else {
      use_i <- stringr::str_detect(
        output,
        '\\\\n'
      )
      if (sum(use_i) > 0){
        eol <- '\\n'
      } else {
        eol <- '\\r'
      }
    }
    
  } else if ((os == 'win') | (os == 'lin')){ # Windows & Linux OS
    
    output <- readChar(
      paste0(
        path,
        '/',
        file.name
      ),
      nchars = 10000
    )
    
    eol <- parse_delim(output)
    
  }
  
  eol
  
}







#' Get provenance metadata
#'
#' @description
#'     Add Provenance Metadata from Level-1 metadata in PASTA to an XML 
#'     document containing a single methods element in the request message 
#'     body.
#'
#' @usage api_get_provenance_metadata(package.id, environment = 'production')
#'
#' @param package.id
#'     (character) Package identifier composed of scope, identifier, and
#'     revision (e.g. 'edi.101.1').
#' @param environment
#'     (character) Data repository environment to create the package in.
#'     Can be: 'development', 'staging', 'production'.
#'
#' @return
#'     ("xml_document" "xml_node") EML metadata.
#'     
#'
#' @export
#'

api_get_provenance_metadata <- function(package.id, environment = 'production'){
  
  message(paste('Retrieving provenance metadata for ', package.id))
  
  r <- httr::GET(
    url = paste0(
      url_env(environment),
      '.lternet.edu/package/provenance/eml/',
      stringr::str_replace_all(package.id, '\\.', '/')
    )
  )
  
  output <- httr::content(
    r,
    as = 'parsed',
    encoding = 'UTF-8'
  )
  
  output
  
}








# Parse delimiter from string -------------------------------------------------

parse_delim <- function(x){
  
  use_i <- stringr::str_detect(
    x,
    '\\r\\n'
  )
  
  if (sum(use_i) > 0){
    eol <- '\\r\\n'
  } else {
    use_i <- stringr::str_detect(
      x,
      '\\n'
    )
    if (sum(use_i) > 0){
      eol <- '\\n'
    } else {
      eol <- '\\r'
    }
  }
  
  eol
  
}











#' Create methods node from .md file
#'
#' @param methods_file (character) Full path to methods.md file
#'
#' @return (list) EML methodStep node
#' 
#' @details Text and LaTeX equations in \code{methods_file} are parsed into /eml/dataset/methodStep/description/markdown nodes. A second step applied by \code{fix_methods()} on the resultant EML file corrects the node names. Without this second step, the EML file will fail on schema validation.
#' 
#' @note LaTex equations must be wrapped in $$ (e.g. $$<my_equation>$$), otherwise they won't be parsed correctly from the text.
#'
set_methods_md <- function(methods_file) {
  md <- readr::read_file(methods_file)
  i_eq <- as.data.frame(stringr::str_locate_all(md, "\\$\\$.*\\$\\$(\\n|\\r\\n|\\r)*")) # Equation indices (start and end)
  res <- list()
  if (nrow(i_eq) > 0) {                                                # Both text and equations are present
    res$markdown <- stringr::str_sub(md, 1, i_eq$start[1]-1)           # Text is start of file to start of first equation 
    for (i in 1:nrow(i_eq)) {                                          # Parse equations and text between equations
      res <- c(res, para = stringr::str_sub(md, i_eq$start[i], i_eq$end[i]))
      if (nrow(i_eq) > i) {                                            # Text is between end of current equation and start of the next
        res <- c(res, markdown = stringr::str_sub(md, i_eq$end[i]+1, i_eq$start[i+1]-1))
      }
    }
    if (nchar(md) > i_eq$end[i]) {                                     # Text remains at end and needs to be extracted
      res <- c(res, markdown = stringr::str_sub(md, i_eq$end[i]+1, nchar(md)))
    }
  } else {                                                             # Only text, no equations
    res$markdown <- stringr::str_sub(md, 1, nchar(md))
  }
  names(res) <- rep("markdown", length(res))                           # Nodes become unordered if named differently
  res <- list(                                                         # Incorporate above results into an EML methodStep node
    methodStep = list(
      description = res))
  return(res)
}







#' Make URL for PASTA+ environment
#'
#' @description
#'     Create the URL suffix to the PASTA+ environment specified by the
#'     environment argument.
#'
#' @usage url_env(environment)
#'
#' @param environment
#'     (character) Data repository environment to perform the evaluation in.
#'     Can be: 'development', 'staging', 'production'.
#'
#' @export
#'

url_env <- function(environment){
  
  environment <- tolower(environment)
  if (environment == 'development'){
    url_env <- 'https://pasta-d'
  } else if (environment == 'staging'){
    url_env <- 'https://pasta-s'
  } else if (environment == 'production'){
    url_env <- 'https://pasta'
  }
  
  url_env
  
}







#' Validate file names
#'
#' @description  
#'     Identify whether input data file names exist in the specified directory.
#'
#' @usage validate_file_names(path, data.files)
#' 
#' @param path 
#'     (character) A character string specifying a path to the dataset working 
#'     directory.
#' @param data.files
#'     A list of character strings specifying the names of the data files of 
#'     your dataset.
#' 
#' @return 
#'     A warning message if the data files don't exist at path, and which of
#'     the input data files are missing.
#'     
#'     The full names of files listed in the data.files argument.
#'
#' @export
#'

validate_file_names <- function(path, data.files){
  
  # Validate file presence ----------------------------------------------------
  
  # Index data.files in path
  files <- list.files(path)
  use_i <- data.files %in% files
  
  # Throw an error if any data.files are missing
  if (sum(use_i) != length(data.files)){
    stop(
      paste0(
        "\nThese files don't exist in the specified directory:\n", 
        paste(data.files[!use_i], collapse = "\n")
      ),
      call. = FALSE
    )
  }
  
  # Check file naming convention ----------------------------------------------
  
  # Index file names that are not composed of alphanumerics and underscores
  use_i <- stringr::str_detect(
    string = tools::file_path_sans_ext(data.files), 
    pattern = "([:blank:]|([:punct:]^_))"
  )
  
  # Issue warning if this best practice is not followed
  if (any(use_i)) {
    warning(
      paste0(
        "Composing file names from only alphanumerics and underscores is a ",
        "best practice. These files don't follow this recommendation:\n",
        paste(data.files[use_i], collapse = "\n"),
        "\nPlease consider renaming these files."
      ),
      call. = FALSE
    )
  }
  
  # Get file names ------------------------------------------------------------
  
  files <- list.files(path)
  use_i <- stringr::str_detect(string = files,
                               pattern = stringr::str_c("^", data.files, collapse = "|"))
  data_files <- files[use_i]
  
  # Reorder file names to match input ordering --------------------------------
  
  data_files_out <- c()
  for (i in 1:length(data.files)){
    use_i <- stringr::str_detect(string = data_files,
                                 pattern = stringr::str_c("^", data.files[i], collapse = "|"))
    data_files_out[i] <- data_files[use_i]
  }
  
  data_files_out
  
}







#' Validate path
#'
#' @description  
#'     Use \code{dir.exists} to determine whether the input path is valid and 
#'     returns an error message if not.
#'
#' @usage validate_path(path)
#' 
#' @param path 
#'     A character string specifying a path to the dataset working directory.
#' 
#' @return 
#'     A warning message if the path leads to a non-existant directory.
#'
#' @export
#'

validate_path <- function(path){
  
  # Validate path -------------------------------------------------------------
  
  if (!dir.exists(path)){
    stop('The directory specified by the argument "path" does not exist! Please enter the correct path for your dataset working directory.')
  }
  
}








#' Resolve terms to a controlled vocabulary
#'
#' @description  
#'     Resolve terms to a controlled vocabulary.
#'
#' @usage 
#'     vocab_resolve_terms(x, cv, messages = FALSE, interactive = FALSE)
#'
#' @param x
#'     (character) Term(s) to resolve to a controlled vocabulary. Can be a 
#'     vector of terms.
#' @param cv
#'     (character) A controlled vocabulary to search. Valid options are:
#'     \itemize{
#'         \item{lter} - The LTER Controlled Vocabulary (http://vocab.lternet.edu/vocab/vocab/index.php)
#'     }
#' @param messages 
#'     (logical) Display diagnostic messages, e.g. alternative spelling options.
#' @param interactive 
#'     (logical) Query user to select from alternative terms and returns back
#'     selection.
#'
#' @return 
#'     (character) Controlled vocabulary names corresponding to successfully
#'     resolved terms.
#'
#' @export
#'


vocab_resolve_terms <- function(x, cv, messages = FALSE, interactive = FALSE){
  
  # Check arguments -----------------------------------------------------------
  
  if (is.character(x) != T){
    stop('Input argument "x" is not of class "character"!')
  }
  if (cv != 'lter'){
    stop('Input argument "cv" is not one of the allowed vocabularies!')
  }
  if (!missing(messages) & messages & !missing(interactive) & interactive){
    stop('Both arguments "messages" & "interactive" can not be used at the same time. Please select one or the other.')
  }
  
  # Initialize output ---------------------------------------------------------
  
  output <- data.frame(
    term = x,
    controlled_vocabulary = character(length(x)),
    stringsAsFactors = F)
  
  # Call specified vocabularies -----------------------------------------------
  
  if (cv == 'lter'){
    
    if (!missing(messages) & messages){
      # Messages
      use_i <- unlist(lapply(x, FUN = vocab_lter_term, messages = T))
      output[use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'      
    } else if (!missing(interactive) & interactive){
      # Interactive
      alternative_terms <- unlist(lapply(x, FUN = vocab_lter_term, interactive = T))
      use_i <- ((alternative_terms == 'NONE OF THE ABOVE') | (is.na(alternative_terms)))
      output[!use_i, 'term'] <- alternative_terms[!use_i]
      output$term[output$term == 'TRUE'] <- x[output$term == 'TRUE']
      output[!use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'      
      use_i <- output$term == FALSE
      output$term[use_i] <- x[use_i]
      output$controlled_vocabulary[use_i] <- ''
    } else {
      # Automatic
      use_i <- unlist(lapply(x, FUN = vocab_lter_term))
      output[use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'
    }
    
  }
  
  # Return output -------------------------------------------------------------
  
  output
  
}











#' Write template to file
#'
#' @param tmplt (data.frame) Template
#' @param name (character) Template file name (including extension)
#' @param path (character) Path to write to
#' @param force (logical) Overwrite existing template?
#'
#' @return (file, logical) Template and TRUE if written
#' 
#' @details Only works for tabular templates.
#' 
write_template <- function(tmplt, name, path, force = FALSE) {
  f <- paste0(path, "/", enc2utf8(name))
  if (file.exists(f) & !isTRUE(force)) {
    warning(f, " exists and will not be overwritten", call. = FALSE)
    return(FALSE)
  } else {
    data.table::fwrite(x = tmplt, file = f, sep = "\t", quote = FALSE)
    return(TRUE)
  }
}