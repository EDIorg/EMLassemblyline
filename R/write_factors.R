#' Define factors and write to file
#'
#' @description  A function for extracting and defining data table factors 
#'     defined in the attributes table.
#'
#' @usage write_factors(path)
#' 
#'     Run this function whenever your data contain factors, and any of your 
#'     \emph{datasetname_datatablename_attributes_draft.xlsx} or referenced 
#'     data tables have changed.
#'
#' @param path A path to the dataset working directory containing the updated 
#'     \emph{datasetname_datatablename_attributes.xlsx}, 
#'     \emph{eml_configuration.R} and referenced data table files.
#'
#' @return A file in the dataset working directory titled 
#'     \emph{datasetname_datatablename_factors.xlsx} containing factor 
#'     definitions, which is called upon by the \emph{create_eml} function.
#'     
#' @details 
#'     This function overwrites any 
#'     \emph{datasetname_datatablename_factors.xlsx} files you have created in 
#'     the dataset working directory. To prevent overwriting of a specific 
#'     file, temporarily move the file out of the working directory.
#'     
#'     This function identifies character class attributes from the file 
#'     \emph{datasetname_datatablename_attributes.xlsx} and then extracts 
#'     unique values of these attribute to the table 
#'     \emph{datasetname_datatablename_factors.xlsx}, which is then opened for 
#'     the user to edit.  
#'
#' @seealso \code{\link{copy_templates}} to copy metadata templates to the 
#'     dataset working directory.
#' @seealso \code{\link{run_guide}} for guidance on completing the template 
#'     files.
#' @seealso \code{\link{write_attributes}} to create the attributes table.
#' @seealso \code{\link{extract_geocoverage}} to extract detailed geographic 
#'     coordinates of sampling sites.
#' @seealso \code{\link{create_eml}} to write the EML file.


write_factors <- function(path) {
  
  # Parameterize function
  
  library("EML")
  library("xlsx")
  #library("rmarkdown")
  
  template <- paste(dataset_name,
                    "_template.docx",
                    sep = "")
  #template <- trimws(list.files(path, pattern = "*_template.docx"))
  
  # Load the configuration file

  source(paste(path, "/eml_configuration.R", sep = ""))

  # Get system information

  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }

  # Identify the attribute files

  attribute_files <- trimws(
    list.files(path,
               pattern = "*_attributes.xlsx"))

  if (length(attribute_files) == 0){
    print("No attribute files found ... run write_attributes() to create them.")
  }

  # Set file names

  fname_table_factors <- c()
  for (i in 1:length(table_names)){
    fname_table_factors[i] <- paste(
      substr(table_names[i], 1, nchar(table_names[i]) - 4),
      "_factors.xlsx",
      sep = ""
    )
  }

  # Issue warning

  answer <- readline(
    "Are you sure you want to build new factors? This will overwrite your previous work! (y or n):  ")

  if (answer == "y"){


    # Loop through data tables ------------------------------------------------

    for (i in 1:length(attribute_files)){

      # if (!is.na(match(
      #   paste(substr(attribute_files[i], 1, nchar(attribute_files[i]) - 16),
      #         ".txt",
      #         sep = ""),
      #   table_names)) == T){


        print(paste("Now working on ... ",
                    attribute_files[i], sep = ""))

        # Read attributes file

        attributes <- read.xlsx2(
          paste(
            path,
            "/",
            attribute_files[i],
            sep = ""),
          sheetIndex = 1,
          colClasses = c(rep("character",7),
                         rep("numeric",2),
                         rep("character",2)))

        for (j in 1:dim(attributes)[2]){
          if (class(attributes[ ,j]) == "factor"){
            attributes[ ,j] <- as.character(attributes[ ,j])
          }
        }

        # Build factor table

        factors_I <- which(attributes$columnClasses %in% "factor")

        df_table <- read.csv(
          paste(
            path,
            "/",
            substr(attribute_files[i], 1, nchar(attribute_files[i]) - 16),
            ".csv",
            sep = ""),
          header=TRUE,
          sep=",",
          quote="\"",
          as.is=TRUE)

        # df_table <- read.table(
        #   paste(
        #     path,
        #     "/",
        #     substr(attribute_files[i], 1, nchar(attribute_files[i]) - 16),
        #     ".txt",
        #     sep = ""),
        #   header=TRUE,
        #   sep="\t",
        #   quote="\"",
        #   as.is=TRUE,
        #   comment.char = "")
        
        
        # If there are no factors then skip to the next file

        if (length(factors_I) > 0){

          rows <- 0
          for (j in 1:length(factors_I)){
            factor_names <- unique(
              eval(
                parse(
                  text = paste(
                    "df_table",
                    "$",
                    attributes$attributeName[factors_I[j]],
                    sep = ""))))

            rows <- length(factor_names) + rows

          }

          factors <- data.frame(attributeName = character(rows),
                                code = character(rows),
                                definition = character(rows),
                                stringsAsFactors = F)

          row <- 1
          for (j in 1:length(factors_I)){

            factor_names <- unique(
              eval(
                parse(
                  text = paste(
                    "df_table",
                    "$",
                    attributes$attributeName[factors_I[j]],
                    sep = ""))))

            factors$attributeName[row:(length(factor_names)+row-1)] <-
              attributes$attributeName[factors_I[j]]

            factors$code[row:(length(factor_names)+row-1)] <- factor_names

            row <- row + length(factor_names)

          }

          # Write factor table

          write.xlsx(factors,
                     paste(path,
                           "/",
                           substr(attribute_files[i], 1, nchar(attribute_files[i]) - 16),
                           "_factors.xlsx",
                           sep = ""),
                     col.names = T,
                     row.names = F,
                     showNA = F)

          # Prompt the user to manually edit the factors file and custom unit files.

          standardUnits <- get_unitList()
          View(standardUnits$units)

          if (os == "mac"){

            system(paste("open",
                         paste(
                           path,
                           "/",
                           fname_table_factors[i],
                           sep = "")))

          } else if (os == "win"){

            shell.exec(paste(path,"/",
                             substr(attribute_files[i],
                                    1,
                                    nchar(attribute_files[i]) - 16),
                             "_factors.xlsx",
                             sep = ""))

          }

          readline(
            prompt = "Press <enter> once factors file and any custom units files have been edited, saved, and closed."
          )
        }

      #}

    }

  }

}

