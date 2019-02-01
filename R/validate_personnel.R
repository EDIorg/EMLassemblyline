#' Validate personnel
#'
#' @description  
#'     This function checks the personnel table for required information and 
#'     adjusts content to meet input requirements of the personnel EML making 
#'     functions.
#'
#' @usage 
#'     validate_personnel(x)
#'
#' @param x
#'     The data frame created from reading personnel.txt.
#'
#' @return 
#'     If incongruence is found an error is returned, else a valid data frame
#'     for inputs to the personnel EML making functions.
#'     
#' @export     
#'   

validate_personnel <- function(x){
  
  if (missing(x)){
    stop('Input argument "x" is missing! Specify the data frame created from reading personnel.txt.')
  }
  
  required_roles(x)
  project_match(x)
  personinfo <- order_pi_list(x)
  
  personinfo
}




# Check for required roles

required_roles <- function(x){
  if (sum(x$role %in% "creator") == 0){
    stop("Your dataset is missing a creator. Add one to personnel.txt.")
  }
  if (sum(x$role %in% "contact") == 0){
    stop("Your dataset is missing a contact. Add one to personnel.txt.")
  }
}




# Check project info is associated with first listed PI

project_match <- function(x){
  use_i <- tolower(x$role) == "pi"
  if (sum(use_i) > 0){
    pis <- x[use_i, ]
    pi_proj <- pis[ , c("projectTitle", "fundingAgency", "fundingNumber")]
    
    if ((sum(pi_proj != "") > 0) & (sum(pi_proj[1, ] == "") == 3)){
      stop("The first Principal Investigator listed in personnel.txt is missing a projectTitle, fundingAgency, or fundingNumber. The first listed PI represents the major project and requires this. Please add one.")
    }
  }
}




# Reorder PI list so PI+project pairs are listed first and PIs without projects
# subsequent.

order_pi_list <- function(x){
  use_i <- (x$role == "pi")
  if (sum(use_i > 0)){
    others <- x[!use_i, ]
    pis <- x[use_i, ]
    primary_pi <- pis[1, ]
    if (nrow(pis) > 1){
      pi_remainder <- pis[2:nrow(pis), ]
      use_i <- (pi_remainder$projectTitle == "") & 
        (pi_remainder$fundingAgency == "") & 
        (pi_remainder$fundingNumber == "")
      no_funding <- pi_remainder[use_i, ]
      aux_funding <- pi_remainder[!use_i, ]
      x <- rbind(others,
                 primary_pi,
                 aux_funding,
                 no_funding)
      x <- x[stats::complete.cases(x), ]
    } else if (nrow(pis) == 1){
      x <- rbind(others,
                 pis)
    }
  }
  
  x
}

