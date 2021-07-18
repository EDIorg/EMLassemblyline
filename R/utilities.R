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