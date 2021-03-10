#' Convert empty text to empty EAL text
#'
#' @param txt (character) Text
#'
#' @return (character) EAL representation of empty values in tabular templates
#'
cnvmt <- function(txt) {
  if (length(txt) == 0) {
    return("")
  } else {
    return(txt)
  }
}








#' Create abstract template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' @param file.type (character) File type to write to (see \code{eml2eal()})
#' 
#' @details Gets abstract node and parses to template via pandoc.
#'
#' @return abstract template
#'
eml2abstract <- function(eml, path, file.type) {
  abstract <- xml2::xml_find_all(eml, "/eml:eml/dataset/abstract")
  if (!is_empty_nodeset(abstract)) {
    nodeset2txt(abstract, file.type, path)
  }
}








#' Create additional_info template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' @param file.type (character) File type to write to (see \code{eml2eal()})
#' 
#' @details Gets additionalInfo node and parses to template via pandoc.
#' 
#' @return additional_info template
#' 
eml2additional_info <- function(eml, path, file.type) {
  addinf <- xml2::xml_find_all(eml, "/eml:eml/dataset/additionalInfo")
  if (!is_empty_nodeset(addinf)) {
    nodeset2txt(addinf, file.type, path)
  }
}








#' Create annotations template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets annotation nodes, listed under subjects or \code{/eml:eml/annotations}, and parse to template.
#' 
#' @return annotations template
#' 
eml2annotations <- function(eml, path) {
  annos <- xml2::xml_find_all(eml, ".//annotation")
  if (!is_empty_nodeset(annos)) {
    res <- lapply(
      annos,
      function(anno) {
        nodeset <- get_reference(anno)
        if (is.null(nodeset)) {
          nodeset <- xml2::xml_parent(anno)
        }
        res <- list(
          id = get_anno_id(nodeset),
          element = get_anno_element(nodeset),
          context = get_anno_context(nodeset),
          subject = get_anno_subject(nodeset),
          predicate_label = xml_val(anno, ".//propertyURI/@label"),
          predicate_uri = xml_val(anno, ".//propertyURI"),
          object_label = xml_val(anno, ".//valueURI/@label"),
          object_uri = xml_val(anno, ".//valueURI"))
      })
    res <- data.table::rbindlist(res)
    res <- res[!duplicated(res), ]
    res <- res[res$id != "/", ] # rm unsupported ResponsibleParty (i.e. no individualName)
    invisible(write_template(res, "annotations.txt", path))
    return(res)
  }
}








#' Create categorical variable template(s) from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets categorical variable nodes and parses to template.
#' 
#' @return categorical variable template(s)
#' 
eml2categorical_variables <- function(eml, path) {
  tbls <- xml2::xml_find_all(eml, "/eml:eml/dataset/dataTable")
  if (!is_empty_nodeset(tbls)) {
    res <- lapply(                 # for each table
      tbls,
      function(tbl) {
        attrs <- xml2::xml_find_all(tbl, "./attributeList/attribute")
        name <- xml_val(tbl, "./physical/objectName")
        attrslist <- lapply(
          attrs,
          function(attr) {         # for each attribute
            if (suppressWarnings(is_catvar(attr))) {
              res <- list(
                attributeName = xml_val(attr, "./attributeName"), 
                code = xml_val(attr, ".//codeDefinition/code"), 
                definition = xml_val(attr, ".//codeDefinition/definition"))
              return(res)
            }
          })
        res <- data.table::rbindlist(attrslist)
        fname <- paste0("catvars_", tools::file_path_sans_ext(name), ".txt")
        invisible(write_template(res, fname, path))
        return(res)
      })
    names(res) <- xml_val(tbls, "./physical/objectName")
    return(res)
  }
}








#' Create custom units template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets unitlist nodes and parses to template.
#' 
#' @return custom units template
#' 
eml2custom_units <- function(eml, path) {
  units <- xml2::xml_find_all(
    eml, "/eml:eml/additionalMetadata/metadata/unitList/unit")
  if (!is_empty_nodeset(units)) {
    unitslist <- lapply(
      units, 
      function(unit) {
        res <- list(
          id = xml2::xml_attr(unit, "id", default = ""), 
          unitType = xml2::xml_attr(unit, "unitType", default = ""), 
          parentSI = xml2::xml_attr(unit, "parentSI", default = ""), 
          multiplierToSI = xml2::xml_attr(unit, "multiplierToSI", default = ""), 
          description = xml_val(unit, "./description"))
        return(res)
      })
    res <- data.table::rbindlist(unitslist)
    invisible(write_template(res, "custom_units.txt", path))
    return(res)
  }
}








#' Create EAL inputs from an EML file
#' 
#' For when you want to work with EML in EAL but don't have the templates and make_eml() function call.
#'
#' @param eml (character) Full path to EML file
#' @param path (character) Where outputs will be written
#' @param file.type (character) File type for abstract, methods, and additional info. Can be: ".txt", ".docx", or ".md". Default is ".txt".
#'
#' @return EAL templates and \code{make_eml()} function call
#' 
#' @details 
#' Each sub-process within this function maps EML to an EAL file based on XPaths and logic representing known communities of practice. Information losses are sent as warnings. Remember, EAL focuses on metadata facilitating reuse (e.g. creator's email address), not antiquated info (e.g. creator's telephone number).
#' 
#' Benefits of \code{file.type} differ. ".docx" supports basic formatting (super/sub scripts, italics, symbols, accented characters) but doesn't support bulleted lists and elaborately formatted equations. ".md" supports less formatting than ".docx" but is open source. ".txt" doesn't support any formatting but is a common file type.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Create working directory
#' mydir <- paste0(tempdir(), "/pkg")
#' dir.create(mydir)
#' 
#' # Translate EML, w/unsupported content, to EAL templates. Note info loss 
#' # warnings.
#' eml <- system.file("eml2eal_test.xml", package = "EMLassemblyline")
#' eml2eal(eml, mydir)
#' dir(mydir)
#' 
#' # Clean up
#' unlink(mydir, recursive = TRUE)
#' }
#' 
eml2eal <- function(eml, path, file.type = ".txt") {
  validate_arguments("eml2eal", as.list(environment()))
  validate_eml_content(eml)
  f <- eml
  eml <- xml2::read_xml(f)
  invisible(try(eml2abstract(eml, path, file.type)))
  invisible(try(eml2additional_info(eml, path, file.type)))
  invisible(try(eml2annotations(eml, path)))
  invisible(try(eml2categorical_variables(eml, path)))
  invisible(try(eml2custom_units(eml, path)))
  invisible(try(eml2geographic_coverage(eml, path)))
  invisible(try(eml2intellectual_rights(eml, path, file.type = ".txt")))
  invisible(try(eml2keywords(eml, path)))
  invisible(try(eml2make_eml(eml, path)))
  invisible(try(eml2methods(eml, path, file.type)))
  eml <- xml2::read_xml(f) # reload eml modified by eml2methods()
  invisible(try(eml2personnel(eml, path)))
  invisible(try(eml2provenance(eml, path)))
  eml <- xml2::read_xml(f) # reload eml modified by eml2provenance()
  invisible(try(eml2table_attributes(eml, path)))
  invisible(try(eml2taxonomic_coverage(eml, path)))
}








#' Check info lost in the EML to EAL translation
#'
#' @param eml (character) Full path to original EML file
#' @param eml.eal (character) Full path to EML file created by \code{make_eml()}
#' 
#' @return (list) Lost values w/xpath names
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' # EML w/unsupported content
#' eml <- system.file("eml2eal_test.xml", package = "EMLassemblyline")
#' 
#' # Same EML but after translation through eml2eal()
#' emleal <- system.file(
#'   "/examples/pkg_260/eml/edi.260.1.xml", 
#'   package = "EMLassemblyline")
#' 
#' # Info losses
#' losses <- eml2eal_losses(eml, emleal)
#' }
#' 
eml2eal_losses <- function(eml, eml.eal) {
  eml <- xml2::read_xml(eml)
  emleal <- xml2::read_xml(eml.eal)
  xpvs_eml <- get_xpaths_and_vals(eml)
  xpvs_eal <- get_xpaths_and_vals(emleal)
  xp_loss <- mapply(                        # no loss if eml xpath+val is found in eal eml
    function(xp, v) {
      v_match <- xpvs_eal %in% v
      if (any(v_match)) {
        xp_match <- rm_pred(xp) %in% rm_pred(names(xpvs_eal[v_match]))
        if (xp_match) {
          return(NULL)                      # no loss, value listed at original xpath
        } else {
          xp_prnts <- rm_pred(
            xml2::xml_path(
              xml2::xml_parents(
                xml2::xml_find_all(eml, xp))))
          proxies <- c(
            "/eml:eml/dataset/abstract",
            "/eml:eml/dataset/additionalInfo",
            "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification")
          if (any(proxies %in% xp_prnts)) { # no loss, all content is preserved even if flattened
            return(NULL)
          }
          return(xp)                        # loss, value listed at different xpath
        }
      } else {
        return(xp)                          # loss, value isn't listed
      }
    },
    xp = names(xpvs_eml),
    v = xpvs_eml)
  xp_loss <- unlist(xp_loss)
  res <- lapply(                            # lost vals w/xpath names
    xp_loss,
    function(xpath) {
      return(xml_val(eml, xpath))
    })
  return(res)
}








#' Create geographic coverage template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets geographicCoverage nodes and parses to template.
#' 
#' @return geographic coverage template
#' 
eml2geographic_coverage <- function(eml, path) {
  geocovs <- xml2::xml_find_all(
    eml, "/eml:eml/dataset/coverage/geographicCoverage")
  if (!is_empty_nodeset(geocovs)) {
    geocovslist <- lapply(
      geocovs,
      function(geocov) {
        res <- list(
          geographicDescription = xml_val(geocov, ".//geographicDescription"), 
          northBoundingCoordinate = xml_val(geocov, ".//northBoundingCoordinate"), 
          southBoundingCoordinate = xml_val(geocov, ".//southBoundingCoordinate"), 
          eastBoundingCoordinate = xml_val(geocov, ".//eastBoundingCoordinate"), 
          westBoundingCoordinate = xml_val(geocov, ".//westBoundingCoordinate"))
        return(res)
      })
    res <- data.table::rbindlist(geocovslist)
    invisible(write_template(res, "geographic_coverage.txt", path))
    return(res)
  }
}








#' Create intellectual_rights template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' @param file.type (character) File type to write to (see \code{eml2eal()})
#' 
#' @details Gets intellectualRights node and parses to template via pandoc.
#' 
#' @return intellectual_rights template
#' 
eml2intellectual_rights <- function(eml, path, file.type) {
  intlrghts <- xml2::xml_find_all(eml, "/eml:eml/dataset/intellectualRights")
  if (!is_empty_nodeset(intlrghts)) {
    nodeset2txt(intlrghts, file.type, path)
  }
}








#' Create keywords template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets keywordSet nodes and parses to template.
#' 
#' @return keywords template
#' 
eml2keywords <- function(eml, path) {
  sets <- xml2::xml_find_all(eml, "/eml:eml/dataset/keywordSet")
  if (!is_empty_nodeset(sets)) {
    setslist <- lapply(
      sets, 
      function(set) {
        res <- list(
          keyword = xml_val(set, "./keyword"), 
          keywordThesaurus = xml_val(set, "./keywordThesaurus"))
        return(res)
      })
    res <- data.table::rbindlist(setslist)
    invisible(write_template(res, "keywords.txt", path))
    return(res)
  }
}








#' Create \code{make_eml()} function call from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets relevants nodes and parse to script.
#' 
#' @return run_EMLassemblyline_for_pkg.R script containing the \code{make_eml()} function call
#' 
eml2make_eml <- function(eml, path) {
  # args and their xpaths
  xpaths <- c(
    dataset.title = "/eml:eml/dataset/title",
    temporal.coverage = "/eml:eml/dataset/coverage/temporalCoverage/rangeOfDates//calendarDate",
    maintenance.description = "/eml:eml/dataset/maintenance/description",
    data.table = "/eml:eml/dataset/dataTable/physical/objectName",
    data.table.name = "/eml:eml/dataset/dataTable/entityName",
    data.table.description = "/eml:eml/dataset/dataTable/entityDescription",
    data.table.quote.character = "/eml:eml/dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/quoteCharacter",
    other.entity = "/eml:eml/dataset/otherEntity/physical/objectName",
    other.entity.name = "/eml:eml/dataset/otherEntity/entityName",
    other.entity.description = "/eml:eml/dataset/otherEntity/entityDescription",
    user.id = "/eml:eml/access/allow/principal",
    user.domain = "/eml:eml/@system",
    package.id = "/eml:eml/@packageId")
  # values from xpaths
  vals <- mapply(
    function(xpath, arg) {
      res <- get_parallel_nodes(eml, xpath)
      # modification of some values
      if (arg == "user.id") {                                     # user.id is always an editor
        perm <- get_parallel_nodes(eml, "/eml:eml/access/allow/permission")
        if (all(perm %in% "")) {
          res <- ""
        } else {
          res <- res[perm %in% "all"]
          for (i in 1:length(res)) {
            dstname <- unlist(stringr::str_split(res[i], ","))    # user.id from LDAP distinguished name
            if (length(dstname) > 1) {
              res[i] <- stringr::str_extract(dstname[1], "(?<==).+")
            }
          }
        }
      } else if (arg == "user.domain") {                          # user.domain from system
        if (res == "https://pasta.lternet.edu") {
          res <- "EDI"
        } else if (res == "https://arcticdata.io") {
          res <- "ADC"
        } else if (res == "knb") {
          res <- "KNB"
        } else {
          res <- "unknown"
        }
      }
      return(res)
    },
    xpath = xpaths,
    arg = names(xpaths))
  vals$user.domain <- rep(vals$user.domain, length(vals$user.id))          # user.domain/user.id equal lengths
  vals[vals == ""] <- NULL                                                 # rm empty args
  vals <- c(path = stringr::str_replace_all(path, "\\\\", "/"), vals)      # add path to args & vals
  vals$package.id <- stringr::str_replace_all(vals$package.id, "/|:", "_") # fix unsupported chars in package.id/fname
  # args and vals to strings
  args <- mapply(
    function(val, arg) {
      if (length(val) > 1) {     # list if > 1
        res <- paste0(
          "  ", arg, " = c(",
          paste(paste0("'", val, "'"), collapse = ", "),
          ")")
      } else {
        res <- paste0("  ", arg, " = '", val, "'")
      }
      if (arg != "package.id") { # comma if not last arg
        res <- paste0(res, ",")
      }
      res <- paste0(res, "\n")   # newline
      return(res)
    },
    val = vals,
    arg = names(vals))
  args <- as.list(args)
  # strings to function call
  funcall <- c(
    "library('EMLassemblyline')\n\n", "make_eml(\n", unlist(args), ")")
  # write
  fname <- paste0(path, "/make_eml.R")
  if (file.exists(fname)) {
    warning(fname, " exists and will not be overwritten", call. = FALSE)
  } else {
    writeLines(funcall, fname, sep = "") 
  }
}








#' Create methods template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' @param file.type (character) File type to write to (see \code{eml2eal()})
#' 
#' @details Gets methods node, removes provenance methodStep, subsumes sampling and qualityControl, and parses to template via pandoc.
#' 
#' @return methods template
#' 
eml2methods <- function(eml, path, file.type) {
  mthdsstps <- xml2::xml_find_all(eml, "/eml:eml/dataset/methods/methodStep") # Remove provenenace
  if (!is_empty_nodeset(mthdsstps)) {
    iprov <- lapply(mthdsstps, is_prov)
    prov <- mthdsstps[unlist(iprov)]
    xml2::xml_remove(prov)
    mthds <- xml2::xml_find_all(eml, "/eml:eml/dataset/methods")              # Write methods
    if (!is_empty_nodeset(mthds)) {
      nodeset2txt(mthds, file.type, path)
    }
  }
}








#' Create personnel template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets personnel and project nodes and parses to template.
#' 
#' @return personnel template
#' 
eml2personnel <- function(eml, path) {
  # Get responsible parties
  xpaths <- c(
    creator = "/eml:eml/dataset/creator",
    contact = "/eml:eml/dataset/contact",
    assocparty = "/eml:eml/dataset/associatedParty")
  rspps <- lapply(
    xpaths,
    function(xpath) {
      prsns <- xml2::xml_find_all(eml, xpath)
      rol <- names(xpaths)[xpaths %in% xpath]
      rsp <- lapply(
        prsns,
        function(prs) {
          res <- get_resparty(prs)
          if (rol == "creator" | rol == "contact") {
            res$role <- rol
          }
          res$projectTitle <-  ""
          res$fundingAgency <- ""
          res$fundingNumber <- ""
          return(res)
        })
      rsp <- data.table::rbindlist(rsp)
      return(rsp)
    })
  rspps <- data.table::rbindlist(rspps)
  projs <- get_proj(eml)
  if (nrow(projs) != 0) {
    projs$role <- "PI"
  }
  res <- rbind(rspps, projs)
  if (nrow(res) != 0) {
    invisible(write_template(res, "personnel.txt", path))
    return(res)
  }
}








#' Create provenance template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets provenance nodes and parses to template.
#' 
#' @return provenance template
#' 
eml2provenance <- function(eml, path) {
  mthdstps <- xml2::xml_find_all(eml, "/eml:eml/dataset/methods/methodStep")
  if (!is_empty_nodeset(mthdstps)) {
    res <- lapply(
      mthdstps,
      function(mthdstp) {
        if (!is_prov(mthdstp)) {
          return(NULL)
        }
        url <- xml_val(mthdstp, ".//distribution/online/url")
        if (stringr::str_detect(url, "https://pasta.lternet.edu")) { # source from EDI
          prts <- unlist(stringr::str_split(url, "/"))
          pkg <- paste0(prts[(length(prts)-2):length(prts)], collapse = ".")
          sys <- "EDI"
          url <- ""
          des <- ""
          ttl <- ""
          rol <- ""
          giv <- ""
          mid <- ""
          sur <- ""
          org <- ""
          email <- ""
        } else {                                                     # source from other
          pkg <- ""
          sys <- ""
          url <- xml_val(mthdstp, ".//distribution/online/url")
          des <- xml_val(mthdstp, ".//description")
          ttl <- xml_val(mthdstp, ".//dataSource/title")
          rspps <- c(
            xml2::xml_find_all(mthdstp, ".//creator"), 
            xml2::xml_find_all(mthdstp, ".//contact"))
          rsp <- lapply(
            rspps,
            function(rsp) {
              res <- get_resparty(rsp)
              res$role <- xml2::xml_name(rsp)
              res$userId <- NULL
              return(res)
            })
          rsp <- data.table::rbindlist(rsp)
          rol <- rsp$role
          giv <- rsp$givenName
          mid <- rsp$middleInitial
          sur <- rsp$surName
          org <- rsp$organizationName
          email <- rsp$electronicMailAddress
        }
        res <- list(
          dataPackageID = pkg, 
          systemID = sys, 
          url = url, 
          onlineDescription = des, 
          title = ttl,
          givenName = giv,
          middleInitial = mid,
          surName = sur,
          role = rol,
          organizationName = org,
          email = email)
        return(res)
      })
    res <- data.table::rbindlist(res)
    if (nrow(res) != 0) {
      invisible(write_template(res, "provenance.txt", path))
      return(res)
    }
  }
}








#' Create table attributes template(s) from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets table attribute nodes and parses to template.
#' 
#' @return table attributes template(s)
#' 
eml2table_attributes <- function(eml, path) {
  tbls <- xml2::xml_find_all(eml, "/eml:eml/dataset/dataTable")
  if (!is_empty_nodeset(tbls)) {
    res <- lapply(                 # for each table
      tbls,
      function(tbl) {
        attrs <- xml2::xml_find_all(tbl, "./attributeList/attribute")
        name <- xml_val(tbl, "./physical/objectName")
        attrslist <- lapply(
          attrs,
          function(attr) {         # for each attribute
            res <- list(
              attributeName = xml_val(attr, "./attributeName"), 
              attributeDefinition = xml_val(attr, "./attributeDefinition"), 
              class = get_class(attr), 
              unit = get_unit(attr), 
              dateTimeFormatString = xml_val(attr, "./measurementScale/dateTime/formatString"),
              missingValueCode = get_misscode(attr),
              missingValueCodeExplanation = get_misscodedef(attr))
            return(res)
          })
        res <- data.table::rbindlist(attrslist)
        fname <- paste0("attributes_", tools::file_path_sans_ext(name), ".txt")
        invisible(write_template(res, fname, path))
        return(res)
      })
    names(res) <- xml_val(tbls, "./physical/objectName")
    return(res)
  }
}








#' Create taxonomic coverage template from EML
#'
#' @param eml (xml_document xml_node) EML
#' @param path (character) Path to write to
#' 
#' @details Gets taxonomicClassification nodes and parses to template.
#' 
#' @return taxonomic coverage template
#' 
eml2taxonomic_coverage <- function(eml, path) {
  txclss <- xml2::xml_find_all(
    eml, "/eml:eml/dataset/coverage/taxonomicCoverage/taxonomicClassification")
  if (!is_empty_nodeset(txclss)) {
    txclsslist <- lapply(
      txclss, 
      function(txcls) {
        nstd <- xml2::xml_find_all(txcls, ".//taxonomicClassification") # all classifications
        if (!is_empty_nodeset(nstd)) {                                  # below requires node not node_set
          dpst <- nstd[[length(nstd)]]                                  # deepest
        } else {
          dpst <- txcls
        }
        authsys <- xml_val(dpst, "./taxonId/@provider") # convert EAL recognized authorities
        if (authsys != "") {
          if (stringr::str_detect(authsys, "itis.gov")) {
            authsys <- "ITIS"
          } else if (stringr::str_detect(authsys, "marinespecies.org")) {
            authsys <- "WORMS"
          } else if (stringr::str_detect(authsys, "gbif.org")) {
            authsys <- "GBIF"
          }
        }
        res <- list(
          name = xml_val(dpst, "./taxonRankValue"), 
          name_type = "",
          name_resolved = xml_val(dpst, "./taxonRankValue"),
          authority_system = authsys,
          authority_id = xml_val(dpst, "./taxonId"))
        return(res)
      })
    res <- data.table::rbindlist(txclsslist)
    invisible(write_template(res, "taxonomic_coverage.txt", path))
    return(res)
  }
}








#' Get context for annotations template
#'
#' @param nodeset (xml_nodeset) Subject nodeset
#' 
#' @return (character) Value for context field of annotations template
#' 
get_anno_context <- function(nodeset) {
  self <- xml2::xml_name(nodeset)
  if (self == "dataset") {
    return("eml")
  } else if (self == "dataTable") {
    return("dataset")
  } else if (self == "attribute" & has_parent(nodeset, "dataTable")) {
    objxpath <- paste0(
      stringr::str_remove(xml2::xml_path(nodeset), "attributeList.*"),
      "physical/objectName")
    obj <- xml_val(nodeset, objxpath)
    return(obj)
  } else if (self == "otherEntity") {
    return("dataset")
  } else if (stringr::str_detect(self, "creator|contact|associatedParty|personnel")) {
    return("dataset")
  }
}








#' Get element for annotations template
#'
#' @param nodeset (xml_nodeset) Subject nodeset
#' 
#' @return (character) Value for element field of annotations template
#' 
get_anno_element <- function(nodeset) {
  self <- xml2::xml_name(nodeset)
  if (self == "dataset") {
    return("/dataset")
  } else if (self == "dataTable") {
    return("/dataTable")
  } else if (self == "attribute" & has_parent(nodeset, "dataTable")) {
    return("/dataTable/attribute")
  } else if (self == "otherEntity") {
    return("otherEntity")
  } else if (stringr::str_detect(self, "creator|contact|associatedParty|personnel")) {
    return("ResponsibleParty")
  }
}








#' Get id for annotations template
#'
#' @param nodeset (xml_nodeset) Subject nodeset
#' 
#' @return (character) Value for id field of annotations template
#' 
get_anno_id <- function(nodeset) {
  self <- xml2::xml_name(nodeset)
  if (self == "dataset") {
    return("/dataset")
  } else if (self == "dataTable") {
    obj <- xml_val(nodeset, ".//physical/objectName")
    return(paste0("/", obj))
  } else if (self == "attribute" & has_parent(nodeset, "dataTable")) {
    objxpath <- paste0(
      stringr::str_remove(xml2::xml_path(nodeset), "attributeList.*"),
      "physical/objectName")
    obj <- xml_val(nodeset, objxpath)
    attr <- xml_val(nodeset, ".//attributeName")
    return(paste0("/", obj, "/", attr))
  } else if (self == "otherEntity") {
    obj <- xml_val(nodeset, ".//physical/objectName")
    return(paste0("/", obj))
  } else if (stringr::str_detect(self, "creator|contact|associatedParty|personnel")) {
    giv <- xml_val(nodeset, ".//givenName[1]")
    mid <- xml_val(nodeset, ".//givenName[2]")
    sur <- xml_val(nodeset, ".//surName")
    return(paste0("/", paste(giv, mid, sur)))
  }
}








#' Get subject for annotations template
#'
#' @param nodeset (xml_nodeset) Subject nodeset
#' 
#' @return (character) Value for subject field of annotations template
#' 
get_anno_subject <- function(nodeset) {
  self <- xml2::xml_name(nodeset)
  if (self == "dataset") {
    return("dataset")
  } else if (stringr::str_detect(self, "dataTable|otherEntity")) {
    obj <- xml_val(nodeset, ".//physical/objectName")
    return(obj)
  } else if (self == "attribute" & has_parent(nodeset, "dataTable")) {
    attr <- xml_val(nodeset, ".//attributeName")
    return(attr)
  } else if (stringr::str_detect(self, "creator|contact|associatedParty|personnel")) {
    id <- get_anno_id(nodeset)
    return(stringr::str_remove(id, "/"))
  }
}









#' Get class of table attribute
#'
#' @param nodeset (xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}
#' 
#' @details Wrapper to \code{is_num(nodeset)}, \code{is_char(nodeset)}, \code{is_catvar(nodeset)}, \code{is_date(nodeset)}
#' 
#' @return (character) numeric, character, categorical, or Date
#' 
get_class <- function(nodeset) {
  cls <-  c(
    numeric = suppressWarnings(is_num(nodeset)),        # warnings handled by validate_eml_content()
    character = is_char(nodeset),
    categorical = suppressWarnings(is_catvar(nodeset)), # warnings handled by validate_eml_content()
    Date = is_date(nodeset))
  res <- names(cls[cls])
  return(res)
}








#' Get missing value code
#'
#' @param nodeset (xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}
#' 
#' @details Get from \code{./missingValueCode/code}. Use first and warn if > 2.
#' 
#' @return (character) Missing value code
#' 
get_misscode <- function(nodeset) {
  codes <- xml2::xml_find_all(nodeset, "./missingValueCode/code")
  if (!is_empty_nodeset(codes)) {
    res <- xml2::xml_text(codes)
    if (length(res) > 1) { # EAL cannot handle >1 one missing value code
      warning(
        "Info loss ... only first missingValueCode of attribute '", 
        xml_val(nodeset, "attributeName"), "' will be kept", call. = F)
    }
    return(res[1])
  }
}








#' Get missing value code definition
#'
#' @param nodeset (xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}
#' 
#' @details Get from \code{./attribute/missingValueCode/codeExplanation}. Use first and warn if > 2.
#' 
#' @return (character) Missing value code definition
#' 
get_misscodedef <- function(nodeset) {
  defs <- xml2::xml_find_all(nodeset, "./missingValueCode/codeExplanation")
  if (!is_empty_nodeset(defs)) {
    res <- xml2::xml_text(defs)
    if (length(defs) > 1) { # EAL cannot handle >1 one missing value code
      warning(
        "Info loss ... only first missingValueCodeExplanation of attribute '", 
        xml_val(nodeset, "attributeName"), "' will be kept", call. = F)
    }
    return(res[1])
  }
}








#' Get parallel node values
#'
#' @param eml (xml_document xml_node) EML
#' @param xpath (character) xpath to node
#'
#' @return (character) "" if node is missing from one nodeset but not the other
#' 
#' @details Works for dataTable and otherEntity, not others
#' 
get_parallel_nodes <- function(eml, xpath) {
  nodesets <- xml2::xml_find_all(
    eml, stringr::str_extract(xpath, ".*dataTable|otherEntity"))
  if (!is_empty_nodeset(nodesets)) {
    res <- lapply(
      nodesets,
      function(nodeset) {
        relxpath <- paste0(
          "./", 
          stringr::str_extract(xpath, "(?<=dataTable|otherEntity).*"))
        res <- xml_val(nodeset, relxpath)
        return(res)
      })
    res <- unlist(res)
    return(res)
  } else {
    res <- xml_val(eml, xpath)
    res <- unlist(res)
    return(res)
  }
}








#' Get project information
#'
#' @param eml (xml_document xml_node) EML
#' 
#' @details Get from \code{/eml:eml/dataset/project} or \code{/eml:eml/dataset/project/relatedProject}.
#' 
#' @return (character) givenName[1], givenName[2] (i.e. middleInitial), surName, organizationName, electronicMailAddress, userId, role, title (i.e. projectTitle), funding (i.e. fundingAgency + fundingNumber)
#' 
get_proj <- function(eml) {
  xpaths <- c(
    "/eml:eml/dataset/project", 
    "/eml:eml/dataset/project/relatedProject")
  res <- lapply(
    xpaths,
    function(xpath) {                                        # for each project type
      projs <- xml2::xml_find_all(eml, xpath)
      projlist <- lapply(
        projs,
        function(proj) {
          nodeset <- xml2::xml_find_all(proj, "./personnel") # get personnel
          res <- data.table::rbindlist(lapply(nodeset, get_resparty))
          res$projectTitle <- xml_val(proj, "./title")       # add funding
          res$fundingAgency <- xml_val(proj, "./funding")
          res$fundingNumber <- ""
          return(res)
        })
      res <- data.table::rbindlist(projlist)
    })
  res <- data.table::rbindlist(res)
  return(res)
}








#' Get referenced node
#'
#' @param nodeset (xml_nodeset) Any nodeset
#' 
#' @return (xml_nodeset) nodeset with matching id attribute
#' 
get_reference <- function(nodeset) {
  ref <- xml_val(nodeset, './/@references')
  if (ref != "") {
    res <- xml2::xml_find_all(
      nodeset, 
      paste0('/eml:eml//*[@id="', ref, '"]'))
    return(res)
  }
}








#' Get responsible party
#'
#' @param nodeset (xml_nodeset) ResponsibleParty nodeset
#' 
#' @details Get from \code{/eml:eml/dataset/creator}, \code{/eml:eml/dataset/contact}, \code{/eml:eml/dataset/associatedParty}.
#' 
#' @return (character) givenName[1], givenName[2] (i.e. middleInitial), surName, organizationName, electronicMailAddress, userId, role
#' 
get_resparty <- function(nodeset) {
  res <- list(
    givenName = xml_val(nodeset, ".//individualName/givenName[1]"), 
    middleInitial = xml_val(nodeset, "./individualName/givenName[2]"), 
    surName = xml_val(nodeset, "./individualName/surName"),
    organizationName = xml_val(nodeset, "./organizationName"),
    electronicMailAddress = xml_val(nodeset, "./electronicMailAddress"),
    userId = xml_val(nodeset, "./userId"),
    role = xml_val(nodeset, "./role"))
  empty_indivname <- all(
    c(res$givenName, res$middleInitial, res$surName) %in% "") # positionName is added via givenName when other names are empty
  if (empty_indivname) {
    res$givenName <- xml_val(nodeset, ".//positionName")
  }
  return(res)
}








#' Get unit
#'
#' @param nodeset (xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}
#' 
#' @details Get unit from \code{./unit/standardUnit} or \code{./attribute/measurementScale/ratio/unit/customUnit}
#' 
#' @return (character) unit
#' 
get_unit <- function(nodeset) {
  res <- xml_val(nodeset, ".//unit")
  return(res)
}








#' Get all xpaths and values in a nodeset
#'
#' @param nodeset (xml_nodeset/xml_node) A nodeset
#' 
#' @return (list) All xpaths and values in \code{nodeset}, where xpaths are stored as value names
#' 
get_xpaths_and_vals <- function(nodeset) {
  res <- list()
  children <- xml2::xml_children(nodeset)
  for (x in 1:length(children)) {
    if (length(xml2::xml_children(children[x])) == 0) {
      xpath <- xml2::xml_path(children[x])
      val <- xml_val(children, xpath)
      names(val) <- xpath
      res <- c(res, val)
    } else {
      res <- c(res, get_xpaths_and_vals(children[x]))
    }
  }
  res <- res[res != ""] # remove empties
  return(res)
}








#' Has parent node
#'
#' @param nodeset (xml_nodeset) A nodeset
#' @param parent (character) Name of parent to search for
#' 
#' @return (logical) TRUE if \code{parent} is in the list of parents
#' 
has_parent <- function(nodeset, parent) {
  parents <- xml2::xml_name(xml2::xml_parents(nodeset))
  return(parent %in% parents)
}









#' Is attribute a categorical variable?
#'
#' @param nodeset (xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}
#' 
#' @return (logical) TRUE if nodeset has \code{./measurementScale/nominal/nonNumericDomain/enumeratedDomain} or \code{./measurementScale/ordinal/nonNumericDomain/enumeratedDomain}
#' 
is_catvar <- function(nodeset) {
  xpaths <- c(
    nominal = "./measurementScale/nominal/nonNumericDomain/enumeratedDomain",
    ordinal = "./measurementScale/ordinal/nonNumericDomain/enumeratedDomain")
  scale <- lapply(
    xpaths,
    function(xpath) {
      !is_empty_nodeset(xml2::xml_find_all(nodeset, xpath))
    })
  if (scale$ordinal) { # EAL does not create ordinal
    warning(
      "Info loss ... measurement scale from ordinal to nominal of attribute '", 
      xml_val(nodeset, "attributeName"), "'", call. = F)
  }
  res <- any(unlist(scale))
  return(res)
}








#' Is attribute a character?
#'
#' @param nodeset (xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}
#' 
#' @return (logical) TRUE if nodeset has \code{./measurementScale/nominal/nonNumericDomain/textDomain} or \code{./measurementScale/ordinal/nonNumericDomain/textDomain}
#' 
is_char <- function(nodeset) {
  xpaths <- c(
    nominal = "./measurementScale/nominal/nonNumericDomain/textDomain",
    ordinal = "./measurementScale/ordinal/nonNumericDomain/textDomain")
  scale <- lapply(
    xpaths,
    function(xpath) {
      !is_empty_nodeset(xml2::xml_find_all(nodeset, xpath))
    })
  if (scale$ordinal) { # EAL does not create ordinal
    warning(
      "Info loss ... ordinal to nominal of attribute '", 
      xml_val(nodeset, "attributeName"), "'", call. = F)
  }
  res <- any(unlist(scale))
  return(res)
}








#' Is attribute a Date?
#'
#' @param nodeset (xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}
#' 
#' @return (logical) TRUE if nodeset has \code{./measurementScale/dateTime}
#' 
is_date <- function(nodeset) {
  dttmdomain <- xml2::xml_find_all(nodeset, "./measurementScale/dateTime")
  res <- !is_empty_nodeset(dttmdomain)
  return(res)
}








#' Is empty nodeset?
#'
#' @param nodeset (xml_nodeset) Any nodeset returned by the xml2 library
#' 
#' @return (logical) TRUE if nodeset length = 0
#' 
is_empty_nodeset <- function(nodeset) {
  res <- length(nodeset) == 0
  return(res)
}








#' Is attribute a numeric variable?
#'
#' @param nodeset (xml_nodeset) Attribute nodeset at \code{/eml:eml/dataset/dataTable[x]/attributeList/attribute[y]}
#' 
#' @return (logical) TRUE if nodeset has \code{./attribute/measurementScale/interval} or \code{./attribute/measurementScale/ratio}
#' 
is_num <- function(nodeset) {
  xpaths <- c(
    interval = "./measurementScale/interval",
    ratio = "./measurementScale/ratio")
  scale <- lapply(
    xpaths,
    function(xpath) {
      !is_empty_nodeset(xml2::xml_find_all(nodeset, xpath))
    })
  if (scale$interval) { # EAL cannot create interval
    warning(
      "Info loss ... measurement scale from interval to ratio of attribute '", 
      xml_val(nodeset, "attributeName"), "'", call. = F)
  }
  res <- any(unlist(scale))
  return(res)
}








#' Is provenance node?
#'
#' @param nodeset (xml_nodeset) methods nodeset at \code{/eml:eml/dataset/methods/methodStep}
#' 
#' @details Looks for provenance in \code{./dataSource}
#' 
#' @return (logical) TRUE if nodeset has provenance
#' 
is_prov <- function(nodeset) {  
  dasource <- xml2::xml_find_all(nodeset, "./dataSource")
  res <- !is_empty_nodeset(dasource)
  return(res)
}








#' Convert nodeset to EAL text type template
#'
#' @param nodeset (xml_nodeset) Nodeset to convert
#' @param file.type (character) File type to write to (see \code{eml2eal()})
#' @param path (character) Path to which outputs will be written
#' 
#' @details Converts EML text type \code{nodeset} (i.e. abstract, methods, additionalInfo, intellectualRights) to EAL \code{file.type} via pandoc
#' 
#' @return abstract, methods, additional_info, or intellectual_rights
#' 
nodeset2txt <- function(nodeset, file.type, path) {
  # Map names: EML to EAL
  if (xml2::xml_name(nodeset) == "additionalInfo") {
    name <- "additional_info"
  } else if (xml2::xml_name(nodeset) == "intellectualRights") {
    name <- "intellectual_rights"
  } else {
    name <- xml2::xml_name(nodeset)
  }
  # Map file type: EAL to pandoc
  if (file.type == ".txt") {
    file.type.pd <- "asciidoc"
  } else if (file.type == ".docx") {
    file.type.pd <- "docx"
  } else if (file.type == ".md") {
    file.type.pd <- "markdown"
  }
  # Write nodeset to file for pandoc
  tmpf <- paste0(tempdir(), "/nodeset.xml")
  xml2::write_xml(nodeset, tmpf)
  on.exit(unlink(tmpf))
  # Convert
  fname <- paste0(path, "/", name, file.type)
  if (file.exists(fname)) {
    warning(fname, " exists and will not be overwritten", call. = FALSE)
  } else {
    rmarkdown::pandoc_convert(input = tmpf, to = file.type.pd, output = fname)
  }
}








#' Remove xpath predicates
#'
#' @param xpath (character) xpath
#' 
#' @return (character) \code{xpath} with predicates removed
#' 
rm_pred <- function(xpath) {  
  res <- stringr::str_remove_all(xpath, "\\[[:digit:]*\\]")
  return(res)
}








#' Check EML for supported/unsupported content
#'
#' @param eml (character) Full path to EML file
#' 
#' @return Warnings/errors/messages of unsupported content found in the EML. This supplements warnings/errors/messages returned by \code{eml2*} functions, which are more context specific.
#' 
validate_eml_content <- function(eml) {
  if (!EML::eml_validate(eml)) {                                            # schema valid
    stop("Input EML is invalid. Cannot proceed. ", call. = F)
  }
  eml <- xml2::read_xml(eml)
  type <- xml2::xml_find_all(                                               # type
    eml, 
    paste0("/eml:eml/citation | /eml:eml/software | /eml:eml/protocol"))
  if (!is_empty_nodeset(type)) {
    warning("Info loss ... citation, software, and protocol types are not ",
            "supported", call. = F)
  }
  hl <- xml_val(eml, ".//numHeaderLines")                                   # number of header lines
  if (!all(hl %in% "1")) {
    warning("Info loss ... data objects with header lines  > 1 are not ",
            "supported.", call. = F)
  }
  ao <- xml_val(eml, ".//attributeOrientation")                             # attribute orientation
  if (!all(ao %in% "column")) {
    warning("Info loss ... data objects with attribute orientations other ",
            "than 'column' are not supported.", call. = F)
  }
  attrs <- xml2::xml_find_all(eml, ".//dataTable//.//attribute")
  invisible(lapply(attrs, is_catvar))                                       # attribute type ordinal
  invisible(lapply(attrs, is_num))                                          # attribute type interval
  et <- xml2::xml_find_all(                                                 # entity type
    eml, 
    paste0(
      ".//spatialRaster | .//spatialVector | .//storedProcedure | ",
      ".//view"))
  if (!is_empty_nodeset(et)) {
    warning("Info loss ... data objects of type 'spatialRaster', ",
            "'spatialVector', 'storedProcedure', and 'view' are not ",
            "supported.", call. = F)
  }
  warning("Potentially more info lost during translation. See ",            # general warning
          "'eml2eal_losses()' for more details", call. = F)
  # TODO: Provenance synonyms?
}








#' Get XML values
#'
#' @param nodeset (xml_node/xml_nodeset) Nodeset
#' @param xpath (character) xpath
#' 
#' @return (character) Value of \code{xpath} within \code{nodeset}. Returns "" if returned character string has length = 1.
#' 
#' @details Simplifies code by wrapping \code{cnvmt(xml2::xml_text(xml2::xml_find_all(...), trim = T))}
#' 
xml_val <- function(nodeset, xpath) {
  res <- cnvmt(
    xml2::xml_text(
      xml2::xml_find_all(nodeset, xpath),
      trim = T))
  return(res)
}