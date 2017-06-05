---
title: "EML Assembly Line"
author: "Colin Smith"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{introduction}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

The Ecological Metadata Language (EML) is a metadata specification developed by the ecological community and for the ecological community. EML communicates detailed information about data and how it was created. This metadata provides a basis for persistence, discoverability, and reuse, thereby extending the life cycle and value of the data. While the highly detailed and structured schema of EML is necessary to realize these benefits, it confronts the inexperienced data provider with a steep learning curve that few have time to tackle. To overcome this obstacle we have created the EML Assembly Line, a linear process and custom tools to craft high quality EML yourself.

EML assembly starts once you have compiled the data tables you would like to publish (Fig. 1). Next, import templates and view instructions to provide the core set of metadata for your dataset. Continue by operating tools to define and extract custom details of your data that you feel is important to communicate. Once you have built all the metadata parts, assemble these parts into the EML schema, run a validation check to ensure it is formatted correctly, and write the EML file. The EML file and your data comprise a data package that can be uploaded into a publicly accessible repository for reuse and citation.

![Figure 1. Assembly line overview.](https://github.com/EDIorg/emlAssemblyLine/blob/master/eml_assembly_line_overview.jpg)

While your first EML assembly may seem arduous and time consuming, we garuntee your next will be simpler and take less time. Really, you have already done the most challenging part. You have collected and processed a publication quality dataset with value for your own work, and possibly others. You have all the information up there in your head, now share and enjoy!

## Current capabilities and requirements
The EML assembly line is currently tooled for comma and tab delimited tables. Expanded functionality will be available in future versions. Currently we are developing the capacity to handle spatial vector and raster data entities.

## Collaborate with us!
User feed back will inform future development. We welcome any contributions (new code, bug reports, etc.) via [GitHub](https://github.com/EDIorg/emlAssemblyLine) and are available via email or phone if you would like to chat.

## Installation
Until the emlAssemblyLine is apart of CRAN, you will need to install it with the devtools package. Run devtools::install_github("EDIorg/emlAssemblyLine").

## References
[Ecological Metadata Language](https://knb.ecoinformatics.org/#external//emlparser/docs/index.html) A link to the EML schema.

[EML R code package](https://github.com/ropensci/EML) A link to the EML R package developed by the great people of the rOpenSci community. Without this package the emlAssemblyLine wouldn't work!

