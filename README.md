# EML Assembly Line

The Ecological Metadata Language (EML) is a metadata standard developed by the ecological community for persistence, discoverability, and reuse of ecological data. EML extends the data lifespan allowing new insights and knowledge to be derived many years from now. While the highly detailed .xml structure of EML is necessary to organize and make searchable the metadata content, it confronts the inexperienced data provider with a steep learning curve that few have time to tackle. To overcome this we've created the EML Assembly Line, a linear process helping you craft high quality EML yourself! The EML Assembly Line is useful for publishing one-off datasets or providing a foundation for an EML factory.

Start creating EML once you have compiled the data for publication (Fig. 1). Next import templates and view instructions to build up the core metadata. If this core metadata doen't adequately describe your data, then call on additional functions to define and extract custom details to include in the final EML file. Once you have built all the metadata parts, assemble them into EML, run a validation check, and write the EML file. The EML and your data comprise a package that can be uploaded into a publicly accessible repository for reuse and citation.

![Figure 1. Assembly line overview.](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/eml_assembly_line_overview.jpg)

Figure 1. Assembly line overview.

While your first EML assembly may take the better part of a work day, your next will be faster. You've already done the most challenging part of collecting and compiling a set of high quality data, now document it and extend it's lifespan!

## Current capabilities and requirements
The EML assembly line accepts comma and tab delimited data tables. We are now developing the capacity to handle spatial vector and raster data entities.

## Contribute
User feed back will inform future development. We welcome contributions (i.e. new code, bug reports, etc.) via [GitHub](https://github.com/EDIorg/EMLassemblyline) or email (colin.smith@wisc.edu).

## Installation
Until this code is apart of CRAN, you will need to install the EMLassemblyline with the devtools package: 

install.packages("devtools")

library(devtools)

install_github("EDIorg/EMLassemblyline")

## References
[Ecological Metadata Language](https://knb.ecoinformatics.org/#external//emlparser/docs/index.html) A link to the EML schema.

[EML R code package](https://github.com/ropensci/EML) A link to the EML R package developed by the great people of the rOpenSci community. Without this package the EMLassemblyline wouldn't work!

