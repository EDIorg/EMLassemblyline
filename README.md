# The EML Assembly Line

The Ecological Metadata Language (EML) is a metadata standard developed by the ecological community for persistence, discoverability, and reuse of ecological data. EML facilitates understanding, persistence, and reuse of data. While the highly detailed structure of the EML schema is necessary to meet these ends it confronts the inexperienced data provider with a steep learning curve that few have time to tackle. To lower the bar for data documentation, while still maintaining a high level of metadata detail and quality, we've created the `EMLassemblyline` R code package. A user friendly workflow helping you craft high quality EML metadata yourself! The assembly line is useful for publishing one-off data packages for submission to a data repository or for providing a foundation to a data package factory.

The `EMLassemblyline` requires little operational knowledge of the R programming language or technical understanding of the EML schema. All you need is to know your dataset and how it was made, a little patience for following our step by step instructions, and how to enter operate a spread sheet editor. If this is your first time on the assembly line, it may take the better part of a day to become oriented and to produce your first EML metadata file.

## Getting started

### Contents

#### Documenation

[A schematic representation of the assembly line process.](https://github.com/EDIorg/EMLassemblyline/blob/development/documentation/schematic.md)

[A list of the EML schema elements that are populated by the assembly line with some notes on the level of detail supplied to these elements.](https://github.com/EDIorg/EMLassemblyline/blob/development/documentation/schema_use.md)

[Instructions for operating the EML assembly line](https://github.com/EDIorg/EMLassemblyline/blob/master/documentation/instructions.md)

#### R package

`EMLassemblyline` is a collection of wrapper functions to the `EML` R package developed by [Boettiger et al. (2017)](https://github.com/ropensci/EML). For an understanding of what is going on under the hood of the assembly line, we recommend you first take a look at the `EML` R package. Once you understand this you will be able to customize the assembly line for your own workflows or become a developer of this project.

The `EMLassemblyline` R package is available here on GitHub. To install, go to your RStudio Console window and enter these lines of code:

```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
```

Now reference the documentation listed above to start operating the assembly line.

## Running the tests

We have not yet formalized our testing.

## Contributing

We welcome contributions of all forms including code, bug reports, and requests for development. Please reference our [code conduct](https://github.com/EDIorg/EMLassemblyline/blob/master/CODE_OF_CONDUCT.md) and [contributing guidelines](https://github.com/EDIorg/EMLassemblyline/blob/master/CONTRIBUTING.md) for submitting pull requrests.

## Versioning

We do not yet have any versions available. Stay tuned!

## Authors

Several people have participated in this project. [View the current list of team members and contributors](https://github.com/EDIorg/EMLassemblyline/blob/master/AUTHORS.md).

## License

This project is licensed under the [CC0 1.0 Universal (CC0 1.0)](https://creativecommons.org/publicdomain/zero/1.0/legalcode) License - see the LICENSE file for details.

## Related materials

[Learn everything you wanted to know about the Ecological Metadata Language standard.](https://knb.ecoinformatics.org/#external//emlparser/docs/index.html)

[Reference the community developed best practices for EML content.](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/step-3-create-eml-metadata/best-practices-for-dataset-metadata-in-ecological-metadata-language-eml/)
