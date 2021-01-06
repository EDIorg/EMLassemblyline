<!-- This comment enables badge extraction to pkgdown site -->

[![Travis-CI Build Status](https://travis-ci.com/EDIorg/EMLassemblyline.svg?branch=master)](https://travis-ci.org/EDIorg/EMLassemblyline)
[![codecov.io](https://codecov.io/github/EDIorg/EMLassemblyline/coverage.svg?branch=master)](https://codecov.io/github/EDIorg/EMLassemblyline?branch=master)
[![DOI](https://zenodo.org/badge/84467795.svg)](https://zenodo.org/badge/latestdoi/84467795)

# EMLassemblyline (EAL)

For scientists and data managers to create high quality [EML](https://eml.ecoinformatics.org/) metadata for dataset publication. EAL is optimized for automating recurring publications (time series or data derived from time series sources) but works well for "one-off" publications, especially through the [MetaShARK](https://github.com/earnaud/MetaShARK-v2) interface. EAL prioritizes automated metadata extraction from data objects to minimize required human effort and encourages EML best practices to make publications Findable, Accessible, Interoperable, and Reusable.

## Features

* Optimized for automating recurring data publications
* Works well for one-off data publications
* Prioritizes automated metadata extraction from data objects
* Aligns with EML best practices of the U.S. Long Term Ecological Research Network (LTER)
* Requires no familiarity with EML
* Requires little familiarity with the R language
* Accepts all data types
* Is data repository agnostic

## Install

```
# Install from GitHub
remotes::install_github("EDIorg/EMLassemblyline")
```

## Usage

[An overview of the EAL use cases](https://ediorg.github.io/EMLassemblyline/articles/overview.html)

## Active projects

* Creating a Shiny interface for editing metadata template files thereby eliminating requirements for text and spreadsheet editors while facilitating use of dictionaries, vocabularies, and ontologies. This is under development in the [MetaShARK](https://github.com/earnaud/MetaShARK-v2) application, developped by the PNDB (French Biodiversity National Data Hub).
* Aligning EAL with a profile of the most commonly used EML elements to provide an exchange interface with other information systems (e.g. [LTER-core-metabase](https://github.com/lter/LTER-core-metabase)).
* See project [issues](https://github.com/EDIorg/EMLassemblyline/issues) for more.

## Contributing

Please contribute! See our [code of conduct](https://github.com/EDIorg/EMLassemblyline/blob/master/CODE_OF_CONDUCT.md) and [contributing guidelines](https://github.com/EDIorg/EMLassemblyline/blob/master/CONTRIBUTING.md) for details.

## Versioning

This project uses [semantic versioning](https://semver.org).

## Authors

Several people have contributed to this project. [List of contributors](https://github.com/EDIorg/EMLassemblyline/blob/master/AUTHORS.md).

## Related materials

* The [Ecological Metadata Language (EML)](https://knb.ecoinformatics.org/#external//emlparser/docs/index.html)
* Some [EML Best Practices](https://ediorg.github.io/data-package-best-practices/EMLmetadata/)
* The [EML R library](https://github.com/ropensci/EML) is the foundation of EAL.
