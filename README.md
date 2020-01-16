<!-- This comment enables badge extraction to pkgdown site -->

[![Travis-CI Build Status](https://travis-ci.com/EDIorg/EMLassemblyline.svg?branch=master)](https://travis-ci.org/EDIorg/EMLassemblyline)
[![codecov.io](https://codecov.io/github/EDIorg/EMLassemblyline/coverage.svg?branch=master)](https://codecov.io/github/EDIorg/EMLassemblyline?branch=master)
[![DOI](https://zenodo.org/badge/84467795.svg)](https://zenodo.org/badge/latestdoi/84467795)

# EMLassemblyline

For scientists and data managers who need to easily create high quality EML metadata for data publication. `EMLassemblyline` is a metadata builder that emphasizes auto-extraction of metadata, appends value added content, and inputs user supplied information through common interfaces thereby minimizing user effort while maximizing metadata features for data discovery and reuse.

## Features

* Requires no familiarity with EML metadata
* Good for one-off data publication
* Great for 10s to 100s of data publications
* Accepts all data and file types
* Automatically extracts metadata from data entities
* Automatically appends value added content
* Accepts user inputs through simple text and spreadsheet editors
* Supports automated data publication
* Supports complex reproducible science workflows
* Interoperable with metadata storage systems via an exchange format
* Incorporates community best practices
* Based on simple file organization scheme
* Is not tied to a specific data repository


## Install

```
# Install from GitHub
remotes::install_github("EDIorg/EMLassemblyline")
```

## Usage

[Check out example use cases in the EMLassemblyline website articles](https://ediorg.github.io/EMLassemblyline/)

## Active projects

* Creating a Shiny interface for editing metadata template files, thus eliminating requirements for text and spreadsheet editors while facilitating use of dictionaries, controlled vocabularies, and ontologies. This is currently developped in the [MetaShARK](https://github.com/earnaud/MetaShARK-v2) application, developped by the PNDB (french Biodiversity National Data Hub).
* Developing an exchange format for environmental metadata so `EMLassemblyline` (and other metadata builders) can be used with any metadata storage/organization system (e.g. [LTER-core-metabase](https://github.com/lter/LTER-core-metabase)).

## Contributing

We welcome contributions of all forms. Please reference our [code conduct](https://github.com/EDIorg/EMLassemblyline/blob/master/CODE_OF_CONDUCT.md) and [contributing guidelines](https://github.com/EDIorg/EMLassemblyline/blob/master/CONTRIBUTING.md) for details.

## Versioning

This project uses [semantic versioning](https://semver.org).

## Authors

Several people have contributed to this project. [List of contributors](https://github.com/EDIorg/EMLassemblyline/blob/master/AUTHORS.md).

## Related materials

[The Ecological Metadata Language (EML)](https://knb.ecoinformatics.org/#external//emlparser/docs/index.html)
