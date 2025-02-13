EMLassemblyline 3.5.7 (2025-02-13)
==================================

### BUG FIXES

  * Update to latest version of `taxonomyCleanr` to address backward 
  compatibility issues introduced in `taxize` version `0.10.0`.

EMLassemblyline 3.5.6 (2025-01-16)
==================================

### BUG FIXES

  * Fix typo in validate_templates error message about mising [sic] attributes. 
  Update associated unit tests.
  * Remove URL validation for provenance metadata to prevent the removal of 
  valid sources that may not be accessible via HTTP GET requests.

### DOCUMENTATION

  * pkgdown is now run on pushes to main and development branches and published 
  to the gh-pages branch.


### TESTS

  * Test-coverage is now run on all pull requests to main and development 
  branches. Test coverage is hosted on Codecov.
  * R-CMD-check is now run on pull requests to main and development branches. 
  Note, this replaces use of Travis CI.
  * Test suite runs faster now that unnecessary HTTP requests have been 
  removed.

EMLassemblyline 3.5.5 (2022-10-31)
==================================

### BUG FIXES

  * `template_geographic_coverage()` now accepts numeric values to the 
  `site.col` parameter (#121).
  * EMLassemblyline now does a better job of distinguishing metadata template
  files from other files in the same directory.

EMLassemblyline 3.5.4 (2022-05-12)
==================================

### BUG FIXES

  * Now imports taxonomyCleanr >= 1.6.2. The EDIutils package was still listed
  for taxonomyCleanr 1.6.1 (#113).


EMLassemblyline 3.5.3 (2022-05-09)
=======================================

### BUG FIXES

  * Now imports taxonomyCleanr >= 1.6.1 so the EDIutils package is not 
  required (#112).
  
### MINOR IMPROVEMENTS

  * To identify EML files created with EMLassemblyline, the application and 
  version number are now included in the additionalMetadata element (#109).

# EMLassemblyline 3.5.2

### Enhancements

* Update the get_eol function - The get_eol() function was refactored for efficiency and clarity in the hymetDP package. The refactored function, unit tests, and updated implementation have been ported here.
* Update provenance.txt documentation - Added some minor details to the provenance.txt documentation.

### Bug fix

* Fix keyword resolver - Keywords were not resolving to the LTER vocabulary due to a regression that occurred during the EDIutils deprecation. The missing functions have been added back to EAL.

# EMLassemblyline 3.5.1

* Removed version requirements from dependencies. Removed unused EDIutils from list of dependencies.

# EMLassemblyline 3.5.0

* Integrated some EDIutils functions and removed the package dependency

# EMLassemblyline 3.4.1

### Bug fix

* A resolved issue in the taxonomyCleanr dependency requires a minimum of version 1.5.4.

# EMLassemblyline 3.4.0

### Enhancement

* Implement support for markdown and LaTex equations embedded in markdown. This only works for methods.md templates. NOTE: LaTex equations must be wrapped in "\$\$", otherwise they won't be parsed correctly. This addresses [issue #85](https://github.com/EDIorg/EMLassemblyline/issues/85).

### Bug fix

* Fix non-resolvable schemaLocation.
* Fix pandoc xml to .txt, .md, and, .docx translation issue.
* A resolved issue in the taxonomyCleanr dependency requires a minimum of version 1.5.3.

# EMLassemblyline 3.3.5

### Bug fix

* Defer to data.table's handling of quote characters during read operations.
* Remove an existing validation issues object from the global environment each time a validation suite is run, otherwise resolved issues may appear new.
* Remove validation checks where not critical to a functions operation.
* Remove title case coercion from personnel roles.
* Relax constraints on data table read operation.

# EMLassemblyline 3.3.4

### Bug fix

* __Schema validation:__ Recent changes to `emld` complicate validation of EML files input to some EAL functions. Validation of such inputs have been suspended until a permanent fix can be made. These changes have no impact on EAL working correctly, unless an invalid EML record is input, in which case the function will fail if there is a critical issue.

# EMLassemblyline 3.3.3

### Bug fix

* __template_table_attributes():__ A minor bug in the column classification logic has been fixed.

# EMLassemblyline 3.3.2

### Bug fix

* __Updated dependency:__ Delimiter detection was underperforming for MacOS. A new version of `EDIutils` fixes this.

# EMLassemblyline 3.3.1

### Bug fix

* __tab field delimiters:__ For tab delimited data tables, an empty white space was being written to the EML rather than the expected \\t. This fix addresses [issue #84](https://github.com/EDIorg/EMLassemblyline/issues/84).

# EMLassemblyline 3.3.0

### Enhancement

* __eml2eal():__ For when you want to work with EML in EAL but don't have the templates and make_eml() function call. This addresses [issue #76](https://github.com/EDIorg/EMLassemblyline/issues/76)
* __Remove last column name constraint:__ Column names can now be composed of any character strings handled by `data.table::fread()`.
* __Remove bookend quote characters mistakenly added to values:__ Some file writers will place quotes around a value when it contains commas. This safeguard identifies and removes these.

### Bug fix

* __Outdated dependencies:__ Some listed dependencies were outdated resulting in warnings/errors. This has been fixed.
* __template_categorical_variables():__ "" numeric categorical variables were being returned in the template as "NA". Now they are correctly returned as "".
* __Bug fix: table & attrs mismatch if unequal lengths:__ Table B would not match attrs B in this case.

# EMLassemblyline 3.2.1

### Bug fix

* __template_taxonomic_coverage():__ Resolving to ITIS began failing due to a change in title (from "ITIS" to "Integrated Taxonomic Information SystemITIS" listed in the return from `taxize::gnr_datasources()`. This issue has been fixed in the `taxonomyCleanr` dependency.

# EMLassemblyline 3.2.0

### Enhancement

* __Validate categorical variables:__ Validation of categorical variables has been enhanced with a check that each variable listed as "categorical" in table attributes templates are also listed in the categorical variables template.

# EMLassemblyline 3.1.0

### Enhancement

* __Improved handling of unsupported taxonomic authorities:__ Methods have been improved for taxa that have been manually resolved to unsupported taxonomic authoritiies (i.e. authorities other than "ITIS", "WORMS", "GBIF"). Values listed in the taxonomic coverage template's "authority_system" and "authority_id" fields, will become annotations in the EML returned by `make_eml()`. See `template_taxonomic_coverage()` function docs for more details. _NOTE: These new methods don't facilitate expansion of a taxon resolved in an unsupported system to the full classification hierarchy that is currently available when using ITIS, WORMS, or GBIF. That will require additional effort._ Additionally, `template_taxonomic_coverage()` now has an `empty` argument for returning an empty template. This enhancement partially addresses [issue #50](https://github.com/EDIorg/EMLassemblyline/issues/50).

# EMLassemblyline 3.0.0

### Enhancements

* __Semantic annotation:__ EML can now be annotated. This implementation supports two use cases:
    1. New EML ... created by the EMLassemblyline workflow:
        - Complete all metadata templates for your dataset (as usual)
        - Run `template_annotations()` to create the annotations template
            - The annotations template (annotations.txt) reports the annotatable elements within your metadata and assigns default predicate annotations. You’ll have to add object annotations from ontologies of your choosing. You can remove annotations by deleting rows and add annotations by copying a subject's row, pasting it to a new line, then modifying the object annotation fields.
            - Default annotations can be changed by the user
            - Instructions for creating annotations.txt from scratch are included in the function docs (for users gathering annotations in other ways).
            - Recurring nodes (e.g. ResponsibleParty) only require one set of annotations within annotations.txt
        - Run `make_eml()`
    2. Old EML ... created in other ways:
        - Run template_annotations() for your EML file
        - Run annotate_eml() to get an annotated revision of your EML file
        
    *Note: All annotated elements are assigned ids and their annotations are placed both immediately under the parent element (subject) and within the /eml/annotations node through id+reference pairs. This redundant approach supports variation in where EML metadata consumers prefer to harvest this information and supports annotation of EML elements requiring id+reference pairs.*

* __Provenance metadata template:__ This extends support for provenance metadata of data sources external to the EDI Data Repository. Create the template with `template_provenance()`. Fixes [issue #8](https://github.com/EDIorg/EMLassemblyline/issues/8)

* __Allow creation of partial EML (part 2):__ This completes implementation of issue #34 by moving all evaluation of inputs to make_eml() (and associated warning and error handling) from various locations in the code base to validate_templates(). With this implementation comes a new approach to communicating input issues to the user via template_issues, an object written to the global environment and formatted into a human readable report (message) when passed through issues().

* __UTF-8 character encoding:__ EMLassemblyline extracts metadata from data objects and may malform this content if the character encoding is not supported. In an attempt to minimize this issue and convert metadata into the UTF-8 encoding expected by EML, the Base R function `enc2utf8()` has been implemented anywhere metadata is extracted from data objects and written to file (i.e. templating functions) and anywhere template content is added to the EML (i.e. `make_eml()`). Because this may create EML that inaccuratly represents the data object it describes (e.g. categorical variables encoded in UTF-8 but the data encoded in something else) warnings are now issued when the input data object is not UTF-8 (or ASCII) encoded as estimated by `readr::guess_encoding()`. Additionally, EMLassemblyline documentation now emphasizes the importance of encoding data objects in UTF-8 first and then beginning the metadata creation process. An encoding conversion of TextType metadata (i.e. abstract, methods, additional_info) has not yet been implemented.

### Deprecation

* __import_templates():__ This function has been replaced by `template_core_metadata()` and `template_table_attributes()`.
* __define_catvars():__ This function has been replaced by `template_categorical_variables()`.
* __extract_geocoverage():__ This function has been replaced by `template_geographic_coverage()`.
* __affiliation argument of make_eml():__ This argument has been replaced by `user.domain`
* __data.files argument of make_eml():__ This argument has been replaced by `data.table`
* __data.files.description argument of make_eml():__ This argument has been replaced by `data.table.description`
* __data.files.quote.character argument of make_eml():__ This argument has been replaced by `data.table.quote.character`
* __data.files.url argument of make_eml():__ This argument has been replaced by `data.table.url`
* __zip.dir argument of make_eml():__ This argument has been replaced by `other.entity`
* __zip.dir.description argument of make_eml():__ This argument has been replaced by `other.entity.description`

# EMLassemblyline 2.18.2

### Bug fix

* __Patch for updated dependency (part 2):__ An updated dependency resulted in `template_table_attributes()` errors. This fix is an addendum to the prior fix (2.18.1).

# EMLassemblyline 2.18.1

### Bug fix

* __Patch for updated dependency:__ An updated dependency resulted in `template_table_attributes()` errors. This has been fixed.

# EMLassemblyline 2.18.0

### Enhancement

* __Allow creation of partial EML:__ During the draft process it is very useful to see a partial EML document, even if incomplete or invalid. Additionally, developers using EMLassemblyline as a backend (e.g. [MetaShARK](https://github.com/earnaud/MetaShARK-v2) and [Excel-to-EML](https://github.com/lkuiucsb/Excel-to-EML)) may not want the current set of input requirements for their applications. To accomodate these use cases, validation checks on inputs to `make_eml()` (often communicating best practice recommendations) have been refactored to return warnings rather than errors. Fixes [issue #34](https://github.com/EDIorg/EMLassemblyline/issues/34).

### Bug fixes

* __Coerce lat.col and lon.col inputs:__ `template_geographic_coverage()` `lat.col` and `lon.col` arguments expect numeric inputs and error if non-NA missing value codes are present. The values are now coerced to numeric, only complete cases returned in the geographic coverage template, and no errors occur.
* __Revert markdown parsing:__ Version 2.13.0 introduced better methods for parsing TextType templates (i.e. abstract, methods, and additional_info) from .docx, .txt, .md file types, however, .md lost some formatting controls. This has been fixed.

# EMLassemblyline 2.17.0

### Enhancement

* __Create EML for non-EDI repositories:__ Create EML for non-EDI repositories by refactoring the logic around the `make_eml()` function arguments `user.id`, `user.domain`, and `package.id`. Details are listed in the function documentation.

# EMLassemblyline 2.16.1

### Bug fix

* __template reader:__ Fixed a bug in the metadata template reader.

# EMLassemblyline 2.16.0

### Enhancements

* __maintenance.description:__ The `maintenance.description` of `make_eml()` is no longer required however, a missing `maintenance.description` will return a warning with the recommended best practice.
* __publisher:__ A data publisher can now be added by listing the person (or organization) with a "publisher" role to the personnel.txt template.
* __project:__ Missing project information (i.e. Principal Investigator and project metadata) return a warning with the recommended practice.
* __formatName:__ The formatName of an otherEntity is now auto-detected using the `mime` library. Undetected MIME Types are listed as "Unknown". Fixes [issue #68](https://github.com/EDIorg/EMLassemblyline/issues/68).
* __distribution:__ Previously, when assigning a .//physical/distribtuion/online/url for two or more data tables or other entities, each was required to have a corresponding URL listed under the `make_eml()` arguments `data.table.url` and `other.entity.url` . Some use cases require assignment of a URL to only one in a list of two or more. This constraint as been relaxed so if a data object doesn't have a corresponding URL then use the values `""` or `NA` (e.g. if in `make_eml()` the argument `data.table = c("nitrogen.csv", "decomp.csv")`, and a URL only exists for the second object, then `data.table.url = c("", "/url/to/decomp.csv")`.

# EMLassemblyline 2.15.0

### Enhancements

* __Installation:__ Simplified instructions so dependencies will be installed but and users will not be asked to upgrade installed packages (a point of confusion among many).

* __Default false numeric attributes to character:__ Default user specified numeric attributes to character class when the attribute contains character values outside of that specified under the missingValueCode field of the attributes.txt template. A warning alerts the user of the issue and preserves the original data by not coercing to numeric.

# EMLassemblyline 2.14.0

### Enhancement

* __Boiler plate EMLassemblyline process:__ Add boilerplate function calls to /inst/templates/run_EMLassemblyline.R. This script is added to the users workspace via `template_directories()`. The boilerplate is meant to be a reminder and save the user a little time. Fixes [issue #36](https://github.com/EDIorg/EMLassemblyline/issues/36).

# EMLassemblyline 2.13.1

### Bug fix

* __schemaLocation:__ Fixed schemaLocation and namespace to be a web resolvable address.

# EMLassemblyline 2.13.0

### Enhancement

* __TextType:__ Conversion of abstract.docx, methods.docx, and additional_info.docx to EML has been improved.

# EMLassemblyline 2.12.4

### Bug fixes

* __Missing contact or creator:__ Contacts and creators are required by `make_eml()` but no errors were returned when missing from personnel.txt. The logic of `validate_templates()` has been updated to fix this issue.
* __Missing value codes in categorical variables:__ Some missing value codes defined in the table attributes template were making their way into the categorical variables template. `template_categorical_variables()` has been updated to recognize more missing value code types.

# EMLassemblyline 2.12.3

### Bug fix

* __Missing \<taxonomicCoverage\>:__ taxonomicCoverage supplied in taxonomic_coverage.txt was missing from the EML. This has been fixed.

# EMLassemblyline 2.12.2

### Bug fix

* __Missing \<access\> node:__ The access node was not being added to the EML. This has been fixed.

# EMLassemblyline 2.12.1

### Bug fix

* __Template reader:__ A bug in the tabular template reader has been fixed ([see commit for details](https://github.com/EDIorg/EMLassemblyline/commit/42276a0a696a8c6e743cc15e28f4e9c22df1100e)).

### Enhancement

* __`make_eml()` code:__ (For developers) The underlying code of `make_eml()` is now more concise and understandable.

# EMLassemblyline 2.11.1

### Bug fixes

* __NAs in templates:__ Revised logic to distinguish the difference between NAs listed in the missingValueCode field of the table attribute template when supplied to `make_eml()` via files or the input argument `x`.
* __Validate personnel roles:__ Revised logic to interpret personnel roles.

# EMLassemblyline 2.11.0

### Enhancement

* __NAs in templates:__ Users often add NAs to templates where EMLassemblyline expects "". This enhancement ignores these extraneous NAs.

# EMLassemblyline 2.10.2

### Bug fix

* __Missing value codes:__ Recent changes broke the proper handling of "NA" missing value codes. This has been fixed.

# EMLassemblyline 2.10.1

### Bug fix

* __Schema validation:__ The referenced schema location was not correct. Now it is ([issue #59](https://github.com/EDIorg/EMLassemblyline/issues/59)).

# EMLassemblyline 2.10.0

### Enhancement

* __Schema validation:__ A schema validation error sporadically occurs under EML 2.1.1. Upgrading to EML 2.2.0 fixes this ([issue #59](https://github.com/EDIorg/EMLassemblyline/issues/59)).

# EMLassemblyline 2.9.0

### Enhancement

* __Template checks:__ A new suite of checks on metadata template content have been added to more effectively communicate issues and reduce errors. Fixes [issue #6](https://github.com/EDIorg/EMLassemblyline/issues/6), [issue #35](https://github.com/EDIorg/EMLassemblyline/issues/35), [issue #37](https://github.com/EDIorg/EMLassemblyline/issues/37), and [issue # 53](https://github.com/EDIorg/EMLassemblyline/issues/53).

# EMLassemblyline 2.8.0

### Enhancement

* __Online distribution:__ The previous implementation for providing URLs by which the data can be publicly downloaded required all data objects to be co-located in the same directory, which is too restrictive. Now URLs can be explicitly listed for each data object. Fixes [issue #56](https://github.com/EDIorg/EMLassemblyline/issues/56).

### Deprecation

* __data.url:__ The `data.url` argument has been deprecated in favor of `data.table.url` and `other.entity.url` but will be supported until March 11, 2021.

# EMLassemblyline 2.7.1

### Bug fix

* __Delimiter guessing:__ Occasionally the content of tabular templates leads `data.table::fread()` to guessing a delimiter other than "\\t". This issue has been fixed by explicitly stating the expected field delimiter.

# EMLassemblyline 2.7.0

### Enhancements

* __File names containing spaces caused `template_categorical_variables()` to crash:__ Errors occurred when input file names contained spaces. Using spaces is still a common practice among users. To accommodate this while continuing to promote best practices, the naming restriction has been relaxed and the best practices have been made a warning. The function checking file presence and naming conventions is `EDIutils::validate_file_name()`. An explicit file name specification (i.e. including extension) is now required, which precludes errors when the same file name is used among different file types in the same directory. Fixes [issue #25](https://github.com/EDIorg/EMLassemblyline/issues/25).
* __Validate units:__ Check that all numeric attributes have corresponding units and these units can be found in the EML standard unit dictionary or the custom_units.txt template. If not, then throw an error along with directions for fixing the issue. This check is called from make_eml(). Fixes [issue #38](https://github.com/EDIorg/EMLassemblyline/issues/38).
* __Multiple inputs to `template_taxonomic_coverage()`:__ If the taxa of a dataset are in more than one table, then a user would want to extract the unique taxa from all the tables and compile into the taxonomic_coverage.txt template. Multiple inputs to the `taxa.table` and `taxa.col` arguments is now supported.

### Bug fixes

* __EML schema validation:__ Schema validation in `make_eml()` began failing with release of the dependency libary EML 2.0.2. This has been fixed.
* __Support `;` delimiters:__ Data tables with semi-colon delimiters were not supported. This was fixed by updating `EDIutils::detect_delimiter()` ([issue #6](https://github.com/EDIorg/EDIutils/issues/6) of the EDIutils package).
* __Entity Name:__ Content from `data.table.description` in `make_eml()` was used to fill in the entity name. However, they are not the same and entity name should be specified separately. This was fixed by adding `data.table.name` and `other.entity.name` as arguments to `make_eml()`. The fix defaults `data.table.name` to `data.table` and `other.entity.name` to `other.entity` with a warning message. Fixes [issue #24](https://github.com/EDIorg/EMLassemblyline/issues/24).
* __NULL output from `template_geographic_coverage()`:__ NULL was output from this function when `empty = TRUE`, which is mostly a cosmetic issue. This was fixed by implementing some simple logic. Fixes [issue #32](https://github.com/EDIorg/EMLassemblyline/issues/32).
* __Updated table readers:__ Some user supplied data tables could not be read by `utils::read.table()`. To fix this `data.table::fread()`, a more autonomous and robust reader, replaced `read.table()` for reading both data and metadata templates. Fixes [issue #41](https://github.com/EDIorg/EMLassemblyline/issues/41).
* __geographic_coverage.txt fields mixed up when translated to EML:__ Further testing revealed the bug doesn't exist. Fixes [issue #43](https://github.com/EDIorg/EMLassemblyline/issues/43).
* __Quotes in license templates:__ Unescaped quotes characters in the license files were being converted to the <quote> element thereby invalidating the EML. This was fixed by adding escape characters to the quotes.
* __Testing `template_taxonomic_coverage()`:__ Travis CI has been failing because of long responses from API calls made by `template_taxonomic_coverage()`. To expedite tests and reduce errors, the example data now contains substantially fewer taxa to be resolved against authority systems.

# EMLassemblyline 2.6.1

### Bug fixes

* __Unit dictionary:__ The `view_unit_dictionary()` function was opening the unit dictionary in a separate non-searchable window. By removing the `utils` namespace from the function call the unit dictionary now opens within the RStudio source pane where searching is supported.
* __Missing value codes:__ The `EML` v2.0.0 refactor resulted in changes to how missing value codes are handled. This fix restores the original functionality where empty character strings in the missing value code and explanation fields don't result in validation errors.
* __Intellectual rights character encoding:__ The intellectual rights licenses (CC0 and CC-BY) contained non-UTF-8 encoded quote characters that produced invalid EML. These have been removed.
* __Geographic coverage sources:__ Only one geographic coverage input is allowed to the `make_eml()` function at a time. Valid sources are the geographic_coverage.txt template, the `geographic.coordinates` and `geographic.description` arguments of `make_eml()`, and the deprecated bounding_boxes.txt template.
* __Missing value codes as categorical variables:__ Missing value codes were being incorrectly listed as categorical variables by `template_categorical_variables()`. This issue has been fixed.
* __Taxonomic coverage:__ Invalid taxonomic coverage was being generated by EDI's `taxonomyCleanr::make_taxonomicCoverage()`. This issue has been fixed in that projects GitHub master branch, and the necessary adjustments have been made to `EMLassemblyline::make_eml()`.

# EMLassemblyline 2.6.0

### Enhancements

* __EML v2.0.0:__ `EMLassemblyline` has been refactored to run with the `EML' v2.0.0 dependency.
* __Text type metadata may now be supplied in .docx and .md files:__ Support for creating abstract, methods, and additional information metadata has been extended from simple text files to Microsoft Word (.docx) and Markdown (.md) file formats. Formatting of these files are translated to EML via `markdown` > Pandoc > docbook.
* __Create an empty geographic_coverage.txt:__ Sometimes the geographic coverage of a dataset cannot be extracted from a table and needs to be entered manually. Use the `template_geographic_coverage()` argument `empty = TRUE` to create an empty geographic_coverage.txt template.

# EMLassemblyline 2.5.3

### Enhancements

* __Add function examples:__ Add examples to function documentation.
* __Change template import:__ Import custom_units.txt with `template_table_attributes()` instead of with `template_core_metadata()`. This is a more logical pairing.

### Bug fixes

* __The argument validator should not check geographic coverage templates:__ This fix moves the presence/absence check for geographic coverage templates to `make_eml()`.
* __v2.4.6 functions should be accessible:__ This fix exports `EMLassemblyline` 2.4.6 functions that should be otherwise accessible for backwards compatibility.
* __File names should not require extensions:__ This fix restores functionality that was lost in the recent refactor.

# EMLassemblyline 2.5.0

### Enhancements

* __Website:__ Improved documentation with vignettes demonstrating common and advanced use cases. Implemented with `pkgdown`.
* __Templating functions:__ Functions creating metadata templates are grouped under the prefix `template_*` to simplify user understanding.
    * `template_arguments()` Create template for all user inputs to `EMLassemblyline` (i.e. metadata template content and function arguments) to entirely programmatic workflows with focus on supporting content ingestion from upstream metadata sources.
    * `template_categorical_variable()` Create categorical variables template (previously named `define_catvars()`).
    * `template_core_metadata()` Create core metadata templates (previously part of `import_templates()`).
    * `template_directories()` Create a simple and effective directory structure for `EMLassemblyline` files and data package contents.
    * `template_geographic_coverage()` Create geographic coverage template (a refactor of `extract_geocoverage()`).
    * `template_table_attributes()` Create table attributes templates (previously part of `import_templates()`).
    * `template_taxonomic_coverage()` Create the taxonomic coverage template for resolving taxa to one or more authority systems and supporting creation of the hierarchical rank specific EML taxonomicCoverage element by `make_eml()`.
* __Support for other entity data packages:__ Data packages comprised completely of other entities (i.e. non-tabular data) is now supported.
* __Geographic coverage:__ All geographic coverage metadata has been moved to _//dataset/coverage_, where most data repositories find it for rendering to maps and other visualizations for users.
* __Make EML for other data repositories:__ Arguments requiring EDI specific content (i.e. `user.id`, `user.domain`, `package.id`) have been relaxed to enable creation of EML for other data repositories.
* __Better entity descriptions:__ Use arguments `data.table.description` and `other.entity.description` for _//dataTable/entityName_ and _//otherEntity/entityName_, respectively. This provides a more meaningful file description than the file name it self.

### Deprecation

Several templating functions, templates, and arguments have been deprecated. Full backwards compatibility of these functions, templates, and arguments will be supported for the next year (i.e. until May 1, 2020).

__Functions:__

* `import_templates()` is deprecated in favor of `template_core_metadata()` (i.e. metadata required by all data packages) and `template_table_attributes()` (i.e. metadata for data tables).
* `define_catvars()` is deprecated in favor of `template_categorical_variables()`.
* `extract_geocoverage()` is deprecated in favor of `template_geographic_coverage()`

__Templates:__

* bounding_boxes.txt is deprecated in favor of geographic_coverage.txt
* geographic_coverage.txt is deprecated in favor of a new version of geographic_coverage.txt that supports both point locations and areas.

__Arguments:__

* `data.files` is deprecated in favor of `data.table`
* `data.files.description` is deprecated in favor of `data.table.description`
* `data.files.quote.character` is deprecated in favor of `data.table.quote.character`
* `data.files.url` is deprecated in favor of `data.url`
* `zip.dir` is deprecated in favor of `other.entity`
* `zip.dir.description` is deprecated in favor of `other.entity.description`
* `affiliation` is deprecated in favor of `user.domain`

### Bug fix

* __otherEntity url:__ Add EML element _//otherEntity/physical/distribution/online/url_ via `make_eml()`. This element was missing though documentation implied its existence.
