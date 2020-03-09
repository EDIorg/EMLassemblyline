# EMLassemblyline 2.7.1

## Bug fixes

* __Delimiter guessing:__ Occasionally the content of tabular templates leads `data.table::fread()` to guessing a delimiter other than "\t". This issue has been fixed by explicitly stating the expected field delimiter.

# EMLassemblyline 2.7.0

## Enhancements

* __File names containing spaces caused `template_categorical_variables()` to crash ([issue #25](https://github.com/EDIorg/EMLassemblyline/issues/25)):__ Errors occurred when input file names contained spaces. Using spaces is still a common practice among users. To accommodate this while continuing to promote best practices, the naming restriction has been relaxed and the best practices have been made a warning. The function checking file presence and naming conventions is `EDIutils::validate_file_name()`. An explicit file name specification (i.e. including extension) is now required, which precludes errors when the same file name is used among different file types in the same directory.
* __Validate units ([issue #38](https://github.com/EDIorg/EMLassemblyline/issues/38)):__ Check that all numeric attributes have corresponding units and these units can be found in the EML standard unit dictionary or the custom_units.txt template. If not, then throw an error along with directions for fixing the issue. This check is called from make_eml().
* __Multiple inputs to `template_taxonomic_coverage()`:__ If the taxa of a dataset are in more than one table, then a user would want to extract the unique taxa from all the tables and compile into the taxonomic_coverage.txt template. Multiple inputs to the `taxa.table` and `taxa.col` arguments is now supported.

## Bug fixes

* __EML schema validation:__ Schema validation in `make_eml()` began failing with release of the dependency libary EML 2.0.2. This has been fixed.
* __Support `;` delimiters:__ Data tables with semi-colon delimiters were not supported. This was fixed by updating `EDIutils::detect_delimiter()` ([issue #6](https://github.com/EDIorg/EDIutils/issues/6) of the EDIutils package).
* __Entity Name ([issue #24](https://github.com/EDIorg/EMLassemblyline/issues/24)):__ Content from `data.table.description` in `make_eml()` was used to fill in the entity name. However, they are not the same and entity name should be specified separately. This was fixed by adding `data.table.name` and `other.entity.name` as arguments to `make_eml()`. The fix defaults `data.table.name` to `data.table` and `other.entity.name` to `other.entity` with a warning message.
* __NULL output from `template_geographic_coverage()` ([issue #32](https://github.com/EDIorg/EMLassemblyline/issues/32)):__ NULL was output from this function when `empty = TRUE`, which is mostly a cosmetic issue. This was fixed by implementing some simple logic.
* __Updated table readers ([issue #41](https://github.com/EDIorg/EMLassemblyline/issues/41)):__ Some user supplied data tables could not be read by `utils::read.table()`. To fix this `data.table::fread()`, a more autonomous and robust reader, replaced `read.table()` for reading both data and metadata templates.
* __geographic_coverage.txt fields mixed up when translated to EML ([issue #43](https://github.com/EDIorg/EMLassemblyline/issues/43)):__ Further testing revealed the bug doesn't exist.
* __Quotes in license templates:__ Unescaped quotes characters in the license files were being converted to the <quote> element thereby invalidating the EML. This was fixed by adding escape characters to the quotes.
* __Testing `template_taxonomic_coverage()`:__ Travis CI has been failing because of long responses from API calls made by `template_taxonomic_coverage()`. To expedite tests and reduce errors, the example data now contains substantially fewer taxa to be resolved against authority systems.

# EMLassemblyline 2.6.1

## Bug fixes

* __Unit dictionary:__ The `view_unit_dictionary()` function was opening the unit dictionary in a separate non-searchable window. By removing the `utils` namespace from the function call the unit dictionary now opens within the RStudio source pane where searching is supported.
* __Missing value codes:__ The `EML` v2.0.0 refactor resulted in changes to how missing value codes are handled. This fix restores the original functionality where empty character strings in the missing value code and explanation fields don't result in validation errors.
* __Intellectual rights character encoding:__ The intellectual rights licenses (CC0 and CC-BY) contained non-UTF-8 encoded quote characters that produced invalid EML. These have been removed.
* __Geographic coverage sources:__ Only one geographic coverage input is allowed to the `make_eml()` function at a time. Valid sources are the geographic_coverage.txt template, the `geographic.coordinates` and `geographic.description` arguments of `make_eml()`, and the deprecated bounding_boxes.txt template.
* __Missing value codes as categorical variables:__ Missing value codes were being incorrectly listed as categorical variables by `template_categorical_variables()`. This issue has been fixed.
* __Taxonomic coverage:__ Invalid taxonomic coverage was being generated by EDI's `taxonomyCleanr::make_taxonomicCoverage()`. This issue has been fixed in that projects GitHub master branch, and the necessary adjustments have been made to `EMLassemblyline::make_eml()`.

# EMLassemblyline 2.6.0

## Enhancements

* __EML v2.0.0:__ `EMLassemblyline` has been refactored to run with the `EML' v2.0.0 dependency.
* __Text type metadata may now be supplied in .docx and .md files:__ Support for creating abstract, methods, and additional information metadata has been extended from simple text files to Microsoft Word (.docx) and Markdown (.md) file formats. Formatting of these files are translated to EML via `markdown` > Pandoc > docbook.
* __Create an empty geographic_coverage.txt:__ Sometimes the geographic coverage of a dataset cannot be extracted from a table and needs to be entered manually. Use the `template_geographic_coverage()` argument `empty = TRUE` to create an empty geographic_coverage.txt template.

# EMLassemblyline 2.5.3

## Enhancements

* __Add function examples:__ Add examples to function documentation.
* __Change template import:__ Import custom_units.txt with `template_table_attributes()` instead of with `template_core_metadata()`. This is a more logical pairing.

## Bug fixes

* __The argument validator should not check geographic coverage templates:__ This fix moves the presence/absence check for geographic coverage templates to `make_eml()`.
* __v2.4.6 functions should be accessible:__ This fix exports `EMLassemblyline` 2.4.6 functions that should be otherwise accessible for backwards compatibility.
* __File names should not require extensions:__ This fix restores functionality that was lost in the recent refactor.

# EMLassemblyline 2.5.0

## Enhancements

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

## Deprecation

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

## Bug fixes

* __otherEntity url:__ Add EML element _//otherEntity/physical/distribution/online/url_ via `make_eml()`. This element was missing though documentation implied its existence.
