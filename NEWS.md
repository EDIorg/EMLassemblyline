# EMLassemblyline 2.5.0

## New features

* __Website:__ Improved documentation with vignettes demonstrating common and advanced use cases. Implemented with `pkgdown`.
* __Templating functions:__ Functions creating metadata templates are grouped under the prefix `template_*` to simplify user identification.
* __template_taxonomic_coverage__ A templating function resolving taxa to one or more authority systems and supporting creation of the hierarchical rank specific EML taxonomicCoverage element by `make_eml`.
* __template_arguments__ A templating function for all user inputs to `EMLassemblyline` (i.e. metadata template content and function arguments) to entirely programmatic workflows with focus on supporting content ingestion from upstream metadata sources.
* __template_directories__ A templating function creating a simple and effective directory structure for `EMLassemblyline` files and data package contents.
* __Support for other entity data packages:__ Data packages comprised completely of other entities (i.e. non-tabular data) is now supported.
* __Geographic coverage:__ All geographic coverage metadata has been moved to //dataset/coverage, where most data repositories find it for rendering to maps and other visualizations for users.
* __Make EML for other data repositories:__ Arguments requiring EDI specific content (i.e. `user.id`, `user.domain`, `package.id`) have been relaxed to enable creation of EML for other data repositories.
* __Better entity descriptions:__ Use arguments `data.table.description` and `other.entity.description` for `//dataTable/entityName` and `//otherEntity/entityName`, respectively. This provides a more meaningful file description than the file name it self.

## Deprecation

Several templating functions, templates, and arguments have been deprecated. Full backwards compatibility of these functions, templates, and arguments will be supported for the next year (i.e. until May 1, 2020).

__Functions:__

* `import_templates` is deprecated in favor of `template_core_metadata` (i.e. metadata required by all data packages) and `template_table_attributes` (i.e. metadata for data tables).
* `define_catvars` is deprecated in favor of `template_categorical_variables.
* `extract_geocoverage` is deprecated in favor of `template_geographic_coverage`

__Templates:__

* `bounding_boxes.txt` is deprecated in favor of `geographic_coverage.txt`
* `geographic_coverage.txt` is deprecated in favor of a new version of `geographic_coverage.txt` that supports both point locations and areas.

__Arguments:__

* `data.files` is deprecated in favor of `data.table`
* `data.files.description` is deprecated in favor of `data.table.description`
* `data.files.quote.character` is deprecated in favor of `data.table.quote.character`
* `data.files.url` is deprecated in favor of `data.url`
* `zip.dir` is deprecated in favor of `other.entity`
* `zip.dir.description` is deprecated in favor of `other.entity.description`
* `affiliation` is deprecated in favor of `user.domain`

## Bug fixes

* __otherEntity url__ Add EML element`//otherEntity/physical/distribution/online/url` via `make_eml`. This element was missing though documentation implied its existence.
