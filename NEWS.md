# EMLassemblyline 2.99.0

* __Enhancement:__ All geographic coverage has been moved to /dataset/coverage, which is where data repositories find this information and place it on maps.
* __Enhancement:__ Support other entity only data packaging use case, thereby extending data packaging use cased to:
    * Data table only
    * Data table and other entity
    * Other entity only
* __Enhancement:__ Enable `make_eml()` to create EML for data repositories other than EDI by relaxing constraints on `user.id`, `user.affiliation`, and `package.id` while returning EDI specific warnings, but not errors. Additional refactoring for creating valid access control credentials will be required.
* __Enhancement:__ Release `read_files()`, a function to read metadata templates, data files, and function arguments into an R list structure as an alternative input to `EMLassemblyline` functions (i.e. rather than supplying the files themselves). This approach is generally useful for interfacing upstream sources with `EMLassemblyline` (e.g. a metabase or database).
* __Enhancement:__ Release `create_directory()`, a function to create a commonly used directory structure for EMLassemblyline inputs and outputs.
* __Enhancement:__ Use arguments `data.table.description` and `other.entity.description` for `//dataTable/entityName` and `//otherEntity/entityName`, respectively. This provides a more meaningful file description than the file name it self.
* __Refactor:__ Rename arguments to align with EML element names.
    * `data.files` -> `data.table`
    * `data.files.description` -> `data.table.description`
    * `data.files.quote.character` -> `data.table.quote.character`
    * `data.files.url` -> `data.url`
* __Refactor:__ Rename arguments to align with EML element names.
    * `zip.dir` -> `other.entity`
    * `zip.dir.description` -> `other.entity.description`
* __Bug fix:__ `import_templates()` no longer fails to import template to empty directories.
* __Bug fix:__ `import_templates()` can now be run on datasets that don't have data tables.
* __Bug fix:__ Add `//otherEntity/physical/distribution/online/url`. This element was missing though documentation implied it existed.
