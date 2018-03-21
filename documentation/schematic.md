# Schematic of the assembly line process

Before creating EML, your dataset should be ready for publication, i.e. all formatting and quality control should be completed, and you are ready to share your data with the world.

Start the EML assembly line with your cleaned dataset and run the functions `import_templates` and `define_catvars` to begin building the core metadata of your dataset (Fig. 1). These functions automatically detect features of your data tables and populate a series of template files for your completion. If your data contain additional features that you'd like to communicate in your metadata, a set of ancillary functions and workflows may be called to help you generate this content. Currently, the `extract_geocoverage` function and [the `taxonomyCleanr` R package](https://github.com/EDIorg/taxonomyCleanr) help you create this custom detail for your dataset. Once you have built all the metadata parts, run `make_eml` to translate the completed metadata templates created in the previous steps into EML, run a validation check, and write the EML to file. Your data and EML metadata comprise a data package that can be uploaded to a publicly accessible repository for persistence, reuse, and citation. [The Environmental Data Initiative (EDI) runs a high quality and secure environmental data repository](https://portal.edirepository.org/nis/home.jsp).  [Here are instructions on how to upload your data package to the EDI repository (see item #4 on this list)](https://environmentaldatainitiative.org/resources/five-phases-of-data-publishing/).

![Figure 1. Assembly line overview.](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/eml_assembly_line_overview.jpg)

Figure 1. An overview of the EML assembly line.
