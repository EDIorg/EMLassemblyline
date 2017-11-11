# Schematic of the assembly line process

Before creating EML your dataset should be ready for publication, i.e. all reformatting and quality control has been completed and you are ready to share your data with the world. [Here is a good reference for preparing your data](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/).

Start the assembly line with the dataset you will publish (Fig. 1). Next run the functions `import_templates` and `view_instructions` to supply the core metadata of your dataset. If your data tables include categorical variables then you will supply definitions to these catagories running the `define_catvars` function. If your dataset contains data from more than one sampling site or area, then you will supply this information by running the function `extract_geocoverage`. Once you have built all the metadata parts, assemble them into EML, run a validation check, and write the EML file with the function `make_eml`. Your data and EML metadata comprise a data package that can be uploaded to a publicly accessible repository for persistence, reuse, and citation. [The Environmental Data Initiative (EDI) runs a high quality and secure environmental data repository](https://portal.edirepository.org/nis/home.jsp).  [Here are instructions on how to upload your data package to the EDI repository (see item #4 on this list)](https://environmentaldatainitiative.org/resources/assemble-data-and-metadata/).

![Figure 1. Assembly line overview.](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/eml_assembly_line_overview.jpg)

Figure 1. Assembly line overview.
