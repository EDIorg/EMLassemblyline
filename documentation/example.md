# An example use of the EML assembly line

Here we demonstrate use of `EMLassemblyline` to create EML for the Global Lakes Ecological Observatory Network (GLEON) Lake Chloride dataset. [The corresponding data package has been curated with the Environmental Data Initiative (EDI) and is publicly accessible through the EDI data portal](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=8).

## Install and load EMLassemblyline

The `EMLassemblyline` R package is available here on GitHub. To install, go to your RStudio Console window and enter these lines of code:

```
# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
```

Every time you want to use `EMLassemblyline` you will have to load it.

```
# Load EMLassemblyline
library(EMLassemblyline)
```

## Compile data

The GLEON Lake Chloride dataset is composed of 3 data entities. Two tables (gleon_chloride_concentrations.csv, gleon_chloride_lake_characteristics.csv) and one zip directory of shapefiles (gleon_chloride_lake_shape_files.zip). We'll cover how to create EML for the tables in this demonstration, and will address shapefile EML in a separate demo.

## View instructions

For a guide through the assembly line process view the documentation the `view_instructions` function and run it for an interactive set of steps:

```
# View documentation for the view_instructions function
?view_instructions

# Walk through the interactive steps
view_instructions()
```

## Create a working directory

Create a working directory for the dataset and name it using underscore characters "_" to span spaces between words (e.g. gleon_chloride). This directory will contain the metadata parts created in the assembly line.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/dataset_working_directory.PNG)

## Import metadata templates

Import metadata templates to the working directory. These metadata templates form the core of the metadata that will be later translated into EML. To import these templates run the `import_templates` in the RStudio console window. The function requires two arguments: (1) a path to the dataset working directory, (2) the dataset name.

```{r eval = FALSE}
# View this functions documentations for additional details about it
?import_templates

# Run the function supplying both a path to the dataset working directory and the dataset name 
import_templates(path = "C:\\Users\\Colin\\Desktop\\gleon_chloride", dataset.name = "gleon_chloride")
```

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/import_templates.PNG)


## Move data to the working directory

Move the data tables to the working directory and name them following the convention "datasetname_datatablename", where datasetname is the name of the dataset (e.g. gleon_chloride), and datatablename is the name of the data table (e.g. lake_characteristics). In this example, the data table names become "gleon_chloride_concentrations.csv" and "gleon_chloride_lake_characteristics.csv".

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/move_data.PNG)

## Complete the metadata templates

Now that we have the metadata templates imported to the working directory we can begin filling them out. For detailed instructions see the documentation for `view_instructions` and for step by step instructions printed to the RStudio console window run `view_instructions` in the RStudio Console window:

```{r eval = FALSE}
?view_instructions  # Opens documentation for this function, including detailed instructions for templates
view_instructions() # Prints instructions to the RStudio console window
```

### Abstract

Write an abstract for the dataset in the file datasetname_abstract.txt. The abstract should cover what, why, when, where, and how of the dataset. The template is a UTF-8 formatted file. Use only UTF-8 symbols and text when completing.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/abstract.PNG)

### Additional information

Place additional info in the file datasetname_additional_info.txt. This is a good place to share additional text based information that doesn't fall under the scope of the abstract or methods (e.g. a list of research articles or theses derived from this dataset). The template is a UTF-8 formatted file. Use only UTF-8 symbols and text. Delete this file if you have no additional information to present.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/additional_info.PNG)


### Intellectual rights

Select an intellectual rights license. We have two recommendations: 'datasetname_cc_by_4.0_intellectual_rights.txt' and 'datasetname_cc0_1_intellectual_rights.txt'. Do not edit the text of these files. Delete the file that you will not be using. Rename the one you will be using following convention datasetname_intellectual_rights (e.g. gleon_chloride_intellectual_rights).

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/intellectual_rights.PNG)


### Methods

Explain the methods used to create this dataset in the file datasetname_methods.txt. Be specific, include instrument descriptions, or point to a protocol online. If this dataset is a synthesis of other datasets please specify dataset origins, preferably their DOI or URL plus general citation information. This file is a UTF-8 formatted file. Use only UTF-8 symbols and text.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/methods.PNG)

### Personnel information

Enter personnel information for the dataset in the file datasetname_personnel.txt. Consult the documentation of `view_instructions` for guide to filling out this template.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/personnel.PNG)

### Table attributes

Add data table attributes to the file datasetname_datatablename_attributes.txt. This is where attribute names (i.e. column names) and data classes are defined, along with some additional details on what the data table contains. Consult the documentation of `view_instructions` for guide to filling out this template. The gleon_chloride dataset contains units that are not apart of the EML standard unit dictionary and need to be defined. Below are snap shots of the custom units for this dataset as well as the attributes for the tables "concentrations" and "lake characteristics".

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/custom_units.PNG)

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/attributes_concentrations.PNG)

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/attributes_characteristics.PNG)

### Configuration file

Fill out the file `eml_configuration.R`. Additional information information about the dataset is stored in this file. Instructions for this file are listed within the file.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/configuration.PNG)

### Close files of the working directory

Make sure all files of the working directory are closed. Some functions will error out if these files are open. The working directory of this dataset is now populated with the core metadata templates.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/dir_populated.PNG)

## Define categorical variables

Since this dataset contains categorical variables, they need to be defined using the function `define_catvars`. This function requires a path to the dataset working directory containing the completed attribute templates. The function looks in each attribute file searching for variables of class = "categorical". Once found the function identifies all uniqe instances of the respective variable and writes them to file for you to edit. 

```{r eval = FALSE}
# Run the function define_catvars
define_catvars(path = "C:\\Users\\Colin\\Desktop\\gleon_chloride")

# Once completed open these files in a spread sheet editor and add definitions.
```

The only attributes file containing categorical variables is "gleon_chloride_lake_characteristics_attributes.txt", and thus only one new file containing categorical variables is written to the working directory (i.e. gleon_chloride_lake_characteristics_catvars.txt").

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/dir_catvars.PNG)

After the catvars file is written, the standard units dictionary is opened in the RStudio Source window to consult if there are units to be defined. We must open this tab delimited file with a spreadsheet editor and provide definitions for each categorical variables in the "definition" column. The columns "attributeName" and "code" contain the unique codes found under each attributeName of class = categorical. Below we have provided definitions for each code. If a code is empty then delete the corresponding row.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/catvars.PNG)

## Extract geographical coverage

This dataset contains detailed geographical information of sampling locations that may be useful to include in the EML because these locations would be discoverable by someone running a search on lakes within a specified area. To create this metadata we need to extract the unique sampling points from the "gleon_chloride_lake_characteristics.csv" file, which contains latitude, longitude, and site name. To extract this information we run `extract_geocoverage` with a path to the working directory containing this data table, the name of the table including the file extension, the name of the latitude column, the name of the longitude column, and the name of the site column.

```{r eval = FALSE}
# Extract detailed geographic coverage for the gleon_chloride dataset
extract_geocoverage(path = "C:\\Users\\Colin\\Desktop\\gleon_chloride",
                    table.name = "gleon_chloride_lake_characteristics.csv",
                    lat.col = "Latitude",
                    lon.col = "Longitude",
                    site.col = "Common_name")
```


This returns a tab delimited UTF-8 formatted file in the working directory titled geographic_coverage.txt and contains decimal degree latitude, decimal degree longitude, and site name.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/dir_geocoverage.PNG)

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/geocoverage.PNG)

## Make EML

Now that all of the metadat parts have been created for this dataset we run the function `make_eml` with a path to the dataset working directory. Make EML compiles the metadata parts, runs a validation check to ensure the EML schema of this dataset complies with the EML standard, and writes the EML to a .xml file in the working directory. If validation fails, open the EML document in a .xml editor to identify the source of error. Often the error is small and quickly resolved with the aid of an editors schema congruence checker.

```{r eval = FALSE}
# Make EML for the gleon_chloride dataset
make_eml(path = "C:\\Users\\Colin\\Desktop\\gleon_chloride")
```

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/eml.PNG)
