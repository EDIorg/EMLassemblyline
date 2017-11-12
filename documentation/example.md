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

Now that we have the metadata templates imported to the working directory we can begin filling them out. For detailed instructions see the documentation for view_instructions.R and for step by step instructions printed to the RStudio console window run view_instructions.R with no arguments:

```{r eval = FALSE}
?view_instructions  # Opens documentation for this function, including detailed instructions for templates
view_instructions() # Prints instructions to the RStudio console window
```

### Abstract

Write an abstract for the dataset in the file datasetname_abstract.txt. The abstract should cover what, why, when, where, and how. The template is a UTF-8 formatted file. Use only UTF-8 symbols and text when completing.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/abstract.PNG)

### Additional information

Place additional info in the file datasetname_additional_info.txt. This is a good place to share additional text based information that doesn't fall under the scope of the abstract or methods (e.g. a list of research articles or theses derived from this dataset). The template is a UTF-8 formatted file. Use only UTF-8 symbols and text. Delete this file if you have no additional information to present.

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/additional_info.PNG)


### Intellectual rights

Select an intellectual rights license. We have two recommendations: 'datasetname_cc_by_4.0_intellectual_rights.txt' and 'datasetname_cc0_1_intellectual_rights.txt'. Do not edit the text of these files. Delete the file that you will not be using. Rename the one you will be using following convention datasetname_intellectual_rights (e.g. gleon_chloride_intellectual_rights).

![](https://github.com/EDIorg/EMLassemblyline/blob/master/vignettes/intellectual_rights.PNG)


### Methods

Explain the methods used to create this dataset in the file datasetname_methods.txt. Rename this file following convention (e.g. gleon_chloride_methods). Be specific, include instrument descriptions, or point to a protocol online. If this dataset is a synthesis of other datasets please specify dataset origins, preferably their DOI or URL plus general citation information. This file is a UTF-8 formatted file. Use only UTF-8 symbols and text.

![](methods.PNG)

### Personnel information

Enter personnel information for the dataset in the file datasetname_personnel.txt. This is a tab delimited file that can be edited with a spreadsheet editor. Rename the file according to convention (e.g. gleon_chloride_personnel). Valid entries for role are: "creator" = dataset creator, "pi" = principal investigator, "contact" = dataset contact. Any other entries into the 'role' column are acceptable and will be defined under the associated party element of this dataset. If a person serves more than one role, duplicate this persons information in another row but with the new role. A dataset creator, contact, and principal investigator are mandatory. This file is a UTF-8 formatted file. Use only UTF-8 symbols and text.

![](personnel.PNG)

### Table attributes

Add data table attributes to the file datasetname_datatablename_attributes.txt. This is a tab delimited file that
can be edited with a spreadsheet editor. Rename the file according to convention (e.g. gleon_chloride_lake_characteristics_attributes). Create an attributes file for each data table. This file is a UTF-8 formatted file. Use only UTF-8 symbols and text. Instructions for completing the attribute table are as follows:

- "attributeDefinition" Define the attribute. Be specific, it can be lengthy.

- "class" Specify the attribute class. This is the type of value stored under the attribute. Valid options are: "numeric" for numeric data, "categorical" for categorical variables, "character" for non-categorical data containing text or symbols, and "Date" for date time data. The list of valid options are case sensitive. If an attribute has class of "numeric" or "Date", then all values of this attribute must be either numeric or date time. If any character strings are present in an otherwise "numeric" attribute, this attribute must be classified as "character". Similarly if any values of a "Date" attribute do not match the date time format string (details below), then this attribute must be classified as "character". 

- "unit" If an attributes class is numeric, then you must provide units. If the attribute is numeric but does not have units, enter "dimensionless". If the attribute class is a character or vector, then leave the unit field blank. If the attribute is numeric and has units search the standard unit dictionary (opened in the source window) for the unit of interest and enter the unit "name" as it appears in the  dictionary. Unit names are case sensitive. If you cannot find a unit in the dictionary, enter the unit in the tab delimited UTF-8 formatted file datasetname_custom_units.txt. Rename this file to accord with the naming convention (e.g. gleon_chloride_custom_units). Valid custom units must be convertible to SI Units (i.e. International System of Units). If it cannot be converted to SI then list it in the attribute defintion and enter "dimensionless" in the unit field. To create a custom unit define the:
    
    - "id" This is equivalent to the unit name. Reference the standard unit dictionary formatting.
    
    - "unitType" The type of unit being defined. Reference the dictionary for examples.
    
    - "parentSI" The SI equivalent of the id you have entered.
    
    - "multiplierToSI" This is the multiplier to convert from your custom unit to the SI unit equivalent.
    
    - "description" A description of the custom unit. Reference the dictionary for examples.

- "dateTimeFormatString" Enter the date time format string for each attribute of "Date" class. Remember, a class of "Date" specifies the attribute as a date, time, or datetime. Enter the format string in this field. If the attribute class is not "Date", leave this field blank. Below are rules for constructing format strings. Additional information is listed under "dateTime-eml-attribute" of the current EML specification (https://knb.ecoinformatics.org/#external//emlparser/docs/index.html). Valid date time formats are a combination of date, time, and time zone strings. Below are a set of best practice recomendations, and are by no means the full list of acceptable format strings.
    
    - Date format strings: YYYY-MM-DD, YYYY/MM/DD YYYY, YYYYMMDD, YYYY-MM, YYYY/MM, YYYYMM, YYYY-DDD, YYYY/DDD, YYYYDDD; where YYYY is year, MM is month, DD is day of month, and DDD"is day of year.
    
    - Time format strings: hh:mm:ss.sss, hhmmss.sss, hh:mm:ss, hhmmss, hh:mm, hhmm, hh; where hh is hour (in 24 hr clock), mm is minute, ss is second, and ss.sss is decimal second.
    
    - Time zone format strings: Z, +hh:mm, +hhmm, +hh, -hh:mm, -hhmm, -hh; where Z (capitalized) is Coordinated Universal Time, and + and - denote times ahead and behind UTC respectively. 
    
If reporting a date without time, select one of the date format strings. If reporting a date and time, select one date and one time format string and combine with a single space (e.g. YYYY-MM-DD hh:mm) or with a "T" (e.g. YYYY-MM-DDThh:mm). If reporting a date and time, it is recommended that a time zone specifier be appended without a space (e.g. YYYY-MM-DD hh:mm-hh:mm, or YYYY-MM-DDThh:mm-hh:mm).

- "missingValueCode" If a code for 'no data' is used, specify it here (e.g. NA, -99999).

- "missingValueCodeExplanation" Define the missing value code here.


![](custom_units.PNG)

![](attributes_concentrations.PNG)


![](attributes_characteristics.PNG)

### Configuration file

Fill out the file eml_configuration.R. Additional information information about the dataset is stored in this file. Instructions for this file are listed as comments within the file.

![](configuration.PNG)

### Close files of the working directory

Make sure all files of the working directory are closed. Some functions will error out if these files are open. The working directory of this dataset is now populated with the core metadata templates.

![](dir_populated.PNG)

## Define categorical variables

Since this dataset contains categorical variables we will need to define them using the function define_catvars.R. This function requires a path to the dataset working directory containing the completed attribute templates. The function looks in each attribute file searching for variables of class = "categorical". Once found the function identifies all uniqe instances of the respective variable and writes them to file for your editing. 

```{r eval = FALSE}
define_catvars(path = "C:\\Users\\Colin\\Desktop\\gleon_chloride")
```

The only attributes file containing categorical variables is "gleon_chloride_lake_characteristics_attributes.txt", and thus only one new file containing categorical variables is written to the working directory (i.e. gleon_chloride_lake_characteristics_catvars.txt").

![](dir_catvars.PNG)

After the catvars file is written, the standard units dictionary is opened in the RStudio Source window to consult if there are units to be defined. We must open this tab delimited file with a spreadsheet editor and provide definitions for each categorical variables in the "definition" column. The columns "attributeName" and "code" contain the unique codes found under each attributeName of class = categorical. Below we have provided definitions for each code. If a code is empty then delete the corresponding row.

![](catvars.PNG)

## Extract geographical coverage

This dataset contains detailed geographical information of sampling locations that may be useful to include in the EML because these locations would be discoverable by someone running a search on lakes within a specified area. To create this metadata we need to extract the unique sampling points from the "gleon_chloride_lake_characteristics.csv" file, which contains latitude, longitude, and site name. To extract this information we run extract_geocoverage.R with a path to the working directory containing this data table, the name of the table including the file extension, the name of the latitude column (containing decimal degrees with latitudes south of the equator prefixed with a minus sign), the name of the longitude column (containing decimal degrees with longitudes west of the prime meridian prefixed with a minus sign), and the name of the site column.

```{r eval = FALSE}
extract_geocoverage(path = "C:\\Users\\Colin\\Desktop\\gleon_chloride",
                    table.name = "gleon_chloride_lake_characteristics.csv",
                    lat.col = "Latitude",
                    lon.col = "Longitude",
                    site.col = "Common_name")
```


This returns a tab delimited UTF-8 formatted file in the working directory titled geographic_coverage.txt and contains decimal degree latitude, decimal degree longitude, and site name.

![](dir_geocoverage.PNG)

![](geocoverage.PNG)

## Make EML

Now that we have created all the metadata parts we run the function make_eml.R with a path to the dataset working directory. Make EML compiles the metadata parts, runs a validation check to ensure the EML schema of this dataset complies with the EML standard, and writes the EML to a .xml file in the working directory. If validation fails, open the EML document in a .xml editor to identify the source of error. Often the error is small and quickly resolved with the aid of an editors schema congruence checker.

```{r eval = FALSE}
make_eml(path = "C:\\Users\\Colin\\Desktop\\gleon_chloride")
```

![](eml.PNG)
