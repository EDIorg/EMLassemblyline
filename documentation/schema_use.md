# Schema use

The EML schema accomodates a tremendous amount of detail about several data types. The assembly line does not utilize all elements of the EML schema, nor does it exercise the limits of detail possible for each of these elements. Below is a list of EML elements created in the assembly line process with some notes on to how much detail each of these elements contain. Detail level is noted in parentheses.


* access
    * allow
* dataset
    * title
    * creator
        * individualName
        * organizationName
        * electronicMailAddress
        * userId
    * pubDate
    * abstract (plain text separated by para elements)
    * keywordSet
        * keyword
        * keywordThesaurus
    * intellectualRights
    * coverage
        * geographicCoverage
        * temporalCoverage
        * taxonomicCoverage
    * maintenance
        * description
    * contact
        * individualName
        * organizationName
        * electronicMailAddress
    * methods
        * methodStep
            * description (plain text separated by para elements)
        * sampling
            * studyExtent (list of geographicCoverage to accomodate many sampling sites)
    * project
        * title
        * personnel
        * funding (a hierarchical listing of funding sources if more than one is present)
    * dataTable
        * entityName
        * entityDescription
        * physical
            * objectName
            * size
            * authentication (MD5)
            * dataFormat
            * distribution
        * attributeList
        * numberOfRecords
    * additionalMetadata (custom units and or plain text separated by para elements)
    
