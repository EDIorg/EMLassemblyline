## Validate keywords
[back to keywords](https://github.com/EDIorg/EMLassemblyline/blob/master/documentation/instructions.md#keywords)

Use `validate_keywords` to browse one or more controlled vocabularies for terms listed in your keywords.txt file. Successful matches are written to keywords.txt, and near-matches are offered for consideration. Arguments to this function:

1. **path** A path to your dataset working directory.
2. **cv** The controlled vocabulary to search. Valid options are:
    * 'lter' - [The LTER Controlled Vocabulary](http://vocab.lternet.edu/vocab/vocab/index.php)

```
# View documentation for this function
?validate_keywords

# Run the function
validate_keywords(path = "/Users/csmith/Desktop/gleon_chloride",
                  cv = 'lter')

```

