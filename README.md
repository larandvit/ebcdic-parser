# Mainframe EBCDIC Data Converter to ASCII

## Description
A Python application is aimed to convert mainframe EBCDIC data into Unicode ASCII delimited text files. 

The converter consists of two parts
1. Engine
2. [Engine conversion rules](https://github.com/larandvit/ebcdic-parser/blob/master/docs/engine_rules_manual.md)
 
Conversion rules are a driver to parse EBCDIC data.

## Features

* Supported layouts
    1. Single schema
    2. Multi-schema fixed record length
    3. Multi-schema variable record length
 
* Fixing anomalies in EBCDIC files
    1. Skip header
    2. Skip footer
    3. Remove invalid characters
  
* Adding relationship keys
    1. parent-child
    2. parent-child-grandchild
     
* Applicable encodings
    1. Python
    2. Java

## Conversion rules sample
Detailed information about conversion rules setup file can be found in [Engine Rules](https://github.com/larandvit/ebcdic-parser/blob/master/docs/engine_rules_manual.md) manual. [COBOL Engine Rules Dictionary](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_engine_rules_dictionary.md) manual describes how to convert COBOL data types into engine rules layout.
```json
{
    "description": "conversion rules sample",
    "header": [
        {
            "name": "header", "type": "skip", "size": 100
        }
    ],
    "footer": [
        {
            "name": "footer", "type": "skip", "size": 52
        }
    ],
    "layouts": [
        {
            "layouttype": "main",
            "layout": [
                {
                    "name": "employee_name", "type": "string", "size": 55
                },
                {
                    "name": "wages", "type": "packedDecimal", "size": 8
                },
                {
                    "name": "employee_id", "type": "integer", "size": 4
                }
            ]
        }
    ]
}
```

## Installation
It doesn't request any special installation if you are planning to use only Python encodings.

In case of using Java encodings, it has to be installed [javabridge](https://pypi.org/project/javabridge/) Python library. There is a constant in the code for including the javabridge library.

## Usage
```
usage: ebcdic_parser.py [-h] 
                        --inputfile "input file path" 
                        --outputfolder "output folder" 
                        --layoutfile "layout file"
                        [--outputdelimiter [delimiter]]
                        [--outputfileextension [extension]]
                        [--ignoreconversionerrors [yes/no]]
                        [--logfolder [log folder]]
                        [--pythonencoding [yes/no]]
                        [--encodingname [encoding name]]
                        [--grouprecords [yes/no]]
                        [--grouprecordslevel2 [yes/no]]
                        [--verbose [yes/no]]

Convert mainframe EBCDIC data to delimited text format. Version 2.1.0

Supported file formats:
(1) Single schema
(2) Multi-schema fixed length
(3) Multi-schema variable length

Features
1. Python doesn't include enough code pages, so it's added Java code pages as well.
   Java is implemented in javabridge module. If javabridge module is not installed, 
   Java functionlaity can be disabled changing JAVABRIDGEINCLUDED = True to False in Python code

optional arguments:
  -h, --help - Show this help message and exit
  --inputfile "input file path" - Input EBCDIC file path
  --outputfolder "output folder" - Output folder to store delimited files
  --layoutfile "layout file" - Layout file path
  --outputdelimiter [delimiter] - Output text file delimiter
  --outputfileextension [extension] - Output text file extension
  --ignoreconversionerrors [yes/no] - Ignore any conversion error
  --logfolder [log folder] - Output folder to store log file
  --pythonencoding [yes/no] - Use Python encoding rather than Java
  --encodingname [encoding name] - Code page name to encode characters (Python or Java)
  --grouprecords [yes/no] - Create relationships between records
  --grouprecordslevel2 [yes/no] - Create relationships between records for level 2
  --verbose [yes/no] - Show information on screen
  
Exit codes: 0 - successful completion, 1 - completion with any error
```

## Samples
### Single schema

```bash
ebcdic_parser.py --inputfile "./test_data/311_calls_for_service_requests_all_strings/311_calls_for_service_requests_sample.dat" --outputfolder "./test_data/311_calls_for_service_requests_all_strings/output" --layoutfile "./layout_repository/311_calls_for_service_requests_all_strings.json" --outputdelimiter , --ignoreconversionerrors no --pythonencoding yes --grouprecords no --verbose yes
```

Output location: ./test_data/311_calls_for_service_requests_all_strings/output
Log file: ./ebcdic_parser.log

## Testing
### Functional tests

Those tests cover testing of each data type. 
```
test_functional.py
```
## System tests
The tests are needed to test entire functionality for each feature.
* Tool to create samples
```
converter_to_ebcdic.py
```
* Run tests
```
test_system_311_calls_for_service_requests_all_strings.py
```

## Contributing
Please read [CONTRIBUTING.md](https://github.com/larandvit/ebcdic-parser/blob/master/CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.
 
## License
This project is licensed under the MIT License - see the [LICENSE](https://github.com/larandvit/ebcdic-parser/blob/master/LICENSE) file for details

## Acknowledgments
* Thank you, [javabridge](https://pypi.org/project/javabridge/)  team, for development and prompt support of the library
* https://www.ibm.com/support/knowledgecenter/en/ssw_ibm_i_72/rzasd/dtdf.htm
* http://www.mainframestechhelp.com/tutorials/cobol/
* http://www.fileformat.info/
* [City Toronto Open Data](https://www.toronto.ca/city-government/data-research-maps/open-data/)
* [Reading COBOL Layouts](http://www.3480-3590-data-conversion.com/article-reading-cobol-layouts-1.html)
