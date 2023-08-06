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
       * Original [COBOL layout](https://github.com/larandvit/ebcdic-parser/blob/master/tests/test_data/pr_p1_p2_gas_disposition/oga0861.pdf) and corresponding [conversion rules](https://github.com/larandvit/ebcdic-parser/blob/master/tests/layout_repository/gsf102_rules.json) file
    2. Multi-schema fixed record length
       * Original [COBOL layout](https://github.com/larandvit/ebcdic-parser/blob/master/tests/test_data/ola013k/ola013k.pdf) and corresponding [conversion rules](https://github.com/larandvit/ebcdic-parser/blob/master/tests/layout_repository/ola013k_rules.json) file
    3. Multi-schema variable record length
    4. Single schema variable record length
       * Original [COBOL layout](https://github.com/larandvit/ebcdic-parser/blob/master/tests/test_data/service_segment_data/129.1DP.pdf) and corresponding [conversion rules](https://github.com/larandvit/ebcdic-parser/blob/master/tests/layout_repository/service_segment_data.json) file
 
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
    
* Debug mode to troubleshoot layouts

* Run the tool
  1. Pip package 
  2. Command prompt
  3. python -m ebcdic_parser

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

## Pip Usage

### Installation

```bash
pip install ebcdic_parser
```

### Sample

```python
from ebcdic_parser.convert import run 


run(r"D:\Projects\ebcdic-parser\tests\test_data\311_calls_for_service_requests_all_strings\311_calls_for_service_requests_sample.dat", 
     r"D:\Projects\test_project",
     r"D:\Projects\ebcdic-parser\tests\layout_repository\311_calls_for_service_requests_all_strings.json",
     outputDelimiter=',',
     logfolder=r'D:\Projects\test_project\log')
```

### Run Function

```
run(inputFile, 
    outputFolder,
    layoutFile,
    logfolder='',
    pythonEncoding=True,
    encodingName='cp037',
    outputDelimiter=`\t`,
    outputFileExtension=`.txt`,
    ignoreConversionErrors=False,
    groupRecords=False,
    groupRecordsLevel2=False,
    verbose=True,
    debug=False,
    cliMode=False):
```


* **inputfile** - input EBCDIC file path. Mandatory parameter. Absolute and relative paths are acceptable.
* **outputfolder** - output folder to store delimited files. Mandatory parameter. Absolute and relative paths are acceptable.
* **layoutfile** - layout file path. Mandatory parameter. Absolute and relative paths are acceptable.
* **outputdelimiter** - output text file delimiter. Optional parameter. Default value is `\t`.
* **outputfileextension** - output text file extension. Optional parameter. Default value is `.txt`.
* **ignoreconversionerrors** - ignore any conversion error. Optional parameter. Default value is `False`.
* **logfolder** - output folder to store log file. Optional parameter. Default value is the current folder. Absolute and relative paths are acceptable.
* **pythonencoding** - use Python encoding rather than Java. Optional parameter. Default value is `True`.
* **encodingname** - code page name to encode characters (Python or Java). Optional parameter. Default value is `cp037`.
* **grouprecords** - create relationships between records. Optional parameter. Default value is `False`.
* **grouprecordslevel2** - create relationships between records for level 2. Optional parameter. Default value is `False`.
* **verbose** - show extended information on screen. Optional parameter. Default value is `True`.
* **debug** - show debug information. Optional parameter. Default value is `False`.
* **cliMode** - a flag how it run in command prompt or Pip installation.  


## Command Prompt Usage
```
usage: convert.py [-h] --inputfile "input file path" --outputfolder
                        "output folder" --layoutfile "layout file"
                        [--outputdelimiter [delimiter]]
                        [--outputfileextension [extension]]
                        [--ignoreconversionerrors [yes/no]]
                        [--logfolder [log folder]] [--pythonencoding [yes/no]]
                        [--encodingname [encoding name]]
                        [--grouprecords [yes/no]]
                        [--grouprecordslevel2 [yes/no]] [--verbose [yes/no]]
                        [--debug [yes/no]]

Convert EBCDIC data into delimited text format. Version x.x.x

Supported file formats:
(1) Single schema
(2) Multi-schema fixed length
(3) Multi-schema variable length
(4) Single schema variable length

Features
1. Python doesn't include enough code pages, so it's added Java code pages as well.
   Java is implemented in javabridge module. If javabridge module is not installed, 
   Java functionlaity can be disabled changing JAVABRIDGEINCLUDED = True to False in Python code

optional arguments:
  -h, --help            show this help message and exit
  --inputfile "input file path"
                        Input EBCDIC file path
  --outputfolder "output folder"
                        Output folder to store delimited files
  --layoutfile "layout file"
                        Layout file path
  --outputdelimiter [delimiter]
                        output text file delimiter
  --outputfileextension [extension]
                        output text file extension
  --ignoreconversionerrors [yes/no]
                        ignore any conversion error
  --logfolder [log folder]
                        Output folder to store log file
  --pythonencoding [yes/no]
                        use Python encoding rather than Java
  --encodingname [encoding name]
                        Code page name to encode characters (Python or Java)
  --grouprecords [yes/no]
                        create relationships between records
  --grouprecordslevel2 [yes/no]
                        create relationships between records for level 2
  --verbose [yes/no]    show information on screen
  --debug [yes/no]      show debug information

Exit codes: 0 - successful completion, 1 - completion with any error
```

### Single schema #1

```bash
convert.py --inputfile "../../tests/test_data/311_calls_for_service_requests_all_strings/311_calls_for_service_requests_sample.dat" --outputfolder "../../tests/test_data/311_calls_for_service_requests_all_strings/output" --layoutfile "../../tests/layout_repository/311_calls_for_service_requests_all_strings.json" --outputdelimiter ,
```

* Output location: ./tests/test_data/311_calls_for_service_requests_all_strings/output
* Outptut format: comma delimited ASCII file
* Log file: ./ebcdic_parser.log

### Single schema in debug mode #1

```bash
convert.py --inputfile "../../tests/test_data/311_calls_for_service_requests_all_strings/311_calls_for_service_requests_sample.dat" --outputfolder "../../tests/test_data/311_calls_for_service_requests_all_strings/output" --layoutfile "../../tests/layout_repository/311_calls_for_service_requests_all_strings.json" --outputdelimiter , --debug yes
```

* Output location: ./tests/tests/test_data/311_calls_for_service_requests_all_strings/output
* Outptut format: comma delimited ASCII file
* Log file: ./ebcdic_parser.log

### Single schema #2

```bash
convert.py --inputfile "../../tests/test_data/pr_p1_p2_gas_disposition/gsf102.ebc" --outputfolder "../../tests/test_data/pr_p1_p2_gas_disposition/output" --layoutfile "../../tests/layout_repository/gsf102_rules.json" --logfolder "../../tests/test_data/pr_p1_p2_gas_disposition/log"
```

* Output location: ./tests/test_data/pr_p1_p2_gas_disposition/output
* Outptut format: tab delimited ASCII file
* Log folder: ./tests/test_data/pr_p1_p2_gas_disposition/log/ebcdic_parser.log

### Multi-schema fixed record length

```bash
convert.py --inputfile "../../tests/test_data/ola013k/olf001l.ebc" --outputfolder "../../tests/test_data/ola013k/output" --layoutfile "../../tests/layout_repository/ola013k_rules.json"
```

* Output location: ./tests/test_data/ola013k/output
* Outptut format: tab delimited ASCII file
* Log folder: ./ebcdic_parser.log

### Single schema variable record length

```bash
convert.py --inputfile "../../tests/test_data/service_segment_data/RG197.SERVSEG.Y70.ebc" --outputfolder "../../tests/test_data/service_segment_data/output" --layoutfile "../../tests/layout_repository/service_segment_data.json"
```

* Output location: ./tests/test_data/service_segment_data/output
* Outptut format: tab delimited ASCII file
* Log folder: ./ebcdic_parser.log

## python -m ebcdic_parser Usage

```bash
python -m ebcdic_parser --inputfile "D:\Projects\ebcdic-parser\tests\test_data\311_calls_for_service_requests_all_strings\311_calls_for_service_requests_sample.dat" --outputfolder "D:\Projects\test_project" --layoutfile "D:\Projects\ebcdic-parser\tests\layout_repository\311_calls_for_service_requests_all_strings.json"  --outputdelimiter "," --logfolder "D:\Projects\test_project\log"
```

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

## Java Encodings
It doesn't request any special installation if you are planning to use only Python encodings.

In case of using Java encodings, it has to be installed [javabridge](https://pypi.org/project/javabridge/) Python library. There is a constant in the code for including the javabridge library.

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
