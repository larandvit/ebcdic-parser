# Mainframe EBCDIC Data Converter to ASCII

## Description
A Python application is aimed to convert mainframe EBCDIC data into Unicode ASCII delimited text files. 

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

## python -m ebcdic_parser Usage

```bash
python -m ebcdic_parser --inputfile "D:\Projects\ebcdic-parser\tests\test_data\311_calls_for_service_requests_all_strings\311_calls_for_service_requests_sample.dat" --outputfolder "D:\Projects\test_project" --layoutfile "D:\Projects\ebcdic-parser\tests\layout_repository\311_calls_for_service_requests_all_strings.json"  --outputdelimiter "," --logfolder "D:\Projects\test_project\log"
```

### Arguments
```
[-h] 
--inputfile "input file path" 
--outputfolder "output folder" 
--layoutfile "layout file"
[--outputdelimiter [delimiter]]
[--outputfileextension [extension]]
[--ignoreconversionerrors [yes/no]]
[--logfolder [log folder]] [--pythonencoding [yes/no]]
[--encodingname [encoding name]]
[--grouprecords [yes/no]]
[--grouprecordslevel2 [yes/no]] [--verbose [yes/no]]
[--debug [yes/no]]

Arguments:
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

## Java Encodings
It doesn't request any special installation if you are planning to use only Python encodings.

In case of using Java encodings, it has to be installed [javabridge](https://pypi.org/project/javabridge/) Python library. There is a constant in the code for including the javabridge library.