# Mainframe EBCDIC Data Converter to ASCII

## Description
A Python application is aimed to convert mainframe EBCDIC data into Unicode ASCII delimited text files. 

The converter consists of two parts
 1. Engine
 2. Engine conversion rules

The engine is smart enough to validate conversion rules and reject them if they are not compatible with the engine.

The main feature is to support major types of layouts along with fixing issues with EBCDIC file.

The supported layouts are
 1. Single schema
 2. Multi-schema fixed record length
 3. Multi-schema variable record length

EBCDIC files can contain any kinds of anomalies and the application can address them. For example, an EBCDIC file might have a number of "garbage" characters at beginning of the file or invalid characters. 

The application supports both Python and Java encodings.

## Conversion rules sample
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
  
Exit codes: 0 - successful completion, 1 - completion with any error
```

## Contributing
Please read [CONTRIBUTING.md](https://github.com/larandvit/ebcdic-parser/blob/master/CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.
 
## License
This project is licensed under the MIT License - see the [LICENSE](https://github.com/larandvit/ebcdic-parser/blob/master/LICENSE) file for details

## Acknowledgments
Thank you, [javabridge](https://pypi.org/project/javabridge/)  team, for development and prompt support of the library

