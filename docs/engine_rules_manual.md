# Engine Rules Manual

## Introduction

The converter consists of two parts: (1) engine (application) and (2) engine rules (layouts). Engines rules are aimed to process varieties of input file types without making changes in the application code.

The engine rules driven approach rather than application one dictates how to convert data and it adds flexibility to mix setups between different layouts.

There are supported layouts
1. Single schema
2. Multi-schema fixed record length
3. Multi-schema variable record length.
    
## Engine Rules Structure

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
    "keyfields":[
        {
            "keytype":"layoutsize","name":"length","type":"integer","size":4
        },
        {
             "keytype":"layouttype","name":"recordtype","type":"string","size":1
        }
    ],
    "layouts": [
        {
            "layouttype":"A",
            "layout":[],
            "layoutvariable":[],
            "terminator":{"size":2}
        }
    ]
}
```

### description
Its an engine rules description. Applicable to all supported layouts.

### header
Number of bytes skipped at the beginning of a file. Applicable to all supported layouts.

### footer
Number of bytes skipped at the end of a file. Applicable to all supported layouts.

### keyfields
A list of fields used to parse EBCDIC file. The field types are
1. Single schema
    * N/A
2. Multi-schema fixed length
    * Record type field description. Key type is layouttype.
3. Multi-schema variable length
    * Record length field description. Key type is layoutsize.
    * Record type field description. Key type is layouttype.
    
It can be added more fields to keyfields section. Those fields can be skipped or added to an output file. 

```json
    "keyfields":[
        {
            "name":"regioncode","type":"integer","size":4
        },
        {
            "name":"separator","type":"skip","size":1
        },
        {
            "keytype":"layoutsize","name":"length","type":"integer","size":4
        },
        {
             "keytype":"layouttype","name":"recordtype","type":"string","size":1
        }
    ]
```

### layouts
A list of schemas are included in an EBCDIC file. Each schema can include static and variable parts. Applicable to all supported layouts.

### layouttype
It's a record type value. It distinguishes different schemas in a file. Applicable to multi-schema fixed and variable layouts.

There is a special type - *laypouttypeconditional*. It's used to separate record types based on a condition. It's supported only *ne* condition. The conditions are the same as in UNIX bash.

```json
            "layouttypeconditional": "ne 0",
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
```

### layout
A list of static fields. Applicable to all supported layouts.

### layoutvariable
A list of variable fields. Applicable to multi-schema variable layout.

### terminator
Length of characters ending a record with static and variable parts. They are skipped. Applicable to multi-schema variable layout.

## Field Description
### name
Name of a field.

### type
Data type. See Layout data types section.

### size
Full field size.

### scale
Size of decimals.

## Layout Data Types
It’s correspond to COBOL data types.
* packedDecimal
* integer
* short
* string
* decimal
* zonedDecimal
* skip – special type to skip any number of bytes. For example, there are many fillers which we don’t need and those fillers can contain "garbage"; we just ignore those fillers.