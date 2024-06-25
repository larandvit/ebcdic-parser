# Engine Rules Manual

## Introduction

The converter consists of two parts: (1) engine (application) and (2) engine rules (layouts). Engines rules are aimed to process varieties of input file types without making changes in the application code.

The engine rules driven approach rather than application one dictates how to convert data and it adds flexibility to mix setups between different layouts.

There are supported layouts
1. Single schema
2. Multi-schema fixed record length
3. Multi-schema variable record length.
4. Single schema variable record length.
    
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
    * How many times variable part is repeated. Key type is variablerepeattimes. It's an optional fields. If skipped, the last record in the layout is a holder. It's applicable to layout fields.
4. Single schema variable length
    * How many times variable part is repeated. Key type is variablerepeattimes. It's an optional fields. If skipped, the last record in the layout is a holder. It's applicable to layout fields.
    
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
                    "name": "employee_type", "type": "string", "size": 1, "flunkif":"p,x,c"
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
[Data type](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_engine_rules_dictionary.md). See Layout data types section.

### size
Full [field size](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_engine_rules_dictionary.md).

### scale
[Size of decimals](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_engine_rules_dictionary.md).

### flunkif
An optional comma seperated value list to exclude from final output for the given field. Currently applies to string values only. 

### keytype
The values is variablerepeattimes. It's a flag which shows a field with number of variable records. The field is optional one.

## Layout Data Types
It’s correspond to [COBOL data types](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_engine_rules_dictionary.md).
* packedDecimal
* integer
* string
* decimal
* skip – special type to skip any number of bytes. For example, there are many fillers which we don’t need and those fillers can contain "garbage"; we just ignore those fillers.
