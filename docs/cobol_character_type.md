# COBOL CHARACTER Type

## Storage Description
The character data type represents characters converted from a source coding to the output one. The output coding is utf-8.

**[ibm037 coding](https://github.com/larandvit/ebcdic-parser/blob/master/docs/ibm037_charset.md)**

The code page is 1 byte. It contains 255 characters and it's supported by Python.

|Converted to uft-8 bytes|H|e|l|l|o||e|v|e|r|y|b|o|d|y|!|
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|hex ibm037 bytes|c8|85|93|93|96|40|85|a5|85|99|a8|82|96|84|a8|5a|

**ibm935 (Chinese) coding**

The code page is 2 bytes. It contains 65535 characters. Python doesn't support this code page and it has to be used Java coding.

English characters

|Converted to uft-8 bytes|H|e|l|l|o||e|v|e|r|y|b|o|d|y|!|
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|hex ibm037 bytes|c8|85|93|93|96|40|85|a5|85|99|a8|82|96|84|a8|5a|

Hello everybody! in Chinese

|Converted to uft-8 bytes|Special byte to indicate start of 2 byte code|大|家|好|Special byte to indicate finish of 2 byte code|!|
|-|-|-|-|-|-|-|
|HEX ibm037 bytes|0e|4af2|4ed1|4dc2|0f|5a|

## COBOL Representation
### Samples
```
01 VAR-CHARACTER PIC X(5).
01 VAR-CHARACTER PIC X(05).
01 VAR-CHARACTER PIC XXXXX.
```
