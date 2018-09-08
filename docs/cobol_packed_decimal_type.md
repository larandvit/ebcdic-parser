
# COBOL Packed-Decimal Type
## Storage Description
Packed-decimal type stores two digits in each byte. An exception is low-order byte which contains one digit in the leftmost portion and the sign (positive or negative) in the rightmost portion. 

Positive numbers are represented by hexadecimal F and negative numbers by hexadecimal D.

Generic formula

|Byte 1|Byte 2|Byte 3|
|-|-|-|
|Digit-Digit|Digit-Digit|Digit-Sign|

For example, 35791

|Byte 1|Byte 2|Byte 3|
|-|-|-|
|0011-0101|0111-1001|0001-1111|

For example, -35791

|Byte 1|Byte 2|Byte 3|
|-|-|-|
|0011-0101|0111-1001|0001-1101|

## COBOL Representation
### Samples
```
01 VAR-PK-DECIMAL PIC 9(5) USAGE COMP-3.
01 VAR-PK-DECIMAL PIC 9(05) USAGE COMP-3.
01 VAR-PK-DECIMAL PIC 9(5) COMP-3.
01 VAR-PK-DECIMAL PIC S9(5) USAGE COMP-3.
01 VAR-PK-DECIMAL PIC 9(5)V9(2) USAGE COMP-3.
01 VAR-PK-DECIMAL PIC 9(5)V9(2) COMP-3.
01 VAR-PK-DECIMAL PIC S9(5)V9(2) COMP-3.
```
