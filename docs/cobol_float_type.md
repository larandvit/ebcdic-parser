
# COBOL FLOAT Type
## Storage description
This is floating point data type. There are two parts
1. mantissa
2. exponent.

To get a floating-point value, the mantissa is multiplied  by 10 raised to the power of the exponent.

For example, 35791 value, 3.5791 is the mantissa and 4 is the exponent
```
3.5791 * (10 ** 4) = 35791
```
**FLOAT 4 bytes**

It contains 8 digits in mantissa and it's called single floating point data type. The leftmost 8 bits stores exponent and the remaining 24 bits stores mantissa.

**FLOAT 8 bytes**

It contains 16 digits in mantissa and it's called double floating point data type. The leftmost 12 bits stores exponent and the remaining 52 bits stores mantissa.

## COBOL Representation
### Samples 4 bytes
```
01 VAR-FLOAT PIC 9(5) USAGE COMP-1.
01 VAR-FLOAT PIC 9(5) COMP-1.
01 VAR-FLOAT PIC 9(5)V9(2) COMP-1.
```
### Samples 8 bytes
```
01 VAR-FLOAT PIC 9(05) USAGE COMP-2.
01 VAR-FLOAT PIC S9(5) USAGE COMP-2.
01 VAR-FLOAT PIC 9(5)V9(2) USAGE COMP-2.
01 VAR-FLOAT PIC S9(5)V9(2) COMP-2.
```
