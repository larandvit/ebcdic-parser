
# COBOL Engine Rules Dictionary
The document describes how to convert COBOL data type into engine rules layout.
 
|COBOL Type|COBOL Representation|Layout Type|Layout Size|Layout Scale|Range|
|------------|--------| -----|----|---|---|
|[Signed BINARY 1 byte](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_binary_type.md)|PIC S9 to S9(3) COMP|integer|1|N/A|-128 to 128|
|[Signed BINARY 2 bytes](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_binary_type.md)|PIC S9(4) to S9(5) COMP|integer|2|N/A|-32768 to 32767|
|[Signed BINARY 4 bytes](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_binary_type.md)|PIC S9(6) to S9(10) COMP|integer|4|N/A|-2147483648 to 2147483647|
|[Signed BINARY 8 bytes](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_binary_type.md)|PIC S9(11) to S9(19) COMP|integer|8|N/A|-9223372036854775808 to 9223372036854775807|
|[Unsigned BINARY 1 byte](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_binary_type_unsigned.md)|PIC 9 to 9(3) COMP|uinteger|1|N/A|0 to 255|
|[Unsigned BINARY 2 bytes](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_binary_type_unsigned.md)|PIC 9(4) to 9(5) COMP|uinteger|2|N/A|0 to 65535|
|[Unsigned BINARY 4 bytes](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_binary_type_unsigned.md)|PIC 9(6) to 9(10) COMP|uinteger|4|N/A|0 to 4294967295|
|[Unsigned BINARY 8 bytes](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_binary_type_unsigned.md)|PIC 9(11) to 9(20) COMP|uinteger|8|N/A|0 to 18446744073709551615|
|[FLOAT 4 bytes](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_float_type.md)|PIC S9(p)V9(s) COMP-1|Not supported|-|-|-3.4028235E+38 to -1.1754944E-38, 0.0E+0, +1.1754944E-38 to +3.4028235E+38|
|[FLOAT 8 bytes](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_float_type.md)|PIC S9(p)V9(s) COMP-2|Not supported|-|-|-1.797693134862315E+308 to -2.225073858507201E-308, 0.0E+0, +2.225073858507201E-308 to +1.797693134862315E+308|
|Alpha [CHARACTER](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_character_type.md) (A-Z, a-z, and space)|PIC A(n)|string|n|N/A|N/A|
|Any [CHARACTER](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_character_type.md)|PIC X(n)|string|n|N/A|N/A|
|Numeric [CHARACTER](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_character_type.md) (numbers 0-9)|PIC X(n)|string|n|N/A|N/A|
|[DECIMAL](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_packed_decimal_type.md)|PIC S9(p)V9(s) COMP-3|packedDecimal|(p+s)/2+1|s|N/A|
|[DISPLAY NUMERIC](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_zoned-decimal-type.md)|PIC S9(p)V9(s)|decimal, zonedDecimal|p+s|s|N/A|

