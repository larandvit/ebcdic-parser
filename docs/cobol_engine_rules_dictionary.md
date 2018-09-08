
# COBOL Engine Rules Dictionary
The document describes how to convert COBOL data type into engine rules layout.
 
|COBOL Type|COBOL Representation|Layout Type|Layout Size|Layout Scale|Range|
|------------|--------| -----|----|---|---|
|BINARY 1 bytes|PIC S9 to S9(3) COMP|short|1|N/A|-128 to 128|
|BINARY 2 bytes|PIC S9(4) to S9(5) COMP|short|2|N/A|-32768 to 32767|
|BINARY 4 bytes|PIC S9(6) to S9(10) COMP|integer|4|N/A|-2147483648 to 2147483647|
|BINARY 8 bytes|PIC S9(11) to S9(20) COMP|-|-|-|-9223372036854775808 to 9223372036854775807|
|CHARACTER|PIC X(n)|string|n|N/A|N/A|
|[DECIMAL](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_packed_decimal_type.md)|PIC S9(p)V9(s) COMP-3|packedDecimal|(p+s)/2+1|s|N/A|
|[DISPLAY NUMERIC](https://github.com/larandvit/ebcdic-parser/blob/master/docs/cobol_zoned-decimal-type.md)|PIC S9(p)V9(s)|decimal, zonedDecimal|p+s|s|N/A|
