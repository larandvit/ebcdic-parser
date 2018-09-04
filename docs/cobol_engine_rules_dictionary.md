
# COBOL Engine Rules Dictionary
The document describes how to convert COBOL data type into engine rules layout.
 
|COBOL Type|COBOL Representation|Layout Type|Layout Size|Layout Scale
|------------|--------| -----|----|---|
|BINARY 2 bytes|PIC S9 to S9(4) COMP|short|2|N/A|
|BINARY 4 bytes|PIC S9(5) to S9(9) COMP|integer|4|N/A|
|CHARACTER|PIC X(n)|string|n|N/A|
|DECIMAL|PIC S9(p)V9(s) COMP-3|packedDecimal|(p+s)/2+1|s|
|DISPLAY NUMERIC|PIC S9(p)V9(s)|decimal|p+s|s|
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTE3NDY0MTA0MDAsLTE1NzcyMDIzNTZdfQ
==
-->