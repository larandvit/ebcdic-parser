# COBOL Zoned-Decimal Type
Each byte of zoned-decimal contains one digit or one character. A storage byte is separated into 2 parts: 

 1. 4 bit zone
 2. 4 bit digit.
 
 The low byte zone includes a sign. Positive numbers are represented  by hexadecimal F and negative numbers by hexadecimal D.
 
 Generic formula
 
|Byte 1|Byte 2|Byte 3|Byte 4|Byte 5|
|-|-|-|-|-|
|Zone-Digit|Zone-Digit|Zone-Digit|Zone-Digit|Sign-Digit|

For example, 35791

|Byte 1|Byte 2|Byte 3|Byte 4|Byte 5|
|-|-|-|-|-|
|1111-0011|1111-0101|1111-0111|1111-1001|1111-0001|

For example, -35791

|Byte 1|Byte 2|Byte 3|Byte 4|Byte 5|
|-|-|-|-|-|
|1111-0011|1111-0101|1111-0111|1111-1001|1101-0001|

<!--stackedit_data:
eyJoaXN0b3J5IjpbLTIwNzU5NDk0NjEsLTE3MjUxMTUyMjNdfQ
==
-->