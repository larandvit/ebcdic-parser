

# COBOL BINARY Type
## Storage Description
Signed integers are represented 0 in the leftmost bit for positive values and 1 for negative ones.

**BINARY 1 (1 byte)**

|bit 7|bit 6|bit 5|bit 4|bit 3|bit 2|bit 1|bit 0|
|-|-|-|-|-|-|-|-|
|Sign|Bit value|Bit value|Bit value|Bit value|Bit value|Bit value|Bit value|

**BINARY 2 (2 bytes)**

|bit 15|bit 14|bit 14|...|bit 3|bit 2|bit 1|bit 0|
|-|-|-|-|-|-|-|-|
|Sign|Bit value|Bit value|...|Bit value|Bit value|Bit value|Bit value|

**BINARY 4 (4 bytes)**

|bit 31|bit 30|bit 29|...|bit 3|bit 2|bit 1|bit 0|
|-|-|-|-|-|-|-|-|
|Sign|Bit value|Bit value|...|Bit value|Bit value|Bit value|Bit value|

**BINARY 8 (8 bytes)**

|bit 63|bit 62|bit 61|...|bit 3|bit 2|bit 1|bit 0|
|-|-|-|-|-|-|-|-|
|Sign|Bit value|Bit value|...|Bit value|Bit value|Bit value|Bit value|

For example, 35791

|Byte 4|Byte 3|Byte 2|Byte 1|
|-|-|-|-|
|00000000|00000000|10001011|11001111|

For example, -35791

|Byte 4|Byte 3|Byte 2|Byte 1|
|-|-|-|-|
|11111111|11111111|01110100|00110001|

Explanation for 35791

|bit 31|...|bit 16|bit 15|bit 14|bit 13|bit 12|bit 11|bit 10|bit 9|bit 8|bit 7|bit 6|bit 5|bit 4|bit 3|bit 2|bit 1|bit 0|
|-|...|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|0|...|0|32768|0|0|0|2048|0|512|256|128|64|0|0|8|4|2|1|

35791 = 32768 + 2048 + 512 + 256 + 128 + 64 + 8 + 4 + 2 + 1

Explanation for -35791

|bit 31|...|bit 15|bit 14|bit 13|bit 12|bit 11|bit 10|bit 9|bit 8|bit 7|bit 6|bit 5|bit 4|bit 3|bit 2|bit 1|bit 0|
|-|...|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|2147483648|...|32768|16384|8192|4096|0|1024|0|0|0|0|32|16|0|0|0|1|

## COBOL Representation
### Samples
```
01 VAR-BINARY PIC S9(5) USAGE COMP.
01 VAR-BINARY PIC S9(05) USAGE COMP.
01 VAR-BINARY PIC S9(5) COMP.
01 VAR-BINARY PIC S9(5) USAGE COMP.
```
