DECLARE CUSTOMTYPE LIBRARY 'Use Customtype for self-referencing a sub written inside your program
    SUB SUB_EXAMPLE (BYVAL passed AS _OFFSET) 'this points to SUB EXAMPLE below, but uses an OFFSET to point to its parameter.
    'NOTE:  The sub/function name *MUST* be the same as QB64 translates it as, for us.
    'General rule of thumb is to make the subname ALL CAPS, preceeded by SUB_ or FUNCTION_ as dictated.

    SUB SUB_EXAMPLE2 (BYVAL passed AS _OFFSET)
END DECLARE

TYPE DataType 'A datatype to use as an example
    x AS STRING * 12
    y AS LONG
    z AS LONG
END TYPE

TYPE DataType2 'a second datatype
    byte1 AS _UNSIGNED _BYTE
    byte2 AS _UNSIGNED _BYTE
    byte3 AS _UNSIGNED _BYTE
    byte4 AS _UNSIGNED _BYTE
    byte5 AS _UNSIGNED _BYTE
    byte6 AS _UNSIGNED _BYTE
    byte7 AS _UNSIGNED _BYTE
    byte8 AS _UNSIGNED _BYTE
    byte9 AS _UNSIGNED _BYTE
    byte10 AS _UNSIGNED _BYTE
    byte11 AS _UNSIGNED _BYTE
    byte12 AS _UNSIGNED _BYTE
    byte13 AS _UNSIGNED _BYTE
    byte14 AS _UNSIGNED _BYTE
    byte15 AS _UNSIGNED _BYTE
    byte16 AS _UNSIGNED _BYTE
    byte17 AS _UNSIGNED _BYTE
    byte18 AS _UNSIGNED _BYTE
    byte19 AS _UNSIGNED _BYTE
    byte20 AS _UNSIGNED _BYTE
END TYPE



DIM m AS _MEM 'A memblock to store some information
m = _MEMNEW(20) 'The proper size to fill the data type that we're interested in passing back to our program.
_MEMPUT m, m.OFFSET, "Hello World" '12 bytes
_MEMPUT m, m.OFFSET + 12, -2 AS LONG '4 more
_MEMPUT m, m.OFFSET + 16, 3 AS LONG '4 more to make all 20

SUB_EXAMPLE m.OFFSET 'Call the sub with the offset to these 20 bytes of memory
SLEEP
SUB_EXAMPLE2 m.OFFSET 'Notice, we passed the same block of memory, but are handling it differently here,
'                            according to the paramters set in the second sub

_MEMFREE m



END

SUB Example (t AS DataType) 'And here, we want to set up the actual sub to work with our example datatype.
    PRINT t.x 'print the values of that memblock
    PRINT t.y
    PRINT t.z
END SUB

SUB Example2 (x AS DataType2)
    COLOR 12
    PRINT x.byte1
    PRINT x.byte2
    PRINT x.byte3
    PRINT x.byte4
    PRINT x.byte5
    PRINT x.byte6
    PRINT x.byte7
    PRINT x.byte8
    PRINT x.byte9
    PRINT x.byte10
    PRINT x.byte11
    PRINT x.byte12
    PRINT x.byte13
    PRINT x.byte14
    PRINT x.byte15
    PRINT x.byte16
    PRINT x.byte17
    PRINT x.byte18
    PRINT x.byte19
    PRINT x.byte20
END SUB

