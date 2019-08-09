DIM x AS INTEGER
DIM m AS _MEM
m = _MEM(x)
PRINT m.OFFSET
PRINT ConvertOffset(m.OFFSET)


FUNCTION ConvertOffset&& (value AS _OFFSET)
    $CHECKING:OFF
    DIM m AS _MEM 'Define a memblock
    m = _MEM(value) 'Point it to use value
    $IF 64BIT THEN
        'On 64 bit OSes, an OFFSET is 8 bytes in size.  We can put it directly into an Integer64
        _MEMGET m, m.OFFSET, ConvertOffset&& 'Get the contents of the memblock and put the values there directly into ConvertOffset&&
    $ELSE
        'However, on 32 bit OSes, an OFFSET is only 4 bytes.  We need to put it into a LONG variable first
        _MEMGET m, m.OFFSET, temp& 'Like this
        ConvertOffset&& = temp& 'And then assign that long value to ConvertOffset&&
    $END IF
    _MEMFREE m 'Free the memblock
    $CHECKING:ON
END FUNCTION

