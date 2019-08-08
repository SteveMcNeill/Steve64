x$ = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way – in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only."
PRINT x$
PRINT
PRINT "****************************************"
PRINT "Original:"; LEN(x$)
x$ = CrunchText(x$)
PRINT "Crunched: "; LEN(x$)
x$ = RestoreText(x$)
PRINT "Original:"; LEN(x$)
PRINT "****************************************"
PRINT
PRINT x$

FUNCTION RestoreText$ (text$)
    OriginalSize = CVL(LEFT$(text$, 4))
    Indexcount = ASC(text$, 5) 'The size of our index
    DIM index(Indexcount) AS _UNSIGNED _BYTE
    FOR BitSize = 1 TO 7 'find the number of bits to best store our information
        IF 2 ^ BitSize >= Indexcount THEN EXIT FOR
    NEXT

    FOR i = 1 TO Indexcount
        index(i) = ASC(text$, i + 5) 'Our Index contents rebuilt
    NEXT

    DIM m AS _MEM
    temp$ = MID$(text$, i + 5)

    MakeBitArray m, 1, LEN(temp$) * 8 + 32 ' 388
    _MEMPUT m, m.OFFSET, temp$
    out$ = "": i = 0: Byte = 0

    DO
        Byte = Byte + 1
        o = 0
        FOR k = 1 TO BitSize
            i = i + 1
            z = GetBitArray(m, i)
            IF z THEN
                o = o + 2 ^ (k - 1)
            END IF
        NEXT
        IF o = 0 THEN out$ = out$ + CHR$(index(Indexcount)) ELSE out$ = out$ + CHR$(index(o))
    LOOP UNTIL Byte >= OriginalSize
    RestoreText = out$
END SUB


FUNCTION CrunchText$ (text$)
    'First count the letters used in the text to build an index
    FOR i = 0 TO 255
        IF INSTR(text$, CHR$(i)) THEN
            IndexCount = IndexCount + 1
            REDIM _PRESERVE Index(IndexCount) AS _UNSIGNED _BYTE
            Index(IndexCount) = i 'Our Index of used letters
        END IF
    NEXT

    FOR BitSize = 1 TO 7 'find the number of bits to best store our information
        IF 2 ^ BitSize >= IndexCount THEN EXIT FOR
    NEXT
    Index(0) = IndexCount 'Store the number of bits.
    PackedSize = _CEIL(LEN(text$) / 8 * BitSize)

    DIM m AS _MEM
    MakeBitArray m, 1, LEN(text$) * BitSize

    FOR i = 1 TO LEN(text$)
        work = ASC(text$, i) 'get the letter we're on.
        FOR j = 1 TO IndexCount
            IF work = Index(j) THEN
                FOR k = 1 TO BitSize
                    IF j AND 2 ^ (k - 1) THEN
                        SetBitArray m, (i - 1) * BitSize + k, 1
                    END IF
                NEXT
            END IF
        NEXT
    NEXT

    DIM n AS _MEM
    n = _MEM(Index())
    index$ = SPACE$(n.SIZE)
    _MEMGET n, n.OFFSET, index$
    _MEMFREE n
    tempcrunch$ = SPACE$(m.SIZE)
    _MEMGET m, m.OFFSET, tempcrunch$
    _MEMFREE m
    CrunchText = MKL$(LEN(text$)) + index$ + tempcrunch$
END SUB















SUB MakeBitArray (m AS _MEM, StartElement AS LONG, EndElement AS LONG)
    Size = EndElement - StartElement + 1
    MemSize = (Size + 7) \ 8 + 4 'Allow for padding if needed.
    m = _MEMNEW(MemSize)
    _MEMPUT m, m.OFFSET, StartElement 'We track what the starting element value is so we can use it later.
    ResetBitArray m
END SUB

SUB SetBitArray (m AS _MEM, Element AS LONG, Value AS _BYTE)
    IF Value <> 0 THEN Value = 1
    DIM StartElement AS LONG
    DIM o AS _OFFSET, b AS _UNSIGNED _BYTE
    DIM UnpackedByte(0 TO 7) AS _UNSIGNED _BIT

    _MEMGET m, m.OFFSET, StartElement
    o = m.OFFSET + 4 'The start of our actual data

    WorkElement = Element - StartElement
    BytePosition = WorkElement \ 8: BitPosition = WorkElement MOD 8
    _MEMGET m, o + BytePosition, b

    FOR i = 0 TO 7
        IF b AND 2 ^ i THEN UnpackedByte(i) = 1 'Get the old values of our packed data
    NEXT
    UnpackedByte(BitPosition) = Value
    b = 0
    FOR i = 0 TO 7
        IF UnpackedByte(i) THEN b = b + 2 ^ i
    NEXT
    _MEMPUT m, o + BytePosition, b
END SUB

FUNCTION GetBitArray~` (m AS _MEM, Element AS LONG)
    DIM StartElement AS LONG
    DIM o AS _OFFSET, b AS _UNSIGNED _BYTE
    DIM UnpackedByte(0 TO 7) AS _UNSIGNED _BIT

    _MEMGET m, m.OFFSET, StartElement
    o = m.OFFSET + 4 'The start of our actual data

    WorkElement = Element - StartElement
    BytePosition = WorkElement \ 8: BitPosition = WorkElement MOD 8
    _MEMGET m, o + BytePosition, b

    IF b AND 2 ^ BitPosition THEN GetBitArray = 1 'Get the old values of our packed data
END FUNCTION

SUB ResetBitArray (m AS _MEM)
    _MEMFILL m, m.OFFSET + 4, m.SIZE - 4, 0 AS _UNSIGNED _BYTE
END SUB

