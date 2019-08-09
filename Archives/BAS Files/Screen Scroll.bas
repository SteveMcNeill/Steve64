SCREEN _NEWIMAGE(640, 480, 32)
PRINT "This is a test of the Steve Scrolling System"
PRINT "This is only a test."
FOR i = 1 TO 15: PRINT i: NEXT
DO
    _LIMIT 30
    a$ = UCASE$(INKEY$)
    IF a$ = "U" THEN ScrollUp
    IF a$ = "D" THEN ScrollDown
LOOP UNTIL a$ = CHR$(27)


SUB ScrollUp
    $CHECKING:OFF
    DIM m AS _MEM
    m = _MEMIMAGE(0)
    p = _PIXELSIZE
    IF p = 0 THEN w = _WIDTH * 2 ELSE w = _FONTHEIGHT * _WIDTH * p
    t$ = SPACE$(m.SIZE - w)
    _MEMGET m, m.OFFSET + w, t$
    CLS
    _MEMPUT m, m.OFFSET, t$
    _MEMFREE m
    $CHECKING:ON
END SUB

SUB ScrollDown
    $CHECKING:OFF
    DIM m AS _MEM
    m = _MEMIMAGE(0)
    p = _PIXELSIZE
    IF p = 0 THEN w = _WIDTH * 2 ELSE w = _FONTHEIGHT * _WIDTH * p
    t$ = SPACE$(m.SIZE - w)
    _MEMGET m, m.OFFSET, t$
    CLS
    _MEMPUT m, m.OFFSET + w, t$
    _MEMFREE m
    $CHECKING:ON
END SUB

