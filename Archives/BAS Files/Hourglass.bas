SCREEN _NEWIMAGE(1024, 720, 32)
_SCREENMOVE _MIDDLE
_DEFINE A-Z AS LONG
DIM SHARED SandCounter
DIM FillColor AS _UNSIGNED LONG
REDIM SHARED Sand(100000) AS Coord
REDIM SHARED RemoveSand(100000) AS Coord
DIM Pause AS _FLOAT
CONST Seconds = 10
f = _LOADFONT("OLDENGL.ttf", 32)
_FONT f

TYPE Coord
    x AS INTEGER
    y AS INTEGER
END TYPE

CenterX = 512: CenterY = 360
FillColor = &HFFFF0000

DrawHourGlass CenterX, CenterY, 200, 50, 3, 3, -1
FillWithSand CenterX, CenterY, FillColor
PCOPY 0, 1
_DONTBLEND
DO
    PCOPY 1, 0
    FOR i = 1 TO SandCounter: Sand(i).x = CenterX: Sand(i).y = CenterY + 1: NEXT
    IF Pause = 0 THEN Pause = SandCounter / Seconds
    CountDown = Seconds
    o$ = STR$(CountDown): _PRINTSTRING (512 - _PRINTWIDTH(o$) \ 2, 570), o$ + "    "
    min = 1: max = 0
    t# = TIMER(0.001)
    DO
        IF max < SandCounter THEN
            max = max + 1
            PSET (RemoveSand(max).x, RemoveSand(max).y), 0
        END IF
        FOR i = min TO max
            IF POINT(Sand(i).x, Sand(i).y + 1) = 0 THEN 'fall down
                PSET (Sand(i).x, Sand(i).y), 0
                Sand(i).y = Sand(i).y + 1
            ELSEIF POINT(Sand(i).x - 1, Sand(i).y + 1) = 0 THEN 'fall down and left
                PSET (Sand(i).x, Sand(i).y), 0
                Sand(i).x = Sand(i).x - 1: Sand(i).y = Sand(i).y + 1
            ELSEIF POINT(Sand(i).x + 1, Sand(i).y + 1) = 0 THEN 'fall down and right
                PSET (Sand(i).x, Sand(i).y), 0
                Sand(i).x = Sand(i).x + 1: Sand(i).y = Sand(i).y + 1
            ELSE 'sit and don't move any more
                min = min + 1
            END IF
            PSET (Sand(i).x, Sand(i).y), FillColor
        NEXT
        IF TIMER - t# >= 1 THEN t# = TIMER(0.001): CountDown = CountDown - 1: o$ = STR$(CountDown): _PRINTSTRING (512 - _PRINTWIDTH(o$) \ 2, 570), o$ + "    "
        _LIMIT Pause 'to set the timing properly (IF possible.  Slow computers may not run this unoptimized code at speed for an hourglass with a low flip time.)
        _DISPLAY
        IF _KEYHIT THEN SYSTEM
    LOOP UNTIL max = SandCounter
LOOP


SUB FillWithSand (x, y, kolor AS _UNSIGNED LONG)
    IF POINT(x - 1, y) = 0 THEN
        PSET (x - 1, y), kolor
        SandCounter = SandCounter + 1
        IF SandCounter > UBOUND(Sand) THEN
            REDIM _PRESERVE Sand(UBOUND(sand) + 100000) AS Coord
            REDIM _PRESERVE RemoveSand(UBOUND(sand) + 100000) AS Coord
        END IF
        RemoveSand(SandCounter).x = x - 1: RemoveSand(SandCounter).y = y
        FillWithSand x - 1, y, kolor
    END IF
    IF POINT(x, y - 1) = 0 THEN
        PSET (x, y - 1), kolor
        SandCounter = SandCounter + 1
        IF SandCounter > UBOUND(Sand) THEN
            REDIM _PRESERVE Sand(UBOUND(sand) + 100000) AS Coord
            REDIM _PRESERVE RemoveSand(UBOUND(sand) + 100000) AS Coord
        END IF
        RemoveSand(SandCounter).x = x: RemoveSand(SandCounter).y = y - 1
        FillWithSand x, y - 1, kolor
    END IF

    IF POINT(x + 1, y) = 0 THEN
        PSET (x + 1, y), kolor
        SandCounter = SandCounter + 1
        IF SandCounter > UBOUND(Sand) THEN
            REDIM _PRESERVE Sand(UBOUND(sand) + 100000) AS Coord
            REDIM _PRESERVE RemoveSand(UBOUND(sand) + 100000) AS Coord
        END IF
        RemoveSand(SandCounter).x = x + 1: RemoveSand(SandCounter).y = y
        FillWithSand x + 1, y, kolor
    END IF
END SUB



SUB DrawHourGlass (x, y, high, wide, gap, thick, kolor AS _UNSIGNED LONG) 'x/y center
    LINE (x - gap, y)-STEP(-wide, -high), kolor
    LINE -STEP(2 * (wide + gap), -thick), kolor, BF
    LINE (x + gap, y)-STEP(wide, -high), kolor
    LINE (x + gap, y)-STEP(wide, high), kolor
    LINE (x - gap, y)-STEP(-wide, high), kolor
    LINE -STEP(2 * (wide + gap), thick), kolor, BF
    FOR thickness = 1 TO thick
        FOR Yborder = 0 TO y + high + thick
            FOR Xborder = 0 TO x
                IF POINT(Xborder + 1, Yborder) THEN PSET (Xborder, Yborder), kolor 'thicken left
            NEXT
            FOR Xborder = x + wide + 2 * gap + thickness TO x + 1 STEP -1
                IF POINT(Xborder - 1, Yborder) THEN PSET (Xborder, Yborder), kolor 'thicken right
            NEXT
        NEXT
    NEXT
END SUB

