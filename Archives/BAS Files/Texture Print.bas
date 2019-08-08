_DEFINE A-Z AS LONG
TYPE TCT 'Text Color Type
    color AS _UNSIGNED LONG
    count AS _UNSIGNED INTEGER
END TYPE
CONST Solid = 0
CONST Stripes = 1
CONST Texture = 2
CONST Pixel = 3
CONST No = 0
CONST Hort = 2
CONST Vert = 1
CONST AutoWrap = -1 'This mimics QB64 pla



SCREEN _NEWIMAGE(640, 480, 32)
f = _LOADFONT("C:\Windows\Fonts\cour.ttf", 64, "MONOSPACE")
_FONT f
fw = _FONTWIDTH - 1: fh = _FONTHEIGHT - 1
DIM SHARED Alpha(255, fw, fh) AS INTEGER
DIM SHARED ColorArray(fw, fh), BGColorArray(fw, fh)
REDIM SHARED PrintColor(0) AS TCT
Image = _LOADIMAGE("camo.jpg")
Image2 = _LOADIMAGE("marble.jpg")



InitPrint 'First Initialize the print letters to use the current font
SetPrintColor _RGB32(255, 255, 0), 0
SetFG No, Solid
ClearPrintColor: SetPrintColor _RGB32(255, 0, 0), 0
SetBG No, Solid
PrintChar 1, 300, 2

ClearPrintColor: SetPrintColor 0, 0
SetBG No, Solid

ClearPrintColor
SetPrintColor _RGB32(255, 255, 0), 2
SetPrintColor _RGB32(0, 0, 255), 2
SetFG Hort, Stripes
PrintChar 40, 300, 65

ClearPrintColor
SetPrintColor _RGB32(255, 0, 0), 0
SetFG No, Solid
ClearPrintColor
SetPrintColor _RGB32(255, 255, 0), 2
SetPrintColor _RGB32(0, 0, 255), 2

SetBG Vert, Stripes
PrintChar 80, 300, 66

ClearPrintColor: SetPrintColor 0, 0
SetBG Image, Texture
SetFG Image2, Texture
PrintOut 400, 400, "testing screen placement for overlong strings"
_PRINTSTRING (200, 50), "test" 'for comparision

ClearPrintColor
SetPrintColor _RGB32(255, 0, 0), 2
SetPrintColor _RGB32(255, 255, 255), 2
SetPrintColor _RGB32(0, 0, 255), 2
SetFG Vert, Pixel
ClearPrintColor: SetPrintColor 0, 0
SetBG No, Solid
PrintOut 200, 150, "test"
SLEEP 'So the user can get a look at the pretty options

CLS


'And a timed test for fun
count1 = 0: count2 = 0
t# = TIMER(0.001) + 5 '5 second test
DO
    _PRINTSTRING (0, 0), "Time Test 1"
    count1 = count1 + 1
LOOP UNTIL t# <= TIMER(0.001)
t# = TIMER(0.001) + 5 '5 second test
CLS
DO
    PrintOut 0, 0, "Time Test 2"
    count2 = count2 + 1
LOOP UNTIL t# <= TIMER(0.001)
_FONT 16
LOCATE 10, 1: PRINT "And here's some surprising news:"
PRINT "_PRINTSTRING can print to the screen "; count1; " times in 5 seconds."
PRINT "PrintOut can print to the screen "; count2; " times in 5 seconds."
PRINT
PRINT "For all the options we offer with the new font routines here, they're still of  comparable speed to the ones built into QB64!"
PRINT
PRINT "And as odd and amazing as it is, the PrintOut routine is actually FASTER for me!"




SUB ClearPrintColor
REDIM PrintColor(-1) AS TCT
END SUB


SUB SetPrintColor (C AS _UNSIGNED LONG, count AS _UNSIGNED INTEGER)
u = UBOUND(PrintColor) + 1
REDIM _PRESERVE PrintColor(u) AS TCT
PrintColor(u).color = C
PrintColor(u).count = count
END SUB



SUB PrintOut (x, y, text$)
fh = _FONTHEIGHT
fw = _FONTWIDTH
w = _WIDTH - fw: h = _HEIGHT - fh
IF AutoWrap AND (x + _PRINTWIDTH(text$)) > _WIDTH THEN x = 0
DO UNTIL i >= LEN(text$)
    i = i + 1
    PrintChar x, y, ASC(text$, i)
    x = x + fw
    IF x > w THEN
        x = 0
        IF y + fh > h THEN ScrollUp ELSE y = y + fh
    END IF
LOOP
END SUB


SUB InitPrint
_CONTROLCHR OFF
fw = _FONTWIDTH: fh = _FONTHEIGHT
t& = _NEWIMAGE(_WIDTH, _HEIGHT, 32)
f& = _FONT
D = _DEST: S = _SOURCE
_DEST t&: _SOURCE t&
_FONT f&

FOR i = 0 TO 255
    CLS , _RGB32(0, 0, 0)
    PRINT CHR$(i)
    FOR x = 0 TO fw - 1
        FOR y = 0 TO fh - 1
            IF POINT(x, y) <> _RGB32(0, 0, 0) THEN Alpha(i, x, y) = 1 ELSE Alpha(i, x, y) = 0
        NEXT
    NEXT
NEXT
_DEST D: _SOURCE S
_FREEIMAGE t&
ClearPrintColor 'initialize printing color
END SUB


SUB PrintChar (x, y, char)
$CHECKING:OFF
fw = _FONTWIDTH - 1: fh = _FONTHEIGHT - 1
DIM m AS _MEM
m = _MEMIMAGE(0)
FOR x1 = 0 TO fw
    FOR y1 = 0 TO fh
        Pout = ((y + y1) * _WIDTH + x + x1) * 4
        IF Alpha(char, x1, y1) THEN
            _MEMPUT m, m.OFFSET + Pout, ColorArray(x1, y1)
        ELSE
            _MEMPUT m, m.OFFSET + Pout, BGColorArray(x1, y1)
        END IF
    NEXT
NEXT
_MEMFREE m
$CHECKING:ON
END SUB


SUB SetFG (Direction AS LONG, mode AS LONG) 'we feed this an array for color
'C() is our color array
'Direction is 1 for vertical, 2 for hortizontal
'mode is 0 for solid, 1 for stripes

SELECT CASE mode
    CASE 0 'solid
        FOR x = 0 TO _FONTWIDTH - 1
            FOR y = 0 TO _FONTHEIGHT - 1
                ColorArray(x, y) = PrintColor(0).color 'only one color, so we use it for a solid color
            NEXT
        NEXT
    CASE 1 'striped
        IF Direction = 1 THEN
            y = 0
            DO
                FOR i = 0 TO UBOUND(PrintColor)
                    FOR j = 0 TO PrintColor(i).count
                        FOR x = 0 TO _FONTWIDTH - 1
                            ColorArray(x, y) = PrintColor(i).color
                        NEXT
                        y = y + 1: IF y > _FONTHEIGHT - 1 THEN EXIT DO
                    NEXT
                NEXT
            LOOP
        ELSE
            x = 0
            DO
                FOR i = 0 TO UBOUND(PrintColor)
                    FOR j = 0 TO PrintColor(i).count
                        FOR y = 0 TO _FONTHEIGHT - 1
                            ColorArray(x, y) = PrintColor(i).color
                        NEXT
                        x = x + 1: IF x > _FONTWIDTH - 1 THEN EXIT DO
                    NEXT
                NEXT
            LOOP
        END IF
    CASE 2 'texture/image
        ni = _NEWIMAGE(_FONTWIDTH, _FONTHEIGHT, 32)
        _PUTIMAGE , Direction, ni
        S = _SOURCE: d = _DEST
        _SOURCE ni
        FOR x = 0 TO _FONTWIDTH - 1
            FOR y = 0 TO _FONTHEIGHT - 1
                ColorArray(x, y) = POINT(x, y)
            NEXT
        NEXT
        _SOURCE S: _DEST d
        _FREEIMAGE ni
    CASE 3 'pixel by pixel
        IF Direction = 1 THEN
            c = 0: i = 0
            FOR x = 0 TO _FONTWIDTH - 1
                FOR y = 0 TO _FONTHEIGHT - 1
                    ColorArray(x, y) = PrintColor(i).color
                    c = c + 1
                    IF c > PrintColor(i).count THEN
                        c = 0
                        i = i + 1
                        IF i > UBOUND(printcolor) THEN i = 0
                    END IF
                NEXT
            NEXT
        ELSE
            c = 0: i = 0
            FOR y = 0 TO _FONTHEIGHT - 1
                FOR x = 0 TO _FONTWIDTH - 1
                    ColorArray(x, y) = PrintColor(i).color
                    c = c + 1
                    IF c > PrintColor(i).count THEN
                        c = 0
                        i = i + 1
                        IF i > UBOUND(printcolor) THEN i = 0
                    END IF
                NEXT
            NEXT
        END IF
END SELECT
END SUB

SUB SetBG (Direction AS LONG, mode AS LONG) 'we feed this an array for color
'C() is our color array
'Direction is 1 for vertical, 2 for hortizontal
'mode is 0 for solid, 1 for stripes

SELECT CASE mode
    CASE 0
        FOR x = 0 TO _FONTWIDTH - 1
            FOR y = 0 TO _FONTHEIGHT - 1
                BGColorArray(x, y) = PrintColor(0).color 'only one color, so we use it for a solid color
            NEXT
        NEXT
    CASE 1
        IF Direction = 1 THEN
            y = 0
            DO
                FOR i = 0 TO UBOUND(PrintColor)
                    FOR j = 0 TO PrintColor(i).count
                        FOR x = 0 TO _FONTWIDTH - 1
                            BGColorArray(x, y) = PrintColor(i).color
                        NEXT
                        y = y + 1: IF y > _FONTHEIGHT - 1 THEN EXIT DO
                    NEXT
                NEXT
            LOOP
        ELSE
            x = 0
            DO
                FOR i = 0 TO UBOUND(PrintColor)
                    FOR j = 0 TO PrintColor(i).count
                        FOR y = 0 TO _FONTHEIGHT - 1
                            BGColorArray(x, y) = PrintColor(i).color
                        NEXT
                        x = x + 1: IF x > _FONTWIDTH - 1 THEN EXIT DO
                    NEXT
                NEXT
            LOOP
        END IF
    CASE 2
        ni = _NEWIMAGE(_FONTWIDTH, _FONTHEIGHT, 32)
        _PUTIMAGE , Direction, ni
        S = _SOURCE: D = _DEST
        _SOURCE ni
        FOR x = 0 TO _FONTWIDTH - 1
            FOR y = 0 TO _FONTHEIGHT - 1
                BGColorArray(x, y) = POINT(x, y)
            NEXT
        NEXT
        _SOURCE S: _DEST D
        _FREEIMAGE ni
    CASE 3 'pixel by pixel
        IF Direction = 1 THEN
            c = 0: i = 0
            FOR x = 0 TO _FONTWIDTH - 1
                FOR y = 0 TO _FONTHEIGHT - 1
                    BGColorArray(x, y) = PrintColor(i).color
                    c = c + 1
                    IF c > PrintColor(i).count THEN
                        c = 0
                        i = i + 1
                        IF i > UBOUND(printcolor) THEN i = 0
                    END IF
                NEXT
            NEXT
        ELSE
            c = 0: i = 0
            FOR y = 0 TO _FONTHEIGHT - 1
                FOR x = 0 TO _FONTWIDTH - 1
                    BGColorArray(x, y) = PrintColor(i).color
                    c = c + 1
                    IF c > PrintColor(i).count THEN
                        c = 0
                        i = i + 1
                        IF i > UBOUND(printcolor) THEN i = 0
                    END IF
                NEXT
            NEXT
        END IF
END SELECT
END SUB


SUB ScrollUp2
t& = _COPYIMAGE(0)
CLS
_PUTIMAGE (0, -_FONTHEIGHT), t&
_FREEIMAGE t&
END SUB

SUB ScrollUp
DIM m AS _MEM
m = _MEMIMAGE(0)
p = _PIXELSIZE
IF p = 0 THEN w = _WIDTH * 2 ELSE w = _FONTHEIGHT * _WIDTH * p
t$ = SPACE$(m.SIZE - w)
_MEMGET m, m.OFFSET + w, t$
CLS
_MEMPUT m, m.OFFSET, t$
_MEMFREE m
END SUB

