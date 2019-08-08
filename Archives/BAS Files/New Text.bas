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
CONST AutoWrap = -1 'This mimics QB-64 behavior for making text print at the start of a row, if it's too long to fit on the row we started
CONST WordWrap = -1 'To autowrap our words at proper line breaks.


REDIM SHARED Alpha1(255, 0, 0) AS _BIT
REDIM SHARED Alpha2(255, 0, 0) AS _BIT
REDIM SHARED Alpha3(255, 0, 0) AS _BIT
REDIM SHARED Alpha4(255, 0, 0) AS _BIT
REDIM SHARED Alpha5(255, 0, 0) AS _BIT
REDIM SHARED ColorArray(0, 0), BGColorArray(0, 0)
REDIM SHARED PrintColor(0) AS TCT
DIM SHARED CurrentFont AS _BYTE


SCREEN _NEWIMAGE(640, 480, 32)
_DELAY 2
Image = _LOADIMAGE("camo.jpg")
Image2 = _LOADIMAGE("marble.jpg")
f = _LOADFONT("C:\Windows\Fonts\cour.ttf", 32, "MonoSpace")
_FONT f
PRINT 12
END


UseFont 1 'Tell the program which font we want to use (from 1 to 5)
InitFont "C:\Windows\Fonts\cour.ttf" 'First Initialize the print letters to use the current font
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
PrintChar 160, 300, 66


ClearPrintColor: SetPrintColor 0, 0
SetBG Image, Texture
SetFG Image2, Texture
PrintOut 200, 400, "testing screen placement for overlong strings"

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



SUB PrintOut (Tempx, Tempy, text$)
x = Tempx: y = Tempy: temp$ = text$
fw = UBOUND(ColorArray, 1): fh = UBOUND(ColorArray, 2)

w = _WIDTH: h = _HEIGHT - fh
IF AutoWrap AND (x + _PRINTWIDTH(temp$)) > _WIDTH THEN x = 0
DO
    IF fw * LEN(temp$) + x <= _WIDTH THEN EXIT DO

    t$ = "": position = 0: l = 0
    DO UNTIL x + l > w OR position > LEN(temp$)
        position = position + 1
        l = fw * position '_PRINTWIDTH(LEFT$(temp$, position))
    LOOP
    t$ = LEFT$(temp$, position - 1)
    temp$ = MID$(temp$, position)
    IF WordWrap THEN
        l1 = 0: position1 = LEN(t$)
        DO UNTIL l1 OR position1 = 0
            r$ = RIGHT$(t$, 1)
            l1 = INSTR(" .,;-?!" + CHR$(34), r$)
            IF l1 = 0 THEN
                t$ = LEFT$(t$, LEN(t$) - 1): temp$ = r$ + temp$
            END IF
            position1 = position1 - 1
        LOOP
    END IF
    I = 0
    DO UNTIL I >= LEN(t$)
        I = I + 1
        PrintChar x, y, ASC(t$, I)
        x = x + fw
    LOOP
    x = 0
    y = y + fh
    IF y > h THEN ScrollUp: y = y - fh 'ELSE y = y + fh
LOOP UNTIL t$ = ""
I = 0

DO UNTIL I >= LEN(temp$)
    I = I + 1

    PrintChar x, y, ASC(temp$, I)
    x = x + fw
LOOP
END SUB

SUB UseFont (number)
IF number < 1 OR number > 5 THEN ERROR 5: EXIT SUB
CurrentFont = number
END SUB

SUB InitFont (font$)
_CONTROLCHR OFF
f = _LOADFONT(font$, 32, "MONOSPACE")
IF f = -1 THEN ERROR 5: EXIT SUB 'bad font, toss an error
'No idea if 5 is the error I want, but I'll look those up later and choose the proper one. :P
'For now ALL Errors are ERROR 5
t& = _NEWIMAGE(_WIDTH, _HEIGHT, 32)
D = _DEST: s = _SOURCE
_DEST t&: _SOURCE t&
_FONT f
fw = _FONTWIDTH(f): fh = _FONTHEIGHT(f)
fontnumber = CurrentFont
SELECT CASE fontnumber
    CASE 1: REDIM Alpha1(255, fw, fh) AS _BIT
    CASE 2: REDIM Alpha2(255, fw, fh) AS _BIT
    CASE 3: REDIM Alpha3(255, fw, fh) AS _BIT
    CASE 4: REDIM Alpha4(255, fw, fh) AS _BIT
    CASE 5: REDIM Alpha5(255, fw, fh) AS _BIT
    CASE ELSE: ERROR 5: GOTO subexit
END SELECT
FOR i = 0 TO 255
    CLS , _RGB32(0, 0, 0)
    PRINT CHR$(i)
    FOR x = 0 TO fw - 1
        FOR y = 0 TO fh - 1
            SELECT CASE fontnumber
                CASE 1: IF POINT(x, y) <> _RGB32(0, 0, 0) THEN Alpha1(i, x, y) = 1 ELSE Alpha1(i, x, y) = 0
                CASE 2: IF POINT(x, y) <> _RGB32(0, 0, 0) THEN Alpha2(i, x, y) = 1 ELSE Alpha2(i, x, y) = 0
                CASE 3: IF POINT(x, y) <> _RGB32(0, 0, 0) THEN Alpha3(i, x, y) = 1 ELSE Alpha3(i, x, y) = 0
                CASE 4: IF POINT(x, y) <> _RGB32(0, 0, 0) THEN Alpha4(i, x, y) = 1 ELSE Alpha4(i, x, y) = 0
                CASE 5: IF POINT(x, y) <> _RGB32(0, 0, 0) THEN Alpha5(i, x, y) = 1 ELSE Alpha5(i, x, y) = 0
            END SELECT
        NEXT
    NEXT
NEXT
subexit:
_DEST D: _SOURCE s
_FREEIMAGE t&
ClearPrintColor 'initialize printing color
END SUB


SUB PrintChar (x, y, char)
'$CHECKING:OFF
fw = UBOUND(ColorArray, 1): fh = UBOUND(ColorArray, 2)
DIM m AS _MEM
m = _MEMIMAGE(0)

SELECT CASE CurrentFont
    CASE 1
        FOR x1 = 0 TO fw
            FOR y1 = 0 TO fh
                Pout = ((y + y1) * _WIDTH + x + x1) * 4
                IF Alpha1(char, x1, y1) THEN
                    _MEMPUT m, m.OFFSET + Pout, ColorArray(x1, y1)
                ELSE
                    _MEMPUT m, m.OFFSET + Pout, BGColorArray(x1, y1)
                END IF
            NEXT
        NEXT
    CASE 2
        FOR x1 = 0 TO fw
            FOR y1 = 0 TO fh
                Pout = ((y + y1) * _WIDTH + x + x1) * 4
                IF Alpha2(char, x1, y1) THEN
                    _MEMPUT m, m.OFFSET + Pout, ColorArray(x1, y1)
                ELSE
                    _MEMPUT m, m.OFFSET + Pout, BGColorArray(x1, y1)
                END IF
            NEXT
        NEXT
    CASE 3
        FOR x1 = 0 TO fw
            FOR y1 = 0 TO fh
                Pout = ((y + y1) * _WIDTH + x + x1) * 4
                IF Alpha3(char, x1, y1) THEN
                    _MEMPUT m, m.OFFSET + Pout, ColorArray(x1, y1)
                ELSE
                    _MEMPUT m, m.OFFSET + Pout, BGColorArray(x1, y1)
                END IF
            NEXT
        NEXT
    CASE 4
        FOR x1 = 0 TO fw
            FOR y1 = 0 TO fh
                Pout = ((y + y1) * _WIDTH + x + x1) * 4
                IF Alpha4(char, x1, y1) THEN
                    _MEMPUT m, m.OFFSET + Pout, ColorArray(x1, y1)
                ELSE
                    _MEMPUT m, m.OFFSET + Pout, BGColorArray(x1, y1)
                END IF
            NEXT
        NEXT
    CASE 5
        FOR x1 = 0 TO fw
            FOR y1 = 0 TO fh
                Pout = ((y + y1) * _WIDTH + x + x1) * 4
                IF Alpha5(char, x1, y1) THEN
                    _MEMPUT m, m.OFFSET + Pout, ColorArray(x1, y1)
                ELSE
                    _MEMPUT m, m.OFFSET + Pout, BGColorArray(x1, y1)
                END IF
            NEXT
        NEXT
END SELECT
_MEMFREE m
$CHECKING:ON
END SUB


SUB SetFG (Direction AS LONG, mode AS LONG) 'we feed this an array for color
'C() is our color array
'Direction is 1 for vertical, 2 for hortizontal
'mode is 0 for solid, 1 for stripes

fontnumber = CurrentFont
SELECT CASE fontnumber
    CASE 1: fw = UBOUND(Alpha1, 2): fh = UBOUND(alpha1, 3)
    CASE 2: fw = UBOUND(Alpha2, 2): fh = UBOUND(alpha2, 3)
    CASE 3: fw = UBOUND(Alpha3, 2): fh = UBOUND(alpha3, 3)
    CASE 4: fw = UBOUND(Alpha4, 2): fh = UBOUND(alpha4, 3)
    CASE 5: fw = UBOUND(Alpha5, 2): fh = UBOUND(alpha5, 3)
END SELECT
'clear the old color array if our sizes don't match and set the new one
IF UBOUND(bgColorArray, 1) <> fw AND UBOUND(bgcolorarray, 2) <> fh THEN REDIM BGColorArray(fw, fh)
REDIM ColorArray(fw, fh)

SELECT CASE mode
    CASE 0 'solid
        FOR x = 0 TO fw - 1
            FOR y = 0 TO fh - 1
                ColorArray(x, y) = PrintColor(0).color 'only one color, so we use it for a solid color
            NEXT
        NEXT
    CASE 1 'striped
        IF Direction = 1 THEN
            y = 0
            DO
                FOR i = 0 TO UBOUND(PrintColor)
                    FOR j = 0 TO PrintColor(i).count
                        FOR x = 0 TO fw - 1
                            ColorArray(x, y) = PrintColor(i).color
                        NEXT
                        y = y + 1: IF y > fh - 1 THEN EXIT DO
                    NEXT
                NEXT
            LOOP
        ELSE
            x = 0
            DO
                FOR i = 0 TO UBOUND(PrintColor)
                    FOR j = 0 TO PrintColor(i).count
                        FOR y = 0 TO fh - 1
                            ColorArray(x, y) = PrintColor(i).color
                        NEXT
                        x = x + 1: IF x > fw - 1 THEN EXIT DO
                    NEXT
                NEXT
            LOOP
        END IF
    CASE 2 'texture/image
        ni = _NEWIMAGE(fw, fh, 32)
        _PUTIMAGE , Direction, ni
        S = _SOURCE: d = _DEST
        _SOURCE ni
        FOR x = 0 TO fw - 1
            FOR y = 0 TO fh - 1
                ColorArray(x, y) = POINT(x, y)
            NEXT
        NEXT
        _SOURCE S: _DEST d
        _FREEIMAGE ni
    CASE 3 'pixel by pixel
        IF Direction = 1 THEN
            C = 0: i = 0
            FOR x = 0 TO fw - 1
                FOR y = 0 TO fh - 1
                    ColorArray(x, y) = PrintColor(i).color
                    C = C + 1
                    IF C > PrintColor(i).count THEN
                        C = 0
                        i = i + 1
                        IF i > UBOUND(printcolor) THEN i = 0
                    END IF
                NEXT
            NEXT
        ELSE
            C = 0: i = 0
            FOR y = 0 TO fh - 1
                FOR x = 0 TO fw - 1
                    ColorArray(x, y) = PrintColor(i).color
                    C = C + 1
                    IF C > PrintColor(i).count THEN
                        C = 0
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

fontnumber = CurrentFont
SELECT CASE fontnumber
    CASE 1: fw = UBOUND(Alpha1, 2): fh = UBOUND(alpha1, 3)
    CASE 2: fw = UBOUND(Alpha2, 2): fh = UBOUND(alpha2, 3)
    CASE 3: fw = UBOUND(Alpha3, 2): fh = UBOUND(alpha3, 3)
    CASE 4: fw = UBOUND(Alpha4, 2): fh = UBOUND(alpha4, 3)
    CASE 5: fw = UBOUND(Alpha5, 2): fh = UBOUND(alpha5, 3)
END SELECT
'clear the old color array if our sizes don't match and set the new one
IF UBOUND(ColorArray, 1) <> fw AND UBOUND(colorarray, 2) <> fh THEN REDIM ColorArray(fw, fh)
REDIM BGColorArray(fw, fh)

SELECT CASE mode
    CASE 0
        FOR x = 0 TO fw - 1
            FOR y = 0 TO fh - 1
                BGColorArray(x, y) = PrintColor(0).color 'only one color, so we use it for a solid color
            NEXT
        NEXT
    CASE 1
        IF Direction = 1 THEN
            y = 0
            DO
                FOR i = 0 TO UBOUND(PrintColor)
                    FOR j = 0 TO PrintColor(i).count
                        FOR x = 0 TO fw - 1
                            BGColorArray(x, y) = PrintColor(i).color
                        NEXT
                        y = y + 1: IF y > fh - 1 THEN EXIT DO
                    NEXT
                NEXT
            LOOP
        ELSE
            x = 0
            DO
                FOR i = 0 TO UBOUND(PrintColor)
                    FOR j = 0 TO PrintColor(i).count
                        FOR y = 0 TO fh - 1
                            BGColorArray(x, y) = PrintColor(i).color
                        NEXT
                        x = x + 1: IF x > fw - 1 THEN EXIT DO
                    NEXT
                NEXT
            LOOP
        END IF
    CASE 2
        ni = _NEWIMAGE(fw, fh, 32)
        _PUTIMAGE , Direction, ni
        S = _SOURCE: D = _DEST
        _SOURCE ni
        FOR x = 0 TO fw - 1
            FOR y = 0 TO fh - 1
                BGColorArray(x, y) = POINT(x, y)
            NEXT
        NEXT
        _SOURCE S: _DEST D
        _FREEIMAGE ni
    CASE 3 'pixel by pixel
        IF Direction = 1 THEN
            C = 0: i = 0
            FOR x = 0 TO fw - 1
                FOR y = 0 TO fh - 1
                    BGColorArray(x, y) = PrintColor(i).color
                    C = C + 1
                    IF C > PrintColor(i).count THEN
                        C = 0
                        i = i + 1
                        IF i > UBOUND(printcolor) THEN i = 0
                    END IF
                NEXT
            NEXT
        ELSE
            C = 0: i = 0
            FOR y = 0 TO fh - 1
                FOR x = 0 TO fw - 1
                    BGColorArray(x, y) = PrintColor(i).color
                    C = C + 1
                    IF C > PrintColor(i).count THEN
                        C = 0
                        i = i + 1
                        IF i > UBOUND(printcolor) THEN i = 0
                    END IF
                NEXT
            NEXT
        END IF
END SELECT
END SUB

SUB ScrollUp
DIM m AS _MEM
m = _MEMIMAGE(0)
w = UBOUND(Colorarray, 2) * _WIDTH * 4
t$ = SPACE$(m.SIZE - w)
_MEMGET m, m.OFFSET + w, t$
CLS
_MEMPUT m, m.OFFSET, t$
_MEMFREE m
END SUB

