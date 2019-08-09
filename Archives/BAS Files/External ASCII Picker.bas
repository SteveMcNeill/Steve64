_TITLE "External ASCII Picker"
SCREEN _NEWIMAGE(640, 480, 32)
f = _LOADFONT("cour.ttf", 128) 'a nice large display font, not a 100% match to QB64's inbuilt version,
_FONT f '                       but it works easily here for a demo highlight character



_CONTROLCHR OFF
DO
    _CLIPBOARD$ = ASCIIbox$ 'get an ASCII character
    CLS
    PRINT _CLIPBOARD$ 'preview it
    SLEEP: _KEYCLEAR 'hit a key to select another
LOOP UNTIL RIGHT$(OUT$, 1) = CHR$(27)

FUNCTION ASCIIbox$
    STATIC temp AS LONG, temp1 AS LONG, ws AS LONG
    d = _DEST
    font = _FONT
    IF temp = 0 THEN 'static backgrounds so we don't have to make them over and over, or worry about freeing them
        temp = _NEWIMAGE(640, 480, 32)
        temp1 = _NEWIMAGE(640, 480, 32)
        ws = _NEWIMAGE(640, 480, 32)
    END IF
    SCREEN temp
    DIM CurrentASC(1 TO 16, 1 TO 16)
    DIM CurrentOne AS INTEGER
    CLS , _RGB(0, 0, 170)
    COLOR , _RGB(0, 0, 170)
    FOR y = 1 TO 16
        FOR x = 1 TO 16
            LINE (x * 40, 0)-(x * 40, 480), _RGB32(255, 255, 0)
            LINE (0, y * 30)-(640, y * 30), _RGB32(255, 255, 0)
            IF counter THEN _PRINTSTRING (x * 40 - 28, y * 30 - 23), CHR$(counter)
            counter = counter + 1
        NEXT
    NEXT

    _DEST temp1
    CLS , _RGB(0, 0, 170)
    COLOR , _RGB(0, 0, 170)
    counter = 0
    FOR y = 1 TO 16
        FOR x = 1 TO 16
            LINE (x * 40, 0)-(x * 40, 480), _RGB32(255, 255, 0)
            LINE (0, y * 30)-(640, y * 30), _RGB32(255, 255, 0)
            text$ = LTRIM$(STR$(counter))
            IF counter THEN _PRINTSTRING (x * 40 - 24 - (LEN(text$)) * 4, y * 30 - 23), text$
            counter = counter + 1
        NEXT
    NEXT
    _DEST temp

    x = 1: y = 1
    _PUTIMAGE , temp, ws
    DO: LOOP WHILE _MOUSEINPUT 'clear the mouse input buffer
    oldmousex = _MOUSEX: oldmousey = _MOUSEY

    DO
        _LIMIT 60
        DO: LOOP WHILE _MOUSEINPUT
        IF oldx <> _MOUSEX AND oldy <> _MOUSEY THEN
            x = _MOUSEX \ 40 + 1 'If mouse moved, where are we now?
            y = _MOUSEY \ 30 + 1
        END IF
        oldx = _MOUSEX: oldy = _MOUSEY

        num = (y - 1) * 16 + x - 1
        IF num = 0 THEN
            text$ = ""
        ELSE
            flashcounter = flashcounter + 1
            IF flashcounter > 30 THEN
                COLOR _RGB32(255, 255, 255), _RGB(0, 0, 170)
                text$ = CHR$(num)
                IF LEN(text$) = 1 THEN text$ = " " + text$ + " "
            ELSE
                COLOR _RGB32(255, 255, 255), _RGB(0, 0, 170)
                text$ = RTRIM$(LTRIM$(STR$(num)))
            END IF
        END IF
        IF flashcounter = 60 THEN flashcounter = 1
        CLS
        IF toggle THEN _PUTIMAGE , temp1, temp ELSE _PUTIMAGE , ws, temp
        _PRINTSTRING (x * 40 - 24 - (LEN(text$)) * 4, y * 30 - 23), text$
        LINE (x * 40 - 40, y * 30 - 30)-(x * 40, y * 30), _RGBA32(255, 255, 255, 150), BF

        k1 = _KEYHIT
        MouseClick = 0: MouseExit = 0
        IF MouseButtonSwapped THEN
            MouseClick = _MOUSEBUTTON(2): MouseExit = _MOUSEBUTTON(1)
        ELSE
            MouseClick = _MOUSEBUTTON(1): MouseExit = _MOUSEBUTTON(2)
        END IF
        SELECT CASE k1
            CASE 13: EXIT DO
            CASE 27
                GOTO cleanexit
            CASE 32: toggle = NOT toggle
            CASE 18432: y = y - 1
            CASE 19200: x = x - 1
            CASE 20480: y = y + 1
            CASE 19712: x = x + 1
        END SELECT

        IF x < 1 THEN x = 1
        IF x > 16 THEN x = 16
        IF y < 1 THEN y = 1
        IF y > 16 THEN y = 16
        _DISPLAY
        IF MouseExit GOTO cleanexit
    LOOP UNTIL MouseClick

    ret% = (y - 1) * 16 + x - 1
    IF ret% > 0 AND ret% < 255 THEN
        ASCIIbox$ = CHR$(ret%)
    END IF
    cleanexit:
    _AUTODISPLAY

    SCREEN d
    _FONT font
    _DEST 0: _DELAY .2

END FUNCTION


