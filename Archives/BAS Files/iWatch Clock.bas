SCREEN _NEWIMAGE(320, 240, 32)
DIM Red AS _UNSIGNED LONG, Blue AS _UNSIGNED LONG, White AS _UNSIGNED LONG


X = 160: Y = 120
Red = _RGB32(255, 0, 0)
White = -1
Blue = _RGB32(0, 0, 255)

DO
    _LIMIT 100
    CLS
    T# = TIMER: t$ = " " + TIME$ + " "
    h = T# \ 3600
    m = (T# - h * 3600) \ 60
    s# = (T# - h * 3600 - m * 60) * 6
    ArcCircle 100, Red, h * 30
    ArcCircle 75, White, m * 6
    ArcCircle 50, Blue, s#
    CircleText t$, 16, _RGB32(255, 255, 0), 0, 160, 120, 18, _RGB32(255, 255, 0), 90
    _DISPLAY
LOOP UNTIL INKEY$ <> ""





SUB ArcCircle (r AS INTEGER, c AS _UNSIGNED LONG, degree)
    ti& = _NEWIMAGE(320, 240, 32)
    _DEST ti&

    CIRCLE (160, 120), r, c 'A circle all the way around the area.
    LINE (160, 120)-(160, 120 - r), c 'The line going straight up to where the 12 would be.

    rad = 0.0174532925 * (degree - 90)
    x2 = 160 + r * COS(rad)
    y2 = 120 + r * SIN(rad)
    LINE (160, 120)-(x2, y2), c 'The line to where our current hour/minute/second would be

    rad = 271 * 0.0174532925
    x1 = 160 + (r - 1) * COS(rad)
    y1 = 120 + (r - 1) * SIN(rad)
    PAINT (x1, y1), c 'Fill in the area inside that circle with our paint fill color

    CIRCLE (160, 120), r, _RGB32(0, 0, 0) 'Remove the outer line so that we get rid of the unused circle (we only filled part of the pie.)

    IF c <> _RGB32(0, 0, 255) THEN 'If our color isn't blue, then fill in the center with a nice black.
        CIRCLE (160, 120), r - 25, _RGB32(0, 0, 0)
    ELSE
        CIRCLE (160, 120), r - 33, _RGB32(0, 0, 0)
    END IF
    PAINT (160, 120), _RGB32(0, 0, 0)
    _SETALPHA 0, _RGB32(0, 0, 0) 'Turn our black transparent so we overlay other images easily.
    _DEST 0
    _PUTIMAGE , ti& 'Put this are on the display screen
    _FREEIMAGE ti& 'Clear the image from memory so we don't have a memory leak.
END SUB

SUB CircleText (text AS STRING, fonthandle AS LONG, fontcolor AS _UNSIGNED LONG, fontbackgroundcolor AS LONG, X1 AS INTEGER, Y1 AS INTEGER, Size AS INTEGER, CircleColor AS _UNSIGNED LONG, Startangle AS INTEGER)
    'text is the text we want to make into a circle
    'fonthandle is the handle of the font we want to use
    'fontcolor is the color of the font we want
    'fontbackground is the background color of the font we want
    'x1,y1 is where we want to center our text circle
    'Size is how big we want we center of our circle to be.
    'CircleColor is whatever color we might want to make our circle into
    'StartAngle is where we want our text to start printing from, as it wraps around in our circle

    t& = TextToImage(text, fonthandle, _RGB32(255, 255, 255), 0, 1)
    w = _WIDTH(t&)
    h = _HEIGHT(t&)
    scale = 360 / w
    ScaleImage t&, scale, scale * .5
    w = _WIDTH(t&)
    h = _HEIGHT(t&)
    S = _SOURCE

    CIRCLE (X1, Y1), Size, CircleColor
    PAINT (X1, Y1), CircleColor
    _SOURCE t&

    FOR x = 0 TO w - 1 STEP .5
        FOR y = 0 TO h - 1 STEP .5
            IF POINT(x, y) = _RGB32(255, 255, 255) THEN
                angle = x - Startangle
                radius = Size + h - y
                Xset = radius * COS(angle * 3.141592654 / 180) + X1
                Yset = radius * SIN(angle * 3.141592654 / 180) + Y1
                PSET (Xset, Yset), fontcolor
            ELSE
                angle = x - Startangle
                radius = Size + h - y
                Xset = radius * COS(angle * 3.141592654 / 180) + X1
                Yset = radius * SIN(angle * 3.141592654 / 180) + Y1
                PSET (Xset, Yset), fontbackgroundcolor

            END IF
        NEXT
    NEXT
    _FREEIMAGE t&
    _SOURCE S
END SUB




FUNCTION TextToImage& (text$, font&, fc&, bfc&, mode AS _BYTE)
    'text$ is the text that we wish to transform into an image.
    'font& is the handle of the font we want to use.
    'fc& is the color of the font we want to use.
    'bfc& is the background color of the font.

    'Mode 1 is print forwards
    'Mode 2 is print backwards
    'Mode 3 is print from top to bottom
    'Mode 4 is print from bottom up
    'Mode 0 got lost somewhere, but it's OK.  We check to see if our mode is < 1 or > 4 and compensate automatically if it is to make it one (default).

    IF mode < 1 OR mode > 4 THEN mode = 1
    dc& = _DEFAULTCOLOR: bgc& = _BACKGROUNDCOLOR
    IF font& <> 0 THEN _FONT font&
    IF mode < 3 THEN
        'print the text lengthwise
        w& = _PRINTWIDTH(text$): h& = _FONTHEIGHT
    ELSE
        'print the text vertically
        FOR i = 1 TO LEN(text$)
            IF w& < _PRINTWIDTH(MID$(text$, i, 1)) THEN w& = _PRINTWIDTH(MID$(text$, i, 1))
        NEXT
        h& = _FONTHEIGHT * (LEN(text$))
    END IF
    TextToImage& = _NEWIMAGE(w&, h&, 32)
    _DEST TextToImage&
    IF font& <> 0 THEN _FONT font&
    COLOR fc&, bfc&

    SELECT CASE mode
        CASE 1
            'Print text forward
            _PRINTSTRING (0, 0), text$
        CASE 2
            'Print text backwards
            temp$ = ""
            FOR i = 0 TO LEN(text$) - 1
                temp$ = temp$ + MID$(text$, LEN(text$) - i, 1)
            NEXT
            _PRINTSTRING (0, 0), temp$
        CASE 3
            'Print text upwards
            'first lets reverse the text, so it's easy to place
            temp$ = ""
            FOR i = 0 TO LEN(text$) - 1
                temp$ = temp$ + MID$(text$, LEN(text$) - i, 1)
            NEXT
            'then put it where it belongs
            FOR i = 1 TO LEN(text$)
                fx = (w& - _PRINTWIDTH(MID$(temp$, i, 1))) / 2 + .99 'This is to center any non-monospaced letters so they look better
                _PRINTSTRING (fx, _FONTHEIGHT * (i - 1)), MID$(temp$, i, 1)
            NEXT
        CASE 4
            'Print text downwards
            FOR i = 1 TO LEN(text$)
                fx = (w& - _PRINTWIDTH(MID$(text$, i, 1))) / 2 + .99 'This is to center any non-monospaced letters so they look better
                _PRINTSTRING (fx, _FONTHEIGHT * (i - 1)), MID$(text$, i, 1)
            NEXT
            _DISPLAY
    END SELECT
    _DEST 0
    COLOR dc&, bgc&
    _AUTODISPLAY
END FUNCTION



SUB ScaleImage (Image AS LONG, xscale AS SINGLE, yscale AS SINGLE)
    w = _WIDTH(Image): h = _HEIGHT(Image)
    w2 = w * xscale: h2 = h * yscale
    NewImage& = _NEWIMAGE(w2, h2, 32)
    _PUTIMAGE , Image&, NewImage&
    _FREEIMAGE Image&
    Image& = NewImage&
END SUB



SUB DisplayImage (Image AS LONG, x AS INTEGER, y AS INTEGER, angle AS SINGLE, mode AS _BYTE)
    'Image is the image handle which we use to reference our image.
    'x,y is the X/Y coordinates where we want the image to be at on the screen.
    'angle is the angle which we wish to rotate the image.
    'mode determines HOW we place the image at point X,Y.
    'Mode 0 we center the image at point X,Y
    'Mode 1 we place the Top Left corner of oour image at point X,Y
    'Mode 2 is Bottom Left
    'Mode 3 is Top Right
    'Mode 4 is Bottom Right


    DIM px(3) AS INTEGER, py(3) AS INTEGER, w AS INTEGER, h AS INTEGER
    DIM sinr AS SINGLE, cosr AS SINGLE, i AS _BYTE
    w = _WIDTH(Image): h = _HEIGHT(Image)
    SELECT CASE mode
        CASE 0 'center
            px(0) = -w \ 2: py(0) = -h \ 2: px(3) = w \ 2: py(3) = -h \ 2
            px(1) = -w \ 2: py(1) = h \ 2: px(2) = w \ 2: py(2) = h \ 2
        CASE 1 'top left
            px(0) = 0: py(0) = 0: px(3) = w: py(3) = 0
            px(1) = 0: py(1) = h: px(2) = w: py(2) = h
        CASE 2 'bottom left
            px(0) = 0: py(0) = -h: px(3) = w: py(3) = -h
            px(1) = 0: py(1) = 0: px(2) = w: py(2) = 0
        CASE 3 'top right
            px(0) = -w: py(0) = 0: px(3) = 0: py(3) = 0
            px(1) = -w: py(1) = h: px(2) = 0: py(2) = h
        CASE 4 'bottom right
            px(0) = -w: py(0) = -h: px(3) = 0: py(3) = -h
            px(1) = -w: py(1) = 0: px(2) = 0: py(2) = 0
    END SELECT
    sinr = SIN(angle / 57.2957795131): cosr = COS(angle / 57.2957795131)
    FOR i = 0 TO 3
        x2 = (px(i) * cosr + sinr * py(i)) + x: y2 = (py(i) * cosr - px(i) * sinr) + y
        px(i) = x2: py(i) = y2
    NEXT
    _MAPTRIANGLE (0, 0)-(0, h - 1)-(w - 1, h - 1), Image TO(px(0), py(0))-(px(1), py(1))-(px(2), py(2))
    _MAPTRIANGLE (0, 0)-(w - 1, 0)-(w - 1, h - 1), Image TO(px(0), py(0))-(px(3), py(3))-(px(2), py(2))
END SUB

