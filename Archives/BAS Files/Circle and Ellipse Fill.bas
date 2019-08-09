SCREEN _NEWIMAGE(800, 600, 32)

DIM TransRed AS _UNSIGNED LONG
DIM TransGreen AS _UNSIGNED LONG
DIM TransBlue AS _UNSIGNED LONG
TransRed = _RGBA(255, 0, 0, 128)
TransGreen = _RGBA(0, 255, 0, 128)
TransBlue = _RGBA(0, 0, 255, 128)

CALL CircleFill(100, 100, 75, TransRed)
CALL CircleFill(120, 120, 75, TransBlue)

CALL EllipseFill(550, 100, 150, 75, TransBlue)
CALL EllipseFill(570, 120, 150, 75, TransGreen)

CALL EllipseTilt(200, 400, 150, 75, 0, TransGreen)
CALL EllipseTilt(220, 420, 150, 75, 3.14 / 4, TransRed)

CALL EllipseTiltFill(0, 550, 400, 150, 75, 3.14 / 6, TransRed)
CALL EllipseTiltFill(0, 570, 420, 150, 75, 3.14 / 4, TransGreen)

END

SUB CircleFill (CX AS INTEGER, CY AS INTEGER, R AS INTEGER, C AS _UNSIGNED LONG)
    ' CX = center x coordinate
    ' CY = center y coordinate
    '  R = radius
    '  C = fill color
    DIM Radius AS INTEGER, RadiusError AS INTEGER
    DIM X AS INTEGER, Y AS INTEGER
    Radius = ABS(R)
    RadiusError = -Radius
    X = Radius
    Y = 0
    IF Radius = 0 THEN PSET (CX, CY), C: EXIT SUB
    LINE (CX - X, CY)-(CX + X, CY), C, BF
    WHILE X > Y
        RadiusError = RadiusError + Y * 2 + 1
        IF RadiusError >= 0 THEN
            IF X <> Y + 1 THEN
                LINE (CX - Y, CY - X)-(CX + Y, CY - X), C, BF
                LINE (CX - Y, CY + X)-(CX + Y, CY + X), C, BF
            END IF
            X = X - 1
            RadiusError = RadiusError - X * 2
        END IF
        Y = Y + 1
        LINE (CX - X, CY - Y)-(CX + X, CY - Y), C, BF
        LINE (CX - X, CY + Y)-(CX + X, CY + Y), C, BF
    WEND
END SUB

SUB EllipseFill (CX AS INTEGER, CY AS INTEGER, a AS INTEGER, b AS INTEGER, C AS _UNSIGNED LONG)
    ' CX = center x coordinate
    ' CY = center y coordinate
    '  a = semimajor axis
    '  b = semiminor axis
    '  C = fill color
    IF a = 0 OR b = 0 THEN EXIT SUB
    DIM h2 AS _INTEGER64
    DIM w2 AS _INTEGER64
    DIM h2w2 AS _INTEGER64
    DIM x AS INTEGER
    DIM y AS INTEGER
    w2 = a * a
    h2 = b * b
    h2w2 = h2 * w2
    LINE (CX - a, CY)-(CX + a, CY), C, BF
    DO WHILE y < b
        y = y + 1
        x = SQR((h2w2 - y * y * w2) \ h2)
        LINE (CX - x, CY + y)-(CX + x, CY + y), C, BF
        LINE (CX - x, CY - y)-(CX + x, CY - y), C, BF
    LOOP
END SUB

SUB EllipseTilt (CX, CY, a, b, ang, C AS _UNSIGNED LONG)
    '  CX = center x coordinate
    '  CY = center y coordinate
    '   a = semimajor axis
    '   b = semiminor axis
    ' ang = clockwise orientation of semimajor axis in radians (0 default)
    '   C = fill color
    FOR k = 0 TO 6.283185307179586 + .025 STEP .025
        i = a * COS(k) * COS(ang) + b * SIN(k) * SIN(ang)
        j = -a * COS(k) * SIN(ang) + b * SIN(k) * COS(ang)
        i = i + CX
        j = -j + CY
        IF k <> 0 THEN
            LINE -(i, j), C
        ELSE
            PSET (i, j), C
        END IF
    NEXT
END SUB

SUB EllipseTiltFill (destHandle&, CX, CY, a, b, ang, C AS _UNSIGNED LONG)
    '  destHandle& = destination handle
    '  CX = center x coordinate
    '  CY = center y coordinate
    '   a = semimajor axis
    '   b = semiminor axis
    ' ang = clockwise orientation of semimajor axis in radians (0 default)
    '   C = fill color
    DIM max AS INTEGER, mx2 AS INTEGER, i AS INTEGER, j AS INTEGER
    DIM prc AS _UNSIGNED LONG
    DIM D AS INTEGER, S AS INTEGER
    D = _DEST: S = _SOURCE
    prc = _RGB32(255, 255, 255, 255)
    IF a > b THEN max = a + 1 ELSE max = b + 1
    mx2 = max + max
    tef& = _NEWIMAGE(mx2, mx2)
    _DEST tef&
    _SOURCE tef&
    FOR k = 0 TO 6.283185307179586 + .025 STEP .025
        i = max + a * COS(k) * COS(ang) + b * SIN(k) * SIN(ang)
        j = max + a * COS(k) * SIN(ang) - b * SIN(k) * COS(ang)
        IF k <> 0 THEN
            LINE (lasti, lastj)-(i, j), prc
        ELSE
            PSET (i, j), prc
        END IF
        lasti = i: lastj = j
    NEXT
    DIM xleft(mx2) AS INTEGER, xright(mx2) AS INTEGER, x AS INTEGER, y AS INTEGER
    FOR y = 0 TO mx2
        x = 0
        WHILE POINT(x, y) <> prc AND x < mx2
            x = x + 1
        WEND
        xleft(y) = x
        WHILE POINT(x, y) = prc AND x < mx2
            x = x + 1
        WEND
        WHILE POINT(x, y) <> prc AND x < mx2
            x = x + 1
        WEND
        IF x = mx2 THEN xright(y) = xleft(y) ELSE xright(y) = x
    NEXT
    _DEST destHandle&
    FOR y = 0 TO mx2
        IF xleft(y) <> mx2 THEN LINE (xleft(y) + CX - max, y + CY - max)-(xright(y) + CX - max, y + CY - max), C, BF
    NEXT
    _DEST D: _DEST S
    _FREEIMAGE tef&
END SUB


