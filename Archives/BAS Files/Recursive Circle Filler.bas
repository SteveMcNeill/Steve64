SCREEN _NEWIMAGE(640, 480, 32)

CONST Red = &HFFFF0000

LINE (200, 200)-(400, 400), Red, B
CircleFiller 300, 300, 10, Red

SLEEP
CLS , 0
CIRCLE (320, 240), 100, Red
CircleFiller 320, 240, 10, Red


SUB CircleFiller (x, y, r, k AS _UNSIGNED LONG)
    IF CircleFillValid(x, y, r, k) THEN
        CircleFill x, y, r, k
        CircleFiller x - r - r - 1, y, r, k
        CircleFiller x + r + r + 1, y, r, k
        CircleFiller x, y - r - r - 1, r, k
        CircleFiller x, y + r + r + 1, r, k
    END IF
END SUB






SUB CircleFill (cx AS INTEGER, cy AS INTEGER, r AS INTEGER, c AS _UNSIGNED LONG)
    DIM a AS LONG, b AS LONG
    DIM x AS LONG, y AS LONG
    DIM xx AS LONG, yy AS LONG
    DIM sx AS LONG, sy AS LONG
    DIM e AS LONG
    DIM rx AS INTEGER, ry AS INTEGER
    rx = r: ry = r

    a = 2 * rx * rx
    b = 2 * ry * ry
    x = rx
    xx = ry * ry * (1 - rx - rx)
    yy = rx * rx
    sx = b * rx

    DO WHILE sx >= sy
        LINE (cx - x, cy - y)-(cx + x, cy - y), c, BF
        IF y <> 0 THEN LINE (cx - x, cy + y)-(cx + x, cy + y), c, BF

        y = y + 1
        sy = sy + a
        e = e + yy
        yy = yy + a

        IF (e + e + xx) > 0 THEN
            x = x - 1
            sx = sx - b
            e = e + xx
            xx = xx + b
        END IF
    LOOP

    x = 0
    y = ry
    xx = rx * ry
    yy = rx * rx * (1 - ry - ry)
    e = 0
    sx = 0
    sy = a * ry

    DO WHILE sx <= sy
        LINE (cx - x, cy - y)-(cx + x, cy - y), c, BF
        LINE (cx - x, cy + y)-(cx + x, cy + y), c, BF

        DO
            x = x + 1
            sx = sx + b
            e = e + xx
            xx = xx + b
        LOOP UNTIL (e + e + yy) > 0

        y = y - 1
        sy = sy - a
        e = e + yy
        yy = yy + a

    LOOP

END SUB


FUNCTION CircleFillValid (cx AS INTEGER, cy AS INTEGER, r AS INTEGER, c AS _UNSIGNED LONG)
    DIM a AS LONG, b AS LONG
    DIM x AS LONG, y AS LONG
    DIM xx AS LONG, yy AS LONG
    DIM sx AS LONG, sy AS LONG
    DIM e AS LONG
    DIM rx AS INTEGER, ry AS INTEGER
    rx = r: ry = r

    a = 2 * rx * rx
    b = 2 * ry * ry
    x = rx
    xx = ry * ry * (1 - rx - rx)
    yy = rx * rx
    sx = b * rx

    DO WHILE sx >= sy
        FOR i = cx - x TO cx + x
            IF POINT(i, cy - y) = c THEN EXIT FUNCTION
        NEXT
        'LINE (cx - x, cy - y)-(cx + x, cy - y), c, BF
        IF y <> 0 THEN
            'LINE (cx - x, cy + y)-(cx + x, cy + y), c, BF
            FOR i = cx - x TO cx + x
                IF POINT(i, cy + y) = c THEN EXIT FUNCTION
            NEXT
        END IF

        y = y + 1
        sy = sy + a
        e = e + yy
        yy = yy + a

        IF (e + e + xx) > 0 THEN
            x = x - 1
            sx = sx - b
            e = e + xx
            xx = xx + b
        END IF
    LOOP

    x = 0
    y = ry
    xx = rx * ry
    yy = rx * rx * (1 - ry - ry)
    e = 0
    sx = 0
    sy = a * ry

    DO WHILE sx <= sy
        'LINE (cx - x, cy - y)-(cx + x, cy - y), c, BF
        'LINE (cx - x, cy + y)-(cx + x, cy + y), c, BF
        FOR i = cx - x TO cx + x
            IF POINT(i, cy - y) = c THEN EXIT FUNCTION
            IF POINT(i, cy + y) = c THEN EXIT FUNCTION
        NEXT

        DO
            x = x + 1
            sx = sx + b
            e = e + xx
            xx = xx + b
        LOOP UNTIL (e + e + yy) > 0

        y = y - 1
        sy = sy - a
        e = e + yy
        yy = yy + a

    LOOP
    CircleFillValid = -1
END SUB


