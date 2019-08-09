SCREEN _NEWIMAGE(1280, 720, 256)
RANDOMIZE TIMER

DIM m AS _MEM
DIM x(5) AS _BYTE
DIM y(7) AS DOUBLE
DIM z(5) AS STRING * 5

'Let's see if we can sort the integer array
'Initialize Data
FOR i = 0 TO 5: x(i) = RND * 100: PRINT x(i),: NEXT: PRINT
PRINT "----------------------------------------------------------------------------------------------------------------------------------------------------------------"

'Sort
m = _MEM(x())
Sort m
_MEMFREE m

'Result
FOR i = 0 TO 5: PRINT x(i),: NEXT: PRINT

PRINT "----------------------------------------------------------------------------------------------------------------------------------------------------------------"
PRINT
PRINT


'Try the same routine with a different data type array to sort
'Initialize Data
FOR i = 0 TO 7: y(i) = RND * 100: PRINT y(i),: NEXT
PRINT
PRINT "----------------------------------------------------------------------------------------------------------------------------------------------------------------"

'Sort
m = _MEM(y())
Sort m
_MEMFREE m

'Result
FOR i = 0 TO 7: PRINT y(i),: NEXT: PRINT
PRINT "----------------------------------------------------------------------------------------------------------------------------------------------------------------"
PRINT
PRINT


'To test with fixed length string arrays
z(0) = "Doggy": z(1) = "Pudding": z(2) = "Frog ": z(3) = "test2": z(4) = "Test2": z(5) = "test1"
FOR i = 0 TO 5: PRINT z(i),: NEXT: PRINT
PRINT "----------------------------------------------------------------------------------------------------------------------------------------------------------------"

m = _MEM(z())
Sort m
_MEMFREE m

'Result
FOR i = 0 TO 5: PRINT z(i),: NEXT: PRINT

SLEEP
SYSTEM



SUB Sort (m AS _MEM)
    DIM i AS _UNSIGNED LONG
    $IF 64BIT THEN
        DIM ES AS _INTEGER64, EC AS _INTEGER64
    $ELSE
        DIM ES AS LONG, EC AS LONG
    $END IF

    IF NOT m.TYPE AND 65536 THEN EXIT SUB 'We won't work without an array
    IF m.TYPE AND 1024 THEN DataType = 10
    IF m.TYPE AND 1 THEN DataType = DataType + 1
    IF m.TYPE AND 2 THEN DataType = DataType + 2
    IF m.TYPE AND 4 THEN IF m.TYPE AND 128 THEN DataType = DataType + 4 ELSE DataType = 3
    IF m.TYPE AND 8 THEN IF m.TYPE AND 128 THEN DataType = DataType + 8 ELSE DataType = 5
    IF m.TYPE AND 32 THEN DataType = 6
    IF m.TYPE AND 512 THEN DataType = 7

    'Convert our offset data over to something we can work with
    DIM m1 AS _MEM: m1 = _MEMNEW(LEN(ES))
    _MEMPUT m1, m1.OFFSET, m.ELEMENTSIZE: _MEMGET m1, m1.OFFSET, ES 'Element Size
    _MEMPUT m1, m1.OFFSET, m.SIZE: _MEMGET m1, m1.OFFSET, EC 'Element Count will temporily hold the WHOLE array size
    _MEMFREE m1

    EC = EC / ES - 1 'Now we take the whole element size / the size of the elements and get our actual element count.  We subtract 1 so our arrays start at 0 and not 1.
    'And work with it!
    DIM o AS _OFFSET, o1 AS _OFFSET, counter AS _UNSIGNED LONG

    SELECT CASE DataType
        CASE 1 'BYTE
            DIM temp1(-128 TO 127) AS _UNSIGNED LONG
            DIM t1 AS _BYTE
            i = 0
            DO
                _MEMGET m, m.OFFSET + i, t1
                temp1(t1) = temp1(t1) + 1
                i = i + 1
            LOOP UNTIL i > EC
            i1 = -128
            DO
                DO UNTIL temp1(i1) = 0
                    _MEMPUT m, m.OFFSET + counter, i1 AS _BYTE
                    counter = counter + 1
                    temp1(i1) = temp1(i1) - 1
                    IF counter > EC THEN EXIT SUB
                LOOP
                i1 = i1 + 1
            LOOP UNTIL i1 > 127
        CASE 2: 'INTEGER
            DIM temp2(-32768 TO 32767) AS _UNSIGNED LONG
            DIM t2 AS INTEGER
            i = 0
            DO
                _MEMGET m, m.OFFSET + i * 2, t2
                temp2(t2) = temp2(t2) + 1
                i = i + 1
            LOOP UNTIL i > EC
            i1 = -32768
            DO
                DO UNTIL temp2(i1) = 0
                    _MEMPUT m, m.OFFSET + counter * 2, i1 AS INTEGER
                    counter = counter + 1
                    temp2(i1) = temp2(i1) - 1
                    IF counter > EC THEN EXIT SUB
                LOOP
                i1 = i1 + 1
            LOOP UNTIL i1 > 32767
        CASE 3 'SINGLE
            DIM T3a AS SINGLE, T3b AS SINGLE
            gap = EC
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    o = m.OFFSET + i * 4
                    o1 = m.OFFSET + (i + gap) * 4
                    IF _MEMGET(m, o, SINGLE) > _MEMGET(m, o1, SINGLE) THEN
                        _MEMGET m, o1, T3a
                        _MEMGET m, o, T3b
                        _MEMPUT m, o1, T3b
                        _MEMPUT m, o, T3a
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > EC
            LOOP UNTIL gap = 1 AND swapped = 0
        CASE 4 'LONG
            DIM T4a AS LONG, T4b AS LONG
            gap = EC
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    o = m.OFFSET + i * 4
                    o1 = m.OFFSET + (i + gap) * 4
                    IF _MEMGET(m, o, LONG) > _MEMGET(m, o1, LONG) THEN
                        _MEMGET m, o1, T4a
                        _MEMGET m, o, T4b
                        _MEMPUT m, o1, T4b
                        _MEMPUT m, o, T4a
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > EC
            LOOP UNTIL gap = 1 AND swapped = 0
        CASE 5 'DOUBLE
            DIM T5a AS DOUBLE, T5b AS DOUBLE
            gap = EC
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    o = m.OFFSET + i * 8
                    o1 = m.OFFSET + (i + gap) * 8
                    IF _MEMGET(m, o, DOUBLE) > _MEMGET(m, o1, DOUBLE) THEN
                        _MEMGET m, o1, T5a
                        _MEMGET m, o, T5b
                        _MEMPUT m, o1, T5b
                        _MEMPUT m, o, T5a
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > EC
            LOOP UNTIL gap = 1 AND swapped = 0
        CASE 6 ' _FLOAT
            DIM T6a AS _FLOAT, T6b AS _FLOAT
            gap = EC
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    o = m.OFFSET + i * 32
                    o1 = m.OFFSET + (i + gap) * 32
                    IF _MEMGET(m, o, _FLOAT) > _MEMGET(m, o1, _FLOAT) THEN
                        _MEMGET m, o1, T6a
                        _MEMGET m, o, T6b
                        _MEMPUT m, o1, T6b
                        _MEMPUT m, o, T6a
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > EC
            LOOP UNTIL gap = 1 AND swapped = 0
        CASE 7 'String
            DIM T7a AS STRING, T7b AS STRING, T7c AS STRING
            T7a = SPACE$(ES): T7b = SPACE$(ES): T7c = SPACE$(ES)
            gap = EC
            DO
                gap = INT(gap / 1.247330950103979)
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    o = m.OFFSET + i * ES
                    o1 = m.OFFSET + (i + gap) * ES
                    _MEMGET m, o, T7a
                    _MEMGET m, o1, T7b
                    IF T7a > T7b THEN
                        T7c = T7b
                        _MEMPUT m, o1, T7a
                        _MEMPUT m, o, T7c
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > EC
            LOOP UNTIL gap = 1 AND swapped = false
        CASE 8 '_INTEGER64
            DIM T8a AS _INTEGER64, T8b AS _INTEGER64
            gap = EC
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    o = m.OFFSET + i * 8
                    o1 = m.OFFSET + (i + gap) * 8
                    IF _MEMGET(m, o, _INTEGER64) > _MEMGET(m, o1, _INTEGER64) THEN
                        _MEMGET m, o1, T8a
                        _MEMGET m, o, T8b
                        _MEMPUT m, o1, T8b
                        _MEMPUT m, o, T8a
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > EC
            LOOP UNTIL gap = 1 AND swapped = 0
        CASE 11: '_UNSIGNED _BYTE
            DIM temp11(0 TO 255) AS _UNSIGNED LONG
            DIM t11 AS _UNSIGNED _BYTE
            i = 0
            DO
                _MEMGET m, m.OFFSET + i, t11
                temp11(t11) = temp11(t11) + 1
                i = i + 1
            LOOP UNTIL i > EC
            i1 = 0
            DO
                DO UNTIL temp11(i1) = 0
                    _MEMPUT m, m.OFFSET + counter, i1 AS _UNSIGNED _BYTE
                    counter = counter + 1
                    temp11(i1) = temp11(i1) - 1
                    IF counter > EC THEN EXIT SUB
                LOOP
                i1 = i1 + 1
            LOOP UNTIL i1 > 255
        CASE 12 '_UNSIGNED INTEGER
            DIM temp12(0 TO 65535) AS _UNSIGNED LONG
            DIM t12 AS _UNSIGNED INTEGER
            i = 0
            DO
                _MEMGET m, m.OFFSET + i * 2, t12
                temp12(t12) = temp12(t12) + 1
                i = i + 1
            LOOP UNTIL i > EC
            i1 = 0
            DO
                DO UNTIL temp12(i1) = 0
                    _MEMPUT m, m.OFFSET + counter * 2, i1 AS _UNSIGNED INTEGER
                    counter = counter + 1
                    temp12(i1) = temp12(i1) - 1
                    IF counter > EC THEN EXIT SUB
                LOOP
                i1 = i1 + 1
            LOOP UNTIL i1 > 65535
        CASE 14 '_UNSIGNED LONG
            DIM T14a AS _UNSIGNED LONG, T14b AS _UNSIGNED LONG
            gap = EC
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    o = m.OFFSET + i * 4
                    o1 = m.OFFSET + (i + gap) * 4
                    IF _MEMGET(m, o, _UNSIGNED LONG) > _MEMGET(m, o1, _UNSIGNED LONG) THEN
                        _MEMGET m, o1, T14a
                        _MEMGET m, o, T14b
                        _MEMPUT m, o1, T14b
                        _MEMPUT m, o, T14a
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > EC
            LOOP UNTIL gap = 1 AND swapped = 0
        CASE 18: '_UNSIGNED _INTEGER64
            DIM T18a AS _UNSIGNED _INTEGER64, T18b AS _UNSIGNED _INTEGER64
            gap = EC
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    o = m.OFFSET + i * 8
                    o1 = m.OFFSET + (i + gap) * 8
                    IF _MEMGET(m, o, _UNSIGNED _INTEGER64) > _MEMGET(m, o1, _UNSIGNED _INTEGER64) THEN
                        _MEMGET m, o1, T18a
                        _MEMGET m, o, T18b
                        _MEMPUT m, o1, T18b
                        _MEMPUT m, o, T18a
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > EC
            LOOP UNTIL gap = 1 AND swapped = 0
    END SELECT
END SUB

