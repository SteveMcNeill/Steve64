CONST limit = 16

DIM test(limit) AS STRING

DATA "123a.3","-123.456","--234","1.23E15","123","dogfood","678.965","54678","-987134","1E15"
DATA "&HFF","&B1001111","&O17","&HFF&&","&B12000222","1.E-12"

FOR i = 1 TO limit
    READ test(i)
NEXT


FOR i = 1 TO limit
    PRINT "TEST #"; i; ": "; test(i) + " "
    result = NumType(test(i))
    IF result = 0 THEN PRINT "INVALID: "; NumErr$
    IF result AND 1 THEN PRINT "Valid Unsigned Bit.  ";
    IF result AND 2 THEN PRINT "Valid Unsigned Byte.  ";
    IF result AND 4 THEN PRINT "Valid Unsigned Integer.  ";
    IF result AND 8 THEN PRINT "Valid Unsigned Long.  ";
    IF result AND 16 THEN PRINT "Valid Unsigned Integer64.  ";
    IF result AND 32 THEN PRINT "Valid Unsigned Bit.  ";
    IF result AND 64 THEN PRINT "Valid Signed Byte.  ";
    IF result AND 128 THEN PRINT "Valid Signed Integer.  ";
    IF result AND 256 THEN PRINT "Valid Signed Long.  ";
    IF result AND 512 THEN PRINT "Valid Signed Integer64.  ";
    IF result AND 1024 THEN PRINT "Valid Single.  ";
    IF result AND 2048 THEN PRINT "Valid Double.  ";
    IF result AND 4096 THEN PRINT "Valid Float.  ";
    IF result AND 8192 THEN PRINT "Valid Unsigned Offset.  ";
    IF result AND 16384 THEN PRINT "Valid Signed Offset.  ";
    PRINT
    PRINT
    SLEEP
NEXT

FUNCTION NumType~% (text$)
    SHARED NumErr$
    temp$ = UCASE$(_TRIM$(text$))
    NumErr$ = "": NumType = 0

    'First look for manually assigned types
    r1$ = RIGHT$(temp$, 1): r = 1
    r2$ = LEFT$(RIGHT$(temp$, 2), 1)
    SELECT CASE r1$
        CASE "`"
            TestFor = 1 'bit
        CASE "%"
            IF r2$ = "%" THEN
                r = 2
                TestFor = 2 'byte
            ELSE
                TestFor = 3 'integer
            END IF
        CASE "&" 'long, int64, offset
            IF r2$ = "&" THEN
                r = 2
                TestFor = 5 'int64
            ELSEIF r2$ = "%" THEN
                r = 2
                TestFor = 9 'offset
            ELSE
                TestFor = 4 'long
            END IF
        CASE "!" 'single
            TestFor = 6
        CASE "#" 'double, float
            IF r2$ = "#" THEN
                r = 2
                TestFor = 8 'float
            ELSE
                TestFor = 7 'double
            END IF
        CASE ELSE 'there's no set type
            TestFor = 0
            r = 0
    END SELECT


    temp$ = LEFT$(temp$, LEN(temp$) - r) 'strip off the type symbol
    SELECT CASE TestFor
        CASE 1 TO 5, 9
            r$ = RIGHT$(temp$, 1)
            IF r$ = "~" THEN Unsigned = -1: temp$ = LEFT$(temp$, LEN(temp$) - 1)
    END SELECT

    'check for valid prefixes

    l$ = LEFT$(temp$, 2)
    SELECT CASE l$
        CASE "&H"
            temp$ = MID$(temp$, 3)
            FOR i = 1 TO LEN(temp$)
                t$ = MID$(temp$, i, 1)
                SELECT CASE t$
                    CASE "0" TO "9", "A" TO "F" 'valid
                    CASE ELSE
                        NumErr$ = NumErr$ + "Invalid Character (" + t$ + ") encountered.  "
                END SELECT
            NEXT
            IF NumErr$ <> "" THEN EXIT FUNCTION
            GOTO evaluateintegers
        CASE "&B"
            temp$ = MID$(temp$, 3)
            FOR i = 1 TO LEN(temp$)
                t$ = MID$(temp$, i, 1)
                SELECT CASE t$
                    CASE "0", "1" 'only valid bit characters
                    CASE ELSE
                        NumErr$ = NumErr$ + "Invalid Character (" + t$ + ") encountered.  "
                END SELECT
            NEXT
            IF NumErr$ <> "" THEN EXIT FUNCTION
            GOTO evaluateintegers
        CASE "&O"
            temp$ = MID$(temp$, 3)
            FOR i = 1 TO LEN(temp$)
                t$ = MID$(temp$, i, 1)
                SELECT CASE t$
                    CASE "0" TO "7" 'only valid oct characters
                    CASE ELSE
                        NumErr$ = NumErr$ + "Invalid Character (" + t$ + ") encountered.  "
                END SELECT
            NEXT
            IF NumErr$ <> "" THEN EXIT FUNCTION
            GOTO evaluateintegers
    END SELECT


    'Test for easy integers
    'First check for positive/negative values; flag for invalid cases of multiple negation.
    IF MID$(temp$, 1, 1) = "-" THEN
        negative = -1: temp$ = MID$(temp$, 2) 'strip off the initial negative
    ELSEIF MID$(temp$, 1, 1) = "+" THEN
        temp$ = MID$(temp$, 2) 'strip off the initial positive
    END IF

    FOR i = 1 TO LEN(temp$)
        IF MID$(temp$, i, 1) = "-" THEN minus = minus + 1
        IF MID$(temp$, i, 1) = "+" THEN plus = plus + 1
        IF MID$(temp$, i, 1) = "." THEN period = period + 1 'Go ahead and check for multiple periods while we're at it.
        IF MID$(temp$, i, 1) = "E" OR MID$(temp$, i, 1) = "D" THEN
            Exponent = Exponent + 1
            IF MID$(temp$, i + 1, 1) = "-" OR MID$(temp$, i + 1, 1) = "+1" THEN ExponentSign = -1
        END IF
    NEXT

    IF period = 0 AND Exponent = 0 THEN 'we should only have integers to process
        FOR i = 1 TO LEN(temp$)
            t$ = MID$(temp$, i, 1)
            IF t$ < "0" OR t$ > "9" THEN NumErr$ = NumErr$ + "Invalid Character (" + t$ + ") encountered.  ": EXIT FUNCTION
        NEXT
        GOTO evaluateintegers
    END IF

    'At this point forward, we should only have REAL numbers to process

    IF Exponent > 1 THEN NumErr$ = NumErr$ + "Multiple E/D exponent characters in string.  ": EXIT FUNCTION

    IF ExponentSign = 0 THEN
        IF minus THEN NumErr$ = NumErr$ + "Multiple negative signs (-) encountered.  ": EXIT FUNCTION
        IF plus THEN NumErr$ = NumErr$ + "Multiple negative signs (-) encountered.  ": EXIT FUNCTION
    ELSE
        IF minus > 1 THEN NumErr$ = NumErr$ + "Multiple negative signs (-) encountered.  ": EXIT FUNCTION
        IF plus > 1 THEN NumErr$ = NumErr$ + "Multiple negative signs (-) encountered.  ": EXIT FUNCTION
    END IF

    IF period > 1 THEN NumErr$ = NumErr$ + "Multiple decimal points (.) encountered.  ": EXIT FUNCTION

    IF Exponent AND period THEN
        e = INSTR(temp$, "E")
        IF e = 0 THEN e = INSTR(temp$, "D")
        p = INSTR(temp$, ".")
        IF p > e THEN NumErr$ = NumErr$ + "Decimal points (.) AFTER E/D exponent encountered.  ": EXIT FUNCTION
    END IF


    FOR i = 1 TO LEN(temp$)
        t$ = MID$(temp$, i, 1)
        SELECT CASE t$
            CASE "0" TO "9", "-", "+", ".", "D", "E" 'we should have validated all these characters earlier
            CASE ELSE 'so anything else is invalid
                NumErr$ = NumErr$ + "Invalid Character (" + t$ + ") encountered.  ": EXIT FUNCTION
        END SELECT
    NEXT

    IF NumErr$ <> "" THEN EXIT FUNCTION


    'We should've passed all the error checking by this point -- I think...


    evaluateintegers:
    t## = VAL(text$)

    'first compare for all types
    IF INT(t##) = t## THEN
        IF t## = -1 OR t## = 0 THEN NumType = NumType OR 32 'signed bit
        IF t## >= -128 AND t## <= 127 THEN NumType = NumType OR 64 'signed byte
        IF t## >= -32768 AND t## <= 32767 THEN NumType = NumType OR 128 'signed integer
        IF t## >= -2147483648 AND t## <= 2147483647 THEN NumType = NumType OR 256 'signed long
        IF t## >= -9223372036854775808 AND t## <= 9223372036854775807 THEN
            NumType = NumType OR 512 'signed integer64
            NumType = NumType OR 16384 'signed offset
        END IF
        IF t## = 1 OR t## = 0 THEN NumType = NumType OR 1 'unsigned bit
        IF t## >= 0 AND t## <= 255 THEN NumType = NumType OR 2 'unsigned byte
        IF t## >= 0 AND t## <= 65535 THEN NumType = NumType OR 4 'unsigned integer
        IF t## >= 0 AND t## <= 4294967295 THEN NumType = NumType OR 8 'unsigned long
        IF t## >= 0 AND t## <= 18446744073709551615 THEN
            NumType = NumType OR 16 'unsigned integer64
            NumType = NumType OR 8192 'unsigned offset
        END IF
    END IF

    IF t## >= -2.802597D45 AND t## <= 3.402823D+38 THEN
        NumType = NumType OR 1024 'single
    END IF
    IF t## >= -4.490656458412465E324 AND t## <= 1.797693134862310E+308 THEN NumType = NumType OR 2048 'double
    IF t## >= -1.18E4932 AND t## <= 1.18E+4932 THEN NumType = NumType OR 4096 'float

    IF r THEN 'we have specific suffix; only decide if the value is valid for it
        NumType = 0
        IF NOT Unsigned THEN 'unsigned
            SELECT CASE TestFor
                CASE 1
                    IF t## = -1 OR t## = 0 THEN NumType = 32 'signed bit
                CASE 2
                    IF t## >= -128 AND t## <= 127 THEN NumType = 64 'signed byte
                CASE 3
                    IF t## >= -32768 AND t## <= 32767 THEN NumType = 128 'signed integer
                CASE 4
                    IF t## >= -2147483648 AND t## <= 2147483647 THEN NumType = 256 'signed long
                CASE 5, 9
                    IF t## >= -9223372036854775808 AND t## <= 9223372036854775807 THEN
                        IF TestFor = 5 THEN
                            NumType = 512 'signed integer64
                        ELSE
                            NumType = 16384 'signed offset
                        END IF
                    END IF
                CASE 6
                    IF t## >= -2.802597E-45 AND t## <= 3.402823E+38 THEN NumType = 1024 'single
                CASE 7
                    IF t## >= -4.490656458412465E-324 AND t## <= 1.797693134862310E+308 THEN NumType = 2048 'double
                CASE 9
                    IF t## >= -1.18E-4932 AND t## <= 1.18E+4932 THEN NumType = 4096 'float
            END SELECT
        ELSE
            SELECT CASE TestFor
                CASE 1
                    IF t## = 0 OR t## = 1 THEN NumType = 1 'unsigned bit
                CASE 2
                    IF t## >= 0 AND t## <= 255 THEN NumType = 2 'unsigned byte
                CASE 3
                    IF t## >= 0 AND t## <= 65535 THEN NumType = 4 'unsigned integer
                CASE 4
                    IF t## >= 0 AND t## <= 4294967295 THEN NumType = 8 'unsigned long
                CASE 5, 9
                    IF t## >= 0 AND t## <= 18446744073709551615 THEN
                        IF TestFor = 5 THEN
                            NumType = 16 'unsigned integer64
                        ELSE
                            NumType = 8192 'unsigned offset
                        END IF
                    END IF
            END SELECT
        END IF
        IF NumType = 0 THEN NumErr$ = "Invalid Suffix.  "
    END IF
END FUNCTION


