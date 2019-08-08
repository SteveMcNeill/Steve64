DIM math(1000)
x$ = "1+2+3-4*5*-60+2^2="

PRINT x$
i = 0: start = 1
DO
    DO
        'look for operator
        i = i + 1
        IF i > LEN(x$) THEN EXIT DO
        temp$ = MID$(x$, i, 1)
        SELECT CASE temp$
            CASE "+", "-", "*", "/", "^", "="
                EXIT DO
            CASE "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"
                'Life is good, proceed
            CASE ELSE
                BEEP: BEEP: BEEP
                'Slap someone with the beeper!   Life is Bad, invalid entry!!
                END
        END SELECT
    LOOP
    IF i > LEN(x$) THEN EXIT DO
    count = count + 1
    operator$(count) = MID$(x$, i, 1)
    IF negative THEN
        math$(count) = "-" + (MID$(x$, start, i - start))
    ELSE
        math$(count) = (MID$(x$, start, i - start))
    END IF
    IF operator$(count) = "-" AND math$(count) = "" THEN
        count = count - 1
        negative = -1
    ELSE
        negative = 0
    END IF
    math(count) = VAL(math$(count))
    start = i + 1
LOOP



FOR i = 1 TO count
    PRINT i, math(i), operator$(i)
NEXT

limit = count
DO
    'Find greatest operator
    'Any ^ powers in there?
    LFP = 0 'Looking for Power is false
    FOR j = 1 TO count
        IF operator$(j) = "^" THEN LFP = -1: EXIT FOR
    NEXT
    IF LFP THEN
        math(j) = math(j) ^ math(j + 1)
        operator$(j) = operator$(j + 1)
        count = count - 1
        FOR j1 = j + 1 TO count
            math(j1) = math(j1 + 1)
            operator$(j1) = operator$(j1 + 1)
        NEXT
    ELSE
        'No powers, so let's look for the 1st multiplication we find.
        LFMD = 0 'Looking for Muliplication/Division is false
        FOR j = 1 TO count
            IF operator$(j) = "*" OR operator$(j) = "/" THEN LFMD = -1: EXIT FOR
        NEXT
        IF LFMD THEN
            IF operator$(j) = "*" THEN
                math(j) = math(j) * math(j + 1)
            ELSE
                math(j) = math(j) / math(j + 1)
            END IF
            operator$(j) = operator$(j + 1)
            count = count - 1
            FOR j1 = j + 1 TO count
                math(j1) = math(j1 + 1)
                operator$(j1) = operator$(j1 + 1)
            NEXT
        ELSE
            'We're down to basic addition and subtraction
            LFPM = 0 'Looking for Plus/Minus is false
            FOR j = 1 TO count
                IF operator$(j) = "+" OR operator$(j) = "-" THEN LFPM = -1: EXIT FOR
            NEXT
            IF LFPM THEN
                IF operator$(j) = "+" THEN
                    math(j) = math(j) + math(j + 1)
                ELSE
                    math(j) = math(j) - math(j + 1)
                END IF
                operator$(j) = operator$(j + 1)
                count = count - 1
                FOR j1 = j + 1 TO count
                    math(j1) = math(j1 + 1)
                    operator$(j1) = operator$(j1 + 1)
                NEXT
            END IF
        END IF
    END IF
    PRINT "************************"
    FOR k = 1 TO count
        PRINT k, j, math(k), operator$(k)
    NEXT
    SLEEP

LOOP UNTIL count < 2

PRINT
PRINT "Final Result:"; x$; math(1)










