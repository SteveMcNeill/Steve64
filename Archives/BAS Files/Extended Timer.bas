SHELL "https://www.epochconverter.com/"
DO
    CLS
    PRINT TIMER, INT(ExtendedTimer)
    PRINT "Compare to the time at https://www.epochconverter.com/"
    _DISPLAY
    _LIMIT 10
LOOP

FUNCTION ExtendedTimer##
    d$ = DATE$
    l = INSTR(d$, "-")
    l1 = INSTR(l + 1, d$, "-")
    m = VAL(LEFT$(d$, l))
    d = VAL(MID$(d$, l + 1))
    y = VAL(MID$(d$, l1 + 1)) - 1970
    FOR i = 1 TO m
        SELECT CASE i 'Add the number of days for each previous month passed
            CASE 1: d = d 'January doestn't have any carry over days.
            CASE 2, 4, 6, 8, 9, 11: d = d + 31
            CASE 3: d = d + 28
            CASE 5, 7, 10, 12: d = d + 30
        END SELECT
    NEXT
    FOR i = 1 TO y
        d = d + 365
    NEXT
    FOR i = 2 TO y STEP 4
        IF m > 2 THEN d = d + 1 'add an extra day for leap year every 4 years, starting in 1970
    NEXT
    d = d - 1 'for year 2000
    s~&& = d * 24 * 60 * 60 'Seconds are days * 24 hours * 60 minutes * 60 seconds
    ExtendedTimer## = (s~&& + TIMER)
END FUNCTION

