DEFLNG A-Z

DO
    a$ = INKEY$
    count = count + 1
    IF timeit = 0 THEN t# = TIMER(0.001) + 5: timeit = -1
LOOP UNTIL timeit AND TIMER(0.001) > t#
LOCATE 1, 1: PRINT count

count = 0: timeit = 0
DO
    a = _KEYHIT
    count = count + 1
    IF timeit = 0 THEN t# = TIMER(0.001) + 5: timeit = -1
LOOP UNTIL timeit AND TIMER(0.001) > t#
LOCATE 2, 1: PRINT count

count = 0: timeit = 0
DO
    IF _KEYDOWN(65) THEN REM 'lets do nothing but check for that keydown event
    count = count + 1
    IF timeit = 0 THEN t# = TIMER(0.001) + 5: timeit = -1
LOOP UNTIL timeit AND TIMER(0.001) > t#
LOCATE 3, 1: PRINT count

DO
    _LIMIT 30
LOOP UNTIL _KEYHIT = 27
