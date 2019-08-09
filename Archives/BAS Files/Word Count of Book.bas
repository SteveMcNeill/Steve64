DEFLNG A-Z

'load the bible
file$ = "kjv10.txt"

OPEN file$ FOR BINARY AS #1
book$ = SPACE$(LOF(1))
GET #1, , book$
CLOSE
book$ = UCASE$(book$)


DIM SHARED Words(20000) AS STRING 'A large enough value to hold all our words
DIM SHARED WordCount(20000) AS LONG

l = 0
DO
    l = 0
    l1 = INSTR(oldl, book$, CHR$(32))
    l2 = INSTR(oldl, book$, CHR$(13)) 'CR
    l3 = INSTR(oldl, book$, CHR$(10)) 'LF
    IF l1 > 0 AND l1 < l2 AND l1 < l3 THEN l = l1: GOTO skipcheck
    IF l2 > 0 AND l2 < l3 THEN l = l2: GOTO skipcheck
    IF l3 > 0 THEN l = l3
    skipcheck:
    IF l = 0 THEN EXIT DO
    word$ = MID$(book$, oldl, l - oldl)
    i = 1
    DO UNTIL i > LEN(word$)
        IF ASC(word$, i) < 65 OR ASC(word$, i) > 90 THEN
            word$ = LEFT$(word$, i - 1) + MID$(word$, i + 1)
        ELSE
            i = i + 1
        END IF
    LOOP
    newword = -1
    FOR i = 1 TO WordTotal
        IF word$ = Words(i) THEN newword = 0: EXIT FOR
    NEXT
    IF newword THEN
        WordTotal = WordTotal + 1
        Words(WordTotal) = word$
        WordCount(WordTotal) = 1
    ELSE
        WordCount(i) = WordCount(i) + 1
    END IF
    oldl = l + 1
    DO UNTIL MID$(word$, oldl) <> CHR$(32) AND MID$(word$, oldl) <> CHR$(13) AND MID$(word$, oldl) <> CHR$(10)
        oldl = oldl + 1
    LOOP
    LOCATE 1, 1: PRINT "Processing book:"; oldl; "/"; LEN(book$)
LOOP

CLS
PRINT "There are "; WordTotal; " words in the bible."
DO
    PRINT "Give me a word to search for in the bible => (FULL LIST to see everything)"
    PRINT " =>";
    INPUT "", search$
    search$ = UCASE$(search$)
    IF search$ = "" THEN SYSTEM
    IF search$ = "FULL LIST" OR search$ = "FULLLIST" THEN
        found = -1
    ELSE
        found = 0
        FOR i = 1 TO WordTotal
            IF search$ = Words(i) THEN PRINT Words(i), WordCount(i): found = -1: EXIT FOR
        NEXT
    END IF
    IF NOT found THEN PRINT "Not in the bible"
LOOP UNTIL search$ = "FULL LIST" OR search$ = "FULLLIST"
PRINT "They are the following, and the appear this number of times each:"
combsort
FOR i = 1 TO UBOUND(words)
    PRINT Words(i), WordCount(i)
    IF i MOD 20 = 0 THEN SLEEP
NEXT

SUB combsort
    'This is the routine I tend to use personally and promote.
    'It's short, simple, and easy to implement into code.

    gap = UBOUND(wordcount)

    DO
        gap = 10 * gap \ 13
        IF gap < 1 THEN gap = 1
        i = 0
        swapped = 0
        DO
            IF WordCount(i) < WordCount(i + gap) THEN
                SWAP WordCount(i), WordCount(i + gap)
                SWAP Words(i), Words(i + gap)
                swapped = -1
            END IF
            i = i + 1
        LOOP UNTIL i + gap > UBOUND(wordcount)
    LOOP UNTIL gap = 1 AND swapped = 0
END SUB
