DEFLNG A-Z
REDIM SHARED WordList(0) AS STRING
REDIM SHARED Match(0) AS STRING
Init
DO
    GetLetters text$
    CheckMatchs text$
    DisplayMatches
LOOP

SUB DisplayMatches
    PRINT
    PRINT "MATCHES :"
    IF UBOUND(match) = 0 THEN PRINT "NONE": EXIT SUB
    FOR i = 1 TO UBOUND(match)
        PRINT Match(i),
    NEXT
    PRINT
END SUB


SUB CheckMatchs (text$)
    text$ = LTRIM$(RTRIM$(text$))
    DIM userletters(26), wordletters(26)
    REDIM Match(0)
    alpha$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    l = LEN(text$)
    work$ = UCASE$(text$)
    wordcount = 1 'the number of letters in the word we're looking for
    FOR i = 1 TO l
        a = ASC(work$, i)
        IF a > 64 AND a < 91 THEN 'it's all good
            userletters(a - 64) = userletters(a - 64) + 1
        ELSE
            PRINT "Invalid letters entered.  Only use A-Z."
            EXIT SUB
        END IF
    NEXT

    FOR i = 1 TO UBOUND(wordlist)
        FOR l = 1 TO 26: wordletters(l) = 0: NEXT 'reset wordletters back to 0
        FOR l = 1 TO LEN(WordList(i)) 'count the letters in the word
            a = ASC(WordList(i), l)
            wordletters(a - 64) = wordletters(a - 64) + 1
        NEXT
        valid = -1 'assume it's a match
        FOR l = 1 TO 26 'compare for matches
            IF wordletters(l) > userletters(l) THEN valid = 0: EXIT FOR
        NEXT
        IF valid THEN
            REDIM _PRESERVE Match(UBOUND(match) + 1)
            Match(UBOUND(match)) = WordList(i)
        END IF
    NEXT
    PRINT
END SUB

SUB GetLetters (text$)
    PRINT
    DO
        PRINT "Give me the letters you want to check for word matches.  (From 2-15 letters) => ";
        INPUT text$
        IF text$ = "" THEN SYSTEM
        l = LEN(text$)
        IF l < 2 OR l > 15 THEN PRINT "Invalid Letters.  Try Again."
    LOOP UNTIL l > 1 AND l < 16
END SUB



SUB Init
    PRINT "Loading Dictionary..."
    file$ = "Scrabble WordList 2006.txt"
    OPEN file$ FOR BINARY AS #1
    l = LOF(1)
    WholeList$ = SPACE$(l)
    GET #1, 1, WholeList$
    PRINT "Parsing Dictionary..."
    c = 0: i = 0
    DO
        c = INSTR(c1, WholeList$, CHR$(13))
        IF c > 0 THEN
            u = UBOUND(wordlist) + 1
            REDIM _PRESERVE WordList(u)
            WordList(u) = MID$(WholeList$, c1, c - c1)
            c1 = c + 2 'our start pointer is now after the finish pointer
        ELSE
            EXIT DO
        END IF
    LOOP
    PRINT u; " words are now loaded and ready for use."
    CLOSE

END SUB

