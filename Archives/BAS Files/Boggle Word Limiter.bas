DEFLNG A-Z
SCREEN _NEWIMAGE(1024, 800, 256)

REDIM SHARED WordList(0) AS STRING
REDIM SHARED Match(0) AS STRING
DIM SHARED BD(1 TO 16, 1 TO 6) AS STRING * 1 'Boggle Dice
DIM SHARED Grid(1 TO 4, 1 TO 4) AS STRING * 1





Init
LimitWords



SUB LimitWords
PRINT "Limiting possible word matches...."
DIM wordletters(1 TO 26), gameletters(1 TO 26)
'Get letter count in game
FOR i = 1 TO 4
    FOR j = 1 TO 4
        gameletters(ASC(Grid(i, j)) - 64) = gameletters(ASC(Grid(i, j)) - 64) + 1
    NEXT
NEXT
u = UBOUND(wordlist)
FOR i = 1 TO u
    FOR j = 1 TO 26: wordletters(j) = 0: NEXT 'reset the counter to 0 to begin with
    FOR j = 1 TO LEN(WordList(i)) 'get the letters in the word
        a = ASC(WordList(i), j) - 64
        wordletters(a) = wordletters(a) + 1
    NEXT
    valid = -1 'let's assume it's valid to start with
    FOR j = 1 TO 26
        IF wordletters(j) > gameletters(j) THEN valid = 0: EXIT FOR 'We need letters that the board doesn't even have to start with
    NEXT
    IF valid THEN
        u = UBOUND(match) + 1
        REDIM _PRESERVE Match(u)
        Match(u) = WordList(i)
    END IF
NEXT
u = UBOUND(match)
PRINT "After limiting by letter count, we now have a dataset of"; u; " words to check for."
PRINT
PRINT "Now limiting by basic grid search."
DIM GridLimitMatches(u) AS STRING
DIM TempGrid(0 TO 5, 0 TO 5) AS STRING * 1
FOR x = 1 TO 4
    FOR y = 1 TO 4
        TempGrid(x, y) = Grid(x, y)
NEXT y, x
c = 0
FOR i = 1 TO u
    FOR x = 1 TO 4
        FOR y = 1 TO 4
            TempGrid(x, y) = Grid(x, y)
    NEXT y, x
    FOR j = 1 TO LEN(Match(i)) - 1
        t$ = MID$(Match(i), j, 1) 'Get the letter we want to compare
        'First, let's see where it's located at.
        valid = 0
        FOR x = 1 TO 4
            FOR y = 1 TO 4
                IF TempGrid(x, y) = t$ THEN 'We found our letter!
                    IF y > 1 THEN CheckAbove = -1 ELSE CheckAbove = 0
                    IF y < 4 THEN CheckBelow = -1 ELSE CheckBelow = 0
                    IF x > 1 THEN CheckLeft = -1 ELSE CheckLeft = 0
                    IF x < 4 THEN CheckRight = -1 ELSE CheckRight = 0
                    t1$ = MID$(Match(i), j + 1, 1)
                    IF CheckAbove AND TempGrid(x, y - 1) = t1$ THEN valid = -1: EXIT FOR
                    IF CheckAbove AND CheckLeft AND TempGrid(x - 1, y - 1) = t1$ THEN valid = -1: EXIT FOR
                    IF CheckAbove AND CheckRight AND TempGrid(x + 1, y - 1) = t1$ THEN valid = -1: EXIT FOR
                    IF CheckBelow AND TempGrid(x, y + 1) = t1$ THEN valid = -1: EXIT FOR
                    IF CheckBelow AND CheckLeft AND TempGrid(x - 1, y + 1) = t1$ THEN valid = -1: EXIT FOR
                    IF CheckBelow AND CheckRight AND TempGrid(x + 1, y + 1) = t1$ THEN valid = -1: EXIT FOR
                    IF CheckLeft AND TempGrid(x - 1, y) = t1$ THEN valid = -1: EXIT FOR
                    IF CheckRight AND TempGrid(x + 1, y) = t1$ THEN valid = -1: EXIT FOR
                END IF
        NEXT y, x
        TempGrid(x, y) = " "
        IF valid = 0 THEN EXIT FOR
    NEXT
    IF valid THEN
        c = c + 1
        GridLimitMatches(c) = Match(i)
    END IF
NEXT
PRINT "After a check of the grid, we're now limited down to these possible words ("; c; ")"
FOR i = 1 TO c
    PRINT GridLimitMatches(i),
NEXT
PRINT
END SUB

SUB Init
RANDOMIZE TIMER
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
        temp$ = MID$(WholeList$, c1, c - c1)
        IF LEN(temp$) > 3 THEN
            u = UBOUND(wordlist) + 1
            REDIM _PRESERVE WordList(u)
            WordList(u) = temp$
        END IF
        c1 = c + 2 'our start pointer is now after the finish pointer
    ELSE
        EXIT DO
    END IF
LOOP
PRINT u; " words are now loaded and ready for use."
PRINT
PRINT "Now preparing Boggle Dice"
FOR i = 1 TO 16
    FOR j = 1 TO 6
        READ BD(i, j)
NEXT j, i
PRINT "Dice ready, now rolling for the grid."
FOR i = 0 TO 3
    FOR j = 1 TO 4
        r = _CEIL(RND * 6) '1 to 6
        Grid(i + 1, j) = BD(i * 4 + j, r)
    NEXT
NEXT
PRINT "Displaying Grid Letters:"
FOR i = 0 TO 3
    FOR j = 1 TO 4
        PRINT Grid(i + 1, j);
    NEXT
    PRINT
NEXT

CLOSE
END SUB


DATA V,I,T,E,G,N
DATA A,C,E,S,L,R
DATA V,A,Z,E,D,N
DATA I,C,A,T,A,O
DATA N,O,D,U,K,T
DATA E,N,I,P,H,S
DATA O,R,I,F,B,X
DATA K,U,L,E,G,Y
DATA E,Y,I,E,H,F
DATA E,S,U,T,L,P
DATA E,W,O,S,D,N
DATA P,E,C,A,D,M
DATA A,L,I,B,T,Y
DATA S,A,H,O,M,R
DATA J,A,B,O,M,Qu
DATA U,R,I,G,L,W

