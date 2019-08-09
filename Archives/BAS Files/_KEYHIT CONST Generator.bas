_TITLE "_KEYHIT CONST Generator"

PRINT "Filename to save to (no extension) => ";
INPUT ; file$
file$ = file$ + ".bi"

OPEN file$ FOR OUTPUT AS #1
CLS

DO
    PRINT "Enter Const Name => ";
    INPUT cname$
    IF cname$ = "" THEN EXIT DO
    _DELAY .2 'Give some time for the user to take their hands off the keypresses
    PRINT "Press the Key for this Const => ";
    _KEYCLEAR 'clear the keyboard buffer
    DO
        a = _KEYHIT
    LOOP UNTIL a <> 0
    PRINT a
    PRINT #1, "CONST "; cname$; " = "; LTRIM$(RTRIM$(STR$(a))); "&"
    count = count + 1
LOOP
CLOSE

CLS
OPEN file$ FOR INPUT AS #1
PRINT "There were"; count; " CONST saved in "; file$
PRINT
FOR i = 1 TO count
    LINE INPUT #1, text$
    PRINT text$
    IF i MOD 20 = 0 THEN PRINT: PRINT "Press <ANY KEY> to see more": SLEEP
NEXT
SLEEP
CLS
PRINT "To use this file in your own program, add this line to the top of your code:"
PRINT "$INCLUDE:'"; file$; "'"

