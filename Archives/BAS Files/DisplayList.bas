SUB DisplayList (x AS INTEGER, y AS INTEGER, w AS INTEGER, l AS INTEGER, s AS INTEGER, choices() AS STRING, numbered AS _BYTE)
'x/y location to place the start of our list
'w is the width of our list on the screen
'l is the length of the list items we want to display at a time
's is the starting element that we want to display on the screen
'choices() is the array that holds the actual list for us
'numbered is the toggle for if we want to autonumber our list or not.  0 is false, any other number is true.

'Some basic error checking is in need here
IF s < LBOUND(choices) THEN s = LBOUND(choices)
IF s + l - 1 > UBOUND(choices) THEN l = UBOUND(choices) - s + 1

LOCATE x
start = s: finish = s + l - 1
FOR i = start TO finish
    counter = counter + 1
    IF numbered THEN counter$ = LTRIM$(STR$(counter)) + ") "
    LOCATE , y: PRINT counter$ + LEFT$(choices(i), w - LEN(counter$))
NEXT

END SUB

