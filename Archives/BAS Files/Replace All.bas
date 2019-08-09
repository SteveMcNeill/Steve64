PRINT "Just Replace DO"
text$ = "do doo de do dadoo"
PRINT text$
ReplaceAll text$, "do", "12345", "", " " 'To check and replace exclusive DO...  For example, Don't wouldn't change, only do would.
PRINT text$
PRINT "**************************"
PRINT
PRINT "Replace DO(whatever)"
text$ = "do doo de do dadoo"
PRINT text$
ReplaceAll text$, "do*", "12345", "*", " " 'To check and replace DO(whatver)...  For example, the do in don't would now  change
'                                                                      but the do in the word ado wouldn't.
PRINT text$
PRINT "**************************"
PRINT
PRINT "Replace (whatever)DO(whatever)"
text$ = "do doo de do dadoo"
PRINT text$
ReplaceAll text$, "*do*", "12345", "*", " " 'To check and replace (whatever)DO(whatver)...
PRINT text$
PRINT "**************************"
PRINT

'Let's say the below is our text.
text$ = "I like doggies and kitties.  My dog eats a lot of dog-food, but my doggy eats table scraps."
PRINT text$
'And we want to change the dog and dog-food to cat quickly as we made a typo.  (This is just an example, so don't scroll up and change it with the arrow keys as I'd normally do!  :P
ReplaceAll text$, "dog", "cat", "", " -" 'This says we use text$, find an exact match to "dog" and change it to "cat".  We don't need any Wildcards, but we only want to change "dog" if it has a leading space or hyphen.  " " or "-" (the " -" at the end).
PRINT text$





SUB ReplaceAll (text$, find$, replace$, Wildcard AS STRING * 1, VS$)
    'text$ is the text we want to make changes in.
    'find$ is what we want to find to change.
    'replace$ is what we want to replace it with.
    'Wildcard is the symbol we choose to use so we don't have to do an EXACT match.
    'VS$ is the list of symbols that we consider to be valid for seperating our words

    ValidSeperator = LEN(VS$)

    DIM VS(1 TO ValidSeperator) AS STRING * 1
    FOR i = 1 TO ValidSeperator
        VS(i) = MID$(VS$, i, 1)
    NEXT


    CheckBefore = -1: CheckAfter = -1

    IF LEFT$(find$, 1) = Wildcard THEN
        CheckBefore = 0: find$ = MID$(find$, 2)
        BC = -1
    END IF
    IF RIGHT$(find$, 1) = Wildcard THEN
        CheckAfter = 0
        find$ = LEFT$(find$, LEN(find$) - 1)
        AC = -1
    END IF

    IF find$ <> " " THEN text$ = " " + text$ + " " 'extra space before and after for the checker


    DO
        l = INSTR(l, text$, find$)
        IF l THEN
            IF CheckBefore THEN
                BC = 0 'Our before check is BAD by default, unless we find a valid seperator character before our string
                FOR i = 1 TO ValidSeperator
                    IF MID$(text$, l - 1, 1) = VS(i) THEN BC = -1: EXIT FOR
                NEXT
            END IF
            IF CheckAfter THEN
                AC = 0 'Our after check is BAD by default, unless we find a valid seperator character before our string
                FOR i = 1 TO ValidSeperator
                    IF MID$(text$, l + LEN(find$), 1) = VS(i) THEN AC = -1: EXIT FOR
                NEXT
            END IF
            IF BC AND AC THEN 'If before check and after check are good, then replace
                l$ = LEFT$(text$, l - 1)
                r$ = MID$(text$, l + LEN(find$))
                text$ = l$ + replace$ + r$
                l = l + LEN(replace$)
            ELSE 'don't replace anything
                l = l + LEN(find$)
            END IF
        END IF
    LOOP UNTIL l = 0
    IF find$ <> " " THEN text$ = MID$(text$, 2, LEN(text$) - 1) 'Remove those extra spaces we added for testing at the start of the sub.
END SUB

