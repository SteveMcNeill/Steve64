SCREEN _NEWIMAGE(1024, 720, 32)
_SCREENMOVE _MIDDLE

CONST DiagRollEm = -1

'The next lines are only needed for manual testing
DIM DiceToRoll AS DiceRoller_Type
DIM SHARED Brief AS LONG

'Feel free to change options as wanted for your program
'DiceToRoll.NumberOfDice = 2
'DiceToRoll.DiceSides = 10
'DiceToRoll.DiceReroll = "=1"
'DiceToRoll.DiceOpenRoll = "=10"
'DiceToRoll.DiceMod = 2
'DiceToRoll.DiceKeepHigh = 1
'DiceToRoll.DiceKeepLow = 1

'DiceToRoll.Set = 10
'DiceToRoll.SetMod = 1
'DiceToRoll.SetReRoll = "<6"
'DiceToRoll.SetOpenRoll = ">10"
'DiceToRoll.TotalMod = 27

'DiceToRoll.SetKeepHigh = 9


PRINT RollEm$("10skh9r2;2d10o20;t2;b2")

PRINT
PRINT "PRESS <ANY KEY> TO CONTINUE"
SLEEP
PRINT

ClearDice
DiceToRoll.Set = 6
DiceToRoll.NumberOfDice = 4
DiceToRoll.DiceSides = 6
DiceToRoll.DiceKeepHigh = 3


Brief = 0
PRINT "NON-BRIEF OUTPUT:"
PRINT "'6s; 4d6 kh3' -- Roll 6 sets of 4 six-sided dice, keep the three highest"
PRINT DiceRoll$
PRINT
PRINT "PRESS <ANY KEY> TO CONTINUE"
SLEEP
PRINT

Brief = 1
PRINT "SEMI-BRIEF OUTPUT:"
PRINT "'6s; 4d6 kh3' -- Roll 6 sets of 4 six-sided dice, keep the three highest"
PRINT DiceRoll$
PRINT
PRINT "PRESS <ANY KEY> TO CONTINUE"
SLEEP
PRINT

Brief = 2
PRINT "MOST BRIEF OUTPUT:"
PRINT "'6s; 4d6 kh3' -- Roll 6 sets of 4 six-sided dice, keep the three highest"
PRINT DiceRoll$
PRINT
PRINT "PRESS <ANY KEY> TO CONTINUE"
SLEEP
PRINT







TYPE DiceRoller_Type
    Set AS LONG
    SetMod AS LONG
    SetReRoll AS STRING * 100
    SetOpenRoll AS STRING * 100
    SetKeepHigh AS LONG
    SetKeepLow AS LONG
    SetDiscardHigh AS LONG
    SetDiscardLow AS LONG

    NumberOfDice AS LONG
    DiceSides AS LONG
    DiceMod AS LONG
    DiceReroll AS STRING * 100
    DiceOpenRoll AS STRING * 100
    DiceKeepHigh AS LONG
    DiceKeepLow AS LONG
    DiceDiscardHigh AS LONG
    DiceDiscardLow AS LONG

    TotalMod AS LONG
END TYPE

SUB StripNumber (m$)
    v = VAL(m$)
    DO UNTIL v = 0
        'PRINT "Stripping number"; m$
        m$ = MID$(m$, 2)
        v = VAL(m$)
    LOOP
    DO UNTIL LEFT$(m$, 1) <> "0" 'strip any zeros
        m$ = MID$(m$, 2)
    LOOP
END SUB


SUB ClearDice
    SHARED DiceToRoll AS DiceRoller_Type
    DiceToRoll.Set = 0
    DiceToRoll.SetMod = 0
    DiceToRoll.SetReRoll = ""
    DiceToRoll.NumberOfDice = 0
    DiceToRoll.DiceSides = 0
    DiceToRoll.DiceMod = 0
    DiceToRoll.DiceReroll = ""
    DiceToRoll.DiceOpenRoll = ""
    DiceToRoll.DiceKeepHigh = 0
    DiceToRoll.DiceKeepLow = 0
    DiceToRoll.DiceDiscardHigh = 0
    DiceToRoll.DiceDiscardLow = 0
    DiceToRoll.TotalMod = 0
END SUB

FUNCTION DiceRoll$
    SHARED Brief AS LONG
    SHARED DiceToRoll AS DiceRoller_Type
    IF DiceToRoll.DiceKeepHigh OR DiceToRoll.DiceKeepLow THEN
        IF DiceToRoll.DiceDiscardHigh OR DiceToRoll.DiceDiscardLow THEN DiceRoll$ = "Error - Can not keep and discard at the same time.": EXIT FUNCTION
    END IF
    IF DiceToRoll.NumberOfDice < 1 THEN DiceRoll$ = "Error - No dice to roll!": EXIT FUNCTION


    RANDOMIZE TIMER
    SHARED DiceToRoll AS DiceRoller_Type
    REDIM rolls(0) AS LONG
    REDIM SetRolls(0) AS LONG
    SetCount = 0
    IF Brief = 2 THEN OUT$ = "("
    FOR j = 1 TO DiceToRoll.Set
        ReRollSet:
        SetTotal = 0
        IF Brief = 0 THEN
            OUT$ = OUT$ + "RAW: ("
        ELSEIF Brief = 1 THEN
            OUT$ = OUT$ + "("
        END IF

        rollcount = -1
        FOR i = 1 TO DiceToRoll.NumberOfDice
            ReRollDice:
            roll = INT(RND(1) * DiceToRoll.DiceSides) + 1

            IF ParseDiceOption(roll, DiceToRoll.DiceReroll) THEN
                DiceOK = 0
                IF Brief = 0 THEN OUT$ = OUT$ + "r" + _TRIM$(STR$(roll)) + ", "
                GOTO ReRollDice
            END IF
            IF ParseDiceOption(roll, DiceToRoll.DiceOpenRoll) THEN
                DiceOK = 0
                DiceTotal = DiceTotal + roll + DiceToRoll.DiceMod
                IF Brief = 0 THEN
                    OUT$ = OUT$ + _TRIM$(STR$(roll)) + "o"
                    IF DiceToRoll.DiceMod THEN OUT$ = OUT$ + " + " + _TRIM$(STR$(DiceToRoll.DiceMod))
                    OUT$ = OUT$ + ","
                END IF
                rollcount = rollcount + 1
                REDIM _PRESERVE rolls(rollcount) AS LONG 'make certain we dont get out of bound errors for crazy reroll scenarios
                rolls(rollcount) = roll + DiceToRoll.DiceMod
                GOTO ReRollDice
            END IF

            rollcount = rollcount + 1
            REDIM _PRESERVE rolls(rollcount) AS LONG 'make certain we dont get out of bound errors for crazy reroll scenarios
            rolls(rollcount) = roll + DiceToRoll.DiceMod
            DiceTotal = DiceTotal + roll + DiceToRoll.DiceMod
            IF Brief = 0 THEN
                OUT$ = OUT$ + _TRIM$(STR$(roll))
                IF DiceToRoll.DiceMod THEN OUT$ = OUT$ + " + " + _TRIM$(STR$(DiceToRoll.DiceMod))
                IF i < DiceToRoll.NumberOfDice THEN 'more dice to roll in this set
                    OUT$ = OUT$ + ", "
                ELSE 'we're finished
                    OUT$ = OUT$ + ")"
                END IF
            END IF
        NEXT

        IF rollcount > 0 THEN Sort rolls() 'No need to try and sort only 1 dice.

        IF Brief = 0 THEN
            OUT$ = OUT$ + "; SORTED: ("
            FOR i = 0 TO rollcount
                OUT$ = OUT$ + _TRIM$(STR$(rolls(i)))
                IF i < rollcount THEN OUT$ = OUT$ + ", " ELSE OUT$ = OUT$ + ")"
            NEXT
        END IF

        REDIM keep(rollcount) AS LONG
        IF DiceToRoll.DiceKeepHigh OR DiceToRoll.DiceKeepLow THEN
            IF DiceToRoll.DiceKeepHigh THEN
                FOR i = DiceToRoll.DiceKeepHigh - 1 TO 0 STEP -1
                    IF i < rollcount THEN keep(rollcount - i) = -1
                NEXT
            END IF
            IF DiceToRoll.DiceKeepLow THEN
                FOR i = 0 TO DiceToRoll.DiceKeepLow - 1
                    IF i < rollcount THEN keep(i) = -1
                NEXT
            END IF
        ELSEIF DiceToRoll.DiceDiscardHigh OR DiceToRoll.DiceDiscardLow THEN
            FOR i = 0 TO rollcount
                keep(i) = -1
            NEXT
            IF DiceToRoll.DiceDiscardHigh THEN
                FOR i = DiceToRoll.DiceDiscardHigh - 1 TO 0 STEP -1
                    IF i < rollcount THEN keep(rollcount - i) = 0
                NEXT
            END IF
            IF DiceToRoll.DiceDiscardLow THEN
                FOR i = 0 TO DiceToRoll.DiceDiscardLow - 1
                    IF i < rollcount THEN keep(i) = 0
                NEXT
            END IF
        ELSE
            FOR i = 0 TO rollcount
                keep(i) = -1
            NEXT
        END IF

        IF Brief = 0 THEN OUT$ = OUT$ + "; KEEP: ("
        KeepTotal = 0
        kept = 0
        FOR i = 0 TO rollcount
            IF keep(i) THEN
                kept = kept + 1
                IF Brief < 2 THEN
                    IF kept > 1 THEN OUT$ = OUT$ + ", "
                    OUT$ = OUT$ + _TRIM$(STR$(rolls(i)))
                END IF
                KeepTotal = KeepTotal + rolls(i)
            END IF
        NEXT
        IF Brief < 2 THEN OUT$ = OUT$ + ") = " + _TRIM$(STR$(KeepTotal))

        IF ParseDiceOption(KeepTotal, DiceToRoll.SetReRoll) THEN
            IF Brief < 2 THEN OUT$ = OUT$ + "r" + CHR$(13)
            GOTO ReRollSet
        END IF

        IF ParseDiceOption(KeepTotal, DiceToRoll.SetOpenRoll) THEN
            SetTotal = SetTotal + KeepTotal + DiceToRoll.SetMod
            SetCount = SetCount + 1
            REDIM _PRESERVE SetRolls(SetCount) AS LONG
            SetRolls(SetCount) = SetTotal
            GrandTotal = GrandTotal + SetTotal
            IF Brief = 2 THEN OUT$ = OUT$ + _TRIM$(STR$(SetTotal))
            OUT$ = OUT$ + "o"
            IF Brief < 2 THEN
                IF DiceToRoll.SetMod THEN OUT$ = OUT$ + " + " + _TRIM$(STR$(DiceToRoll.SetMod))
                OUT$ = OUT$ + " = " + _TRIM$(STR$(SetTotal))
                OUT$ = OUT$ + CHR$(13)
            ELSE
                OUT$ = OUT$ + ", "
            END IF

            GOTO ReRollSet
        END IF

        SetTotal = SetTotal + KeepTotal + DiceToRoll.SetMod
        SetCount = SetCount + 1
        REDIM _PRESERVE SetRolls(SetCount) AS LONG
        SetRolls(SetCount) = SetTotal
        GrandTotal = GrandTotal + SetTotal

        IF Brief < 2 THEN
            IF DiceToRoll.SetMod THEN
                OUT$ = OUT$ + " + " + _TRIM$(STR$(DiceToRoll.SetMod))
                OUT$ = OUT$ + " = " + _TRIM$(STR$(SetTotal))
            END IF
            OUT$ = OUT$ + CHR$(13)
        ELSE
            OUT$ = OUT$ + _TRIM$(STR$(SetTotal))
            IF j < DiceToRoll.Set THEN OUT$ = OUT$ + ", " ELSE OUT$ = OUT$ + ")"
        END IF


    NEXT
    IF Brief < 2 THEN OUT$ = OUT$ + CHR$(13) + "GRAND TOTAL:"

    IF DiceToRoll.TotalMod THEN
        IF Brief < 2 THEN OUT$ = OUT$ + STR$(GrandTotal) + " +" + STR$(DiceToRoll.TotalMod)
    END IF

    GrandTotal = GrandTotal + DiceToRoll.TotalMod
    OUT$ = OUT$ + " =" + STR$(GrandTotal)

    Sort SetRolls()
    IF Brief = 0 THEN
        OUT$ = OUT$ + CHR$(13) + CHR$(13) + "Sorted Set: ("
        FOR i = 1 TO SetCount
            OUT$ = OUT$ + _TRIM$(STR$(SetRolls(i)))
            IF i < SetCount THEN OUT$ = OUT$ + ", " ELSE OUT$ = OUT$ + ")"
        NEXT
    END IF

    REDIM keep(SetCount) AS LONG
    IF DiceToRoll.SetKeepHigh OR DiceToRoll.SetKeepLow THEN
        IF DiceToRoll.SetKeepHigh THEN
            FOR i = DiceToRoll.SetKeepHigh - 1 TO 0 STEP -1
                IF i < SetCount THEN keep(SetCount - i) = -1
            NEXT
        END IF
        IF DiceToRoll.SetKeepLow THEN
            FOR i = 0 TO DiceToRoll.SetKeepLow - 1
                IF i < SetCount THEN keep(i) = -1
            NEXT
        END IF
    ELSEIF DiceToRoll.SetDiscardHigh OR DiceToRoll.SetDiscardLow THEN
        FOR i = 0 TO SetCount
            keep(i) = -1
        NEXT
        IF DiceToRoll.SetDiscardHigh THEN
            FOR i = DiceToRoll.SetDiscardHigh - 1 TO 0 STEP -1
                IF i < SetCount THEN keep(SetCount - i) = 0
            NEXT
        END IF
        IF DiceToRoll.SetDiscardLow THEN
            FOR i = 0 TO DiceToRoll.SetDiscardLow - 1
                IF i < SetCount THEN keep(i) = 0
            NEXT
        END IF
    ELSE
        FOR i = 0 TO SetCount
            keep(i) = -1
        NEXT
    END IF




    OUT$ = OUT$ + CHR$(13) + "Set Kept: ("
    IF Brief = 2 THEN OUT$ = "("
    KeepTotal = 0
    keep = 0
    FOR i = 1 TO SetCount
        IF keep(i) THEN
            keep = keep + 1
            IF keep > 1 THEN OUT$ = OUT$ + ", "
            OUT$ = OUT$ + _TRIM$(STR$(SetRolls(i)))
            KeepTotal = KeepTotal + SetRolls(i)
        END IF
    NEXT
    KeepTotal = KeepTotal + DiceToRoll.TotalMod
    OUT$ = OUT$ + ") = " + _TRIM$(STR$(KeepTotal))
    DiceRoll$ = OUT$
END FUNCTION


FUNCTION ParseDiceOption (num, t_temp$)
    SHARED DiceToRoll AS DiceRoller_Type
    temp$ = _TRIM$(t_temp$)
    IF temp$ = "" THEN EXIT FUNCTION
    IF RIGHT$(temp$, 1) <> "," THEN temp$ = temp$ + ","
    DO
        f = INSTR(temp$, ",")
        IF f THEN
            o$ = LEFT$(temp$, f - 1)
            temp$ = MID$(temp$, f + 1)
            o = VAL(MID$(o$, 2))
            o$ = LEFT$(o$, 1)
            SELECT CASE o$
                CASE "=": IF num = o THEN ParseDiceOption = -1: EXIT FUNCTION
                CASE "<": IF num < o THEN ParseDiceOption = -1: EXIT FUNCTION
                CASE ">": IF num > o THEN ParseDiceOption = -1: EXIT FUNCTION
            END SELECT
        END IF
    LOOP UNTIL f = 0 OR temp$ = ""
END FUNCTION

SUB Sort (Array() AS LONG)
    'The dice sorting routine, optimized to use _MEM and a comb sort algorithm.
    'It's more than fast enough for our needs here I think.  ;)
    DIM m AS _MEM
    DIM o AS _OFFSET, o1 AS _OFFSET
    DIM t AS LONG, t1 AS LONG
    m = _MEM(Array())
    $CHECKING:OFF
    gap = rollcount

    DO
        gap = 10 * gap \ 13
        IF gap < 1 THEN gap = 1
        i = 0
        swapped = 0
        DO
            o = m.OFFSET + i * 4
            o1 = m.OFFSET + (i + gap) * 4
            IF _MEMGET(m, o, LONG) > _MEMGET(m, o1, LONG) THEN
                _MEMGET m, o1, t1
                _MEMGET m, o, t
                _MEMPUT m, o1, t
                _MEMPUT m, o, t1
                swapped = -1
            END IF
            i = i + 1
        LOOP UNTIL i + gap > UBOUND(Array)
    LOOP UNTIL swapped = 0 AND gap = 1
    $CHECKING:ON
    _MEMFREE m
END SUB


FUNCTION RollEm$ (temp$)
    SHARED DiceToRoll AS DiceRoller_Type
    text1$ = UCASE$(temp$)
    FOR i = 1 TO LEN(text1$) 'check for invalid characters
        m$ = MID$(text1$, i, 1)
        SELECT CASE m$
            CASE "0" TO "9", "+", "-", "D", "K", "H", "L", "S", "T", ";", "=", "<", ">", ",", "R", "O", "B"
                text$ = text$ + m$ 'add valid characters to make text$
            CASE " " 'do nothing to a space
            CASE ELSE
                'invalid
        END SELECT
    NEXT
    'IF DiagRollEM THEN PRINT "Verified: "; text$

    IF text$ = "" THEN EXIT SUB 'can't do nothing with an empty string
    ClearDice
    DO
        semicolon = INSTR(text$, ";")
        IF semicolon THEN
            l$ = LEFT$(text$, semicolon - 1)
            text$ = MID$(text$, semicolon + 1)
        ELSE
            l$ = text$
        END IF

        'IF DiagRollEM THEN PRINT "PROCESSING: "; l$

        found = 0
        s = INSTR(l$, "S"): IF s THEN found = found + 1
        d = INSTR(l$, "D"): IF d THEN found = found + 1
        t = INSTR(l$, "T"): IF t THEN found = found + 1
        b = INSTR(l$, "B"): IF b THEN found = found + 1
        IF found <> 1 THEN EXIT SUB 'we should only find ONE element each pass, and there should always be one.  IF not, somebody screwed up.

        IF s THEN
            DiceToRoll.Set = VAL(LEFT$(l$, s - 1))
            IF DiagRollEm THEN PRINT "Number of Sets = "; DiceToRoll.Set
            m$ = MID$(l$, s + 1)

            pass = 0
            DO UNTIL m$ = ""
                pass = pass + 1
                'IF DiagRollEM THEN PRINT "SUBPROC "; m$
                n$ = LEFT$(m$, 1)
                IF n$ = "K" OR n$ = "D" THEN n$ = LEFT$(m$, 2)
                m$ = MID$(m$, LEN(n$) + 1)
                v = VAL(m$)
                SELECT CASE n$
                    CASE "+" '+ set mod
                        IF v > 0 THEN DiceToRoll.SetMod = v
                        IF DiagRollEm THEN PRINT "Set Mod"; v
                    CASE "-" ' - set mod
                        IF v > 0 THEN DiceToRoll.SetMod = -v
                        IF DiagRollEm THEN PRINT "Set Mod"; v
                    CASE "KH" 'keep high
                        IF v > 0 THEN DiceToRoll.SetKeepHigh = v
                        IF DiagRollEm THEN PRINT "Keep the Highest"; v; " Sets"
                    CASE "KL"
                        IF v > 0 THEN DiceToRoll.SetKeepLow = v
                        IF DiagRollEm THEN PRINT "Keep the Lowest"; v; " Sets"
                    CASE "DH"
                        IF v > 0 THEN DiceToRoll.SetDiscardHigh = v
                        IF DiagRollEm THEN PRINT "Discard the Highest"; v; " Sets"
                    CASE "DL"
                        IF v > 0 THEN DiceToRoll.SetDiscardLow = v
                        IF DiagRollEm THEN PRINT "Discard the Lowest"; v; " Sets"
                    CASE "R", "O" 'reroll or open roll
                        finished = 0: t$ = "": innerpass = 0
                        DO UNTIL finished
                            innerpass = innerpass + 1
                            v = VAL(m$)
                            IF v <> 0 THEN 'it's an o/r followed by a number
                                t$ = t$ + "=" + _TRIM$(STR$(v)) + ","
                            ELSE
                                n1$ = LEFT$(m$, 1)
                                SELECT CASE n1$
                                    CASE "="
                                        m$ = MID$(m$, 2)
                                        v = VAL(m$)
                                        t$ = t$ + "=" + _TRIM$(STR$(v)) + ","
                                    CASE "<"
                                        m$ = MID$(m$, 2)
                                        v = VAL(m$)
                                        t$ = t$ + "<" + _TRIM$(STR$(v)) + ","
                                    CASE ">"
                                        m$ = MID$(m$, 2)
                                        v = VAL(m$)
                                        t$ = t$ + ">" + _TRIM$(STR$(v)) + ","
                                    CASE ","
                                        m$ = MID$(m$, 2)
                                    CASE ELSE 'a character not a number, or =<>,
                                        finished = -1
                                END SELECT
                            END IF
                            StripNumber m$
                            IF n$ = "R" THEN
                                DiceToRoll.SetReRoll = t$
                                IF DiagRollEm THEN PRINT "Reroll Sets "; DiceToRoll.SetReRoll
                            ELSE
                                DiceToRoll.SetOpenRoll = t$
                                IF DiagRollEm THEN PRINT "Openroll Sets "; DiceToRoll.SetOpenRoll
                            END IF
                            IF m$ = "" THEN finished = -1
                            IF innerpass > 255 THEN IF DiagRollEm THEN PRINT "Error -- Too many loops processing Set ReRoll or OpenRoll": EXIT FUNCTION
                        LOOP
                END SELECT
                StripNumber m$
                n$ = LEFT$(m$, 1)
                SELECT CASE n$
                    CASE "K", "D", "R", "O", "+", "-" 'see if it's another command without a comma
                    CASE ELSE
                        comma = INSTR(m$, ",")
                        IF comma THEN m$ = MID$(m$, comma + 1)
                END SELECT
                IF pass > 100 THEN IF DiagRollEm THEN PRINT "Error - endless processing loop deciphering SET information": EXIT FUNCTION
            LOOP
        END IF


        IF d THEN
            v = VAL(LEFT$(l$, d))
            IF v < 1 THEN DiceToRoll.NumberOfDice = 1 ELSE DiceToRoll.NumberOfDice = v
            IF DiagRollEm THEN PRINT "Number of Dice To Roll = "; DiceToRoll.NumberOfDice
            m$ = MID$(l$, d + 1)

            v = VAL(m$)
            IF v > 0 THEN DiceToRoll.DiceSides = v
            StripNumber m$
            IF DiagRollEm THEN PRINT "Dice Sides = "; DiceToRoll.DiceSides
            pass = 0
            DO UNTIL m$ = ""
                pass = pass + 1
                'IF DiagRollEM THEN PRINT "SUBPROC "; m$
                n$ = LEFT$(m$, 1)
                IF n$ = "K" OR n$ = "D" THEN n$ = LEFT$(m$, 2)
                m$ = MID$(m$, LEN(n$) + 1)
                v = VAL(m$)
                SELECT CASE n$
                    CASE "+" '+ set mod
                        IF v > 0 THEN DiceToRoll.DiceMod = v
                        IF DiagRollEm THEN PRINT "DM"; v
                    CASE "-" ' - set mod
                        IF v > 0 THEN DiceToRoll.DiceMod = -v
                        IF DiagRollEm THEN PRINT "DM"; v
                    CASE "KH" 'keep high
                        IF v > 0 THEN DiceToRoll.DiceKeepHigh = v
                        IF DiagRollEm THEN PRINT "DKH"; v
                    CASE "KL"
                        IF v > 0 THEN DiceToRoll.DiceKeepLow = v
                        IF DiagRollEm THEN PRINT "DKL"; v
                    CASE "DH"
                        IF v > 0 THEN DiceToRollDiceDiscardHigh = v
                        IF DiagRollEm THEN PRINT "DDH"; v
                    CASE "DL"
                        IF v > 0 THEN DiceToRoll.DiceDiscardLow = v
                        IF DiagRollEm THEN PRINT "DDL"; v
                    CASE "R", "O" 'reroll or open roll
                        finished = 0: t$ = "": innerpass = 0
                        DO UNTIL finished
                            innerpass = innerpass + 1
                            v = VAL(m$)
                            IF v <> 0 THEN 'it's an o/r followed by a number
                                t$ = t$ + "=" + _TRIM$(STR$(v)) + ","
                            ELSE
                                n1$ = LEFT$(m$, 1)
                                SELECT CASE n1$
                                    CASE "="
                                        m$ = MID$(m$, 2)
                                        v = VAL(m$)
                                        t$ = t$ + "=" + _TRIM$(STR$(v)) + ","
                                    CASE "<"
                                        m$ = MID$(m$, 2)
                                        v = VAL(m$)
                                        t$ = t$ + "<" + _TRIM$(STR$(v)) + ","
                                    CASE ">"
                                        m$ = MID$(m$, 2)
                                        v = VAL(m$)
                                        t$ = t$ + ">" + _TRIM$(STR$(v)) + ","
                                    CASE ","
                                        m$ = MID$(m$, 2)
                                    CASE ELSE 'a character not a number, or =<>,
                                        finished = -1
                                END SELECT
                            END IF
                            StripNumber m$
                            IF n$ = "R" THEN
                                DiceToRoll.DiceReroll = t$
                                IF DiagRollEm THEN PRINT "DR: "; DiceToRoll.DiceReroll
                            ELSE
                                DiceToRoll.DiceOpenRoll = t$
                                IF DiagRollEm THEN PRINT "DO: "; DiceToRoll.DiceOpenRoll
                            END IF
                            IF m$ = "" THEN finished = -1
                            IF innerpass > 255 THEN IF DiagRollEm THEN PRINT "Error -- Too many loops processing Dice ReRoll or OpenRoll": EXIT FUNCTION
                        LOOP
                END SELECT
                StripNumber m$
                n$ = LEFT$(m$, 1)
                SELECT CASE n$
                    CASE "K", "D", "R", "O", "+", "-" 'see if it's another command without a comma
                    CASE ELSE
                        comma = INSTR(m$, ",")
                        IF comma THEN m$ = MID$(m$, comma + 1)
                END SELECT
                IF pass > 100 THEN IF DiagRollEm THEN PRINT "Error - endless processing loop deciphering SET information": EXIT FUNCTION
            LOOP
        END IF


        IF t THEN
            DiceToRoll.TotalMod = VAL(MID$(l$, 2))
            IF DiagRollEm THEN PRINT "Dice Total Modifier = "; DiceToRoll.TotalMod
        END IF

        IF b THEN
            Brief = VAL(MID$(l$, 2))
            IF DiagRollEm THEN PRINT "Roll Information Displayed: ";
            SELECT CASE Brief
                CASE 0: IF DiagRollEm THEN PRINT "Full"
                CASE 1: IF DiagRollEm THEN PRINT "Reduced"
                CASE 2: IF DiagRollEm THEN PRINT "Final Results Only"
            END SELECT
        END IF

    LOOP UNTIL l$ = text$
    RollEm$ = DiceRoll$
END FUNCTION

'************************************************
'*                  DOCUMENTATION               *
'************************************************

'This little library is able to do just about anything you'd need it to do, as far as dice rolling for RPGs go, as long as you follow the basic syntax and send it a proper string.

'First Concept of Library: Our "dice rolling formula" is broken down into segments seperated by a semicolon.

'Our 4 basic segments are:
'Sets
'Dice
'Total mod
'Brief output

'For Sets, the syntax is:
'##S  -- the number of sets, followed by S

'For Dice, the syntax is:
'##D##  -- the number of dice, followed by D, followed by the sides on the dice.  NOTE: the first set of numbers are optional, so you could simply use D6 to roll a single six sided dice.

'For Total modifed, the syntax is:
'T##  -- T followed by the total to add or subtract to the total dice roll

'For Brief output, the syntax is:
'##B  -- the number to represent how little output we want, followed by B
'0 = full output
'1 = reduced output
'2 = minimal output (basically only the final results)

'Second Concept of Library: Our segments can be further limited by optional parameters

'+## (or -##) -- add (or subtract) number  to segment

'KH## -- Keep the Highest number of "segment"
'KL## -- Keep the Lowest number of "segment"
'DH## -- Discard the Highest number of "segment"
'DL## -- Discard the Lowest number of "segment"

'R + stuff -- Reroll according to stuff
'O + stuff-- Openroll according to stuff

'stuff -- A string composed of numbers, operators,  and commas, to represent what to reroll or openroll.  It sounds complex, but its not.

'R1  -- Reroll all 1's
'R=2,=3  -- Reroll all rolls that are equal to 2 and equal to 3
'R<4 -- Reroll all rolls that are less than 4

'O1<2>3 -- (a silly rule set, but hey, it's an example)... Openroll all 1's, all numbers less than two, and all numbers greater than 3

'Putting it all together:

'In the end, what we end up with is formulas which look like the following:

'3S;2D10 -- Roll 3 sets; of 2 10-sided dice.

'6S;4D6KH3 -- Roll 6 sets; of 4 6-sided dice, keeping the 3 highest rolls

'10SKH1KL1;2D10+2 -- Roll 10 sets of dice and only keep the highest set and the lowest set; of 2 10-sided dice, and add 2 to each dice.

'Depending on what you want, you can generate some rather impressive formulas and take all the bite out of the dice rolling process completely for your games.

'NOTE: Spaces are optional, so if they help you understand your "dice rolling formulas" better, feel free to use them:

'4S; 3D10 KH2 O20 R2 ; T1; B2 -- Roll 4 sets of; 3 10-sided dice, keeping the 2 highest dice, and openrolling if the dice total to 20, and rerolling if the dice total to 2; then add 1 to the final total; and all we want to see are the final results...

