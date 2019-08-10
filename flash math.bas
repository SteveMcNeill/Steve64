$RESIZE:SMOOTH

DEFLNG A-Z
RANDOMIZE TIMER
CONST Skyblue~& = &HFF87CEEB
CONST Gold~& = &HFFFFD700
CONST Black~& = &HFF000000
CONST LightGray~& = &HFFD3D3D3


SCREEN _NEWIMAGE(640, 480, 32)

DIM SHARED F75, F50, F25, F15
F75 = _LOADFONT("courbd.ttf", 75)
F50 = _LOADFONT("courbd.ttf", 50)
F25 = _LOADFONT("courbd.ttf", 25)
F15 = _LOADFONT("courbd.ttf", 20)

REDIM SHARED OName(0) AS STRING 'Operation Name
REDIM SHARED PL(0) AS INTEGER 'Priority Level
DIM SHARED QuickReturn AS INTEGER
Set_OrderOfOperations 'This will also make certain our directories are valid, and if not make them.



TYPE OptionType
    x AS INTEGER
    y AS INTEGER
    wide AS INTEGER
    high AS INTEGER
    choices AS INTEGER 'from 0 to choices, so subtract 1 from total number of choices
    captions AS STRING ' "12345678901234567890" --10 spaces per choice, so the left would represent two choices
    clicks AS INTEGER 'binary value of click state for our choices; up to 16 max
    Font AS INTEGER
END TYPE

DIM SHARED Options(1 TO 3) AS OptionType

Options(1).x = 70: Options(1).y = 100: Options(1).high = 50: Options(1).wide = 100 '1 is mode
Options(1).choices = 4: Options(1).captions = "+         -        *          /         RND       "
Options(1).Font = F25: Options(1).clicks = 16 'rnd selected as default

Options(2).x = 70: Options(2).y = 200: Options(2).high = 50: Options(2).wide = 100 '2 is time
Options(2).choices = 4: Options(2).captions = "15        30       60         120       300       "
Options(2).Font = F25: Options(2).clicks = 4 '60 second default game

Options(3).x = 70: Options(3).y = 300: Options(3).high = 50: Options(3).wide = 100 '3 is difficulty
Options(3).choices = 4:: Options(3).captions = "TRIVIAL   EASY     AVERAGE    HARD      INSANE    "
Options(3).Font = F15: Options(3).clicks = 4 'average default setting

DIM SHARED opt$
DIM SHARED FinishTime AS _FLOAT
DIM SHARED LowNum AS INTEGER, Highnum AS INTEGER

DIM SHARED Problem(10000) AS STRING
DIM SHARED Answer(10000) AS STRING
DIM SHARED Guess(10000) AS STRING



_DELAY .2
_SCREENMOVE _MIDDLE

DO
    SetupOptions
    PlayGame

    _LIMIT 60
LOOP UNTIL _KEYHIT

SUB PlayGame
    oldmouse = -1: oldmouse2 = -1
    s = SaveState
    _FONT F75
    ProblemsSolved = 0
    DO
        RedoDivisionByZero:
        num1 = RND * Highnum + LowNum
        num2 = RND * Highnum + LowNum
        o$ = MID$(opt$, INT(RND * LEN(opt$) + 1), 1)
        IF o$ = "/" THEN num1 = num1 * num2: IF num2 = 0 GOTO RedoDivisionByZero
        IF o$ = "-" AND num1 < num2 THEN SWAP num1, num2
        math$ = _TRIM$(STR$(num1)) + o$ + _TRIM$(STR$(num2))
        answer$ = Evaluate_Expression$(math$)
        guess$ = STRING$(LEN(answer$), "?"): g = 1
        DO
            TimeLeft = FinishTime - TIMER
            IF TimeLeft <= 0 THEN EXIT DO
            _LIMIT 30
            CLS , 0
            COLOR &HFFFA0000, 0: _PRINTSTRING (0, 0), STR$(TimeLeft)
            FOR i = 0 TO 4
                BoxTitle 45 + i * 110, 240, 110, 100, 3, &HFF0FFF00, Gold, _TRIM$(STR$(i))
                BoxTitle 45 + i * 110, 340, 110, 100, 3, &HFF0FFF00, Gold, _TRIM$(STR$(i + 5))
            NEXT

            WHILE _MOUSEINPUT: WEND
            IF _MOUSEBUTTON(1) AND NOT oldmouse THEN 'it's a click
                Xclick = INT((_MOUSEX - 45) / 110)
                Yclick = INT((_MOUSEY - 240) / 100)
                Choice = Xclick + 5 * Yclick
                IF Choice >= 0 AND Choice <= 9 THEN
                    MID$(guess$, g, 1) = _TRIM$(STR$(Choice))
                    g = g + 1
                END IF
            END IF
            oldmouse = _MOUSEBUTTON(1): oldmouse2 = _MOUSEBUTTON(2)
            display$ = math$ + "=" + guess$
            COLOR &HFFFF0000, 0: BoxTitle 45, 100, 550, 100, 3, Skyblue, Gold, display$
            _DISPLAY
        LOOP UNTIL g > LEN(answer$)
        ProblemsSolved = ProblemsSolved + 1
        Problem(ProblemsSolved) = math$
        Answer(ProblemsSolved) = answer$
        Guess(ProblemsSolved) = guess$
        IF guess$ = answer$ THEN
            FinishTime = FinishTime + 1 '1 second extra time for each correct answer
        ELSE
            FinishTime = FinishTime - 1 'and a 1 second penalty for a wrong answer
        END IF
        TimeLeft = FinishTime - TIMER
        IF TimeLeft <= 0 THEN EXIT DO
    LOOP
    _AUTODISPLAY

    CLS
    _FONT F25
    PRINT "PROBLEM";
    LOCATE , 200: PRINT "ANSWER";
    LOCATE , 400: PRINT "GUESS"
    i = 1
    DO UNTIL i > ProblemsSolved
        COLOR -1
        PRINT i; Problem(i);
        LOCATE , 200: PRINT Answer(i);
        IF Guess(i) = Answer(i) THEN COLOR &HFF00FF00 ELSE COLOR &HFFFF0000
        LOCATE , 400: PRINT Guess(i)
        i = i + 1
        IF i MOD 15 = 1 THEN
            Mousepause
            CLS
            PRINT "PROBLEM";
            LOCATE , 200: PRINT "ANSWER";
            LOCATE , 400: PRINT "GUESS"
        END IF
    LOOP
    IF i > ProblemsSolved THEN Mousepause ELSE SYSTEM
    RestoreState s
END SUB

SUB Mousepause
    PRINT "Click <Left Button> for more results"
    click = 0
    DO
        WHILE _MOUSEINPUT: WEND
        IF _MOUSEBUTTON(1) THEN
            DO
                _LIMIT 30
                j = _MOUSEINPUT
            LOOP UNTIL _MOUSEBUTTON(1) = 0
            click = -1
        END IF
        _LIMIT 30
    LOOP UNTIL click
END SUB


SUB SetupOptions
    s = SaveState
    DO
        _LIMIT 30
        CLS
        FOR i = 1 TO 3 '3 option sets to display
            _FONT Options(i).Font
            x1 = Options(i).x: y1 = Options(i).y
            w = Options(i).wide: H = Options(i).high
            clicks = Options(i).clicks
            FOR j = 0 TO Options(i).choices
                t$ = _TRIM$(MID$(Options(i).captions, j * 10, 10))
                IF clicks AND 2 ^ j THEN
                    COLOR Black, 0: BoxTitle x1 + j * w, y1, w, H, 3, Skyblue, Gold, t$
                ELSE
                    COLOR LightGray, 0: BoxTitle x1 + j * w, y1, w, H, 3, Black, Gold, t$
                END IF
            NEXT
        NEXT
        _FONT F50
        COLOR Skyblue, 0
        CenterText 0, 0, 640, 100, "Math Flash!"
        COLOR Black, 0
        BoxTitle 170, 390, 300, 60, 3, &HFF0FFF00, Gold, "PLAY"

        _DISPLAY
        WHILE _MOUSEINPUT: WEND

        IF _MOUSEBUTTON(1) AND NOT oldmouse THEN 'if button was up, but is now down, then it's a click
            ClickX = INT((_MOUSEX - 70) / 100): ClickY = _CEIL((_MOUSEY - 100) / 50)
            IF ClickX >= 0 AND ClickX <= 4 THEN 'Click on menu item 0 to 4
                SELECT CASE ClickY '1 = top , 3 = middle, 5 = bottom
                    CASE 1
                        IF 2 ^ ClickX = 16 THEN
                            IF Options(1).clicks AND 16 THEN Options(1).clicks = 0 ELSE Options(1).clicks = 16
                        ELSE
                            Options(1).clicks = Options(1).clicks XOR 2 ^ ClickX
                            IF Options(1).clicks <> 16 THEN Options(1).clicks = Options(1).clicks AND NOT 16
                        END IF
                    CASE 3, 5
                        Options(ClickY \ 2 + 1).clicks = 2 ^ ClickX
                END SELECT
            END IF
            IF _MOUSEX > 170 AND _MOUSEX < 470 AND _MOUSEY > 390 AND _MOUSEY < 450 THEN EXIT DO
        END IF
        oldmouse = _MOUSEBUTTON(1)
    LOOP
    opt$ = ""
    IF Options(1).clicks = 16 THEN Options(1).clicks = INT(RND * 15) + 1
    IF Options(1).clicks AND 1 THEN opt$ = opt$ + "+"
    IF Options(1).clicks AND 2 THEN opt$ = opt$ + "-"
    IF Options(1).clicks AND 4 THEN opt$ = opt$ + "/"
    IF Options(1).clicks AND 8 THEN opt$ = opt$ + "*"
    SELECT CASE Options(2).clicks
        CASE 1: FinishTime = TIMER + 15
        CASE 2: FinishTime = TIMER + 30
        CASE 4: FinishTime = TIMER + 60
        CASE 8: FinishTime = TIMER + 120
        CASE 16: FinishTime = TIMER + 300
    END SELECT
    SELECT CASE Options(3).clicks
        CASE 1: LowNum = 1: Highnum = 5
        CASE 2: LowNum = 0: Highnum = 10
        CASE 4: LowNum = 0: Highnum = 12
        CASE 8: LowNum = 0: Highnum = 99
        CASE 16: LowNum = 10: Highnum = 89
    END SELECT
    RestoreState s
END SUB


SUB BoxTitle (x1, y1, x2, y2, thick, fg AS _UNSIGNED LONG, bg AS _UNSIGNED LONG, title$)
    Box x1, y1, x2, y2, thick, fg, bg
    CenterText x1, y1 + thick, x1 + x2, y1 + y2 + thick, title$
END SUB


SUB Box (x, y, wide, high, thick, Kolor AS _UNSIGNED LONG, Trim AS _UNSIGNED LONG)
    LINE (x, y)-STEP(wide, high), Trim, BF
    LINE (x + thick, y + thick)-STEP(wide - 2 * thick, high - 2 * thick), Kolor, BF
END SUB


SUB CenterText (x1, y1, x2, y2, text$)
    text$ = _TRIM$(text$)
    xmax = x2 - x1: ymax = y2 - y1
    textlength = _PRINTWIDTH(text$)
    xpos = (xmax - textlength) / 2
    ypos = (ymax - _FONTHEIGHT) / 2
    _PRINTSTRING (x1 + xpos, y1 + ypos), text$
END SUB

FUNCTION SaveState
    TYPE SaveStateType
        InUse AS INTEGER
        DC AS INTEGER
        BG AS INTEGER
        F AS INTEGER
        D AS INTEGER
        S AS INTEGER
        Disp AS INTEGER
        CurX AS INTEGER
        CurY AS INTEGER
    END TYPE
    DIM SS AS SaveStateType, Temp AS SaveStateType
    SHARED NSS AS LONG 'Number of Saved States
    SHARED SaveMem AS _MEM
    IF NOT _MEMEXISTS(SaveMem) THEN
        SaveMem = _MEMNEW(LEN(SS) * 255) 'Save up to 255 save states; More than 255 and we toss an error
        $CHECKING:OFF
        _MEMFILL SaveMem, SaveMem.OFFSET, SaveMem.SIZE, 0 AS _UNSIGNED _BYTE
        $CHECKING:ON
    END IF

    'Data to Save
    SS.InUse = -1
    SS.F = _FONT
    SS.DC = _DEFAULTCOLOR
    SS.BG = _BACKGROUNDCOLOR
    SS.D = _DEST
    SS.S = _SOURCE
    SS.Disp = _AUTODISPLAY
    SS.CurX = POS(0)
    SS.CurY = CSRLIN
    $CHECKING:OFF
    FOR i = 1 TO NSS
        O = (i - 1) * LEN(SS)
        _MEMGET SaveMem, SaveMem.OFFSET + O, Temp
        IF Temp.InUse = 0 THEN
            _MEMPUT SaveMem, SaveMem.OFFSET + O, SS
            SaveState = i
            EXIT FUNCTION
        END IF
    NEXT
    _MEMPUT SaveMem, SaveMem.OFFSET + NSS * LEN(SS), SS
    $CHECKING:ON
    NSS = NSS + 1
    SaveState = NSS
END SUB

SUB RestoreState (WhichOne AS LONG)
    DIM SS AS SaveStateType
    SHARED NSS AS LONG 'Number of Saved States
    SHARED SaveMem AS _MEM
    _MEMGET SaveMem, SaveMem.OFFSET + (WhichOne - 1) * LEN(SS), SS
    $CHECKING:ON
    IF SS.InUse THEN
        SS.InUse = 0 'Let the routine know that we're no longer in use for this handle
        $CHECKING:OFF
        _MEMPUT SaveMem, SaveMem.OFFSET + (WhichOne - 1) * LEN(SS), SS
        $CHECKING:ON
        _FONT SS.F
        COLOR SS.DC, SS.BG
        _DEST SS.D
        _SOURCE SS.S
        IF SS.Disp THEN _AUTODISPLAY ELSE _DISPLAY
        LOCATE SS.CurY, SS.CurX
    END IF
END SUB

FUNCTION Evaluate_Expression$ (e$)
    t$ = e$ 'So we preserve our original data, we parse a temp copy of it

    b = INSTR(UCASE$(e$), "EQL") 'take out assignment before the preparser sees it
    IF b THEN t$ = MID$(e$, b + 3): var$ = UCASE$(LTRIM$(RTRIM$(MID$(e$, 1, b - 1))))

    QuickReturn = 0
    PreParse t$

    IF QuickReturn THEN Evaluate_Expression$ = t$: EXIT FUNCTION

    IF LEFT$(t$, 5) = "ERROR" THEN Evaluate_Expression$ = t$: EXIT FUNCTION

    'Deal with brackets first
    exp$ = "(" + t$ + ")" 'Starting and finishing brackets for our parse routine.

    DO
        Eval_E = INSTR(exp$, ")")
        IF Eval_E > 0 THEN
            c = 0
            DO UNTIL Eval_E - c <= 0
                c = c + 1
                IF Eval_E THEN
                    IF MID$(exp$, Eval_E - c, 1) = "(" THEN EXIT DO
                END IF
            LOOP
            s = Eval_E - c + 1
            IF s < 1 THEN PRINT "ERROR -- BAD () Count": END
            eval$ = " " + MID$(exp$, s, Eval_E - s) + " " 'pad with a space before and after so the parser can pick up the values properly.
            ParseExpression eval$

            eval$ = LTRIM$(RTRIM$(eval$))
            IF LEFT$(eval$, 5) = "ERROR" THEN Evaluate_Expression$ = eval$: EXIT SUB
            exp$ = DWD(LEFT$(exp$, s - 2) + eval$ + MID$(exp$, Eval_E + 1))
            IF MID$(exp$, 1, 1) = "N" THEN MID$(exp$, 1) = "-"

            temppp$ = DWD(LEFT$(exp$, s - 2) + " ## " + eval$ + " ## " + MID$(exp$, E + 1))
        END IF
    LOOP UNTIL Eval_E = 0
    c = 0
    DO
        c = c + 1
        SELECT CASE MID$(exp$, c, 1)
            CASE "0" TO "9", ".", "-" 'At this point, we should only have number values left.
            CASE ELSE: Evaluate_Expression$ = "ERROR - Unknown Diagnosis: (" + exp$ + ") ": EXIT SUB
        END SELECT
    LOOP UNTIL c >= LEN(exp$)

    Evaluate_Expression$ = exp$
END FUNCTION



SUB ParseExpression (exp$)
    DIM num(10) AS STRING
    'We should now have an expression with no () to deal with
    IF MID$(exp$, 2, 1) = "-" THEN exp$ = "0+" + MID$(exp$, 2)
    FOR J = 1 TO 250
        lowest = 0
        DO UNTIL lowest = LEN(exp$)
            lowest = LEN(exp$): OpOn = 0
            FOR P = 1 TO UBOUND(OName)
                'Look for first valid operator
                IF J = PL(P) THEN 'Priority levels match
                    IF LEFT$(exp$, 1) = "-" THEN op = INSTR(2, exp$, OName(P)) ELSE op = INSTR(exp$, OName(P))
                    IF op > 0 AND op < lowest THEN lowest = op: OpOn = P
                END IF
            NEXT
            IF OpOn = 0 THEN EXIT DO 'We haven't gotten to the proper PL for this OP to be processed yet.
            IF LEFT$(exp$, 1) = "-" THEN op = INSTR(2, exp$, OName(OpOn)) ELSE op = INSTR(exp$, OName(OpOn))
            numset = 0

            '*** SPECIAL OPERATION RULESETS
            IF OName(OpOn) = "-" THEN 'check for BOOLEAN operators before the -
                SELECT CASE MID$(exp$, op - 3, 3)
                    CASE "NOT", "XOR", "AND", "EQV", "IMP"
                        EXIT DO 'Not an operator, it's a negative
                END SELECT
                IF MID$(exp$, op - 3, 2) = "OR" THEN EXIT DO 'Not an operator, it's a negative
            END IF

            IF op THEN
                c = LEN(OName(OpOn)) - 1
                DO
                    SELECT CASE MID$(exp$, op + c + 1, 1)
                        CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ".", "N": numset = -1 'Valid digit
                        CASE "-" 'We need to check if it's a minus or a negative
                            IF OName(OpOn) = "_PI" OR numset THEN EXIT DO
                        CASE ELSE 'Not a valid digit, we found our separator
                            EXIT DO
                    END SELECT
                    c = c + 1
                LOOP UNTIL op + c >= LEN(exp$)
                E = op + c

                c = 0
                DO
                    c = c + 1
                    SELECT CASE MID$(exp$, op - c, 1)
                        CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ".", "N" 'Valid digit
                        CASE "-" 'We need to check if it's a minus or a negative
                            c1 = c
                            bad = 0
                            DO
                                c1 = c1 + 1
                                SELECT CASE MID$(exp$, op - c1, 1)
                                    CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."
                                        bad = -1
                                        EXIT DO 'It's a minus sign
                                    CASE ELSE
                                        'It's a negative sign and needs to count as part of our numbers
                                END SELECT
                            LOOP UNTIL op - c1 <= 0
                            IF bad THEN EXIT DO 'We found our seperator
                        CASE ELSE 'Not a valid digit, we found our separator
                            EXIT DO
                    END SELECT
                LOOP UNTIL op - c <= 0
                s = op - c
                num(1) = MID$(exp$, s + 1, op - s - 1) 'Get our first number
                num(2) = MID$(exp$, op + LEN(OName(OpOn)), E - op - LEN(OName(OpOn)) + 1) 'Get our second number
                IF MID$(num(1), 1, 1) = "N" THEN MID$(num(1), 1) = "-"
                IF MID$(num(2), 1, 1) = "N" THEN MID$(num(2), 1) = "-"
                num(3) = EvaluateNumbers(OpOn, num())
                IF MID$(num(3), 1, 1) = "-" THEN MID$(num(3), 1) = "N"
                'PRINT "*************"
                'PRINT num(1), OName(OpOn), num(2), num(3), exp$
                IF LEFT$(num(3), 5) = "ERROR" THEN exp$ = num(3): EXIT SUB
                exp$ = LTRIM$(N2S(DWD(LEFT$(exp$, s) + RTRIM$(LTRIM$(num(3))) + MID$(exp$, E + 1))))
                'PRINT exp$
            END IF
            op = 0
        LOOP
    NEXT

END SUB



SUB Set_OrderOfOperations
    'PL sets our priortity level. 1 is highest to 65535 for the lowest.
    'I used a range here so I could add in new priority levels as needed.
    'OName ended up becoming the name of our commands, as I modified things.... Go figure!  LOL!

    'Constants get evaluated first, with a Priority Level of 1
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_PI"
    REDIM _PRESERVE PL(i): PL(i) = 1
    'I'm not certain where exactly percentages should go.  They kind of seem like a special case to me.  COS10% should be COS.1 I'd think...
    'I'm putting it here for now, and if anyone knows someplace better for it in our order of operations, let me know.
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "%"
    REDIM _PRESERVE PL(i): PL(i) = 5
    'Then Functions with PL 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ACOS"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ASIN"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ARCSEC"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ARCCSC"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ARCCOT"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_SECH"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_CSCH"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_COTH"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "COS"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "SIN"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "TAN"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "LOG"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "EXP"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ATN"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_D2R"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_D2G"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_R2D"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_R2G"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_G2D"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_G2R"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ABS"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "SGN"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "INT"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ROUND"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "FIX"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_SEC"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_CSC"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_COT"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ASC"
    REDIM _PRESERVE PL(i): PL(i) = 10
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "CHR$"
    REDIM _PRESERVE PL(i): PL(i) = 10

    'Exponents with PL 20
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "^"
    REDIM _PRESERVE PL(i): PL(i) = 20
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "SQR"
    REDIM _PRESERVE PL(i): PL(i) = 20
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ROOT"
    REDIM _PRESERVE PL(i): PL(i) = 20
    'Multiplication and Division PL 30
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "*"
    REDIM _PRESERVE PL(i): PL(i) = 30
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "/"
    REDIM _PRESERVE PL(i): PL(i) = 30
    'Integer Division PL 40
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "\"
    REDIM _PRESERVE PL(i): PL(i) = 40
    'MOD PL 50
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "MOD"
    REDIM _PRESERVE PL(i): PL(i) = 50
    'Addition and Subtraction PL 60
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "+"
    REDIM _PRESERVE PL(i): PL(i) = 60
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "-"
    REDIM _PRESERVE PL(i): PL(i) = 60

    'Relational Operators =, >, <, <>, <=, >=   PL 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "<>"
    REDIM _PRESERVE PL(i): PL(i) = 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "><" 'These next three are just reversed symbols as an attempt to help process a common typo
    REDIM _PRESERVE PL(i): PL(i) = 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "<="
    REDIM _PRESERVE PL(i): PL(i) = 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = ">="
    REDIM _PRESERVE PL(i): PL(i) = 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "=<" 'I personally can never keep these things straight.  Is it < = or = <...
    REDIM _PRESERVE PL(i): PL(i) = 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "=>" 'Who knows, check both!
    REDIM _PRESERVE PL(i): PL(i) = 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = ">"
    REDIM _PRESERVE PL(i): PL(i) = 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "<"
    REDIM _PRESERVE PL(i): PL(i) = 70
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "="
    REDIM _PRESERVE PL(i): PL(i) = 70
    'Logical Operations PL 80+
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "NOT"
    REDIM _PRESERVE PL(i): PL(i) = 80
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "AND"
    REDIM _PRESERVE PL(i): PL(i) = 90
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "OR"
    REDIM _PRESERVE PL(i): PL(i) = 100
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "XOR"
    REDIM _PRESERVE PL(i): PL(i) = 110
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "EQV"
    REDIM _PRESERVE PL(i): PL(i) = 120
    i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "IMP"
    REDIM _PRESERVE PL(i): PL(i) = 130

END SUB

FUNCTION EvaluateNumbers$ (p, num() AS STRING)
    DIM n1 AS _FLOAT, n2 AS _FLOAT, n3 AS _FLOAT
    SELECT CASE OName(p) 'Depending on our operator..
        CASE "_PI": n1 = 3.14159265358979323846264338327950288## 'Future compatable in case something ever stores extra digits for PI
        CASE "%": n1 = (VAL(num(1))) / 100 'Note percent is a special case and works with the number BEFORE the % command and not after
        CASE "_ACOS": n1 = _ACOS(VAL(num(2)))
        CASE "_ASIN": n1 = _ASIN(VAL(num(2)))
        CASE "_ARCSEC": n1 = _ARCSEC(VAL(num(2)))
        CASE "_ARCCSC": n1 = _ARCCSC(VAL(num(2)))
        CASE "_ARCCOT": n1 = _ARCCOT(VAL(num(2)))
        CASE "_SECH": n1 = _SECH(VAL(num(2)))
        CASE "_CSCH": n1 = _CSCH(VAL(num(2)))
        CASE "_COTH": n1 = _COTH(VAL(num(2)))
        CASE "COS": n1 = COS(VAL(num(2)))
        CASE "SIN": n1 = SIN(VAL(num(2)))
        CASE "TAN": n1 = TAN(VAL(num(2)))
        CASE "LOG": n1 = LOG(VAL(num(2)))
        CASE "EXP": n1 = EXP(VAL(num(2)))
        CASE "ATN": n1 = ATN(VAL(num(2)))
        CASE "_D2R": n1 = 0.0174532925 * (VAL(num(2)))
        CASE "_D2G": n1 = 1.1111111111 * (VAL(num(2)))
        CASE "_R2D": n1 = 57.2957795 * (VAL(num(2)))
        CASE "_R2G": n1 = 0.015707963 * (VAL(num(2)))
        CASE "_G2D": n1 = 0.9 * (VAL(num(2)))
        CASE "_G2R": n1 = 63.661977237 * (VAL(num(2)))
        CASE "ABS": n1 = ABS(VAL(num(2)))
        CASE "SGN": n1 = SGN(VAL(num(2)))
        CASE "INT": n1 = INT(VAL(num(2)))
        CASE "_ROUND": n1 = _ROUND(VAL(num(2)))
        CASE "FIX": n1 = FIX(VAL(num(2)))
        CASE "_SEC": n1 = _SEC(VAL(num(2)))
        CASE "_CSC": n1 = _CSC(VAL(num(2)))
        CASE "_COT": n1 = _COT(VAL(num(2)))
        CASE "^": n1 = VAL(num(1)) ^ VAL(num(2))
        CASE "SQR": n1 = SQR(VAL(num(2)))
        CASE "ROOT"
            n1 = VAL(num(1)): n2 = VAL(num(2))
            IF n2 = 1 THEN EvaluateNumbers$ = RTRIM$(LTRIM$(STR$(n1))): EXIT FUNCTION
            IF n1 < 0 AND n2 >= 1 THEN sign = -1: n1 = -n1 ELSE sign = 1
            n3 = 1## / n2
            IF n3 <> INT(n3) AND n2 < 1 THEN sign = SGN(n1): n1 = ABS(n1)
            n1 = sign * (n1 ^ n3)
        CASE "*": n1 = VAL(num(1)) * VAL(num(2))
        CASE "/": n1 = VAL(num(1)) / VAL(num(2))
        CASE "\"
            IF VAL(num(2)) <> 0 THEN
                n1 = VAL(num(1)) \ VAL(num(2))
            ELSE
                EvaluateNumbers$ = "ERROR - Bad operation (We shouldn't see this)"
                EXIT FUNCTION
            END IF
        CASE "MOD": n1 = VAL(num(1)) MOD VAL(num(2))
        CASE "+": n1 = VAL(num(1)) + VAL(num(2))
        CASE "-": n1 = VAL(num(1)) - VAL(num(2))
        CASE "=": n1 = VAL(num(1)) = VAL(num(2))
        CASE ">": n1 = VAL(num(1)) > VAL(num(2))
        CASE "<": n1 = VAL(num(1)) < VAL(num(2))
        CASE "<>", "><": n1 = VAL(num(1)) <> VAL(num(2))
        CASE "<=", "=<": n1 = VAL(num(1)) <= VAL(num(2))
        CASE ">=", "=>": n1 = VAL(num(1)) >= VAL(num(2))
        CASE "NOT": n1 = NOT VAL(num(2))
        CASE "AND": n1 = VAL(num(1)) AND VAL(num(2))
        CASE "OR": n1 = VAL(num(1)) OR VAL(num(2))
        CASE "XOR": n1 = VAL(num(1)) XOR VAL(num(2))
        CASE "EQV": n1 = VAL(num(1)) EQV VAL(num(2))
        CASE "IMP": n1 = VAL(num(1)) IMP VAL(num(2))
        CASE ELSE
            EvaluateNumbers$ = "ERROR - Bad operation (We shouldn't see this)" 'Let's say we're bad...
    END SELECT
    EvaluateNumbers$ = RTRIM$(LTRIM$(STR$(n1)))
END FUNCTION

FUNCTION DWD$ (exp$) 'Deal With Duplicates
    'To deal with duplicate operators in our code.
    'Such as --  becomes a +
    '++ becomes a +
    '+- becomes a -
    '-+ becomes a -
    t$ = exp$
    DO
        bad = 0
        DO
            l = INSTR(t$, "++")
            IF l THEN t$ = LEFT$(t$, l - 1) + "+" + MID$(t$, l + 2): bad = -1
        LOOP UNTIL l = 0
        DO
            l = INSTR(t$, "+-")
            IF l THEN t$ = LEFT$(t$, l - 1) + "-" + MID$(t$, l + 2): bad = -1
        LOOP UNTIL l = 0
        DO
            l = INSTR(t$, "-+")
            IF l THEN t$ = LEFT$(t$, l - 1) + "-" + MID$(t$, l + 2): bad = -1
        LOOP UNTIL l = 0
        DO
            l = INSTR(t$, "--")
            IF l THEN t$ = LEFT$(t$, l - 1) + "+" + MID$(t$, l + 2): bad = -1
        LOOP UNTIL l = 0
    LOOP UNTIL NOT bad
    DWD$ = t$
    VerifyString t$
END FUNCTION

SUB PreParse (e$)
    DIM f AS _FLOAT

    t$ = e$

    'First strip all spaces
    t$ = ""
    FOR i = 1 TO LEN(e$)
        IF MID$(e$, i, 1) <> " " THEN t$ = t$ + MID$(e$, i, 1)
    NEXT

    t$ = UCASE$(t$)
    IF t$ = "" THEN e$ = "ERROR -- NULL string; nothing to evaluate": EXIT SUB

    'ERROR CHECK by counting our brackets
    l = 0
    DO
        l = INSTR(l + 1, t$, "("): IF l THEN c = c + 1
    LOOP UNTIL l = 0
    l = 0
    DO
        l = INSTR(l + 1, t$, ")"): IF l THEN c1 = c1 + 1
    LOOP UNTIL l = 0
    IF c <> c1 THEN e$ = "ERROR -- Bad Parenthesis:" + STR$(c) + "( vs" + STR$(c1) + ")": EXIT SUB

    'Modify so that NOT will process properly
    l = 0
    DO
        l = INSTR(l + 1, t$, "NOT")
        IF l THEN
            'We need to work magic on the statement so it looks pretty.
            ' 1 + NOT 2 + 1 is actually processed as 1 + (NOT 2 + 1)
            'Look for something not proper
            l1 = INSTR(l + 1, t$, "AND")
            IF l1 = 0 OR (INSTR(l + 1, t$, "OR") > 0 AND INSTR(l + 1, t$, "OR") < l1) THEN l1 = INSTR(l + 1, t$, "OR")
            IF l1 = 0 OR (INSTR(l + 1, t$, "XOR") > 0 AND INSTR(l + 1, t$, "XOR") < l1) THEN l1 = INSTR(l + 1, t$, "XOR")
            IF l1 = 0 OR (INSTR(l + 1, t$, "EQV") > 0 AND INSTR(l + 1, t$, "EQV") < l1) THEN l1 = INSTR(l + 1, t$, "EQV")
            IF l1 = 0 OR (INSTR(l + 1, t$, "IMP") > 0 AND INSTR(l + 1, t$, "IMP") < l1) THEN l1 = INSTR(l + 1, t$, "IMP")
            IF l1 = 0 THEN l1 = LEN(t$) + 1
            t$ = LEFT$(t$, l - 1) + "(" + MID$(t$, l, l1 - l) + ")" + MID$(t$, l + l1 - l)
            l = l + 3
            'PRINT t$
        END IF
    LOOP UNTIL l = 0

    'Check for bad operators before a ( bracket
    l = 0
    DO
        l = INSTR(l + 1, t$, "(")
        IF l AND l > 2 THEN 'Don't check the starting bracket; there's nothing before it.
            good = 0
            FOR i = 1 TO UBOUND(OName)
                IF MID$(t$, l - LEN(OName(i)), LEN(OName(i))) = OName(i) AND PL(i) > 1 AND PL(i) <= 250 THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
            NEXT
            IF NOT good THEN e$ = "ERROR - Improper operations before (.": EXIT SUB
            l = l + 1
        END IF
    LOOP UNTIL l = 0

    'Check for bad operators after a ) bracket
    l = 0
    DO
        l = INSTR(l + 1, t$, ")")
        IF l AND l < LEN(t$) THEN
            good = 0
            FOR i = 1 TO UBOUND(OName)
                IF MID$(t$, l + 1, LEN(OName(i))) = OName(i) AND PL(i) > 1 AND PL(i) <= 250 THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
            NEXT
            IF MID$(t$, l + 1, 1) = ")" THEN good = -1
            IF NOT good THEN e$ = "ERROR - Improper operations after ).": EXIT SUB
            l = l + 1
        END IF
    LOOP UNTIL l = 0 OR l = LEN(t$) 'last symbol is a bracket

    'Turn all &H (hex) numbers into decimal values for the program to process properly
    l = 0
    DO
        l = INSTR(t$, "&H")
        IF l THEN
            E = l + 1: finished = 0
            DO
                E = E + 1
                comp$ = MID$(t$, E, 1)
                SELECT CASE comp$
                    CASE "0" TO "9", "A" TO "F" 'All is good, our next digit is a number, continue to add to the hex$
                    CASE ELSE
                        good = 0
                        FOR i = 1 TO UBOUND(OName)
                            IF MID$(t$, E, LEN(OName(i))) = OName(i) AND PL(i) > 1 AND PL(i) <= 250 THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
                        NEXT
                        IF NOT good THEN e$ = "ERROR - Improper &H value. (" + comp$ + ")": EXIT SUB
                        E = E - 1
                        finished = -1
                END SELECT
            LOOP UNTIL finished OR E = LEN(t$)
            t$ = LEFT$(t$, l - 1) + LTRIM$(RTRIM$(STR$(VAL(MID$(t$, l, E - l + 1))))) + MID$(t$, E + 1)
        END IF
    LOOP UNTIL l = 0

    'Turn all &B (binary) numbers into decimal values for the program to process properly
    l = 0
    DO
        l = INSTR(t$, "&B")
        IF l THEN
            E = l + 1: finished = 0
            DO
                E = E + 1
                comp$ = MID$(t$, E, 1)
                SELECT CASE comp$
                    CASE "0", "1" 'All is good, our next digit is a number, continue to add to the hex$
                    CASE ELSE
                        good = 0
                        FOR i = 1 TO UBOUND(OName)
                            IF MID$(t$, E, LEN(OName(i))) = OName(i) AND PL(i) > 1 AND PL(i) <= 250 THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
                        NEXT
                        IF NOT good THEN e$ = "ERROR - Improper &B value. (" + comp$ + ")": EXIT SUB
                        E = E - 1
                        finished = -1
                END SELECT
            LOOP UNTIL finished OR E = LEN(t$)
            bin$ = MID$(t$, l + 2, E - l - 1)
            FOR i = 1 TO LEN(bin$)
                IF MID$(bin$, i, 1) = "1" THEN f = f + 2 ^ (LEN(bin$) - i)
            NEXT
            t$ = LEFT$(t$, l - 1) + LTRIM$(RTRIM$(STR$(f))) + MID$(t$, E + 1)
        END IF
    LOOP UNTIL l = 0

    t$ = N2S(t$)
    VerifyString t$

    e$ = t$
END SUB



SUB VerifyString (t$)
    'ERROR CHECK for unrecognized operations
    j = 1
    DO
        comp$ = MID$(t$, j, 1)
        SELECT CASE comp$
            CASE "0" TO "9", ".", "(", ")": j = j + 1
            CASE ELSE
                good = 0
                FOR i = 1 TO UBOUND(OName)
                    IF MID$(t$, j, LEN(OName(i))) = OName(i) THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
                NEXT
                IF NOT good THEN t$ = "ERROR - Bad Operational value. (" + comp$ + ")": EXIT SUB
                j = j + LEN(OName(i))
        END SELECT
    LOOP UNTIL j > LEN(t$)
END SUB

FUNCTION N2S$ (exp$) 'scientific Notation to String
    t$ = LTRIM$(RTRIM$(exp$))
    IF LEFT$(t$, 1) = "-" THEN sign$ = "-": t$ = MID$(t$, 2)

    dp = INSTR(t$, "D+"): dm = INSTR(t$, "D-")
    ep = INSTR(t$, "E+"): em = INSTR(t$, "E-")
    check1 = SGN(dp) + SGN(dm) + SGN(ep) + SGN(em)
    IF check1 < 1 OR check1 > 1 THEN N2S = exp$: EXIT SUB 'If no scientic notation is found, or if we find more than 1 type, it's not SN!

    SELECT CASE l 'l now tells us where the SN starts at.
        CASE IS < dp: l = dp
        CASE IS < dm: l = dm
        CASE IS < ep: l = ep
        CASE IS < em: l = em
    END SELECT

    l$ = LEFT$(t$, l - 1) 'The left of the SN
    r$ = MID$(t$, l + 1): r&& = VAL(r$) 'The right of the SN, turned into a workable long


    IF INSTR(l$, ".") THEN 'Location of the decimal, if any
        IF r&& > 0 THEN
            r&& = r&& - LEN(l$) + 2
        ELSE
            r&& = r&& + 1
        END IF
        l$ = LEFT$(l$, 1) + MID$(l$, 3)
    END IF

    SELECT CASE r&&
        CASE 0 'what the heck? We solved it already?
            'l$ = l$
        CASE IS < 0
            FOR i = 1 TO -r&&
                l$ = "0" + l$
            NEXT
            l$ = "0." + l$
        CASE ELSE
            FOR i = 1 TO r&&
                l$ = l$ + "0"
            NEXT
    END SELECT

    N2S$ = sign$ + l$
END SUB

