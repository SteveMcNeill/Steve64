_TITLE "Flash Card Math Trainer by Steve McNeil, 2017-10-28 fix by bplus" ' >>> yeah for the +
' >>> all >>>>>>> and <<<<<< comments are from bplus
' >>> 2017-10-28 This fix does not involve SHELL or RUN, just an outer loop
' >>> that sets up a new test, then the inner loop conducts the test.
' >>> As it should be done! ;-))  ;D
' >>> PS a credit to Steve that I can follow his code enough to fix it.

' >>> Now who wants to fix all the text at extreme left side of the screen in setups for test?


TYPE ResultsType
    num1 AS LONG
    op AS LONG
    num2 AS LONG
    num3 AS LONG
    answer AS LONG
    correct AS LONG
END TYPE


DEFLNG A-Z
$RESIZE:SMOOTH
_DELAY 0.5
WS = _NEWIMAGE(1280, 720, 32) 'WorkScreen
SCREEN WS
_SCREENMOVE 100, 20

DIM SHARED WF 'Workfont
WF = _LOADFONT("C:\Windows\Fonts\Cour.ttf", 150)


DIM SHARED RST 'Result Font
RST = _LOADFONT("C:\Windows\Fonts\Cour.ttf", 50)

REDIM SHARED Results(0) AS ResultsType
DIM SHARED GameTime, Difficulty, Again

CONST Red = _RGB32(255, 0, 0)
CONST White = _RGB32(255, 255, 255)
CONST Black = _RGB32(0, 0, 0)
CONST Blue = _RGB32(0, 0, 255)
CONST Green = _RGB32(0, 255, 0)
CONST Yellow = _RGB32(255, 255, 0)

CONST Plus = 1, Minus = 2, Times = 3, Divide = 4

RANDOMIZE TIMER

t1 = _FREETIMER
ON TIMER(t1, 1) CountDown
TIMER(t1) ON '<<<<<<<<< don't worry Gametime will be reset at right time

WHILE -1 '<<<<<<<   setup and then do 1 test in this outer loop


    '>>> clear last set and setup for new
    REDIM Results(0) AS ResultsType
    MainChoiceScreen mode, Difficulty, length

    '>>> GameTime reset here in outer loop
    SELECT CASE length 'This is the number of seconds we're going to test ourselves
        CASE 1: GameTime = 15
        CASE 2: GameTime = 30
        CASE 3: GameTime = 60
        CASE 4: GameTime = 120
        CASE 5: GameTime = 300
    END SELECT

    SELECT CASE Difficulty 'This determines how hard the game will be
        CASE 1: LowLimit = 0: HighLimit = 10
        CASE 2: LowLimit = 0: HighLimit = 12
        CASE 3: LowLimit = 0: HighLimit = 99
        CASE 4: LowLimit = 10: HighLimit = 89
    END SELECT


    '>>>>>>> test until no GameTime left

    '>>>>>>> but don't interrupt once a question is presented!!

    DO '>>>>>>>>>>>>>>>>>>>>>>>> inner loop tests until GameTime <=0

        _LIMIT 10 '??? OK
        IF GameTime <= 0 THEN EXIT DO
        num1 = RND * HighLimit + LowLimit

        SELECT CASE mode 'Only give the user the numbers they wanted to play with
            CASE 1: op = 1
            CASE 2: op = 2
            CASE 3: op = RND + 1
            CASE 4: op = 3
            CASE 5: op = 4
            CASE 6: op = RND + 3
            CASE 7: op = RND: IF op = 0 THEN op = 1 ELSE op = 3
            CASE 8: op = INT(RND * 4) + 1
        END SELECT

        num2 = RND * HighLimit + LowLimit

        IF op = 4 THEN 'it's divide, let's get the numbers in the proper order
            DO
                num1 = RND * HighLimit + LowLimit
                num2 = RND * HighLimit + LowLimit
                num1 = num1 * num2
            LOOP UNTIL num1 < 100
        END IF
        IF op = 2 AND num1 < num2 THEN SWAP num1, num2
        DrawCard num1, op, num2
        DrawUserClickAreas
        GetUserAnswer answer$
        CheckAnswer num1, op, num2, answer$
        DisplayTime
        _DISPLAY
    LOOP

    '>>>>>>>>>>>>>>>>>>>>>> NOW DO THIS !!!
    ShowResults

    '>>>>>>>>>>>>>>>>>>>>> Loop around if the user hasn't chosen to quit in ShowResults
WEND

SUB MainChoiceScreen (mode, difficulty, length)
    CLS
    _FONT RST
    PRINT "Welcome to Math Flash!"
    _FONT 16
    LOCATE 5, 1: PRINT "Your personal math trainer!"
    PRINT
    PRINT "What would you like to learn to work with today?"
    PRINT "1) Addition"
    PRINT "2) Subtraction"
    PRINT "3) Addition and Subtraction"
    PRINT "4) Multiplication"
    PRINT "5) Division"
    PRINT "6) Multiplication and Division"
    PRINT "7) Addition and Multiplication"
    PRINT "8) ALL of them"
    PRINT "9) QUIT THE TRAINER"

    DO
        _LIMIT 10
        A = VAL(INKEY$)
    LOOP UNTIL A > 0 AND A < 10
    IF A = 9 THEN SYSTEM

    PRINT
    PRINT
    PRINT "How difficult do you wish for this game to be?"
    PRINT "1) Easy"
    PRINT "2) Normal"
    PRINT "3) Hard"
    PRINT "4) Grueling"
    DO
        _LIMIT 10
        b = VAL(INKEY$)
    LOOP UNTIL b > 0 AND b < 5

    PRINT
    PRINT
    PRINT "How long a game do you want?"
    PRINT "1) Very Short"
    PRINT "2) Short"
    PRINT "3) Average"
    PRINT "4) Long"
    PRINT "5) Bring It On, Long!"

    DO
        c = VAL(INKEY$)
    LOOP UNTIL c > 0 AND c < 6

    _FONT RST
    CLS
    LOCATE 7
    DO: LOOP UNTIL INKEY$ = "" 'Clear the keyboard buffer
    PRINT "<LEFT CLICK> TO BEGIN MATH FLASH CARDS!"
    DO
        _LIMIT 10
        x = mouseclick
    LOOP UNTIL x = 1

    CLS
    _FONT WF
    mode = A: difficulty = b: length = c
END SUB


SUB DisplayTime
    _FONT RST
    COLOR White, Black
    LOCATE 1, 1000: PRINT GameTime; "LEFT  "
    _FONT WF
END SUB

SUB CountDown
    '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Just Count down!!!
    GameTime = GameTime - 1

    '>>> let's do this somewhere else
    '>>> IF GameTime <= 0 THEN ShowResults 'Clock has ran out
END SUB

SUB ShowResults
    Score = 0
    _AUTODISPLAY
    CLS , Black
    _FONT 16
    COLOR White, Black
    PRINT "YOUR ANSWER"; TAB(40); "CORRECT ANSWER"
    FOR i = 1 TO UBOUND(results)
        text$ = Num2Str(Results(i).num1)
        SELECT CASE Results(i).op
            CASE 1: text$ = text$ + " + "
            CASE 2: text$ = text$ + " - "
            CASE 3: text$ = text$ + " * "
            CASE 4: text$ = text$ + " / "
        END SELECT

        IF Results(i).correct = -1 THEN
            COLOR Green, Black
            Correct = Correct + 1
            YourText$ = text$ + Num2Str(Results(i).num2) + " = " + Num2Str(Results(i).answer)
        ELSE
            COLOR Red, Black
            Wrong = Wrong + 1
            IF Results(i).correct = 1 THEN
                YourText$ = text$ + Num2Str(Results(i).num2) + " = " + "SKIPPED"
            ELSE
                YourText$ = text$ + Num2Str(Results(i).num2) + " = " + Num2Str(Results(i).answer)
            END IF
        END IF
        CorrectText$ = text$ + Num2Str(Results(i).num2) + " = " + Num2Str(Results(i).num3)
        PRINT YourText$; TAB(40);
        COLOR White, Black
        PRINT CorrectText$
    NEXT
    PRINT
    PRINT "You got "; Correct; " correct answers, and "; Wrong; " wrong."
    PRINT "If this was a test, you would have scored "; INT(100 * (Correct / (i - 1))); "%"
    ScoreMultiplier = 25 * Difficulty
    Score = ScoreMultiplier * Correct + ScoreMultiplier * -Wrong
    PRINT
    PRINT
    PRINT "YOUR GAME SCORE IS: "; Score
    PRINT
    SELECT CASE Difficulty
        CASE 1: file$ = "Easy.txt": diff$ = "EASY"
        CASE 2: file$ = "Average.txt": diff$ = "NORMAL"
        CASE 3: file$ = "Hard.txt": diff$ = "HARD"
        CASE 4: file$ = "Grueling.txt": diff$ = "GRUELING"
    END SELECT
    IF _FILEEXISTS(file$) THEN
        'life should be good as we have an existing file.
    ELSE
        'if not, then let's make a blank highscore file for the proper difficulty.
        OPEN file$ FOR OUTPUT AS #1
        FOR i = 1 TO 25
            PRINT #1, "None"
            PRINT #1, 0
            PRINT #1, "None"
        NEXT
        CLOSE #1
    END IF
    OPEN file$ FOR INPUT AS #1
    DIM person(25) AS STRING, score(25), diff$(25)
    HS = 0 'no high score until we validate it
    FOR i = 1 TO 25
        LINE INPUT #1, person(i)
        INPUT #1, score(i)
        LINE INPUT #1, diff$(i)
        IF Score > score(i) AND NOT HS THEN
            PRINT "CONGRATULATIONS!!  YOU GOT A HIGH SCORE!"
            PRINT "What name would you like to be known by? ";
            INPUT person$
            IF i < 25 THEN person(i + 1) = person(i): score(i + 1) = score(i): diff$(i + 1) = diff$(i)
            person(i) = person$: score(i) = Score: diff$(i) = diff$
            i = i + 1: HS = -1
        END IF
    NEXT
    CLOSE #1

    PRINT "Press <ANY KEY> to see the "; diff$; " HIGH SCORES"
    SLEEP
    CLS
    PRINT diff$; " HIGH SCORERS"
    FOR i = 1 TO 25
        PRINT person(i); TAB(20); score(i); TAB(40); diff$(i)
    NEXT

    OPEN file$ FOR OUTPUT AS #1
    FOR i = 1 TO 25
        PRINT #1, person(i)
        PRINT #1, score(i)
        PRINT #1, diff$(i)
    NEXT
    CLOSE


    PRINT
    PRINT "Do the test again? (Y/N)"
    DO
        _LIMIT 10
        a$ = UCASE$(INKEY$)
    LOOP UNTIL a$ = "Y" OR a$ = "N"

    '>>> automatic loop unless No Now
    IF a$ = "N" THEN
        _DELAY 1
        SYSTEM
    END IF
END SUB

SUB CheckAnswer (num1, op, num2, answer$)
    ans = VAL(answer$)
    SELECT CASE op
        CASE Plus: num3 = num1 + num2
        CASE Minus: num3 = num1 - num2
        CASE Times: num3 = num1 * num2
        CASE Divide: num3 = num1 / num2
    END SELECT
    QuestionsAsked = UBOUND(results) + 1
    REDIM _PRESERVE Results(QuestionsAsked) AS ResultsType
    Results(QuestionsAsked).num1 = num1
    Results(QuestionsAsked).op = op
    Results(QuestionsAsked).num2 = num2
    Results(QuestionsAsked).num3 = num3
    Results(QuestionsAsked).answer = ans

    _FONT RST
    COLOR White, Black
    LOCATE 1, 1: PRINT "Last Answer:"
    IF ans = num3 AND answer$ <> "" THEN
        Correct = Correct + 1
        GameTime = GameTime + 1 'A bonus to time for each correct answer
        COLOR Green, Black
        LOCATE 1, 400: PRINT "CORRECT!"
        Results(QuestionsAsked).correct = -1
    ELSE
        Wrong = Wrong + 1
        GameTime = GameTime - 2 'A penalty to time for each wrong answer
        COLOR Red, Black
        LOCATE 1, 400: PRINT "WRONG!  "
        Results(QuestionsAsked).correct = 0
        IF answer$ = "" THEN Results(QuestionsAsked).correct = 1
    END IF
    COLOR White, Black
    _FONT WF
    DisplayTime
    _DISPLAY
END SUB


SUB DisplayUserAnswer (answer$)
    COLOR Black, White
    LINE (700, 100)-(1100, 300), White, BF
    top = 125
    left = (400 - GetPrintWidth(answer$, WF)) / 2 + 700
    _PRINTSTRING (left, top), answer$
END SUB


SUB GetUserAnswer (answer$)
    done = 0: answer$ = ""
    DO
        _LIMIT 30
        Button = mouseclick
        IF Button = 1 THEN 'We have a mouse click somewhere
            x = _MOUSEX: y = _MOUSEY
            IF y >= 400 AND y <= 600 THEN 'they clicked down where the numbers and OK button are
                num = 0
                IF x \ 100 = (x + 10) \ 100 THEN num = x \ 100 'they didn't click on the 10 pixel blank space between numbers
                IF num > 0 AND num < 12 THEN 'It's a click on either a number or the OK button on the top row
                    IF num = 10 THEN num = 0
                    IF num = 11 THEN
                        answer$ = LEFT$(answer$, LEN(answer$) - 1) '  done = -1
                    ELSE
                        answer$ = answer$ + LTRIM$(RTRIM$(STR$(num)))
                    END IF
                END IF
            END IF
            IF y >= 610 AND y <= 710 AND x >= 100 AND x <= 1200 THEN done = -1 'Click on the bottom ENTER area
        END IF
        DisplayUserAnswer answer$
        DisplayTime
        _DISPLAY

        '>>> I tried the following but this inflates the wrong answers
        '>>> Why discourage a child with bad coding?
        'IF GameTime <= 0 THEN EXIT DO    '<<< Nope guarantees a wrong answer almost every time!

    LOOP UNTIL done
END SUB



SUB DrawUserClickAreas
    FOR i = 1 TO 10
        left = 100 * i
        LINE (left, 400)-(left + 90, 600), Yellow, BF
        COLOR Black, 0
        _PRINTSTRING (left + 10, 425), Num2Str$(i MOD 10)
    NEXT
    LINE (100, 610)-(1200, 710), Green, BF
    LINE (1100, 400)-(1200, 710), Green, BF
    _PRINTSTRING (1120, 425), "C"
    _PRINTSTRING (425, 595), "ENTER"
END SUB



SUB DrawCard (num1, operator, num2) 'Pass it our 3 numbers like 3, plus, 3 would be 3 + 3 = ?
    COLOR Red, 0
    LINE (100, 100)-(650, 300), White, BF
    text$ = Num2Str(num1)
    SELECT CASE operator
        CASE Plus: text$ = text$ + "+"
        CASE Minus: text$ = text$ + "-"
        CASE Times: text$ = text$ + "*"
        CASE Divide: text$ = text$ + "/"
    END SELECT
    text$ = text$ + Num2Str(num2) + "="
    top = 125
    left = (550 - GetPrintWidth(text$, WF)) / 2 + 100
    _PRINTSTRING (left, top), text$
END SUB

FUNCTION Num2Str$ (num)
    Num2Str$ = LTRIM$(RTRIM$(STR$(num)))
END FUNCTION

FUNCTION GetPrintWidth (text AS STRING, fonthandle)
    w = _WIDTH: h = _HEIGHT
    dc = _DEFAULTCOLOR: bg = _BACKGROUNDCOLOR
    d = _DEST: s = _SOURCE
    t = _NEWIMAGE(w, h, 32)
    _DEST t: _SOURCE t
    _FONT fonthandle
    COLOR _RGB32(255, 0, 0), _RGB32(255, 255, 255)
    PRINT text;
    DIM m AS _MEM
    m = _MEMIMAGE(t)
    p = 0 'pointer
    DO
        _MEMGET m, m.OFFSET + p, c 'Get our color
        p = p + 4 'increase 4 as we're working with 4-byte long values for RGBA color
        GetPrintWidth = GetPrintWidth + 1
    LOOP UNTIL NOT c 'until we don't have no color no more
    _MEMFREE m
    _FREEIMAGE t
    _DEST d: _SOURCE s
END FUNCTION

FUNCTION mouseclick%
    DO WHILE _MOUSEINPUT 'check mouse status
        scroll% = scroll% + _MOUSEWHEEL ' if scrollwheel changes, watch the change here
        IF _MOUSEBUTTON(1) THEN 'left mouse pushed down
            speedup = 1
        ELSEIF _MOUSEBUTTON(2) THEN 'right mouse pushed down
            speedup = 2
        ELSEIF _MOUSEBUTTON(3) THEN 'middle mouse pushed down
            speedup = 3
        END IF
        IF speedup THEN 'buton was pushed down
            mouseclickxxx1% = _MOUSEX: mouseclickyyy1% = _MOUSEY 'see where button was pushed down at
            DO WHILE _MOUSEBUTTON(speedup) 'while button is down
                i% = _MOUSEINPUT
            LOOP 'finishes when button is let up
            IF mouseclickxxx1% >= _MOUSEX - 2 AND mouseclickxxx1% <= _MOUSEX + 2 AND mouseclickyyy1% >= _MOUSEY - 2 AND mouseclickyyy1% <= _MOUSEY + 2 THEN 'if mouse hasn't moved (ie  clicked down, dragged somewhere, then released)
                mouseclick% = speedup
            ELSE
                mouseclick% = 0
            END IF
        END IF
    LOOP
    IF scroll% < 0 THEN mouseclick% = 4
    IF scroll% > 0 THEN mouseclick% = 5
END FUNCTION


