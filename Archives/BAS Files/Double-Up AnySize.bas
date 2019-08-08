_DEFINE A-Z AS _INTEGER64
DIM SHARED GameSize
$RESIZE:ON
_RESIZE , _SMOOTH
DO
    INPUT "How large a grid would you like to try your skill against?"; Gridsize
    IF Gridsize < 4 OR Gridsize > 10 THEN PRINT "Grid must between 4 an 10 tiles."
LOOP UNTIL Gridsize > 3 AND Gridsize < 11
GameSize = Gridsize


DIM SHARED Grid(0 TO GameSize + 1, 0 TO GameSize + 1) AS INTEGER
CONST Left = 19200
CONST Right = 19712
CONST Down = 20480
CONST Up = 18432
CONST ESC = 27
CONST LCtrl = 100306
CONST RCtrl = 100305

'The highscore IRC client shared variables
DIM SHARED Client AS LONG, Server AS STRING, Channel AS STRING
DIM SHARED Speaker AS STRING
DIM SHARED LastSpeaker AS STRING, nick$
DIM SHARED respond



Init
MakeNewGame
DO
    _LIMIT 30
    ShowGrid
    CheckInput flag
    IF flag THEN GetNextNumber
    _DISPLAY
LOOP





SUB CheckInput (flag)
flag = 0
k = _KEYHIT
SELECT CASE k
    CASE ESC: SYSTEM
    CASE 83, 115 'S
        IF _KEYDOWN(LCtrl) OR _KEYDOWN(RCtrl) THEN
            GetHighScores
            MakeNewGame
        END IF
    CASE Left
        MoveLeft
        flag = -1 'we hit a valid move key.  Even if we don't move, get a new number
    CASE Up
        MoveUp
        flag = -1
    CASE Down
        MoveDown
        flag = -1
    CASE Right
        MoveRight
        flag = -1
END SELECT
END SUB

SUB GetHighScores
_AUTODISPLAY
DC = _DEFAULTCOLOR: BG = _BACKGROUNDCOLOR
CLS
f& = _FONT
_FONT 16
DO: i$ = INKEY$: k = _KEYHIT: LOOP UNTIL k = 0 AND i$ = "" 'clear the keyboard buffer
INPUT "Please Enter your name =>"; player$

player$ = LTRIM$(UCASE$(player$))


CLS
FOR x = 1 TO GameSize
    FOR y = 1 TO GameSize
        IF Grid(x, y) > score THEN score = Grid(x, y)
    NEXT
NEXT
PRINT "Checking online for the high player list.  This might take a moment."

result$ = HighScore$("Double Up" + STR$(GameSize), player$, score)
PRINT "Finished Checking"
IF LEFT$(result$, 5) = "ERROR" THEN PRINT result$: CLOSE: GOTO finishup
_AUTODISPLAY
'COLOR DC, BG
OPEN "temp.txt" FOR OUTPUT AS #1: CLOSE #1 'make a blank file to start with
OPEN "temp.txt" FOR BINARY AS #1
PUT #1, , result$
CLOSE
OPEN "temp.txt" FOR INPUT AS #1
c = 0
CLS
PRINT "DOUBLE UP HIGH SCORES!"

DO
    c = c + 1
    INPUT #1, p$
    p$ = UCASE$(LTRIM$(RTRIM$(p$)))
    IF p$ = "SKB64 DONE" THEN EXIT DO
    INPUT #1, s
    PRINT STR$(c), p$, s
LOOP
CLOSE #1

finishup:
DO: i$ = INKEY$: k = _KEYHIT: LOOP UNTIL k = 0 AND i$ = "" 'clear the keyboard buffer

PRINT "Press <ANY KEY> to continue"
DO
    _LIMIT 30
    k = _KEYHIT
LOOP UNTIL k <> 0
CLS
_FONT f&
_DISPLAY
CLOSE Client
END SUB


SUB MoveDown
'first move everything left to cover the blank spaces
DO
    moved = 0
    FOR y = GameSize TO 1 STEP -1
        FOR x = 1 TO GameSize
            IF Grid(x, y) = 0 THEN 'every point above this moves down
                FOR j = y TO 1 STEP -1
                    Grid(x, j) = Grid(x, j - 1)
                    IF Grid(x, j) <> 0 THEN moved = -1
                NEXT
            END IF
        NEXT
    NEXT
    IF moved THEN y = y + 1 'recheck the same column
LOOP UNTIL NOT moved
FOR y = GameSize TO 1 STEP -1
    FOR x = 1 TO GameSize
        IF Grid(x, y) <> 0 AND Grid(x, y) = Grid(x, y - 1) THEN 'add them together and every point above this moves
            Grid(x, y) = Grid(x, y) * 2
            FOR j = y - 1 TO 1
                Grid(x, j) = Grid(x, j - 1)
            NEXT
        END IF
    NEXT
NEXT
END SUB



SUB MoveLeft
'first move everything to cover the blank spaces
DO
    moved = 0
    FOR x = 1 TO GameSize
        FOR y = 1 TO GameSize
            IF Grid(x, y) = 0 THEN 'every point right of this moves left
                FOR j = x TO GameSize
                    Grid(j, y) = Grid(j + 1, y)
                    IF Grid(j, y) <> 0 THEN moved = -1
                NEXT
            END IF
        NEXT
    NEXT
    IF moved THEN x = x - 1 'recheck the same row
LOOP UNTIL NOT moved
FOR x = 1 TO GameSize
    FOR y = 1 TO GameSize
        IF Grid(x, y) <> 0 AND Grid(x, y) = Grid(x + 1, y) THEN 'add them together and every point right of this moves left
            Grid(x, y) = Grid(x, y) * 2
            FOR j = x + 1 TO GameSize
                Grid(j, y) = Grid(j + 1, y)
            NEXT
        END IF
    NEXT
NEXT
END SUB

SUB MoveUp
'first move everything to cover the blank spaces
DO
    moved = 0
    FOR y = 1 TO GameSize
        FOR x = 1 TO GameSize
            IF Grid(x, y) = 0 THEN 'every point below of this moves up
                FOR j = y TO GameSize
                    Grid(x, j) = Grid(x, j + 1)
                    IF Grid(x, j) <> 0 THEN moved = -1
                NEXT
            END IF
        NEXT
    NEXT
    IF moved THEN y = y - 1 'recheck the same column
LOOP UNTIL NOT moved
FOR y = 1 TO GameSize
    FOR x = 1 TO GameSize
        IF Grid(x, y) <> 0 AND Grid(x, y) = Grid(x, y + 1) THEN 'add them together and every point below this moves
            Grid(x, y) = Grid(x, y) * 2
            FOR j = y + 1 TO GameSize
                Grid(x, j) = Grid(x, j + 1)
            NEXT
            Grid(x, GameSize) = 0
        END IF
    NEXT
NEXT
END SUB


SUB MoveRight
'first move everything to cover the blank spaces
DO
    moved = 0
    FOR x = GameSize TO 1 STEP -1
        FOR y = 1 TO GameSize
            IF Grid(x, y) = 0 THEN 'every point right of this moves left
                FOR j = x TO 1 STEP -1
                    Grid(j, y) = Grid(j - 1, y)
                    IF Grid(j, y) <> 0 THEN moved = -1
                NEXT
            END IF
        NEXT
    NEXT
    IF moved THEN x = x - 1 'recheck the same row
LOOP UNTIL NOT moved

FOR x = GameSize TO 1 STEP -1
    FOR y = 1 TO GameSize
        IF Grid(x, y) <> 0 AND Grid(x, y) = Grid(x - 1, y) THEN 'add them together and every point right of this moves left
            Grid(x, y) = Grid(x, y) * 2
            FOR j = x - 1 TO 1 STEP -1
                Grid(j, y) = Grid(j - 1, y)
            NEXT
        END IF
    NEXT
NEXT
END SUB



SUB ShowGrid
'SUB MakeBox (Mode AS INTEGER, x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER,
'Caption AS STRING, FontColor AS _UNSIGNED LONG, FontBackground AS _UNSIGNED LONG,
'BoxColor AS _UNSIGNED LONG, BoxHighLight AS _UNSIGNED LONG, XOffset AS INTEGER, YOffset AS INTEGER)
w = _WIDTH / GameSize
h = _HEIGHT / GameSize
FOR x = 1 TO GameSize
    FOR y = 1 TO GameSize
        t$ = LTRIM$(STR$(Grid(x, y)))
        IF t$ = "0" THEN t$ = ""
        MakeBox GameSize, (x - 1) * w, (y - 1) * h, w, h, t$, -1, 0, 0, -1, 0, 0
    NEXT
NEXT
END SUB



SUB Init
ws = _NEWIMAGE(640, 640, 32)
SCREEN ws
_DELAY 1
_TITLE "Anysize Double Up"
_SCREENMOVE _MIDDLE
RANDOMIZE TIMER
f& = _LOADFONT("C:\Windows\Fonts\courbd.ttf", 48 * 4 \ GameSize, "MONOSPACE")
_FONT f&

END SUB

SUB MakeNewGame
FOR x = 1 TO GameSize
    FOR y = 1 TO GameSize
        Grid(x, y) = 0
    NEXT
NEXT
GetNextNumber
GetNextNumber
END SUB

SUB GetNextNumber
FOR x = 1 TO GameSize
    FOR y = 1 TO GameSize
        IF Grid(x, y) = 0 THEN valid = -1
    NEXT
NEXT
IF valid THEN 'If all the grids are full, we can't add any more numbers
    'This doesn't mean the game is over, as the player may be able to
    DO
        x = _CEIL(RND * GameSize)
        y = _CEIL(RND * GameSize)
    LOOP UNTIL Grid(x, y) = 0
    Grid(x, y) = 2
END IF
END SUB

SUB MakeBox (Mode AS INTEGER, x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, Caption AS STRING, FontColor AS _UNSIGNED LONG, FontBackground AS _UNSIGNED LONG, BoxColor AS _UNSIGNED LONG, BoxHighLight AS _UNSIGNED LONG, XOffset AS INTEGER, YOffset AS INTEGER)

'This is an upgrade version of my original Button routine.
'It's more versitile (but complex) than the original.
'Mode 0 (or any unsupported number) will tell the box to size itself from X1,Y1 to X2,Y2
'Mode 1 will tell the box to autosize itself according to whatever text is placed within it.
'Mode 2 will tell the box to use X2 and Y2 as relative coordinates and not absolute coordinates.
'Mode 3 will tell the box to autocenter text with X2, Y2 being absolute coordinates.
'Mode GameSize will tell the box to autocenter text with X2, Y2 being relative coordinates.
'Mode otherwise is unused, but available for expanded functionality.
'X1 carries the X location of where we want to place our box on the screen.
'Y2 carries the Y location of where we want to place our box on the screen.
'X2 is the X boundry of our box on the screen, depending on our mode.
'Y2 is the Y boundry of our box on the screen, depending on our mode.

'Caption is the text that we want our box to contain.

'FontColor is our font color for our caption
'FontBackground is the font background color for our caption
'NOTE: IF FONTCOLOR OR FONTBACKGROUND IS SET TO ZERO, THEY WILL **NOT** AFFECT THE COLOR BEHIND THEM.
'This can be used to mimic the function of _KEEPBACKGROUND, _FILLBACKGROUND, or _ONLYBACKGROUND


'BoxColor is our box color
'BoxHighlight is our box highligh colors
'NOTE: SAME WITH BOXCOLOR AND BOXHIGHLIGHT.  IF SET TO ZERO, THEY WILL HAVE **NO** COLOR AT ALL TO THEM, AND WILL NOT AFFECT THE BACKGROUND OF ANYTHING BEHIND THEM.

'XOffset is used to offset our text # pixels from the X1 top.
'YOffset is used to offset our text # pixels from the Y1 top.
'These can be used to place our text wherever we want on our box.
'But remember, if Mode = 3 or GameSize, the box will autocenter the text and ignore these parameters completely.

DIM BoxBlack AS _UNSIGNED LONG

dc& = _DEFAULTCOLOR: bg& = _BACKGROUNDCOLOR
IF Black <> 0 THEN
    'We have black either as a CONST or a SHARED color
    BoxBlack = Black
ELSE
    'We need to define what Black is for our box.
    BoxBlack = _RGB32(0, 0, 0)
END IF

IF _FONTWIDTH <> 0 THEN cw = _FONTWIDTH * LEN(Caption) ELSE cw = _PRINTWIDTH(Caption)
ch = _FONTHEIGHT

tx1 = x1: tx2 = x2: ty1 = y1: ty2 = y2
SELECT CASE Mode
    CASE 0
        'We use the X2, Y2 coordinates provided as absolute coordinates
    CASE 1
        tx2 = tx1 + cw + 8
        ty2 = ty1 + ch + 8
        XOffset = 5: YOffset = 5
    CASE 2
        tx2 = tx1 + x2
        ty2 = ty1 + y2
    CASE 3
        XOffset = (tx2 - tx1 - cw) \ 2
        YOffset = (ty2 - ty1 - ch) \ 2
    CASE GameSize
        tx2 = tx1 + x2
        ty2 = ty1 + y2
        XOffset = (tx2 - tx1) \ 2 - cw \ 2
        YOffset = (ty2 - ty1 - ch) \ 2
END SELECT
LINE (tx1, ty1)-(tx2, ty2), BoxBlack, BF
LINE (tx1 + 1, ty1 + 1)-(tx2 - 1, ty2 - 1), BoxHighLight, B
LINE (tx1 + 2, ty1 + 2)-(tx2 - 2, ty2 - 2), BoxHighLight, B
LINE (tx1 + 3, ty1 + 3)-(tx2 - 3, ty2 - 3), BoxBlack, B
LINE (tx1, ty1)-(tx1 + 3, ty1 + 3), BoxBlack
LINE (tx2, ty1)-(tx2 - 3, ty1 + 3), BoxBlack
LINE (tx1, ty2)-(tx1 + 3, ty2 - 3), BoxBlack
LINE (tx2, ty2)-(tx2 - 3, ty2 - 3), BoxBlack
LINE (tx1 + 3, y1 + 3)-(tx2 - 3, ty2 - 3), BoxColor, BF
COLOR FontColor, FontBackground
_PRINTSTRING (tx1 + XOffset, ty1 + YOffset), Caption$
COLOR dc&, bg&
END SUB

FUNCTION HighScore$ (Game$, Player$, Score)
Score$ = LTRIM$(STR$(Score))

crlf$ = CHR$(13) + CHR$(10)

nick$ = "QB6GameSize_" + LTRIM$(STR$(INT(RND * 1000000)))
pass$ = ""
Server = "irc.DarkMyst.org"
Channel = "#S-Game"


PRINT "Connecting to High Score Server"
CLOSE #Client
Client = _OPENCLIENT("TCP/IP:6667:" + Server)


IF Client = 0 THEN HighScore$ = "ERROR: High Scores Not Availible at this moment.  Sorry.": EXIT FUNCTION
IF pass$ <> "" THEN SendInfo "PASS " + pass$
SendInfo "NICK " + nick$
SendInfo "USER " + nick$ + " 0 * : " + nick$
PRINT "Connected to Online ScoreKeeper Server"
t# = TIMER

PRINT "Waiting for PING to respond"
respond = 0
DO
    GET #Client&, , In$
    'IF In$ <> "" THEN PRINT In$
    l = INSTR(In$, "PING :")
    IF l THEN 'Respond with PONG
        res$ = "PONG " + MID$(In$, l + 5)
        'PRINT res$
        PUT #Client, , res$
        respond = -1
    END IF
    IF TIMER - t# > 15 THEN HighScore$ = "ERROR: High Scores Not Availible at this moment.  Sorry.": EXIT FUNCTION
LOOP UNTIL respond
PRINT "Responded with PONG"

In$ = ""

PRINT "Attempting to join Channel"
SendInfo "JOIN " + Channel
SendInfo "TOPIC " + Channel
PRINT "Joined proper Channel"


t# = TIMER
DO
    _LIMIT 10
    'COLOR 7
    GET #Client, , In$
    l = INSTR(In$, "PING :")
    IF l THEN 'Respond with PONG
        res$ = "PONG " + MID$(In$, l + 5)
        PRINT res$
        PUT #Client, , res$
        l = 0
    ELSEIF In$ <> "" THEN
        'PRINT LEFT$(In$, LEN(In$) - 2) 'Unremark this is we want to see what's being typed by everyone.
    END IF
    IF In$ <> "" AND respond THEN ProcessInput In$, Returned$
    IF INSTR(In$, "End of Mess") THEN respond = -3 'Don't start responding to the automatic server messages, like an idiot bot!
    IF respond = -2 THEN 'this will trigger as soon as we get our name list
        IF INSTR(In$, "SKB64") THEN respond = -3 ELSE HighScore$ = "ERROR: High Scores Not Availible at this moment.  Sorry.": EXIT FUNCTION
    END IF
    IF respond = -3 THEN 'that means we found the SKB64 scorekeeperbot-6GameSize
        PRINT "Sending Current Player Score"
        PrivateReply "HIGHSCORE <" + Game$ + "> " + Player$ + "," + Score$, "SKB64"
        respond = -GameSize 'We've sent our score
    END IF
    IF respond = -5 THEN
        PRINT "Retrieving High Score List"
        HighScore$ = Returned$
        temp$ = "QUIT :Finished Checking Scores ; " + nick$
        PUT #Client, , temp$
        EXIT FUNCTION
    END IF
    IF TIMER - t# > 15 THEN HighScore$ = "ERROR: High Scores Not Availible at this moment.  Sorry.": EXIT FUNCTION
LOOP


END SUB




'SUB PROCESS INPUT IS WHERE WE PROCESS THE INPUT.  PBBBTTTBT!
SUB ProcessInput (text$, Returned$)


Speaker$ = MID$(text$, 2, INSTR(text$, "!") - 2)
c$ = UCASE$(Channel) + " :"
In$ = UCASE$(LEFT$(text$, LEN(text$) - 2)) + " " ' Strip off the CRLF


L = INSTR(In$, "PRIVMSG")
t$ = LTRIM$(RTRIM$(MID$(In$, L + 8)))

l1 = LEN(nick$)
IF L = 0 THEN
    L = -INSTR(In$, "NOTICE")
    IF L THEN eval$ = " " + MID$(In$, INSTR(In$, nick$ + " :") + 2 + LEN(nick$)) + " " ELSE eval$ = In$
ELSE
    IF UCASE$(LEFT$(t$, LEN(nick$))) = nick$ THEN
        L = -L
        eval$ = " " + MID$(In$, INSTR(UCASE$(In$), nick$ + " :") + 2 + LEN(nick$)) + " "
    ELSE
        eval$ = " " + MID$(In$, INSTR(In$, c$) + LEN(c$)) + " "
    END IF
END IF


'PRINT eval$

eval$ = LTRIM$(eval$)

IF L AND Speaker$ = "SKB64" AND respond = -GameSize THEN 'It's a message from the scorekeeperbot 6GameSize!
    Returned$ = Returned$ + eval$
    IF INSTR(eval$, "SKB64 DONE") THEN respond = -5: EXIT SUB
ELSE 'It's not a message from a user, so it's probably a system message.
    'And this bott doesn't care a whitt about those.

END IF
IF out$ <> "" THEN
    'COLOR GameSize0
    PRINT Speaker$; " on "; MID$(In$, L + 8) 'I put a print here, so we can see what our bot is responding to, no matter what.
    PrivateReply out$, Speaker$
END IF
END SUB




SUB SendInfo (text$)
text$ = text$ + CHR$(13) + CHR$(10)
PUT #Client&, , text$
'COLOR GameSize: PRINT LEFT$(text$, LEN(text$) - 2)
END SUB



SUB SendReply (text$)
IF LEN(LTRIM$(RTRIM$(text$))) = 0 THEN EXIT SUB
limit = GameSize50
DO UNTIL LEN(text$) < limit
    f = limit: f$ = ""
    DO UNTIL f$ = " " OR f = 0
        f$ = MID$(text$, f, 1)
        IF f$ <> " " THEN f = f - 1
    LOOP
    IF f = 0 THEN f = limit
    t$ = "PRIVMSG " + Channel + " :" + LEFT$(text$, f) + CHR$(13) + CHR$(10)
    PUT #Client&, , t$
    text$ = MID$(text$, f + 1)
    'PRINT LEFT$(t$, LEN(t$) - 2)
    _DELAY 1
LOOP
t$ = "PRIVMSG " + Channel + " :" + text$ + CHR$(13) + CHR$(10)
PUT #Client&, , t$
'COLOR 1GameSize: PRINT LEFT$(t$, LEN(t$) - 2)
END SUB

'text$ = "CPRIVMSG " + person$ + " " + Channel + " :" + text$ + CHR$(13) + CHR$(10)
SUB PrivateReply (text$, person$)
IF LEN(LTRIM$(RTRIM$(text$))) = 0 THEN EXIT SUB
'COLOR 1GameSize
limit = GameSize50
DO UNTIL LEN(text$) < limit
    f = limit: f$ = ""
    DO UNTIL f$ = " " OR f = 0
        f$ = MID$(text$, f, 1)
        IF f$ <> " " THEN f = f - 1
    LOOP
    IF f = 0 THEN f = limit
    t$ = "PRIVMSG " + person$ + " :" + LEFT$(text$, f) + CHR$(13) + CHR$(10)
    PUT #Client&, , t$
    text$ = MID$(text$, f + 1)
    'PRINT LEFT$(t$, LEN(t$) - 2)
    _DELAY 1
LOOP
t$ = "PRIVMSG " + person$ + " :" + text$ + CHR$(13) + CHR$(10)
PUT #Client&, , t$
'PRINT LEFT$(t$, LEN(t$) - 2)
END SUB

