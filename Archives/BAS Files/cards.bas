DEFLNG A-Z
DIM SHARED Deck(0 TO 51), Hand(0 TO 4)
DIM SHARED OldFont, NewFont
DIM SHARED user$, wealth, bet, bet$
DIM SHARED GCHW

OldFont = _LOADFONT("C:\Windows\Fonts\OLDENGL.TTF", 38)
NewFont = _LOADFONT("C:\Windows\Fonts\courbd.ttf", 18)
Init
LogIn
DO
    AnteUp
    InitialHand
    DisplayCards
    SwapCards
    DisplayCards
    EvaluateHand
LOOP

SUB EvaluateHand
DIM CardCounter(12), SuitCount(3)

bet = VAL(MID$(bet$, 2))
wealth = wealth - bet
CLS
ShowBet
DisplayCards
FOR i = 0 TO 4
    suit = Hand(i) \ 13
    face = Hand(i) MOD 13
    CardCounter(face) = CardCounter(face) + 1
    SuitCount(suit) = SuitCount(suit) + 1
NEXT
'First let's look for 4 of a kind.
FOR i = 0 TO 12
    IF CardCounter(i) = 4 THEN
        winnings = bet * 50 + bet
        result$ = "Four of a Kind!  You win $" + LTRIM$(STR$(winnings))
        GOTO results
    END IF
NEXT
'Then a Full House, Three of Kind, 2 Pairs, and Pair all get checked together
FOR i = 0 TO 12
    IF CardCounter(i) = 3 THEN three = -1
    IF CardCounter(i) = 2 THEN two = -1: twocount = twocount + 1
NEXT
IF three AND two THEN 'full house
    winnings = bet * 10 + bet
    result$ = "Full House!  You win $" + LTRIM$(STR$(winnings))
    GOTO results
END IF
IF three THEN '3 of a Kind
    winnings = bet * 3 + bet
    result$ = "Three of a Kind!  You win $" + LTRIM$(STR$(winnings))
    GOTO results
END IF
IF twocount = 2 THEN '2 pair!
    winnings = bet * 2 + bet
    result$ = "Two Pair!  You win $" + LTRIM$(STR$(winnings))
    GOTO results
END IF
IF two THEN 'pair
    winnings = bet + bet
    result$ = "Pair!  You win $" + LTRIM$(STR$(winnings))
    GOTO results
END IF

'Now let's check for flushes
FOR i = 0 TO 3
    IF SuitCount(i) = 5 THEN 'We have a flush of some sort
        IF CardCounter(0) AND CardCounter(12) AND CardCounter(11) AND CardCounter(10) AND CardCounter(9) THEN
            'it's a royal flush
            winnings = bet * 500 + bet
            result$ = "Royal Flush!  You win $" + LTRIM$(STR$(winnings))
            GOTO results
        ELSE
            flush = -1
        END IF
    END IF
NEXT
'And now to check for straights
FOR i = 0 TO 8
    IF CardCounter(i) AND CardCounter(i + 1) AND CardCounter(i + 2) AND CardCounter(i + 3) AND CardCounter(i + 4) THEN
        straight = -1
        EXIT FOR
    END IF
    IF CardCounter(0) AND CardCounter(12) AND CardCounter(11) AND CardCounter(10) AND CardCounter(9) THEN straight = -1
NEXT

IF straight AND flush THEN
    winnings = bet * 100 + bet
    result$ = "Straight Flush!  You win $" + LTRIM$(STR$(winnings))
    GOTO results
END IF
IF straight THEN
    winnings = bet * 5 + bet
    result$ = "Straight!  You win $" + LTRIM$(STR$(winnings))
    GOTO results
END IF
IF flush THEN
    winnings = bet * 8 + bet
    result$ = "Flush!  You win $" + LTRIM$(STR$(winnings))
    GOTO results
END IF

result$ = "No Match!"

results:
wealth = wealth + winnings
x1 = Position * 75 + 25: x2 = 72
y1 = 125: y2 = 96
_FONT NewFont
MakeBox 4, 25, 265, 372, 30, result$, _RGB32(255, 255, 0), 0, _RGB32(255, 0, 128), 0, 0, 0
_FONT 16
DO
    x = MouseClick
LOOP UNTIL x
WHILE _MOUSEINPUT: WEND
Shuffle
END SUB

SUB ShowBet
_FONT OldFont
COLOR _RGB32(255, 0, 128), 0
_PRINTSTRING (100, 10), user$
COLOR _RGB32(0, 255, 0), 0
_PRINTSTRING (350, 10), bet$
_FONT 16
END SUB


SUB AnteUp
CLS
_FONT OldFont
COLOR _RGB32(255, 0, 128), 0
_PRINTSTRING (100, 10), user$
_PRINTSTRING (350, 10), "$" + LTRIM$(STR$(wealth))
_FONT 16

DIM caption$(4)
_FONT NewFont
caption$(0) = "$5": caption$(1) = "$10": caption$(2) = "$25"
caption$(3) = "$100": caption$(4) = "$250"
FOR Position = 0 TO 4
    x1 = Position * 75 + 25: x2 = 72
    y1 = 125: y2 = 96
    MakeBox 4, x1, y1, x2, y2, caption$(Position), _RGB32(0, 0, 0), 0, _RGB32(0, 255, 0), 0, 0, 0
NEXT
MakeBox 4, 400, y1, x2, y2, "Quit", _RGB32(0, 0, 0), 0, _RGB32(255, 0, 0), 0, 0, 0
_FONT OldFont
_PRINTSTRING (200, 300), "Place Bet"
_FONT 16
DO
    click = MouseClick: x = _MOUSEX: y = _MOUSEY
    IF click AND y >= 125 AND y <= 197 THEN
        'someone clicked the mouse in the proper x-area for a toggle
        IF x > 25 AND x < 475 THEN
            x = x - 25
            x1 = x \ 75: x2 = x MOD 75
            IF x2 > 0 AND x2 < 72 THEN 'It's in a box and not a blank space between them
                bet = x1
                IF bet = 5 THEN QuitGame
                EXIT DO
            END IF
        END IF
    END IF
LOOP
bet$ = caption$(bet)
CLS
ShowBet
END SUB

SUB QuitGame
file$ = "Card Player List.txt"
file1$ = "Card Player List.bak"
OPEN file$ FOR INPUT AS #1
OPEN file1$ FOR OUTPUT AS #2
DO
    Startbyte = SEEK(1) + 1
    INPUT #1, temp$, temp
    IF LCASE$(user$) = LCASE$(temp$) THEN
        WRITE #2, user$, wealth
    ELSE
        WRITE #2, temp$, temp
    END IF
LOOP UNTIL EOF(1)
CLOSE
KILL file$
NAME file1$ AS file$
SYSTEM
END SUB

SUB LogIn
_FONT OldFont
MakeBox 4, 250, 220, 200, 40, "Log In", _RGB32(0, 0, 0), 0, _RGB32(255, 0, 0), 0, 0, 0
DO
    DO: LOOP WHILE _MOUSEINPUT
    _LIMIT 30
LOOP UNTIL _KEYHIT OR _MOUSEBUTTON(1)
CLS
_FONT NewFont

DO
    k = _KEYHIT
    IF k <> 0 THEN
        IF k = 8 THEN text$ = LEFT$(text$, LEN(text$) - 1)
        IF k > 31 AND k < 127 AND LEN(text$) < 17 THEN text$ = text$ + CHR$(k)
        IF k = 13 THEN EXIT DO
    END IF
    MakeBox 4, 250, 220, 200, 40, text$, _RGB32(0, 0, 0), 0, _RGB32(255, 0, 0), 0, 0, 0
    _DISPLAY
LOOP
_FONT 16

CLS
file$ = "Card Player List.txt"
IF _FILEEXISTS(file$) THEN
    'it's all good
    OPEN file$ FOR INPUT AS #1
    DO
        INPUT #1, user$, wealth
        IF LCASE$(user$) = LCASE$(text$) THEN userfound = -1: EXIT DO
    LOOP UNTIL EOF(1)
    CLOSE
    IF userfound = 0 THEN
        OPEN file$ FOR APPEND AS #1
        WRITE #1, text$, 500.00
        CLOSE
        wealth = 500
    END IF
ELSE
    OPEN file$ FOR OUTPUT AS #1
    WRITE #1, text$, 500.00
    CLOSE
    user$ = text$: wealth = 500
END IF
IF wealth = 0 THEN wealth = 500 'in case someone quit with no money in the bank, we give them a new start.
_AUTODISPLAY
END SUB


SUB SwapCards
DIM Discard(4)
MakeBox 4, 25, 265, 372, 30, "Do It!", _RGB32(255, 255, 0), 0, _RGB32(255, 0, 128), 0, 0, 0

DO
    _LIMIT 30

    FOR i = 0 TO 4 'draw the 5 discard buttons
        x1 = i * 75 + 25: x2 = x1 + 72
        y1 = 230: y2 = 260
        IF Discard(i) THEN
            MakeBox 3, x1, y1, x2, y2, "Keep", _RGB32(0, 0, 0), 0, _RGB32(0, 255, 0), _RGB32(0, 0, 128), 0, 0
        ELSE
            MakeBox 3, x1, y1, x2, y2, "Discard", _RGB32(255, 255, 0), 0, _RGB32(255, 0, 0), _RGB32(0, 0, 128), 0, 0
        END IF
    NEXT

    click = MouseClick: x = _MOUSEX: y = _MOUSEY
    IF click AND x >= 25 AND x <= 397 THEN
        'someone clicked the mouse in the proper x-area for a toggle
        SELECT CASE y
            CASE 265 TO 295 'This is the Do It! area, so let's leave the loop and do whatever we need to do!
                EXIT DO
            CASE 230 TO 260 'This is down where the button area is.  Let's see which one got clicked.
                x = x - 25
                x1 = x \ 75: x2 = x MOD 75
                IF x2 > 0 AND x2 < 72 THEN 'It's in a box and not a blank space between them
                    Discard(x1) = NOT Discard(x1)
                END IF
        END SELECT
    END IF
    FOR i = 0 TO 4
        IF Discard(i) THEN _PUTIMAGE (i * 75 + 25, 125)-(i * 75 + 97, 221), GCHW
    NEXT
    _DISPLAY
LOOP
_AUTODISPLAY
nextcard = 5
FOR i = O TO 4
    IF Discard(i) THEN Hand(i) = Deck(nextcard): nextcard = nextcard + 1
NEXT
END SUB



SUB InitialHand
FOR i = 0 TO 4
    Hand(i) = Deck(i)
NEXT
END SUB


SUB DisplayCards
SHARED cardimage

cardwidth = 72
cardheight = 96
FOR Position = 0 TO 4
    suit = Hand(Position) \ 13
    face = Hand(Position) MOD 13

    x1 = Position * 75 + 25: x2 = x1 + cardwidth
    y1 = 125: y2 = y1 + cardheight
    sx1 = face * cardwidth: sx2 = sx1 + cardwidth
    sy1 = suit * cardheight: sy2 = sy1 + cardheight
    _PUTIMAGE (x1, y1)-(x2, y2), cardimage, ws, (sx1, sy1)-(sx2, sy2)
NEXT
END SUB



SUB Init
SHARED ws, cardimage
ws = _NEWIMAGE(640, 480, 32)
SCREEN ws
_DELAY .5
_TITLE "Steve's Poker Playhouse"
_SCREENMOVE _MIDDLE
RANDOMIZE TIMER
cardimage = _LOADIMAGE("Cards.bmp", 32) '936 x 384
FOR i = 0 TO 51: Deck(i) = i: NEXT
Shuffle
_DISPLAYORDER _SOFTWARE , _HARDWARE
GrayCard = _NEWIMAGE(72, 96, 32)
D = _DEST: _DEST GrayCard: CLS , _RGBA32(255, 255, 0, 128): _DEST D
GCHW = _COPYIMAGE(GrayCard, 33)
_FREEIMAGE GrayCard

END SUB


SUB Shuffle
FOR j = 1 TO 10 'Shuffle the deck 10 times to get it all nice and mixed up.
    FOR i = LBOUND(deck) TO UBOUND(deck)
        x = _CEIL(RND * UBOUND(deck))
        SWAP Deck(i), Deck(x)
NEXT i, j
END SUB


SUB MakeBox (Mode AS INTEGER, x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, Caption AS STRING, FontColor AS _UNSIGNED LONG, FontBackground AS _UNSIGNED LONG, BoxColor AS _UNSIGNED LONG, BoxHighLight AS _UNSIGNED LONG, XOffset AS INTEGER, YOffset AS INTEGER)

'This is an upgrade version of my original Button routine.
'It's more versitile (but complex) than the original.
'Mode 0 (or any unsupported number) will tell the box to size itself from X1,Y1 to X2,Y2
'Mode 1 will tell the box to autosize itself according to whatever text is placed within it.
'Mode 2 will tell the box to use X2 and Y2 as relative coordinates and not absolute coordinates.
'Mode 3 will tell the box to autocenter text with X2, Y2 being absolute coordinates.
'Mode 4 will tell the box to autocenter text with X2, Y2 being relative coordinates.
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
'But remember, if Mode = 3 or 4, the box will autocenter the text and ignore these parameters completely.

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
    CASE 4
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

FUNCTION MouseClick%
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
            MouseClick% = speedup
        ELSE
            MouseClick% = 0
        END IF
    END IF
LOOP
IF scroll% < 0 THEN MouseClick% = 4
IF scroll% > 0 THEN MouseClick% = 5
END FUNCTION

