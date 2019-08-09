CONST TopLine = "       ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
CONST MidLine = "       บ                                                               บ"
CONST BotLine = "       ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"

CONST BoldNumbers = -1 'Change to 0 for what I consider more of a SCREEN 0 look

CONST TopNewsFeed = "http://feeds.reuters.com/reuters/topNews"


_TITLE "Steve's Screen 0 News Clock"

GetTopNews t$
news$ = ParseNews(t$)

DO
    _LIMIT 1
    CLS
    COLOR 1
    LOCATE 5
    PRINT TopLine
    FOR i = 1 TO 10
        PRINT MidLine
    NEXT
    PRINT BotLine

    TW = 8 * _FONTWIDTH
    TH = _FONTHEIGHT

    temp& = _NEWIMAGE(TW, TH, 256)
    _DEST temp&
    LOCATE 1, 1
    PRINT TIME$;
    _DEST 0
    _SOURCE temp&

    COLOR 14
    FOR x = 0 TO TW - 1
        FOR y = 0 TO TH - 1
            IF POINT(x, y) THEN
                LOCATE y + 4, x + 9
                IF BoldNumbers THEN
                    PRINT "Û";
                ELSE
                    PRINT "*";
                END IF
            END IF
        NEXT
    NEXT
    _SOURCE 0
    _FREEIMAGE temp&
    IF TIMER MOD 5 = 0 THEN news$ = ParseNews(t$)
    COLOR 15
    LOCATE 18, 1
    PRINT news$;
    IF news$ = "" THEN GetTopNews t$
    _DISPLAY
LOOP UNTIL INKEY$ <> ""
SYSTEM

SUB GetTopNews (text$)
    link$ = TopNewsFeed
    DIM l AS _INTEGER64, lod AS _INTEGER64

    url2$ = RTRIM$(LTRIM$(link$))
    url4$ = RTRIM$(LTRIM$(link$))
    IF LEFT$(UCASE$(url2$), 7) = "HTTP://" THEN url4$ = MID$(url2$, 8)
    x = INSTR(url4$, "/")
    IF x THEN url2$ = LEFT$(url4$, x - 1)
    NewsClient = _OPENCLIENT("TCP/IP:80:" + url2$)
    IF NewsClient = 0 THEN EXIT FUNCTION
    e$ = CHR$(13) + CHR$(10) ' end of line characters
    url3$ = RIGHT$(url4$, LEN(url4$) - x + 1)
    x$ = "GET " + url3$ + " HTTP/1.1" + e$
    x$ = x$ + "Host: " + url2$ + e$ + e$
    PUT #NewsClient, , x$

    text$ = ""

    t! = TIMER ' start time
    head$ = ""
    cont_type$ = ""
    FOR i = 1 TO 25 'usually 25 different newsheadlines
        _LIMIT 100
        GET #NewsClient, , a$
        IF LTRIM$(a$) > "" THEN text$ = text$ + a$
    NEXT
    CLOSE #NewsClient

END SUB




FUNCTION ParseNews$ (text$)

    DO
        l = INSTR(text$, CHR$(10))
        newsfeed$ = LEFT$(text$, l - 2)
        text$ = MID$(text$, l + 1)
        IF l THEN
            t = INSTR(UCASE$(newsfeed$), "<TITLE>")
            IF t THEN
                title$ = UCASE$(MID$(newsfeed$, t + 7))
                t1 = INSTR(UCASE$(title$), "</TITLE>")
                IF t1 THEN title$ = LEFT$(title$, t1 - 1)
                ParseNews$ = title$ + CHR$(13)
            END IF
            t = INSTR(UCASE$(newsfeed$), "<LINK>")
            IF t THEN
                link$ = MID$(newsfeed$, t + 6)
                t1 = INSTR(UCASE$(link$), "</LINK>")
                IF t1 THEN link$ = LEFT$(link$, t1 - 1)
                ParseNews$ = ParseNews$ + link$ + CHR$(13)
            END IF
            t = INSTR(UCASE$(newsfeed$), "<DESC")
            IF t THEN
                desc$ = MID$(newsfeed$, t + 13) ', t1 - t - 13)
                DO
                    good = -1
                    DO UNTIL UCASE$(LEFT$(desc$, 4)) <> "<" AND UCASE$(LEFT$(desc$, 4)) <> "P>"
                        desc$ = MID$(desc$, 5): good = 0
                    LOOP
                    DO UNTIL UCASE$(LEFT$(desc$, 5)) <> "P>"
                        desc$ = MID$(desc$, 6): good = 0
                    LOOP
                LOOP UNTIL good
                eod = 0
                DO UNTIL eod
                    t1 = INSTR(UCASE$(desc$), "</DESC"): eod = -1
                    IF t1 THEN desc$ = LEFT$(desc$, t1 - 1)
                    t1 = INSTR(UCASE$(desc$), "<"): eod = -1
                    IF t1 THEN desc$ = LEFT$(desc$, t1 - 1)
                    ParseNews$ = ParseNews$ + desc$ + CHR$(13)
                LOOP
            END IF
        END IF
    LOOP UNTIL l = 0 OR l >= LEN(text$) OR eod
END SUB

