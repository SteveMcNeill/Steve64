ConfigFile$ = "config.txt"
ConfigBak$ = "config.bak"


WriteConfigSetting "'[COLOR SETTING]", "Background", "_RGB32(255,0,255)"
DisplayConfigFile
WriteConfigSetting "'[COLOR SETTING]", "TextColor", "_RGB32(255,0,0)"
DisplayConfigFile
WriteConfigSetting "'[COLOR SETTING]", "Background", "_RGB32(255,0,0) 'REDONE"
DisplayConfigFile
WriteConfigSetting "'[COLOR SETTING]", "TextColor", "_RGB32(255,0,255) 'REDONE ALSO"
DisplayConfigFile
PRINT
PRINT "============================================="
PRINT
work = ReadConfigSetting("TextColor", value$)
IF work THEN
    PRINT "Textcolor value found.  It's: "; value$
ELSE
    PRINT "No textcolor found"
END IF

SUB DisplayConfigFile
    SHARED ConfigFile$
    InFile = FREEFILE: OPEN ConfigFile$ FOR BINARY AS #InFile
    IF LOF(InFile) THEN
        DO UNTIL EOF(InFile)
            LINE INPUT #InFile, junk$
            PRINT junk$
        LOOP
    ELSE
        KILL "config.txt" 'remove the blank file from the drive
        PRINT "No config file found."
    END IF
    CLOSE InFile
END SUB

SUB WriteConfigSetting (heading$, item$, value$)
    SHARED ConfigFile$, ConfigBak$
    DIM CRLF AS STRING
    IF INSTR(_OS$, "WIN") THEN CRLF = CHR$(13) + CHR$(10) ELSE CRLF = CHR$(10)
    _TITLE STR$(LEN(CRLF))

    InFile = FREEFILE: OPEN ConfigFile$ FOR BINARY AS #InFile
    OutFile = FREEFILE: OPEN ConfigBak$ FOR OUTPUT AS #OutFile
    placed = 0
    IF LOF(InFile) THEN
        DO UNTIL EOF(InFile)
            LINE INPUT #InFile, junk$
            'we really don't care about heading$ here; it's only used to make things easier for the user to locate in the config file
            junk$ = LTRIM$(RTRIM$(junk$))
            IF _STRICMP(LEFT$(junk$, LEN(item$)), item$) = 0 THEN
                PRINT #OutFile, item$; " = "; value$
                placed = -1
            ELSE
                PRINT #OutFile, junk$
            END IF
        LOOP
    END IF

    CLOSE #InFile, #OutFile
    IF NOT placed THEN 'we didn't find the proper setting already in the file somewhere.
        'Either the file was corrupted, or the user deleted this particulat setting sometime in the past.
        'Now we look to see if the heading exists in the file or not.
        'If it does, then we place the new setting under that heading.
        'If not then we write that heading to the end of the file to make it easier for the user to locate in the future
        'and then we write it below there.
        OPEN ConfigBak$ FOR BINARY AS #InFile
        l = LOF(InFile)
        out$ = item$ + " = " + value$ + CRLF
        temp$ = SPACE$(l)
        GET #InFile, 1, temp$

        l1 = INSTR(temp$, heading$)
        IF l1 THEN
            l1 = l1 + LEN(heading$) + LEN(CRLF)
            PUT #InFile, l1, out$
            r$ = MID$(temp$, l1)
            PUT #InFile, , r$
            placed = -1
        END IF
        IF NOT placed THEN
            PUT #InFile, l + 1, CRLF
            PUT #InFile, , heading$
            PUT #InFile, , CRLF
            PUT #InFile, , out$
            PUT #InFile, , CRLF
        END IF
        CLOSE InFile
    END IF
    KILL ConfigFile$
    NAME ConfigBak$ AS ConfigFile$
END SUB

FUNCTION ReadConfigSetting (item$, value$)
    SHARED ConfigFile$
    value$ = "" 'We start by blanking the value$ as a default return state
    InFile = FREEFILE: OPEN ConfigFile$ FOR BINARY AS #InFile
    IF LOF(InFile) THEN
        found = 0
        DO UNTIL EOF(InFile)
            LINE INPUT #InFile, temp$
            temp$ = LTRIM$(RTRIM$(temp$))
            IF LEFT$(UCASE$(temp$), LEN(item$)) = UCASE$(item$) THEN found = -1: EXIT DO
        LOOP
        CLOSE InFile
        IF found THEN 'we found what we're looking for
            l = INSTR(temp$, "=") 'return the value after the = sign
            IF l THEN
                value$ = MID$(temp$, l + 1)
                l = INSTR(value$, CHR$(13)) 'we only want what's before a CR
                IF l THEN value$ = LEFT$(value$, l)
                l = INSTR(value$, CHR$(10)) 'or a LineFeed
                'These are basic text files; they shouldn't have stray CHR$(10) or CHR$(13) characters in them!
                IF l THEN value$ = LEFT$(value$, l)
                value$ = LTRIM$(RTRIM$(value$))
                ReadConfigSetting = -1
                EXIT FUNCTION
            END IF
        END IF
    END IF
    ReadConfigSetting = 0 'failed to find the setting
END FUNCTION

