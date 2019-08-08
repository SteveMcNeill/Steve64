DECLARE DYNAMIC LIBRARY "user32"
    FUNCTION SystemParametersInfoA& (BYVAL uiAction~&, BYVAL uiParam~&, pvParam$, BYVAL fWinlni~&)
END DECLARE

CONST SPI_SETDESKTOPWALLPAPER = &H0014
CONST SPI_UPDATEINIFILE = &H0001

CONST WaitBetweenChanges = 5 'seconds
CONST PhotoFolder = "C:\Users\Public\Pictures\"
CONST ScaleMode = 1 '0 to center, 1 to stretch while maintaining aspect ratio, 2 to stretch to fill screen
CONST AlwaysRefreshListing = 0 'If -1 then we'll always update the photo listing each time we run the program.  If 0, we use the existing list and save needing to create a new one each run.
'The following settings are for calendar support, if wanted

CONST DrawCalander = -1 'draw the calender overlay, or not
CONST OffsetW = 10, OffsetH = 50 'Offset from bottom right corner of the screen.
'                                 Change OffsetW to move the calendar further to the left of your screen (W = Width)
'                                 Change OffsetH to move the calendar further up on your screen. (H = Height)
CONST Kolor = _RGBA32(255, 255, 255, 150) 'The color of the calendar and the text
CONST BackKolor = _RGBA32(0, 0, 255, 150) 'The color of the background which we see under the month

IF _FILEEXISTS("C:\ProgramData\PhotoList.txt") = 0 OR AlwaysRefreshListing THEN
    'will create the new listing if your directory doesn't contain one,
    'OR, will create one every time you run the program, if you set the flag to make it do so.
    PhotoList$ = PhotoFolder + "*.bmp " + PhotoFolder + "*.jpg " + PhotoFolder + "*.png " + PhotoFolder + "*.gif "
    SHELL "DIR " + PhotoList$ + "/b /s /a-d >C:\ProgramData\PhotoList.txt"
END IF

OPEN "C:\ProgramData\PhotoList.txt" FOR BINARY AS #1
DO UNTIL EOF(1)
    LINE INPUT #1, junk$
    PhotoCount = PhotoCount + 1
LOOP

SEEK #1, 1 'back to the beginning

DIM FileList(PhotoCount) AS STRING
FOR i = 1 TO PhotoCount
    LINE INPUT #1, FileList(i)
    FileList(i) = FileList(i) + CHR$(0)
NEXT

RANDOMIZE TIMER

SCREEN _NEWIMAGE(_DESKTOPWIDTH, _DESKTOPHEIGHT, 32)
'SCREEN _NEWIMAGE(640, 480, 32)
_SCREENMOVE _MIDDLE
_SCREENHIDE

_TITLE "Wallpaper Changer"

temp$ = "C:\ProgramData\Wallpaper Changer Image.bmp"

Today$ = DATE$ 'mm-dd-yyyy
Day = VAL(MID$(Today$, 4, 2))
Month = VAL(Today$)
Year = VAL(MID$(Today$, 7))
FirstDay = GetDay(Month, 1, Year)
SELECT CASE Month
    CASE 1, 3, 5, 7, 8, 10, 12
        DaysInMonth = 31
    CASE 2
        DaysInMonth = 28 'need to add leap year later.
    CASE 4, 6, 9, 11
        DaysInMonth = 30
END SELECT
SELECT CASE Month
    CASE 1: Month$ = "January"
    CASE 2: Month$ = "February"
    CASE 3: Month$ = "March"
    CASE 4: Month$ = "April"
    CASE 5: Month$ = "May"
    CASE 6: Month$ = "June"
    CASE 7: Month$ = "July"
    CASE 8: Month$ = "August"
    CASE 9: Month$ = "September"
    CASE 10: Month$ = "October"
    CASE 11: Month$ = "November"
    CASE 12: Month$ = "December"
END SELECT


_FONT 8
COLOR Kolor, 0

DO
    CLS
    loops = 0
    DO
        f = INT(RND * PhotoCount) + 1
        f$ = FileList(f)
        IF _FILEEXISTS(f$) THEN 'try a few times in case invalid files (like TXT files) are in the list.
            'I was lazy and didn't bother to just search for image files after all...
            f = _LOADIMAGE(f$, 32)
            IF f <> -1 THEN
                w = _WIDTH(f): h = _HEIGHT(f)
                scalew = _WIDTH / w: scaleh = _HEIGHT / h
                SELECT CASE ScaleMode
                    CASE 0
                        _PUTIMAGE ((_WIDTH - w) \ 2, (_HEIGHT - h) \ 2)-STEP(w, h), f
                    CASE 1
                        IF scalew < scaleh THEN scale = scalew ELSE scale = scaleh
                        w1 = w * scale: h1 = h * scale
                        _PUTIMAGE ((_WIDTH - w1) \ 2, (_HEIGHT - h1) \ 2)-STEP(w1, h1), f
                    CASE 2
                        _PUTIMAGE , f
                END SELECT

                IF DrawCalander THEN
                    Day = VAL(MID$(DATE$, 4, 2))
                    LINE (_WIDTH - OffsetW, _HEIGHT - OffsetH)-STEP(-175, -140), Kolor, B
                    FOR i = 1 TO 5
                        LINE (_WIDTH - OffsetW - 1, _HEIGHT - OffsetH - 20 * i)-STEP(-173, 0), Kolor
                    NEXT
                    FOR i = 1 TO 7
                        LINE (_WIDTH - OffsetW - 1 - i * 25, _HEIGHT - OffsetH - 1)-STEP(0, -118), Kolor
                    NEXT
                    LINE (_WIDTH - OffsetW, _HEIGHT - OffsetH - 120)-STEP(-175, 0), Kolor, B
                    LINE (_WIDTH - OffsetW - 1, _HEIGHT - OffsetH - 121)-STEP(-173, -18), BackKolor, BF
                    count = 0
                    FOR y = 1 TO 6 'weeks
                        FOR x = 1 TO 7 'days
                            IF (y - 1) * 7 + x >= FirstDay THEN
                                count = count + 1
                                IF count <= DaysInMonth THEN
                                    T$ = _TRIM$(STR$(count))
                                    IF count = Day THEN
                                        LINE (_WIDTH - OffsetW - 175 + (x - 1) * 25, _HEIGHT - OffsetH - 120 + (y - 1) * 20)-STEP(23, 18), BackKolor, BF
                                    END IF
                                    _PRINTSTRING (_WIDTH - OffsetW - 163 + (x - 1) * 25 - _PRINTWIDTH(T$) \ 2, _HEIGHT - OffsetH - 116 + (y - 1) * 20), T$
                                END IF
                            END IF
                        NEXT
                    NEXT
                    T$ = Month$ + STR$(Year)
                    _PRINTSTRING (_WIDTH - OffsetW - 87 - _PRINTWIDTH(T$) \ 2, _HEIGHT - OffsetH - 135), T$
                END IF

                SaveBMP temp$, 0, 0, 0, _WIDTH - 1, _HEIGHT - 1
                result = SystemParametersInfoA&(SPI_SETDESKTOPWALLPAPER, 0, temp$ + CHR$(0), SPI_UPDATEINIFILE)
                result = -1
                _FREEIMAGE f
            ELSE
                loops = loops + 1
            END IF
        END IF
    LOOP UNTIL result OR loops > 100
    IF loops > 100 THEN PRINT "ERROR: Over 100 failures and no success... Terminating.": END
    PRINT "Current Background: "; f$
    _DELAY WaitBetweenChanges
LOOP

SUB SaveBMP (filename$, image&, x1%, y1%, x2%, y2%)
    'Super special STEVE-Approved BMP Export routine for use with any QB64 graphic mode.
    IF x2% = _WIDTH(image&) THEN x2% = x2% - 1
    IF y2% = _HEIGHT(image&) THEN y2% = y2% - 1

    IF _PIXELSIZE(image&) = 0 THEN
        IF SaveTextAs256Color THEN
            tempimage& = TextScreenToImage256&(image&)
        ELSE
            tempimage& = TextScreenToImage32&(image&)
        END IF
        F = _FONT(image&)
        FW = _FONTWIDTH(F): FH = _FONTHEIGHT(F)
        SaveBMP filename$, tempimage&, x1% * FW, y1% * FH, x2% * FW, y2% * FH
        _FREEIMAGE tempimage&
        EXIT FUNCTION
    END IF

    TYPE BMPFormat
        ID AS STRING * 2
        Size AS LONG
        Blank AS LONG
        Offset AS LONG
        Hsize AS LONG
        PWidth AS LONG
        PDepth AS LONG
        Planes AS INTEGER
        BPP AS INTEGER
        Compression AS LONG
        ImageBytes AS LONG
        Xres AS LONG
        Yres AS LONG
        NumColors AS LONG
        SigColors AS LONG
    END TYPE


    DIM BMP AS BMPFormat
    DIM x AS LONG, y AS LONG
    DIM temp AS STRING

    DIM n AS _MEM, o AS _OFFSET, m AS _MEM
    m = _MEMIMAGE(image&)

    IF x1% > x2% THEN SWAP x1%, x2%
    IF y1% > y2% THEN SWAP y1%, y2%
    IF x2% = _WIDTH(imagehandle%) THEN x2% = _WIDTH(imagehandle%) - 1 'troubleshoot in case user does a common mistake for 0-width instead of 0 - (width-1) for fullscreen
    IF y2% = _HEIGHT(imagehandle%) THEN y2% = _HEIGHT(imagehandle%) - 1 'troubleshoot in case user does a common mistake for 0-width instead of 0 - (width-1) for fullscreen

    s& = _SOURCE
    _SOURCE image&

    BMP.PWidth = (x2% - x1%) + 1
    BMP.PDepth = (y2% - y1%) + 1
    BMP.ID = "BM"
    BMP.Blank = 0
    BMP.Hsize = 40
    BMP.Planes = 1
    BMP.Compression = 0
    BMP.Xres = 0
    BMP.Yres = 0

    BMP.SigColors = 0

    SELECT CASE _PIXELSIZE(image&)
        CASE 1
            temp = SPACE$(x2% - x1% + 1)
            OffsetBITS& = 54 + 1024 'add palette in 256 color modes
            BMP.BPP = 8
            IF BMP.PWidth MOD 4 THEN ZeroPAD$ = SPACE$(4 - (BMP.PWidth MOD 4))
            ImageSize& = (BMP.PWidth + LEN(ZeroPAD$)) * BMP.PDepth
            BMP.ImageBytes = ImageSize&
            BMP.NumColors = 256
            BMP.Size = ImageSize& + OffsetBITS&
            BMP.Offset = OffsetBITS&
        CASE 4
            temp = SPACE$(3)
            OffsetBITS& = 54 'no palette in 24/32 bit
            BMP.BPP = 24
            IF ((BMP.PWidth * 3) MOD 4) THEN ZeroPAD$ = SPACE$(4 - ((BMP.PWidth * 3) MOD 4))
            ImageSize& = (BMP.PWidth + LEN(ZeroPAD$)) * BMP.PDepth
            BMP.ImageBytes = ImageSize&
            BMP.NumColors = 0
            BMP.Size = ImageSize& * 3 + OffsetBITS&
            BMP.Offset = OffsetBITS&
    END SELECT

    F = FREEFILE
    n = _MEMNEW(BMP.Size)
    _MEMPUT n, n.OFFSET, BMP
    o = n.OFFSET + 54
    zp& = LEN(ZeroPAD$)
    $CHECKING:OFF

    IF BMP.BPP = 8 THEN 'Store the Palette for 256 color mode
        FOR c& = 0 TO 255 ' read BGR color settings from JPG image + 1 byte spacer(CHR$(0))
            cv& = _PALETTECOLOR(c&, image) ' color attribute to read.
            b$ = CHR$(_BLUE32(cv&)) + CHR$(_GREEN32(cv&)) + CHR$(_RED32(cv&)) + CHR$(0) 'spacer byte
            _MEMPUT n, o, b$
            o = o + 4
        NEXT
        y = y2% + 1
        w& = _WIDTH(image&)
        x = x2% - x1% + 1
        DO
            y = y - 1
            _MEMGET m, m.OFFSET + (w& * y + x1%), temp
            _MEMPUT n, o, temp
            o = o + x
            _MEMPUT n, o, ZeroPAD$
            o = o + zp&
        LOOP UNTIL y = y1%
    ELSE
        y = y2% + 1
        w& = _WIDTH(image&)
        DO
            y = y - 1: x = x1% - 1
            DO
                x = x + 1
                _MEMGET m, m.OFFSET + (w& * y + x) * 4, temp
                _MEMPUT n, o, temp
                o = o + 3
            LOOP UNTIL x = x2%
            _MEMPUT n, o, ZeroPAD$
            o = o + zp&
        LOOP UNTIL y = y1%
    END IF
    $CHECKING:ON
    _MEMFREE m
    OPEN filename$ FOR BINARY AS #F
    t1$ = SPACE$(BMP.Size)
    _MEMGET n, n.OFFSET, t1$
    PUT #F, , t1$
    _MEMFREE n
    CLOSE #F
    _SOURCE s&
END SUB

FUNCTION GetDay (mm, dd, yyyy) 'use 4 digit year
    'From Zeller's congruence: https://en.wikipedia.org/wiki/Zeller%27s_congruence
    IF mm < 3 THEN mm = mm + 12: yyyy = yyyy - 1
    century = yyyy MOD 100
    zerocentury = yyyy \ 100
    result = (dd + INT(13 * (mm + 1) / 5) + century + INT(century / 4) + INT(zerocentury / 4) + 5 * zerocentury) MOD 7
    IF result = 0 THEN
        GetDay = 7
    ELSE
        GetDay = result
    END IF
    'Function changed to return a numeric value instead of a string for this program
    '    SELECT CASE result
    '        CASE 7: GetDay$ = "Saturday"
    '        CASE 1: GetDay$ = "Sunday"
    '        CASE 2: GetDay$ = "Monday"
    '        CASE 3: GetDay$ = "Tuesday"
    '        CASE 4: GetDay$ = "Wednesday"
    '        CASE 5: GetDay$ = "Thursday"
    '        CASE 6: GetDay$ = "Friday"
    '    END SELECT
END FUNCTION

