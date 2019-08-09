$SCREENHIDE

CONST MOD_SHIFT = 4

' these next two constants determine the hotkey.
' the current example is Shift+A
CONST fsModifiers = MOD_SHIFT '   http://msdn.microsoft.com/en-us/library/ms646309(v=vs.85).aspx
CONST vk = &H41 '                 http://msdn.microsoft.com/en-us/library/dd375731(v=vs.85).aspx

CONST mode = 0

'mode 0 makes the thread wait for the hotkey. It will not respond to requests
'to terminate, until it gets a hotkey message. Otherwise, Windows will
'consider it to be not responding.
'mode 1 causes the thread to wake up at 1 second intervals. This makes it
'use some CPU time, but it won't appear to be not responding.

'As $SCREENHIDE is used, you won't have an X to click, so you would probably
'use task manager or process explorer to kill it either way. However, mode
'might make a difference in how much it delays your log off/shutdown time.

CONST WM_HOTKEY = &H0312
CONST PM_REMOVE = 1
CONST WAIT_FAILED = -1
CONST QS_ALLEVENTS = &H04BF

CONST ERROR_ALREADY_EXISTS = &HB7
DECLARE DYNAMIC LIBRARY "kernel32"
    FUNCTION GetLastError~& ()
    FUNCTION CreateMutexA~%& (BYVAL lpMutexAttributes~%&, BYVAL bInitialOwner&, BYVAL lpName~%&)
END DECLARE

DECLARE DYNAMIC LIBRARY "user32"
    FUNCTION RegisterHotKey& (BYVAL hWnd~%&, BYVAL id&, BYVAL fsModifiers~&, BYVAL vk~&)
    FUNCTION UnregisterHotKey& (BYVAL hWnd~%&, BYVAL id&)
    FUNCTION GetMessageW& (BYVAL lpMsg~%&, BYVAL hWnd~%&, BYVAL wMsgFilterMin~&, BYVAL wMsgFilterMax~&)
    FUNCTION PeekMessageW& (BYVAL lpMsg~%&, BYVAL hWnd~%&, BYVAL wMsgFilterMin~&, BYVAL wMsgFilterMax~&, BYVAL wRemoveMsg~&)
    FUNCTION MsgWaitForMultipleObjects~& (BYVAL nCount~&, BYVAL pHandles~%&, BYVAL bWaitAll&, BYVAL dwMilliseconds~&, BYVAL dwWakeMask~&)
END DECLARE

TYPE POINT
    x AS LONG
    y AS LONG
END TYPE

TYPE MSG
    hwnd AS _UNSIGNED _OFFSET
    message AS _UNSIGNED LONG
    wParam AS _UNSIGNED _OFFSET
    lParam AS _OFFSET
    time AS _UNSIGNED LONG
    pt AS POINT
END TYPE


DIM h AS LONG
DIM bRet AS LONG
DIM msg AS MSG
DIM t AS STRING

t = "Global\qb64 hotkey demo" + CHR$(0)
IF 0 = CreateMutexA(0, 0, _OFFSET(t)) THEN showerr "CreateMutexA"
IF ERROR_ALREADY_EXISTS = GetLastError THEN showerr "(Multiple instances?) "

IF 0 = RegisterHotKey(0, 0, fsModifiers, vk) THEN showerr "RegisterHotKey"

IF mode THEN

    DO
        bRet = GetMessageW(_OFFSET(msg), 0, 0, 0)
        SELECT CASE bRet
            CASE 0: EXIT DO
            CASE -1: showerr "GetMessageW"
            CASE ELSE
                IF PeekMessageW(_OFFSET(msg), 0, 0, 0, PM_REMOVE) THEN
                    IF WM_HOTKEY = (&HFFFF~& AND msg.message) THEN
                        h = _SCREENIMAGE
                        SaveBMP "ScreenCapture" + timestamp + ".bmp", h, 0, 0, _WIDTH(h), _HEIGHT(h)
                        _FREEIMAGE h
                    END IF
                END IF
        END SELECT
    LOOP
ELSE
    DO
        _LIMIT 10
        'IF WAIT_FAILED = MsgWaitForMultipleObjects(0, 0, 0, 1000, QS_ALLEVENTS) THEN showerr "MsgWaitForMultipleObjects"
        IF PeekMessageW(_OFFSET(msg), 0, 0, 0, PM_REMOVE) THEN
            IF WM_HOTKEY = (&HFFFF~& AND msg.message) THEN
                h = _SCREENIMAGE
                SaveBMP "ScreenCapture" + timestamp + ".bmp", h, 0, 0, _WIDTH(h), _HEIGHT(h)
            END IF
        END IF
    LOOP

END IF

IF 0 = UnregisterHotKey(0, 0) THEN showerr "UnRegisterHotKey"
SYSTEM


SUB showerr (f AS STRING)
    _SCREENSHOW
    PRINT f; " failed. Error: 0x" + LCASE$(HEX$(GetLastError))
    END
END SUB


FUNCTION timestamp$
    DIM d AS STRING * 10
    DIM t AS STRING * 8
    DO
        d = DATE$
        t = TIME$
    LOOP WHILE d <> DATE$ ' try to prevent the situation where midnight is crossed between getting the date$ and time$
    MID$(t, 3, 1) = " "
    MID$(t, 6, 1) = " "
    MID$(d, 3, 1) = " "
    timestamp = RIGHT$(d, 4) + " " + LEFT$(d, 5) + "--" + t
END FUNCTION

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
    DIM temp AS STRING, t AS STRING * 1

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

