DECLARE LIBRARY "GuiAppFrame" 'Do not add .h here !!
    FUNCTION FindColor& (BYVAL r&, BYVAL g&, BYVAL b&, BYVAL i&, BYVAL mi&, BYVAL ma&)
    'This is a replacement for the _RGB function. It works for upto 8-bit
    '(256 colors) images only and needs a valid image. It can limit the
    'number of pens to search and uses a better color matching algorithm.
END DECLARE

_DEFINE A-Z AS _UNSIGNED LONG
REDIM SHARED fsNearCol%(&HFFFFFF)

ws = _NEWIMAGE(640, 480, 32) 'A 32 bit screen
ts = _NEWIMAGE(640, 480, 256) 'A 256 color screen, which is only used so I can get the standard 256 color paletter from it.
SCREEN ws
RANDOMIZE TIMER
DIM color256 AS _UNSIGNED LONG

CONST ConvertToStandard256Palette = -1 'Change to 0 and you can see that we preseve the second pass's
'                                       color information perfectly.
'                                       If the CONST is set, then we convert our colors on the screen
'                                       to as close of a match as possible, while preserving the standard
'                                       QB64 256-color palette.

CLS , _RGB32(0, 0, 0)

FOR j = 1 TO 2
    IF j = 1 THEN
        FOR i = 1 TO 100 '100 random colors
            'if we want to use the standard 256 color screen palette, we can do so as below
            color256 = _RGB32(_RED(i, ts), _GREEN(i, ts), _BLUE(i, ts))
            LINE (RND * 640, RND * 480)-(RND * 640, RND * 480), color256, BF
        NEXT
    ELSE 'we can go with completely random colors with the following instead:
        FOR i = 1 TO 100 '100 random colors
            LINE (RND * 640, RND * 480)-(RND * 640, RND * 480), &HF0000000 + RND * &HFFFFFFF, BF
        NEXT
    END IF
    PRINT "This is the original screen, pass"; j
    SLEEP 'show the original screen

    t = Image32To256(ws)
    SCREEN t 'show the standard 256 image screen with the image converted over
    '         this keeps us from having to learn or use any new/unique palettes the image may have
    '         but, it does cause us to lose details and hues.
    PRINT "This is the 256-color screen, pass"; j
    SLEEP
    SCREEN ws
    _FREEIMAGE t
    CLS
NEXT


l = _LOADIMAGE("C:\QB64 Repo\Steve64\Beautiful_colorful_bird_wallpaper01.jpg", 32)
SCREEN l
_SCREENMOVE 0, 0 'move the screen to use as much of the screen as possible, since it's so damn huge!
PRINT "This is the original 32-bit screen."
SLEEP 'to show the 32-bit image of the colorful bird I found

t = Image32To256(l)
SCREEN t 'show the 256 image screen with the image converted over
_SCREENMOVE 0, 0 'move this one too!
PRINT "This is the converted 256 color screen."
'And we're done.  You should now be seeing a pretty little 256 color version of the bird


FUNCTION Image32To256 (image&)
    DIM o AS _OFFSET
    DIM a AS _UNSIGNED _BYTE, r AS _UNSIGNED _BYTE
    DIM g AS _UNSIGNED _BYTE, b AS _UNSIGNED _BYTE
    DIM t AS _UNSIGNED LONG, color256 AS _UNSIGNED LONG
    DIM index256 AS _UNSIGNED LONG
    TYPE Pal_type
        c AS _UNSIGNED LONG 'color index
        n AS LONG 'number of times it appears
    END TYPE
    DIM Pal(255) AS _UNSIGNED LONG
    I256 = _NEWIMAGE(_WIDTH(image&), _HEIGHT(image&), 256)
    DIM m(1) AS _MEM: m(0) = _MEMIMAGE(image&): m(1) = _MEMIMAGE(I256)
    DO 'get the palette and number of colors used
        _MEMGET m(0), m(0).OFFSET + o, t 'Get the colors from the original screen
        FOR i = 0 TO colors 'check to see if they're in the existing palette we're making
            IF Pal(i) = t THEN EXIT FOR
        NEXT
        IF i > colors THEN
            Pal(colors) = t
            colors = colors + 1 'increment the index for the new color found
            IF colors > 255 THEN 'no need to check any further; it's not a normal QB64 256 color image
                Image32To256 = RemapImageFS(image&, I256)
                _FREEIMAGE I256
                _MEMFREE m()
                EXIT FUNCTION 'and we're done, with 100% image compatability saved
            END IF
        END IF
        o = o + 4
    LOOP UNTIL o >= m(0).SIZE

    '  we might be working with a standard qb64 256 color screen
    '  check for that first
    colors = colors - 1 'back up one, as we found our limit and aren't needing to set another
    FOR i = 0 TO colors 'comparing palette against QB64 256 color palette
        t = Pal(i)
        index256 = _RGBA(_RED(t), _GREEN(t), _BLUE(t), _ALPHA(t), I256)
        color256 = _RGBA32(_RED(index256, I256), _GREEN(index256, I256), _BLUE(index256, I256), _ALPHA(index256, I256))
        IF t <> color256 THEN NSCU = -1: EXIT FOR
    NEXT
    IF NSCU THEN 'it's not a standard QB64 256 color palette, but it's still less than 256 total colors.
        IF ConvertToStandard256Palette THEN
            TI256 = RemapImageFS(image&, I256)
            _MEMFREE m(1) 'free the old memory
            _FREEIMAGE I256 'and the old image
            I256 = TI256 'replace with the new image
            m(1) = _MEMIMAGE(I256) 'and point the mem block to the new image
        ELSE
            FOR i = 0 TO colors: _PALETTECOLOR i, Pal(i), I256: NEXT 'set the palette
        END IF
    END IF
    'If we didn't change the palette above, we should work 100% with qb64's internal 256 color palette
    o = 0
    DO 'Get the colors, put them to a 256 color screen, as is
        _MEMGET m(0), m(0).OFFSET + o + 3, a
        _MEMGET m(0), m(0).OFFSET + o + 2, r
        _MEMGET m(0), m(0).OFFSET + o + 1, g
        _MEMGET m(0), m(0).OFFSET + o + 0, b
        _MEMPUT m(1), m(1).OFFSET + o \ 4, _RGBA(r, g, b, a, I256) AS _UNSIGNED _BYTE
        o = o + 4
    LOOP UNTIL o >= m(0).SIZE
    _MEMFREE m()
    Image32To256 = I256
END FUNCTION

FUNCTION RemapImageFS& (ohan&, dhan&)
    RemapImageFS& = -1 'so far return invalid handle
    shan& = ohan& 'avoid side effect on given argument
    IF shan& < -1 THEN
        '--- check/adjust source image & get new 8-bit image ---
        swid% = _WIDTH(shan&): shei% = _HEIGHT(shan&)
        IF _PIXELSIZE(shan&) <> 4 THEN
            than& = _NEWIMAGE(swid%, shei%, 32)
            IF than& >= -1 THEN EXIT FUNCTION
            _PUTIMAGE , shan&, than&
            shan& = than&
        ELSE
            than& = -1 'avoid freeing below
        END IF
        nhan& = _NEWIMAGE(swid%, shei%, 256)
        '--- Floyd-Steinberg error distribution arrays ---
        rhan& = _NEWIMAGE(swid%, 2, 32) 'these are missused as LONG arrays,
        ghan& = _NEWIMAGE(swid%, 2, 32) 'with CHECKING:OFF this is much faster
        bhan& = _NEWIMAGE(swid%, 2, 32) 'than real QB64 arrays
        '--- curr/next row offsets (for distribution array access) ---
        cro% = 0: nro% = swid% * 4 'will be swapped after each pixel row
        '--- the matrix values are extended by 16384 to avoid slow floating ---
        '--- point ops and to allow for integer storage in the above arrays ---
        '--- also it's a power of 2, which may be optimized into a bitshift ---
        seven% = (7 / 16) * 16384 'X+1,Y+0 error fraction
        three% = (3 / 16) * 16384 'X-1,Y+1 error fraction
        five% = (5 / 16) * 16384 'X+0,Y+1 error fraction
        one% = (1 / 16) * 16384 'X+1,Y+1 error fraction
        '--- if all is good, then start remapping ---
        $CHECKING:OFF
        IF nhan& < -1 AND rhan& < -1 AND ghan& < -1 AND bhan& < -1 THEN
            _COPYPALETTE dhan&, nhan& 'dest palette to new image
            '--- for speed we do direct memory access ---
            DIM sbuf AS _MEM: sbuf = _MEMIMAGE(shan&): soff%& = sbuf.OFFSET
            DIM nbuf AS _MEM: nbuf = _MEMIMAGE(nhan&): noff%& = nbuf.OFFSET
            DIM rbuf AS _MEM: rbuf = _MEMIMAGE(rhan&): roff%& = rbuf.OFFSET
            DIM gbuf AS _MEM: gbuf = _MEMIMAGE(ghan&): goff%& = gbuf.OFFSET
            DIM bbuf AS _MEM: bbuf = _MEMIMAGE(bhan&): boff%& = bbuf.OFFSET
            '--- iterate through pixels ---
            FOR y% = 0 TO shei% - 1
                FOR x% = 0 TO swid% - 1
                    '--- curr/prev/next pixel offsets ---
                    cpo% = x% * 4: ppo% = cpo% - 4: npo% = cpo% + 4
                    '--- get pixel ARGB value from source ---
                    srgb~& = _MEMGET(sbuf, soff%&, _UNSIGNED LONG)
                    '--- add distributed error, shrink by 16384, clear error ---
                    '--- current pixel X+0, Y+0 (= cro% (current row offset)) ---
                    poff% = cro% + cpo% 'pre-calc full pixel offset
                    sr% = ((srgb~& AND &HFF0000~&) \ 65536) + (_MEMGET(rbuf, roff%& + poff%, LONG) \ 16384) 'red
                    sg% = ((srgb~& AND &HFF00~&) \ 256) + (_MEMGET(gbuf, goff%& + poff%, LONG) \ 16384) 'green
                    sb% = (srgb~& AND &HFF~&) + (_MEMGET(bbuf, boff%& + poff%, LONG) \ 16384) 'blue
                    _MEMPUT rbuf, roff%& + poff%, 0 AS LONG 'clearing each single pixel error using _MEMPUT
                    _MEMPUT gbuf, goff%& + poff%, 0 AS LONG 'turns out even faster than clearing the entire
                    _MEMPUT bbuf, boff%& + poff%, 0 AS LONG 'pixel row using _MEMFILL at the end of the loop
                    '--- find nearest color ---
                    crgb~& = _RGBA32(sr%, sg%, sb%, 0) 'used for fast value clipping + channel merge
                    IF fsNearCol%(crgb~&) > 0 THEN
                        npen% = fsNearCol%(crgb~&) - 1 'already known
                    ELSE
                        npen% = FindColor&(sr%, sg%, sb%, nhan&, 24, 255 - guiReservedPens%) 'not known, find one
                        fsNearCol%(crgb~&) = npen% + 1 'save for later use
                    END IF
                    '--- put colormapped pixel to dest ---
                    _MEMPUT nbuf, noff%&, npen% AS _UNSIGNED _BYTE
                    '------------------------------------------
                    '--- Floyd-Steinberg error distribution ---
                    '------------------------------------------
                    '--- You may comment this block out, to see the
                    '--- result without applied FS matrix.
                    '-----
                    '--- get dest palette RGB value, calc error to clipped source ---
                    nrgb~& = _PALETTECOLOR(npen%, nhan&)
                    er% = ((crgb~& AND &HFF0000~&) - (nrgb~& AND &HFF0000~&)) \ 65536
                    eg% = ((crgb~& AND &HFF00~&) - (nrgb~& AND &HFF00~&)) \ 256
                    eb% = (crgb~& AND &HFF~&) - (nrgb~& AND &HFF~&)
                    '--- distribute error according to FS matrix ---
                    IF x% > 0 THEN
                        '--- X-1, Y+1 (= nro% (next row offset)) ---
                        poff% = nro% + ppo% 'pre-calc full pixel offset
                        _MEMPUT rbuf, roff%& + poff%, _MEMGET(rbuf, roff%& + poff%, LONG) + (er% * three%) AS LONG 'red
                        _MEMPUT gbuf, goff%& + poff%, _MEMGET(gbuf, goff%& + poff%, LONG) + (eg% * three%) AS LONG 'green
                        _MEMPUT bbuf, boff%& + poff%, _MEMGET(bbuf, boff%& + poff%, LONG) + (eb% * three%) AS LONG 'blue
                    END IF
                    '--- X+0, Y+1 (= nro% (next row offset)) ---
                    poff% = nro% + cpo% 'pre-calc full pixel offset
                    _MEMPUT rbuf, roff%& + poff%, _MEMGET(rbuf, roff%& + poff%, LONG) + (er% * five%) AS LONG 'red
                    _MEMPUT gbuf, goff%& + poff%, _MEMGET(gbuf, goff%& + poff%, LONG) + (eg% * five%) AS LONG 'green
                    _MEMPUT bbuf, boff%& + poff%, _MEMGET(bbuf, boff%& + poff%, LONG) + (eb% * five%) AS LONG 'blue
                    IF x% < (swid% - 1) THEN
                        '--- X+1, Y+0 (= cro% (current row offset)) ---
                        poff% = cro% + npo% 'pre-calc full pixel offset
                        _MEMPUT rbuf, roff%& + poff%, _MEMGET(rbuf, roff%& + poff%, LONG) + (er% * seven%) AS LONG 'red
                        _MEMPUT gbuf, goff%& + poff%, _MEMGET(gbuf, goff%& + poff%, LONG) + (eg% * seven%) AS LONG 'green
                        _MEMPUT bbuf, boff%& + poff%, _MEMGET(bbuf, boff%& + poff%, LONG) + (eb% * seven%) AS LONG 'blue
                        '--- X+1, Y+1 (= nro% (next row offset)) ---
                        poff% = nro% + npo% 'pre-calc full pixel offset
                        _MEMPUT rbuf, roff%& + poff%, _MEMGET(rbuf, roff%& + poff%, LONG) + (er% * one%) AS LONG 'red
                        _MEMPUT gbuf, goff%& + poff%, _MEMGET(gbuf, goff%& + poff%, LONG) + (eg% * one%) AS LONG 'green
                        _MEMPUT bbuf, boff%& + poff%, _MEMGET(bbuf, boff%& + poff%, LONG) + (eb% * one%) AS LONG 'blue
                    END IF
                    '------------------------------------------
                    '--- End of FS ----------------------------
                    '------------------------------------------
                    noff%& = noff%& + 1 'next dest pixel
                    soff%& = soff%& + 4 'next source pixel
                NEXT x%
                tmp% = cro%: cro% = nro%: nro% = tmp% 'exchange distribution array row offsets
            NEXT y%
            '--- memory cleanup ---
            _MEMFREE bbuf
            _MEMFREE gbuf
            _MEMFREE rbuf
            _MEMFREE nbuf
            _MEMFREE sbuf
            '--- set result ---
            RemapImageFS& = nhan&
            nhan& = -1 'avoid freeing below
        END IF
        $CHECKING:ON
        '--- remapping done or error, cleanup remains ---
        IF bhan& < -1 THEN _FREEIMAGE bhan&
        IF ghan& < -1 THEN _FREEIMAGE ghan&
        IF rhan& < -1 THEN _FREEIMAGE rhan&
        IF nhan& < -1 THEN _FREEIMAGE nhan&
        IF than& < -1 THEN _FREEIMAGE than&
    END IF
END FUNCTION
