'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%GIF STUFF%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DIM SHARED MakeGif_OutBuffer AS STRING
DIM SHARED MakeGif_OStartAddress AS INTEGER, MakeGif_OAddress AS INTEGER
DIM SHARED MakeGif_OEndAddress AS INTEGER, MakeGif_OSeg AS INTEGER
DIM SHARED MakeGif_CodeSize AS INTEGER, MakeGif_CurrnetBit AS INTEGER, MakeGif_Char AS LONG
DIM SHARED MakeGIF_BlockLength AS INTEGER, MakeGif_X AS INTEGER, MakeGif_Y AS INTEGER
DIM SHARED MakeGif_MinX AS INTEGER, MakeGif_MinY AS INTEGER
DIM SHARED MakeGif_MaxX AS INTEGER, MakeGif_MaxY AS INTEGER
DIM SHARED MakeGif_Done AS INTEGER, MakeGif_GIFfile AS INTEGER, MakeGif_LastLoc AS LONG
'%%%%%%%%%%%%%%%%%%%%%%%%%%%END OF GIF STUFF%%%%%%%%%%%%%%%%%%%%%%%%%%%%




DIM Demo_T
SCREEN _NEWIMAGE(640, 480, 256)
RANDOMIZE TIMER
FOR i = 1 TO 100
    LINE (RND * 640, RND * 480)-(RND * 640, RND * 480), i, BF 'draw some junk on the screen
NEXT
SaveGIF "booga.gif", 0, 0, 0, 639, 479
SLEEP
CLS
SLEEP
Demo_T = _LOADIMAGE("booga.gif")
SCREEN Demo_T


'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%GIF STUFF%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'-----------------------------------------------------------------------
' PDS 7.1 & QB4.5 GIF Compression Routine v1.00 By Rich Geldreich 1992
'      Bug fixed and Overhauled for QB64 by Steve McNeill 2019
'-----------------------------------------------------------------------
SUB SaveGIF (file$, image AS LONG, Xstart, YStart, Xend, Yend)
    CONST Table.Size = 7177 'hash table's size - must be a prime number!
    'Variables all DIMMED so as to avoid any OPTION _EXPLICIT errors.
    DIM Prefix(Table.Size - 1) AS INTEGER
    DIM Suffix(Table.Size - 1) AS INTEGER
    DIM code(Table.Size - 1) AS INTEGER
    DIM ScreenX AS INTEGER
    DIM ScreenY AS INTEGER
    DIM B AS STRING
    DIM NumColors AS INTEGER
    DIM BitsPixel AS INTEGER
    DIM StartSize AS INTEGER
    DIM StartCode AS INTEGER
    DIM StartMax AS INTEGER
    DIM ColorBits AS INTEGER
    DIM a1 AS INTEGER
    DIM a AS STRING
    DIM R AS INTEGER
    DIM G AS INTEGER
    DIM B1 AS INTEGER
    DIM ImageWidth AS INTEGER
    DIM ImageHeight AS INTEGER
    DIM MaxCode AS INTEGER
    DIM ClearCode AS INTEGER
    DIM EOFCode AS INTEGER
    DIM NextCode AS INTEGER
    DIM a2 AS LONG
    DIM Prefix AS INTEGER
    DIM Suffix AS INTEGER
    DIM Found AS INTEGER
    DIM index AS INTEGER
    DIM Offset AS INTEGER
    DIM D AS INTEGER
    DIM S AS INTEGER
    D = _DEST: S = _SOURCE
    _DEST image&: _SOURCE image&

    'MakeGif_MinX, MakeGif_MinY, MakeGif_MaxX, MakeGif_MaxY have the encoding window
    ScreenX = _WIDTH: ScreenY = _HEIGHT
    MakeGif_MinX = Xstart: MakeGif_MinY = YStart
    MakeGif_MaxX = Xend: MakeGif_MaxY = Yend

    'Open GIF output file
    MakeGif_GIFfile = FREEFILE 'use next free file
    OPEN file$ FOR BINARY AS MakeGif_GIFfile
    'Put GIF87a header at beginning of file
    B$ = "GIF87a"
    PUT MakeGif_GIFfile, , B$
    'See how many colors are in this image...
    NumColors = 256 'who cares about the old school graphic modes with fewer colors?  Not me!  Find a different encoder. :)
    BitsPixel = 8 '8 bits per pixel
    StartSize = 9 'first LZW code is 9 bits
    StartCode = 256 'first free code
    StartMax = 512 'maximum code in 9 bits
    ColorBits = 6 'VGA

    PUT MakeGif_GIFfile, , ScreenX 'put screen's dimensions
    PUT MakeGif_GIFfile, , ScreenY

    'pack colorbits and bits per pixel
    a1 = 215 ' precalculated value: for 128 + (ColorBits - 1) * 16 + (BitsPixel - 1)
    PUT MakeGif_GIFfile, , a1

    'throw a zero into the GIF file; reserved for future expansion of format (which will never come)
    a$ = CHR$(0)
    PUT MakeGif_GIFfile, , a$

    'Get the RGB palette from the screen and put it into the file...
    FOR a1 = 0 TO 255
        'Note: a BIOS call could be used here, but then we have to use
        'the messy CALL INTERRUPT subs...
        R = _RED(a1, image&)
        G = _GREEN(a1, image&)
        B1 = _BLUE(a1, image&)
        a$ = CHR$(R): PUT MakeGif_GIFfile, , a$
        a$ = CHR$(G): PUT MakeGif_GIFfile, , a$
        a$ = CHR$(B1): PUT MakeGif_GIFfile, , a$
    NEXT


    'write out an image descriptor...
    a$ = "," '"," is image seperator
    PUT MakeGif_GIFfile, , a$ 'write it
    PUT MakeGif_GIFfile, , MakeGif_MinX 'write out the image's location
    PUT MakeGif_GIFfile, , MakeGif_MinY
    ImageWidth = (MakeGif_MaxX - MakeGif_MinX + 1) 'find length & width of image
    ImageHeight = (MakeGif_MaxY - MakeGif_MinY + 1)
    PUT MakeGif_GIFfile, , ImageWidth 'store them into the file
    PUT MakeGif_GIFfile, , ImageHeight
    a$ = CHR$(BitsPixel - 1) '# bits per pixel in the image
    PUT MakeGif_GIFfile, , a$

    a$ = CHR$(StartSize - 1) 'store the LZW minimum code size
    PUT MakeGif_GIFfile, , a$

    'Initialize the vars needed by PutCode

    MakeGif_CurrnetBit = 0: MakeGif_Char = 0
    MaxCode = StartMax 'the current maximum code size
    MakeGif_CodeSize = StartSize 'the current code size
    ClearCode = StartCode 'ClearCode & EOF code are the
    EOFCode = StartCode + 1 ' first two entries
    StartCode = StartCode + 2 'first free code that can be used
    NextCode = StartCode 'the current code

    MakeGif_OutBuffer = STRING$(5000, 32) 'output buffer; for speedy disk writes
    a2& = SADD(MakeGif_OutBuffer) 'find address of buffer
    a2& = a2& - 65536 * (a2& < 0)
    MakeGif_OSeg = VARSEG(MakeGif_OutBuffer) + (a2& \ 16) 'get segment + offset >> 4
    MakeGif_OAddress = a2& AND 15 'get address into segment
    MakeGif_OEndAddress = MakeGif_OAddress + 5000 'end of disk buffer
    MakeGif_OStartAddress = MakeGif_OAddress 'current location in disk buffer
    DEF SEG = MakeGif_OSeg

    FOR a1 = 0 TO Table.Size - 1 'clears the hashing table
        Prefix(a1) = -1 '-1 = invalid entry
        Suffix(a1) = -1
        code(a1) = -1
    NEXT

    PutCode ClearCode ' clear code

    MakeGif_X = Xstart: MakeGif_Y = YStart 'MakeGif_X & MakeGif_Y have the current pixel
    Prefix = GetByte 'the first pixel is a special case
    MakeGif_Done = 0 '-1 when image is complete

    DO 'while there are more pixels to encode
        DO 'until we have a new string to put into the table
            'get a pixel from the screen and see if we can find
            'the new string in the table
            Suffix = GetByte
            GOSUB Hash 'is it there?
            IF Found = -1 THEN Prefix = code(index) 'yup, replace the
            'prefix:suffix string with whatever
            'code represents it in the table
        LOOP WHILE Found AND NOT MakeGif_Done 'don't stop unless we find a new string
        PutCode Prefix 'output the prefix to the file
        Prefix(index) = Prefix 'put the new string in the table
        Suffix(index) = Suffix
        code(index) = NextCode 'we've got to keep track if what code this is!
        Prefix = Suffix 'Prefix=the last pixel pulled from the screen
        NextCode = NextCode + 1 'get ready for the next code
        IF NextCode = MaxCode + 1 THEN 'can an output code ever exceed
            'the current code size?
            'yup, increase the code size
            MaxCode = MaxCode * 2
            'Note: The GIF89a spec mentions something about a deferred clear
            'code. When the clear code is deferred, codes are not entered
            'into the hash table anymore. When the compression of the image
            'starts to fall below a certain threshold, the clear code is
            'sent and the hash table is cleared. The overall result is
            'greater compression, because the table is cleared less often.
            'This version of MakeGIF doesn't support this, because some GIF
            'decoders crash when they attempt to enter too many codes
            'into the string table.

            IF MakeGif_CodeSize = 12 THEN 'is the code size too big?
                PutCode ClearCode 'yup; clear the table and
                FOR a1 = 0 TO Table.Size - 1 'clears the hashing table
                    Prefix(a1) = -1 '-1 = invalid entry
                    Suffix(a1) = -1
                    code(a1) = -1
                NEXT
                NextCode = StartCode
                MakeGif_CodeSize = StartSize
                MaxCode = StartMax
            ELSE
                MakeGif_CodeSize = MakeGif_CodeSize + 1 'just increase the code size if
            END IF 'it's not too high( not > 12)
        END IF
    LOOP UNTIL MakeGif_Done 'while we have more pixels
    'Once MakeGif_Done, write out the last pixel, clear the disk buffer
    'and fix up the last block so its count is correct
    PutCode Prefix 'write last pixel
    PutCode EOFCode 'send EOF code
    IF MakeGif_CurrnetBit <> 0 THEN
        PutCode 0 'flush out the last code...
    END IF
    PutByte 0
    MakeGif_OutBuffer = LEFT$(MakeGif_OutBuffer, MakeGif_OAddress - MakeGif_OStartAddress)
    PUT MakeGif_GIFfile, , MakeGif_OutBuffer
    a$ = ";" + STRING$(8, &H1A) 'the 8 EOF chars is not standard,
    'but many GIF's have them, so how
    'much could it hurt?
    PUT MakeGif_GIFfile, , a$
    a$ = CHR$(255 - MakeGIF_BlockLength) 'correct the last block's count
    PUT MakeGif_GIFfile, MakeGif_LastLoc, a$
    CLOSE MakeGif_GIFfile
    _DEST D: _SOURCE S 'restore the destination and source now that we're done.
    EXIT SUB 'so we won't have any issues trying to run the hash routines below.

    'this is only one of a plethora of ways to search the table for
    'a match! I used a binary tree first, but I switched to hashing
    'cause it's quicker(perhaps the way I implemented the tree wasn't
    'optimal... who knows!)

    Hash:
    'hash the prefix & suffix(there are also many ways to do this...)
    '?? is there a better formula?
    index = ((Prefix * 256&) XOR Suffix) MOD Table.Size
    '
    '(Note: the table size(7177 in this case) must be a prime number, or
    'else there's a chance that the routine will hang up... hate when
    'that happens!)
    '
    'Calculate an offset just in case we don't find what we want on the
    'first try...

    IF index = 0 THEN 'can't have Table.Size-0 !
        Offset = 1
    ELSE
        Offset = Table.Size - index
    END IF

    DO 'until we (1) find an empty entry or (2) find what we're lookin for
        IF code(index) = -1 THEN 'is this entry blank?
            Found = 0 'yup- we didn't find the string
            RETURN
            'is this entry the one we're looking for?
        ELSEIF Prefix(index) = Prefix AND Suffix(index) = Suffix THEN
            'yup, congrats you now understand hashing!!!
            Found = -1
            RETURN
        ELSE
            'shoot! we didn't find anything interesting, so we must
            'retry- this is what slows hashing down. I could of used
            'a bigger table, that would of speeded things up a little
            'because this retrying would not happen as often...
            index = index - Offset
            IF index < 0 THEN 'too far down the table?
                'wrap back the index to the end of the table
                index = index + Table.Size
            END IF
        END IF
    LOOP
END SUB

'Puts a byte into the GIF file & also takes care of each block.
SUB PutByte (a) STATIC
    MakeGIF_BlockLength = MakeGIF_BlockLength - 1 'are we at the end of a block?
    IF MakeGIF_BlockLength <= 0 THEN ' yup,
        MakeGIF_BlockLength = 255 'block length is now 255
        MakeGif_LastLoc = LOC(MakeGif_GIFfile) + 1 + (MakeGif_OAddress - MakeGif_OStartAddress) 'remember the pos.
        BufferWrite 255 'for later fixing
    END IF
    BufferWrite a 'put a byte into the buffer
END SUB

'Puts an LZW variable-bit code into the output file...
SUB PutCode (a) STATIC
    MakeGif_Char = MakeGif_Char + a * 2 ^ MakeGif_CurrnetBit 'put the char were it belongs;
    MakeGif_CurrnetBit = MakeGif_CurrnetBit + MakeGif_CodeSize ' shifting it to its proper place
    DO WHILE MakeGif_CurrnetBit > 7 'do we have a least one full byte?
        PutByte MakeGif_Char AND 255 ' yup! mask it off and write it out
        MakeGif_Char = MakeGif_Char \ 256 'shift the bit buffer right 8 bits
        MakeGif_CurrnetBit = MakeGif_CurrnetBit - 8 'now we have 8 less bits
    LOOP 'until we don't have a full byte
END SUB


SUB BufferWrite (a) STATIC
    IF MakeGif_OAddress = MakeGif_OEndAddress THEN 'are we at the end of the buffer?
        PUT MakeGif_GIFfile, , MakeGif_OutBuffer ' yup, write it out and
        MakeGif_OAddress = MakeGif_OStartAddress ' start all over
    END IF
    POKE MakeGif_OAddress, a 'put byte in buffer
    MakeGif_OAddress = MakeGif_OAddress + 1 'increment position
END SUB

'This routine gets one pixel from the display.
FUNCTION GetByte STATIC
    GetByte = POINT(MakeGif_X, MakeGif_Y) 'get the "byte"
    MakeGif_X = MakeGif_X + 1 'increment MakeGif_X coordinate
    IF MakeGif_X > MakeGif_MaxX THEN 'are we too far?
        MakeGif_X = MakeGif_MinX 'go back to start
        MakeGif_Y = MakeGif_Y + 1 'increment MakeGif_Y coordinate
        IF MakeGif_Y > MakeGif_MaxY THEN 'are we too far down?
            MakeGif_Done = -1 ' yup, flag it then
        END IF
    END IF
END FUNCTION
'%%%%%%%%%%%%%%%%%%%%%%%%%%%END OF GIF STUFF%%%%%%%%%%%%%%%%%%%%%%%%%%%%
