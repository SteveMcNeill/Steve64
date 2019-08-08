$CHECKING:OFF
SCREEN _NEWIMAGE(1280, 720, 32)
_DELAY .5
_SCREENMOVE _MIDDLE

DEFLNG A-Z
TYPE DataType
    Word AS STRING * 25 'up to 25 letters in a word!
    Value AS STRING * 26
END TYPE


REDIM Words(0 TO 30000) AS DataType
DIM Anagrams(0 TO 30000, 0 TO 10) AS LONG
DIM EndLine AS STRING, Endlength AS LONG
DIM t AS _FLOAT 'high precisition timer
DIM letters(255) AS _UNSIGNED _BYTE
DIM m1 AS _MEM, m2 AS _MEM, m3 AS _MEM
DIM a AS _UNSIGNED _BYTE
DIM matched(30000) AS _BYTE
m1 = _MEM(letters()): m2 = _MEM(Words()): m3 = _MEM(wd)



t = TIMER
OPEN "unixdict.txt" FOR BINARY AS #1
temp$ = SPACE$(LOF(1))
GET #1, 1, temp$ 'just grab the whole datafile from the drive in one swoop
CLOSE #1
PRINT USING "##.#### seconds to load data from disk."; TIMER - t
t = TIMER


IF INSTR(temp$, CHR$(13)) THEN
    EndLine = CHR$(13) + CHR$(10)
    Endlength = 2
ELSE
    EndLine = CHR$(10)
    Endlength = 1
END IF

index = -1 'we want our first word to be indexed at 0, for ease of array/mem swappage
DO 'and parse it manually into our array
    skip:
    enter = INSTR(oldenter + 1, temp$, EndLine)
    IF enter THEN
        wd$ = MID$(temp$, oldenter, enter - oldenter)
        oldenter = enter + Endlength
    ELSE
        wd$ = MID$(temp$, oldenter)
    END IF


    l = LEN(wd$)

    FOR j = 1 TO 26 'Remove all invalid words, such as those with 's at the end of them.
        IF INSTR("abcdefghijklmnopqrstuvwxyz ", MID$(wd$, j, 1)) = 0 THEN GOTO skip
    NEXT

    index = index + 1: Words(index).Word = wd$
    ERASE letters
    j = 0
    DO UNTIL j = l
        _MEMGET m2, m2.OFFSET + m2.ELEMENTSIZE * (index) + j, a
        letters(a) = letters(a) + 1 'and count them
        j = j + 1
    LOOP
    _MEMCOPY m1, m1.OFFSET + 97, 26 TO m2, m2.OFFSET + m2.ELEMENTSIZE * (index) + 25

LOOP UNTIL enter = 0
CLOSE #1
PRINT USING "##.#### seconds to parse data into array."; TIMER - t
t = TIMER

combsort Words(), index


i = 1
DO UNTIL i > index

    IF matched(i) = 0 THEN
        count = 0
        DO
            count = count + 1
            c = i + count
            IF c > index THEN EXIT DO
            IF Words(i).Value <> Words(c).Value THEN EXIT DO
            Anagrams(anagram_count, count) = c
            matched(c) = -1
        LOOP
        IF count > 1 THEN
            Anagrams(anagram_count, 0) = i
            Anagrams(anagram_count, 10) = count
            i = c - 1
            anagram_count = anagram_count + 1
        END IF
    END IF
    i = i + 1
LOOP

PRINT USING "##.#### seconds to make matches."; TIMER - t
$CHECKING:ON
PRINT
PRINT

INPUT "Anagram Pool Limit Size (Or larger) =>"; limit
IF limit < 1 THEN END
FOR i = 0 TO anagram_count - 1
    v = Anagrams(i, 10)
    IF v >= limit THEN
        FOR j = 0 TO v
            SELECT CASE j
                CASE 0
                CASE v: PRINT
                CASE ELSE: PRINT ", ";
            END SELECT
            PRINT LEFT$(Words(Anagrams(i, j)).Word, INSTR(Words(Anagrams(i, j)).Word, " "));
        NEXT
    END IF
NEXT
END


SUB combsort (array() AS DataType, index AS LONG)
    DIM gap AS LONG
    'This is the routine I tend to use personally and promote.
    'It's short, simple, and easy to implement into code.

    gap = UBOUND(array)

    DO
        gap = INT(gap / 1.247330925103979)
        IF gap < 1 THEN gap = 1
        i = 0
        swapped = 0
        DO
            IF array(i).Value > array(i + gap).Value THEN
                SWAP array(i), array(i + gap)
                swapped = -1
            END IF
            i = i + 1
        LOOP UNTIL i + gap > index
    LOOP UNTIL gap = 1 AND swapped = 0
END SUB

