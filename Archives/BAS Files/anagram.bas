$CHECKING:OFF

SCREEN _NEWIMAGE(1280, 720, 32)
_SCREENMOVE _MIDDLE

DIM EndLine AS STRING, EndLength AS LONG
DIM word(22, 5000) AS STRING '2d word list, by lenth, count
DIM MemWord AS _MEM
MemWord = _MEMNEW(30 * 5000 * 30) '30 word lists  (from 0 to 29 letters), of up to 5000 words, of up to 30 characters each -- much mmore than what is actually needed for the program, but might be useful if other dictionaries are used in the future.
DIM count(25) AS LONG 'a count of numbers
DIM letters1(65 TO 90) AS _BYTE, letters2(65 TO 90) AS _BYTE
DIM match(22, 5000) AS LONG
DIM Anagrams(10000) AS STRING
DIM m1 AS _MEM, m2 AS _MEM
DIM t1 AS STRING * 26, t2 AS STRING * 26
DIM a AS _UNSIGNED _BYTE, highest AS LONG
m1 = _MEM(letters1()): m2 = _MEM(letters2())
erase$ = STRING$(26, 0)


t = TIMER
OPEN "unixdict.txt" FOR BINARY AS #1
temp$ = SPACE$(LOF(1))
GET #1, 1, temp$ 'just grab the whole datafile from the drive in one swoop
CLOSE #1

IF INSTR(temp$, CHR$(13)) THEN
    EndLine = CHR$(13) + CHR$(10)
    EndLength = 2
ELSE
    EndLine = CHR$(10)
    EndLength = 1
END IF

DO 'and parse it manually into our array
    enter = INSTR(oldenter + 1, temp$, EndLine)
    IF enter THEN
        wd$ = MID$(temp$, oldenter, enter - oldenter)
        oldenter = enter + EndLength
    ELSE
        wd$ = MID$(temp$, oldenter)
    END IF
    l = LEN(wd$)
    IF l > 2 THEN
        wd$ = UCASE$(wd$)
        badword = 0
        FOR j = 1 TO 26 'Remove all invalid words, such as those with 's at the end of them.
            IF INSTR("ABCDEFGHIJKLMNOPQRSTUVWXYZ", MID$(wd$, j, 1)) = 0 THEN badword = -1: EXIT FOR
        NEXT
        IF NOT badword THEN
            count(l) = count(l) + 1
            word(l, count(l)) = wd$
            _MEMPUT MemWord, MemWord.OFFSET + (l * 5000 * 30) + (count(l) * 30), wd$
        END IF
    END IF
LOOP UNTIL enter = 0
CLOSE #1
PRINT TIMER - t; "seconds presorting"
t = TIMER

'PRINT "Word Count by Length"
'FOR i = 1 TO 22: PRINT i, count(i): NEXT
'FOR i = 1 TO count(3): PRINT word(3, i); "  ";: NEXT




l = 3
DO UNTIL l > 22
    IF count(l) > 4 THEN 'only if we have more than 5 words to begin with, do we even bother
        i = 1
        DO UNTIL i > count(l) - 1
            IF match(l, i) <> 0 THEN GOTO nextword 'no need to check words we've already matched
            highest = 0
            a$ = SPACE$(l)
            _MEMGET MemWord, MemWord.OFFSET + (l * 5000 * 30) + (i * 30), a$

            'a$ = word(l, i)
            _MEMPUT m1, m1.OFFSET, erase$
            '_MEMPUT m3, m3.OFFSET, a$

            j = 0: t& = i * 30
            DO UNTIL j = l
                '_MEMPUT m4, m4.OFFSET, _MEMGET(m3, m3.OFFSET + j - 1, _UNSIGNED _BYTE) AS _UNSIGNED _BYTE 'get the individual letter
                _MEMGET MemWord, MemWord.OFFSET + (l * 150000) + t& + j, a
                IF a > highest THEN highest = a
                letters1(a) = letters1(a) + 1 'and count them
                j = j + 1
            LOOP
            anagram_count = 0

            j = i + 1
            DO UNTIL j > count(l)
                _MEMPUT m2, m2.OFFSET, erase$
                '_MEMPUT m3, m3.OFFSET, word(l, j)
                k = 0: t& = j * 30
                DO UNTIL k = l
                    '_MEMPUT m4, m4.OFFSET, _MEMGET(m3, m3.OFFSET + k - 1, _UNSIGNED _BYTE) AS _UNSIGNED _BYTE
                    _MEMGET MemWord, MemWord.OFFSET + (l * 150000) + t& + k, a
                    IF k = 0 AND a > highest THEN GOTO checkdone
                    letters2(a) = letters2(a) + 1 'and count them
                    IF letters2(a) > letters1(a) THEN GOTO nextcheck 'We have more of one type of letter than the first word contained
                    k = k + 1
                LOOP

                _MEMGET m1, m1.OFFSET, t1: _MEMGET m2, m2.OFFSET, t2 'compare the letter count of both words
                IF t1 <> t2 THEN GOTO nextcheck 'if they're not a match, then check the next word
                anagram_count = anagram_count + 1
                match(l, j) = -1
                a$ = a$ + ", " + word(l, j)
                nextcheck:
                j = j + 1
            LOOP
            checkdone:
            IF anagram_count THEN total_anagrams = total_anagrams + 1: Anagrams(total_anagrams) = a$
            nextword:
            i = i + 1
        LOOP
    END IF
    l = l + 1
LOOP
PRINT TIMER - t; "seconds matching all anagrams ("; total_anagrams; ")"

FOR i = 1 TO total_anagrams
    l = ((i - 1) MOD 5) * 32 + 1
    LOCATE , l
    IF l < 120 THEN PRINT Anagrams(i); ELSE PRINT Anagrams(i)
    IF i MOD 200 = 0 THEN SLEEP: CLS
NEXT


