DECLARE CUSTOMTYPE LIBRARY "direntry"
    FUNCTION FILE_load_dir& ALIAS load_dir (s AS STRING)
    FUNCTION FILE_has_next_entry& ALIAS has_next_entry ()
    SUB FILE_close_dir ALIAS close_dir ()
    SUB FILE_get_next_entry ALIAS get_next_entry (s AS STRING, flags AS LONG, file_size AS LONG)
    SUB FILE_get_current_dir ALIAS get_current_dir (s AS STRING)
    FUNCTION FILE_current_dir_length& ALIAS current_dir_length ()
END DECLARE


_DEFINE A-Z AS LONG
SCREEN _NEWIMAGE(800, 600, 32)
f$ = SelectFile$("*.BI;*.BAS;*.BM;*.*", 60, 60)
PRINT "You picked "; f$




FUNCTION SelectFile$ (search$, x AS INTEGER, y AS INTEGER)
'save some old values
LoadFile_DC = _DEFAULTCOLOR: LoadFile_BG = _BACKGROUNDCOLOR
LoadFile_s = _SOURCE: LoadFile_d = _DEST
f = _FONT: _FONT 16
'some variables

LoadFile_BoxColor = &HFFAAAAFF
LoadFile_FolderColor = &HFFFFFF00
LoadFile_FileColor = &HFFFFFFFF
IF INSTR(_OS$, "[WINDOWS]") THEN LoadFile_Slash$ = "\" ELSE LoadFile_Slash$ = "/"
LoadFile_Dir$ = SPACE$(FILE_current_dir_length)
FILE_get_current_dir LoadFile_Dir$
LoadFile_Dir$ = LoadFile_Dir$ + LoadFile_Slash$
LoadFile_w = 639: LoadFile_h = 479
REDIM LoadFile_Label(0) AS STRING: LoadFile_Label(0) = "DIR"
REDIM LoadFile_DirList(-1 TO 9, -1 TO 9999) AS STRING
LoadFile_last = 1
REDIM Drives(0) AS STRING

'some error checking
IF search$ = "" THEN EXIT SUB 'We can't search for nothing!

'Copy background
PCOPY 0, 1
'set workscreen
LoadFile_ws = _NEWIMAGE(640, 480, 32)

'Count our filetypes to display
LoadFile_TypeCount = 0
DO
    LoadFile_TypeCount = LoadFile_TypeCount + 1
    LoadFile_l = INSTR(LoadFile_l + 1, search$, ";") ' look for ; to denote more files
    REDIM _PRESERVE LoadFile_Label(LoadFile_TypeCount) AS STRING
    IF LoadFile_l > 0 THEN LoadFile_Label(LoadFile_TypeCount) = MID$(search$, LoadFile_last + 1, LoadFile_l - LoadFile_last - 1) ELSE LoadFile_Label(LoadFile_TypeCount) = MID$(search$, LoadFile_last + 1, LEN(search$) - LoadFile_last)
    LoadFile_last = LoadFile_l + 1
LOOP UNTIL LoadFile_l = 0
LoadFile_l = 640 / (LoadFile_TypeCount + 1)
REDIM LoadFile_start(LoadFile_TypeCount), LoadFile_previous(LoadFile_TypeCount), LoadFile_more(LoadFile_TypeCount), LoadFile_Count(LoadFile_TypeCount)
FOR i = 0 TO LoadFile_TypeCount: LoadFile_start(i) = 1: NEXT

'Get the windows drive letters
IF INSTR(_OS$, "[WINDOWS]") THEN
    SHELL _HIDE CHR$(34) + "wmic logicaldisk get name" + CHR$(34) + ">TempDirList.txt"
    REDIM Drives(0) AS STRING

    OPEN "TempDirList.txt" FOR INPUT AS #1
    LINE INPUT #1, junk$ 'First line is  name
    counter = 0
    DO UNTIL EOF(1)
        counter = counter + 1
        INPUT #1, junk$ 'drive name
        REDIM _PRESERVE Drives(counter) AS STRING
        IF LEN(junk$) > 1 THEN junk$ = MID$(junk$, 2, 1) + ":" ELSE junk$ = "": counter = counter - 1
        IF junk$ <> "" THEN
            Drives(counter) = junk$
        END IF
    LOOP
    CLOSE #1
    KILL "TempDirList.txt"
END IF


_SOURCE LoadFile_ws: _DEST LoadFile_ws
DO

    FOR i = 0 TO LoadFile_TypeCount
        LoadFile_Count(i) = 0
        FOR j = 0 TO 9999
            LoadFile_DirList(i, j) = ""
        NEXT
    NEXT
    'Generate our updated directory listings.

    IF FILE_load_dir&(LoadFile_Dir$ + CHR$(0)) THEN
        DO
            LoadFile_length = FILE_has_next_entry 'Get length of next entry
            IF LoadFile_length > -1 THEN 'If we have a next entry
                LoadFile_nam$ = SPACE$(LoadFile_length) 'Set the size of our string
                FILE_get_next_entry LoadFile_nam$, LoadFile_flags, LoadFile_file_size 'Get the file's name, size, and 'flags'
                'Check if it's a file or a directory

                IF _DIREXISTS(LoadFile_Dir$ + LoadFile_nam$) THEN
                    IF LoadFile_nam$ <> "." THEN
                        LoadFile_Count(0) = LoadFile_Count(0) + 1
                        LoadFile_DirList(0, LoadFile_Count(0)) = LoadFile_nam$
                    END IF
                ELSE 'We have a file
                    FOR i = 1 TO LoadFile_TypeCount
                        LoadFile_ext$ = RIGHT$(LoadFile_nam$, LEN(LoadFile_Label(i)))
                        IF UCASE$(LoadFile_ext$) = UCASE$(LoadFile_Label(i)) THEN
                            LoadFile_Count(i) = LoadFile_Count(i) + 1
                            LoadFile_DirList(i, LoadFile_Count(i)) = LEFT$(LoadFile_nam$, LEN(LoadFile_nam$) - LEN(LoadFile_Label(i)))
                            EXIT FOR
                        ELSEIF LoadFile_Label(i) = ".*" THEN
                            LoadFile_Count(i) = LoadFile_Count(i) + 1
                            LoadFile_DirList(i, LoadFile_Count(i)) = LoadFile_nam$
                        END IF
                    NEXT
                END IF
            END IF
        LOOP UNTIL LoadFile_length = -1
        FILE_close_dir
    END IF

    FOR i = 1 TO UBOUND(drives)
        LoadFile_Count(0) = LoadFile_Count(0) + 1
        LoadFile_DirList(0, LoadFile_Count(0)) = Drives(i)
    NEXT

    updatelist:

    CLS , &HFF005050 'Draw a nice display box
    COLOR , 0
    LINE (0, 0)-(LoadFile_w, LoadFile_h + 5 - 2 * 16), LoadFile_BoxColor, B
    LINE (1, 1)-(LoadFile_w - 1, LoadFile_h + 6 - 2 * 16), LoadFile_BoxColor, B
    LINE (0, 0)-(LoadFile_w, LoadFile_h), LoadFile_BoxColor, B
    LINE (1, 1)-(LoadFile_w - 1, LoadFile_h - 1), LoadFile_BoxColor, B

    LINE (0, 16 + 3)-(LoadFile_w, 16 + 3), LoadFile_BoxColor
    LINE (0, 16 + 4)-(LoadFile_w, 16 + 4), LoadFile_BoxColor
    FOR i = 0 TO LoadFile_TypeCount
        _PRINTSTRING (i * LoadFile_l + (LoadFile_l - 8 * LEN(LoadFile_Label(i))) / 2, 2), LoadFile_Label(i)
        LINE (i * LoadFile_l, 0)-(i * LoadFile_l, LoadFile_h + 5 - 2 * 16), LoadFile_BoxColor
    NEXT

    LINE (627, 2)-(637, 18), &HFFFF0000, BF
    LINE (626, 2)-(637, 18), &HFF000000, B

    _PRINTSTRING (628, 2), "X"
    IF selection > 0 THEN
        IF LoadFile_Label(row) <> ".*" AND LoadFile_Label(row) <> "DIR" THEN temp$ = LoadFile_DirList(row, selection) + LoadFile_Label(row) ELSE temp$ = LoadFile_DirList(row, selection)
        IF LoadFile_DirList(row, selection) = "" THEN temp$ = ""
        selection = 0
    END IF
    _PRINTSTRING (10, 28 * 16 + 7), LoadFile_Dir$
    _PRINTSTRING (630 - LEN(temp$) * 8, 28 * 16 + 7), temp$
    IF temp$ = "" THEN oldselection = 0
    IF oldselection > 0 THEN LINE (row * LoadFile_l, (oldselection + 1) * 16 + 5)-((row + 1) * LoadFile_l, (oldselection + 2) * 16 + 5), &HAAAAA000, BF

    FOR i = 0 TO UBOUND(LoadFile_label)
        IF i = 0 THEN COLOR LoadFile_FolderColor ELSE COLOR LoadFile_FileColor
        counter = 0
        FOR j = LoadFile_start(i) TO LoadFile_start(i) + 24
            counter = counter + 1
            IF LoadFile_DirList(i, j) = "" THEN EXIT FOR
            _PRINTSTRING (i * LoadFile_l + 5, (counter + 1) * 16 + 7), LEFT$(LoadFile_DirList(i, j), LoadFile_l / 8 - 2)
        NEXT
        IF j = LoadFile_start(i) + 25 THEN LoadFile_more(i) = -1 ELSE LoadFile_more(i) = 0
        IF LoadFile_start(i) > 1 THEN LoadFile_previous(i) = -1 ELSE LoadFile_previous(i) = 0
        IF LoadFile_more(i) THEN
            LINE (i * LoadFile_l + 2, 27 * 16 + 5)-((i + 1) * LoadFile_l - 3, 28 * 16 + 3), &HFFFF0000, BF
            LINE (i * LoadFile_l + 2, 27 * 16 + 5)-((i + 1) * LoadFile_l - 3, 28 * 16 + 3), BoxColor, B
            COLOR &HFFFFFF00: _PRINTSTRING (i * LoadFile_l + (LoadFile_l - 8 * 11) / 2, 27 * 16 + 5), "SCROLL DOWN"
            COLOR LoadFile_FileColor
        END IF
        IF LoadFile_previous(i) THEN
            LINE (i * LoadFile_l + 2, 16 + 5)-((i + 1) * LoadFile_l - 3, 2 * 16 + 3), &HFFFF0000, BF
            LINE (i * LoadFile_l + 2, 16 + 5)-((i + 1) * LoadFile_l - 3, 2 * 16 + 3), BoxColor, B
            COLOR &HFFFFFF00: _PRINTSTRING (i * LoadFile_l + (LoadFile_l - 8 * 9) / 2, 16 + 5), "SCROLL UP"
            COLOR LoadFile_FileColor
        END IF
    NEXT

    _PUTIMAGE (0 + x, 0 + y)-(640 + x, 480 + y), LoadFile_ws, 0
    _DISPLAY

    change = 0
    DO
        _DELAY .05
        LoadFile_LMB = 0 'This sets the left mouse button as unacceptable.
        a = _KEYHIT
        SELECT CASE a
            CASE 8 'backspace
                temp$ = LEFT$(temp$, LEN(temp$) - 1)
                change = -1
            CASE 13 'enter
                DO: LOOP UNTIL INKEY$ = "" 'Clear the keyboard buffer so it doesn't affect the main program.
                temp$ = LoadFile_Dir$ + temp$
                COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY: SelectFile$ = temp$ 'Restore our old settings
                _FONT f
                EXIT SUB 'And leave
            CASE 27 'If ESC is pressed then...
                DO: LOOP UNTIL INKEY$ = "" 'Clear the keyboard buffer so it doesn't affect the main program.
                COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY: SelectFile$ = "" 'Restore our old settings
                _FONT f
                EXIT SUB 'And leave
            CASE 32 TO 126
                temp$ = temp$ + CHR$(a)
                change = -1
        END SELECT
        DO
            MS = MS + _MOUSEWHEEL
            IF _MOUSEBUTTON(1) = 0 THEN LoadFile_LMB = -1 'Only by lifting the mouse, will we count it as down
            'Note: we ignore LoadFile_LMB for the scroll bars, so we can just hold it down and scroll happily forever and ever...
            'or until we get to the limit of our file list.
            'We only check LoadFile_LMB when actually trying to select an item from our list.   No more "OOP!  I held it too long and did something I didn't want to do!"
            'Now we click once to select, click again to accept that selection.
        LOOP WHILE _MOUSEINPUT
        MX = _MOUSEX: MY = _MOUSEY
        IF _MOUSEBUTTON(2) OR (LoadFile_LMB AND MX > 626 + x AND MX < 638 + x AND MY > 1 + y AND MY < 19 + y AND _MOUSEBUTTON(1)) THEN
            'restore those old values, and just exit.  Right mouse is an escape
            COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY: SelectFile$ = ""
            _FONT f
            EXIT SUB
        END IF





        IF _MOUSEBUTTON(1) THEN 'Without the mouse being down, we don't need to check squat!
            'Check the 2 roLoadFile_ws for a click in the proper Y position
            IF MY >= 16 + 5 + y AND MY <= 2 * 16 + 3 + y THEN 'We're on the top row
                FOR j = 0 TO UBOUND(LoadFile_label)
                    IF LoadFile_previous(j) AND MX >= j * LoadFile_l + 2 + x AND MX <= (j + 1) * LoadFile_l - 3 + x THEN
                        LoadFile_start(j) = LoadFile_start(j) - 1
                        change = -1: selection = 0: click = 0: temp$ = ""
                        EXIT FOR
                    END IF
                NEXT
            ELSEIF MY >= 27 * 16 + 5 + y AND MY <= 28 * 16 + 3 + y THEN 'We're on the bottom row
                FOR j = 0 TO UBOUND(LoadFile_label)
                    IF LoadFile_more(j) AND MX >= j * LoadFile_l + 2 + x AND MX <= (j + 1) * LoadFile_l - 3 + x THEN
                        LoadFile_start(j) = LoadFile_start(j) + 1
                        change = -1: selection = 0: click = 0: temp$ = ""
                        EXIT FOR
                    END IF
                NEXT
            ELSEIF MY >= 37 + y AND MY <= 437 + y AND LoadFile_LMB THEN 'It's in a column somewhere.  Did someone click an item?!
                FOR j = 0 TO UBOUND(LoadFile_label)
                    IF MX >= j * LoadFile_l + 2 + x AND MX <= (j + 1) * LoadFile_l - 3 + x THEN
                        row = j
                        oldselection = INT((MY - y - 37) / 16) + 1
                        selection = LoadFile_start(j) + oldselection - 1
                        change = -1
                        click = -1
                        EXIT FOR
                    END IF
                NEXT
            END IF
        END IF
        IF MS <> 0 THEN
            IF MY >= 37 + y AND MY <= 437 + y AND LoadFile_LMB THEN 'It's in a column somewhere.  Did someone click an item?!
                FOR j = 0 TO UBOUND(LoadFile_label)
                    IF MX >= j * LoadFile_l + 2 + x AND MX <= (j + 1) * LoadFile_l - 3 + x THEN
                        IF LoadFile_previous(j) AND MS < 1 THEN
                            LoadFile_start(j) = LoadFile_start(j) - 5
                            IF LoadFile_start(j) < 1 THEN LoadFile_start(j) = 1
                            change = -1: selection = 0: click = 0: temp$ = ""
                            MS = 0
                        ELSEIF LoadFile_more(j) AND MS > 1 THEN
                            LoadFile_start(j) = LoadFile_start(j) + 5
                            change = -1: selection = 0: click = 0: temp$ = ""
                            MS = 0
                        END IF
                        EXIT FOR
                    END IF
                NEXT
            ELSE MS = 0
            END IF
        END IF
        _DISPLAY
    LOOP UNTIL change
    IF click THEN 'we clicked something besides a scroll bar
        IF LoadFile_Label(row) <> ".*" AND LoadFile_Label(row) <> "DIR" THEN temp1$ = LoadFile_DirList(row, selection) + LoadFile_Label(row) ELSE temp1$ = LoadFile_DirList(row, selection)
        IF temp$ = temp1$ THEN
            'We picked one!
            SELECT CASE LoadFile_Label(row)
                CASE "DIR"
                    SELECT CASE LoadFile_DirList(row, selection)
                        CASE "" 'Do nothing with blank directories
                        CASE ".." 'Up a folder
                            DO
                                LoadFile_Dir$ = LEFT$(LoadFile_Dir$, LEN(LoadFile_Dir$) - 1)
                            LOOP UNTIL RIGHT$(LoadFile_Dir$, 1) = LoadFile_Slash$ OR LEN(LoadFile_Dir$) = 0
                        CASE ELSE 'To a specific folder
                            IF LEN(LoadFile_DirList(row, selection)) = 2 AND RIGHT$(LoadFile_DirList(row, selection), 1) = ":" THEN
                                'It's a directory change
                                LoadFile_Dir$ = LoadFile_DirList(row, selection) + LoadFile_Slash$
                            ELSE
                                LoadFile_Dir$ = LoadFile_Dir$ + LoadFile_DirList(row, selection) + LoadFile_Slash$
                            END IF
                    END SELECT
                    FOR i = 0 TO UBOUND(Loadfile_start)
                        LoadFile_start(i) = 1
                    NEXT
                    selection = 0: temp$ = "": oldselection = 0
                CASE ".*": SelectFile$ = LoadFile_Dir$ + temp$: EXIT DO
                CASE ELSE: SelectFile$ = LoadFile_Dir$ + temp$: EXIT DO
            END SELECT
        END IF
        IF row > 0 THEN _DELAY .2: GOTO updatelist
    ELSE
        _DELAY .05
        GOTO updatelist
    END IF
LOOP
'restore those old values
COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY
_FONT f
END SUB

'If you don't have a copy of direntry.h in your QB64 folder, then copy the following code into a new IDE window.
'Then remove the remarks.
'And save it as direntry.h
'direntry.h is required for this to work properly with the library files.
'I thought adding the code here would be a way to make certain that it'd be easy to recover the file
'in case something ever happened and it was accidently deleted off the drive for some reason.

'#include <dirent.h>
'#include <sys/stat.h>
'#include <unistd.h>

'const int IS_DIR_FLAG = 1, IS_FILE_FLAG = 2;

'DIR *pdir;
'struct dirent *next_entry;
'struct stat statbuf1;

'char current_dir[FILENAME_MAX];
'#ifdef QB64_WINDOWS
'  #define GetCurrentDir _getcwd
'#else
'  #define GetCurrentDir getcwd
'#endif

'int load_dir (char * path) {
'  struct dirent *pent;
'  struct stat statbuf1;
'//Open current directory
'pdir = opendir(path);
'if (!pdir) {
'return 0; //Didn't open
'}
'return -1;
'}

'int has_next_entry () {
'  next_entry = readdir(pdir);
'  if (next_entry == NULL) return -1;

'  stat(next_entry->d_name, &statbuf1);
'  return strlen(next_entry->d_name);
'}

'void get_next_entry (char * nam, int * flags, int * file_size) {
'  strcpy(nam, next_entry->d_name);
'  if (S_ISDIR(statbuf1.st_mode)) {
'    *flags = IS_DIR_FLAG;
'  } else {
'    *flags = IS_FILE_FLAG;
'  }
'  *file_size = statbuf1.st_size;
'  return ;
'}

'void close_dir () {
'  closedir(pdir);
'  pdir = NULL;
'  return ;
'}

'int current_dir_length () {
'  GetCurrentDir(current_dir, sizeof(current_dir));
'  return strlen(current_dir);
'}

'void get_current_dir(char *dir) {
'  memcpy(dir, current_dir, strlen(current_dir));
'  return ;
'}

