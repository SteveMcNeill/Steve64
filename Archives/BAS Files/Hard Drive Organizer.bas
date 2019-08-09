'NOTE:  USE THIS AT YOUR OWN RISK.
'AS WITH ANY PROGRAM WHICH MAKES MASS FILE/FOLDER CHANGES, THIS **COULD** MAKE A MESS OF THE DIRECTORY YOU POINT IT AT.
'BE AWARE OF THE POTENTIAL FOR UNDESIRABLE RESULTS BEFORE RUNNING.




ON ERROR GOTO handler 'A simple resume on error trap.
'This is to let us continue on without pause in case of renaming errors like the following:
'Stephen King - Book.txt
'King, Stephen - Book.txt
'Say both the above were in our folder and we tried to rename the second book to proper order; what would happen?
'We'd get an error message!  (We can't have 2 files with the exact same names!)

'So the error handler gets rid of that error message and let's us continue on, uninterrupted.
'However, we still didn't change the name on the file that would have errored out.
'It would still be King, Stephen - Book.txt because of the conflict with the other file.
'Be certain to look for unchanged names (sort folders alphabetically) to check for this.

SCREEN _NEWIMAGE(1280, 720, 32)
Dir$ = "z:\" 'Set to the directory which you want to work with
ShellCommand$ = "dir /b " + Dir$ + " >temp.txt"
SHELL ShellCommand$

CONST SetLowerCaseExtension = -1 '-1 to make a .TXT file become a .txt file for example.  Else 0 to leave the extentions alone.
CONST StripUnderScores = -1 '-1 to remove underscores from a books title, 0 to leave them alone.
CONST SetAuthorName = -1 '-1 to manually set the name you want before the first hyphon, 0 to have the program unscramble from LAST, FIRST to FIRST LAST name order.
CONST Author = "G. K. Chesterton" 'If SetAuthorName is -1 then this must be set to the name you want to use for the author.  Else, it can be anything as it's ignored.
CONST SaveChanges = 0 '-1 to make the actual changes, 0 to just preview them to see how things will look.
CONST ChangeFolder = 0 '-1 to change folder names instead of file names (does not add extensions to anything)
CONST AddAuthor = 0 '-1 to prefix the author name assigned above to a list of files.  Useful in case all you have is the book title without any author name at all.

'NOTE:  Not all these flags are ment to work together.  If you're changing a set of folder names, don't have files in the folder as well.
'This is what I generally use for my own mass renaming purposes, and is more or less here as a code example so someone could use it to generate
'the actual result that they'd want for their own personal needs.

'I'd suggest always setting SaveChanges to 0 for a trial run to see how things might look, before doing any actual changes to files or folders.


OPEN "temp.txt" FOR INPUT AS #1
DO UNTIL EOF(1)
    LINE INPUT #1, f$ 'file name
    h = INSTR(f$, "-") 'hyphen location
    IF ChangeFolder THEN h = LEN(f$) + 1
    IF SetAuthorName = 0 THEN
        'We want the program to arrange an existing name to first name, last name; instead of last name, first name.
        c = INSTR(f$, ",") 'comma location
        IF c > 0 AND c < h THEN
            'Our comma is before the hyphen, so our book filename is written as "King, Stephen - Book Title.txt"
            ln$ = RTRIM$(LTRIM$(LEFT$(f$, c - 1))) 'author last name "King"
            fn$ = RTRIM$(LTRIM$(MID$(f$, c + 1, h - c - 1))) 'author first name "Stephen"
            MID$(ln$, 1, 1) = UCASE$(MID$(ln$, 1, 1)) 'be certain to capitalize last name
            MID$(fn$, 1, 1) = UCASE$(MID$(fn$, 1, 1)) 'be certain to capitalize first name
            a$ = fn$ + " " + ln$ 'author name in proper order as "Stephen King"
        ELSE
            'We want to manually choose what name we want to use to put before the hyphon
            a$ = RTRIM$(LTRIM$(LEFT$(f$, h - 1)))
        END IF
    ELSE
        a$ = Author$
    END IF
    t$ = LTRIM$(RTRIM$(MID$(f$, h + 1))) 'the title of the book itself.
    MID$(t$, 1, 1) = UCASE$(MID$(t$, 1, 1)) 'be certain to capitalize the first letter of the book title

    IF h OR SetAuthorName THEN
        b$ = a$ + " - " + t$ 'The complete book name now.  "Stephen King - Book Title.txt"
    ELSE
        b$ = f$ 'We had no hyphon, and we're not setting an author of our own, so the title is just the title with perhaps a few corrections.
    END IF
    IF ChangeFolder THEN b$ = a$
    IF AddAuthor THEN b$ = Author$ + " - " + f$

    IF SetLowerCaseExtension THEN MID$(b$, LEN(b$) - 2, 3) = LCASE$(MID$(b$, LEN(b$) - 2, 3)) 'make the extension lowercase if the flag is set.
    IF StripUnderScores THEN Remove_US b$ 'Remove underscores from the booktitle if we have the CONST set so we do so.

    f$ = Dir$ + f$: b$ = Dir$ + b$ 'Add in the directory path before renaming the file

    'Let's print out the changes so we can see it in action
    COLOR _RGB32(255, 255, 255) 'Original in White
    PRINT f$
    COLOR _RGB32(255, 0, 0) ' Changes in Red
    PRINT b$

    'Unremark the following line to make the actual changes to the files/folders themselves.
    IF SaveChanges THEN NAME f$ AS b$
LOOP

CLOSE
KILL "temp.txt" 'Perform clean-up

PRINT
PRINT "There were "; Bug; " problems which we ignored."
IF Bug THEN PRINT "Be certain to check your folder to see which filenames were not changed."

SLEEP
SYSTEM

handler:
Bug = Bug + 1
RESUME NEXT


SUB Remove_US (text$)
    DO 'The following loop replaces underscores with plain spaces. It seems I find things like King,Stephen-This_book.txt quite a lot.
        l = INSTR(text$, "_")
        IF l > 0 THEN MID$(text$, l, 1) = " "
    LOOP UNTIL l = 0
END SUB

