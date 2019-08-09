'DBF to CSV text converter

'Program written by Steve McNeill @ 9/19/2012

'Code is free to use, abuse, modify, destroy, steal, copy, share, and alter in any way anyone wishes.
'Just be aware, I'm not responsible if it melts your computer, fries your brain, or makes you sing like a drunken sailor.
'Use is purely at your own risk, but it seems safe enough to me!

'All this does is convert old dbf files into a simple CSV text file, which can then be read into any program which you wish to use the data with.
'Your old files stay as they are, and it does nothing to them except read them and then give you a new, converted file to work with.

'change file$ and file1$ to the name of your DBF and new converted filename, respectively.

'No credit, cash, check, or money order needed for this.  Enjoy!!

REM $DYNAMIC

TYPE DBF_Header
    FileType AS _UNSIGNED _BYTE
    Year AS _UNSIGNED _BYTE
    Month AS _UNSIGNED _BYTE
    Day AS _UNSIGNED _BYTE
    RecordNumber AS _UNSIGNED LONG
    FirstRecord AS _UNSIGNED INTEGER
    RecordLength AS _UNSIGNED INTEGER
    ReservedJunk AS STRING * 16
    TableFlag AS _UNSIGNED _BYTE
    CodePageMark AS _UNSIGNED _BYTE
    ReservedJunk1 AS STRING * 2
END TYPE

TYPE Field_Subrecord
    FieldName AS STRING * 11
    FieldType AS STRING * 1
    Displacement AS _UNSIGNED LONG
    FieldLength AS _UNSIGNED _BYTE
    FieldDecimal AS _UNSIGNED _BYTE
    FieldFlags AS _UNSIGNED _BYTE
    AutoNext AS _UNSIGNED LONG
    AutoStep AS _UNSIGNED _BYTE
    ReservedJunk AS STRING * 8
END TYPE

TYPE DBF_HeaderTerminator
    EndCode AS _UNSIGNED _BYTE 'Our End of Field Code is a CHR$(13), or 13 if we read it as a byte
END TYPE

TYPE DBF_VFPInfo
    Info AS STRING * 263
END TYPE

DIM DataH AS DBF_Header
DIM DataFS(1) AS Field_Subrecord
DIM DataHT AS DBF_HeaderTerminator
DIM DataVFP AS DBF_VFPInfo

file$ = ".\tempdata.dbf"
file2$ = ".\converted.txt"

Get_Header file$, DataH
'Display_Header DataH
Get_Fields file$, DataFS()
'Display_Fields DataFS()
Print_Data file$, DataH, DataFS(), file2$
PRINT "Your file has been converted."
PRINT "The original file was: "; file$
PRINT "The converted file is: "; file2$

END


SUB Display_Header (DataH AS DBF_Header)
    PRINT "Data File Type: ";
    SELECT CASE DataH.FileType
        CASE 2: PRINT "FoxBASE"
        CASE 3: PRINT "FoxBASE+/Dbase III plus, no memo"
        CASE 48: PRINT "Visual FoxPro"
        CASE 49: PRINT "Visual FoxPro, autoincrement enabled"
        CASE 50: PRINT "Visual FoxPro with field type Varchar or Varbinary"
        CASE 67: PRINT "dBASE IV SQL table files, no memo"
        CASE 99: PRINT "dBASE IV SQL system files, no memo"
        CASE 131: PRINT "FoxBASE+/dBASE III PLUS, with memo"
        CASE 139: PRINT "dBASE IV with memo"
        CASE 203: PRINT "dBASE IV SQL table files, with memo"
        CASE 229: PRINT "HiPer-Six format with SMT memo file"
        CASE 245: PRINT "FoxPro 2.x (or earlier) with memo"
        CASE 251: PRINT "FoxBASE"
        CASE ELSE: PRINT "Unknown File Type"
    END SELECT
    PRINT "Date: "; DataH.Month; "/"; DataH.Day; "/"; DataH.Year
    PRINT "Number of Records: "; DataH.RecordNumber
    PRINT "First Record: "; DataH.FirstRecord
    PRINT "Record Length: "; DataH.RecordLength
    PRINT "Reserved Junk: "; DataH.ReservedJunk
    PRINT "Table Flags: ";
    none = 0
    IF DataH.TableFlag AND 1 THEN PRINT "file has a structural .cdx ";: none = -1
    IF DataH.TableFlag AND 2 THEN PRINT "file has a Memo field ";: none = -1
    IF DataH.TableFlag AND 4 THEN PRINT "file is a database (.dbc) ";: none = -1
    IF none THEN PRINT ELSE PRINT "None"
    PRINT "Code Page Mark: "; DataH.CodePageMark
    PRINT "Reserved Junk: "; DataH.ReservedJunk1
END SUB

SUB Display_Fields (DataH() AS Field_Subrecord)
    FOR r = 1 TO UBOUND(DataH)
        PRINT "Field Name :"; DataH(r).FieldName
        PRINT "Field Type :"; DataH(r).FieldType
        PRINT "Field Displacement :"; DataH(r).Displacement
        PRINT "Field Length :"; DataH(r).FieldLength
        PRINT "Field Decimal :"; DataH(r).FieldDecimal
        PRINT "Field Flags :"; DataH(r).FieldFlags
        PRINT "Field AutoNext :"; DataH(r).AutoNext
        PRINT "Field SutoStep :"; DataH(r).AutoStep
        PRINT "Field Reserved Junk :"; DataH(r).ReservedJunk
        SLEEP
        PRINT "**************************"
    NEXT
END SUB

SUB Get_Header (file$, DataH AS DBF_Header)
    OPEN file$ FOR BINARY AS #1 LEN = LEN(DataH)
    GET #1, 1, DataH
    CLOSE
END SUB

SUB Get_Fields (file$, DataH() AS Field_Subrecord)
    DIM databyte AS _UNSIGNED _BYTE
    DIM temp AS Field_Subrecord
    OPEN file$ FOR BINARY AS #1 LEN = 1
    counter = -1: s = 33
    DO
        counter = counter + 1
        GET #1, s, databyte
        s = s + 32
    LOOP UNTIL databyte = 13
    REDIM DataH(counter) AS Field_Subrecord
    IF counter < 1 THEN BEEP: BEEP: PRINT "Database has no file records.": END
    CLOSE
    OPEN file$ FOR BINARY AS #1 LEN = 32
    FOR r = 1 TO counter
        GET #1, 32 * r + 1, DataH(r) 'record 1 is our header info, so we need to start our field info at record 2
    NEXT

    CLOSE
END SUB

SUB Print_Data (file$, DataH AS DBF_Header, DataFS() AS Field_Subrecord, file2$)
    DIM databyte AS _UNSIGNED _BYTE
    OPEN file$ FOR BINARY AS #1
    OPEN file2$ FOR OUTPUT AS #2
    SEEK #1, DataH.FirstRecord + 1
    DO
        GET #1, , databyte 'This is the first byte which tells us if the record is good, or has been deleted.
        IF databyte = 32 THEN WRITE #2, "Good Record", ELSE WRITE #2, "Deleted Record",
        FOR i = 1 TO UBOUND(DataFS)
            SELECT CASE DataFS(i).FieldType
                CASE "C", "0"
                    'C is for Characters, or basically STRING characters.
                    '0 is for Null Flags, which I have no clue what they're for.  I'm basically reading them here as worthless characters until I learn otherwise.
                    temp$ = ""
                    FOR j = 1 TO DataFS(i).FieldLength
                        GET #1, , databyte
                        temp$ = temp$ + CHR$(databyte)
                    NEXT
                CASE "Y"
                    'Y is for currency, which is an _INTEGER 64, with an implied 4 spaces for decimal built in.
                    REDIM temp AS _INTEGER64
                    GET #1, , temp
                    temp$ = STR$(temp)
                    l = LEN(temp$)
                    temp$ = LEFT$(temp$, l - 4) + "." + RIGHT$(temp$, 4)
                CASE "N", "F", "M", "G"
                    'N is for numberic, F is for Floating numbers, and both seem to work in the same manner.
                    'M is for Memo's, which are stored in a different  DBT file.  What we have here is the block number of the memo location in that file, stored as a simple set of characters.
                    'G is for OLE files.  We store the info for it just the same as we do for a Memo.
                    'we read the whole thing as a string, which is an odd way for dBase to write it, but I don't make the rules.  I just convert them!
                    temp$ = ""
                    FOR j = 1 TO DataFS(i).FieldLength
                        GET #1, , databyte
                        temp$ = temp$ + CHR$(databyte)
                    NEXT
                CASE "D"
                    'D is for Date fields.
                    'Dates are stored as a string, in the format YYYYMMDD
                    temp$ = ""
                    FOR j = 1 TO DataFS(i).FieldLength
                        GET #1, , databyte
                        temp$ = temp$ + CHR$(databyte)
                    NEXT
                    year$ = LEFT$(temp$, 4)
                    month$ = MID$(temp$, 5, 2)
                    day$ = RIGHT$(temp$, 2)
                    temp$ = day$ + "/" + month$ + "/" + year$
                CASE "L"
                    'L is our logical operator.  Basically, it's simply True or False Boolean logic
                    GET #1, , databyte
                    IF databyte = 32 THEN temp$ = "True" ELSE temp$ = "false"
                CASE "@", "O"
                    '@ are Timestamps, which I'm too lazy to fully support at the moment.
                    'They are 8 bytes - two longs, first for date, second for time.
                    'The date is the number of days since  01/01/4713 BC.
                    'Time is hours * 3600000L + minutes * 60000L + Seconds * 1000L
                    'All I'm going to do is read both longs as a single _Integer64 and then write that data to the disk.
                    'Be certain to convert it as needed to make use of the Timestamp.
                    'I'm just lazy and don't wanna convert anything right now!  :P

                    'O are double long integers -- basically Integer 64s.  Since I'm reading a timestamp as an Int64, this routine works for them as well.
                    REDIM temp1 AS _INTEGER64
                    GET #1, , temp1
                    temp$ = STR$(temp1)
                CASE "I", "+"
                    'Long Integers.  Basically 4 byte numbers
                    '+ are auto-increments.  Stored the same way as a Long.
                    REDIM temp2 AS LONG
                    GET #1, , temp2
                    temp$ = STR$(temp2)
            END SELECT
            IF i = UBOUND(datafs) THEN WRITE #2, temp$ ELSE WRITE #2, temp$,
        NEXT
    LOOP UNTIL EOF(1)
    CLOSE
END SUB

