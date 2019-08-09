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

'The next two types really seem unimportant to us.
'I mainly put them in here so that no one would forget that our database has them.
'After the Field_Subrecords, there is a ENTER byte (CHR$(13)), for an End-of-Subrecord code.
'After that, there MAY be 263 bytes of information which FoxPro tosses into the dbd file.
'Honestly, it seems useless to me, but should be remembered just for documention sake if nothing else.


TYPE DBF_HeaderTerminator
    EndCode AS _UNSIGNED _BYTE 'Our End of Field Code is a CHR$(13), or 13 if we read it as a byte
END TYPE

TYPE DBF_VFPInfo
    Info AS STRING * 263
END TYPE

DIM DataH AS DBF_Header
DIM DataFS(1) AS Field_Subrecord
'Notice I didn't even bother to define a variable for our other two types?  That's how important they seem to me.  ;)


file$ = ".\tempdata.dbf"
file2$ = ".\converted.txt"

Get_Header file$, DataH
'Display_Header DataH 'You can unremark this line, if you want to see what the Header looks like itself.
Get_Fields file$, DataFS()
'Display_Fields DataFS() ' Unremark this line to see how many fields we have, what types, and their properties, if you want.
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

SUB Get_Fields (file$, DataFS() AS Field_Subrecord)
    DIM databyte AS _UNSIGNED _BYTE
    DIM temp AS Field_Subrecord
    OPEN file$ FOR BINARY AS #1 LEN = 1
    counter = -1: s = 33
    DO
        counter = counter + 1
        GET #1, s, databyte
        s = s + 32
    LOOP UNTIL databyte = 13
    REDIM DataFS(counter) AS Field_Subrecord
    IF counter < 1 THEN BEEP: BEEP: PRINT "Database has no file records.": END
    CLOSE
    OPEN file$ FOR BINARY AS #1 LEN = 32
    FOR r = 1 TO counter
        GET #1, 32 * r + 1, DataFS(r) 'record 1 is our header info, so we need to start our field info at record 2
    NEXT

    CLOSE
END SUB

SUB Print_Data (file$, DataH AS DBF_Header, DataFS() AS Field_Subrecord, file2$)
    DIM databyte AS _UNSIGNED _BYTE
    OPEN file$ FOR BINARY AS #1
    OPEN file2$ FOR OUTPUT AS #2
    SEEK #1, DataH.FirstRecord + 1


    PRINT #2, "TYPE DB_Header"
    PRINT #2, "    FileType AS _UNSIGNED _BYTE"
    PRINT #2, "    Year AS _UNSIGNED _BYTE"
    PRINT #2, "    Month AS _UNSIGNED _BYTE"
    PRINT #2, "    Day AS _UNSIGNED _BYTE"
    PRINT #2, "    RecordNumber AS _UNSIGNED LONG"
    PRINT #2, "    FirstRecord AS _UNSIGNED INTEGER"
    PRINT #2, "    RecordLength AS _UNSIGNED INTEGER"
    PRINT #2, "    ReservedJunk AS STRING * 16"
    PRINT #2, "    TableFlag AS _UNSIGNED _BYTE"
    PRINT #2, "    CodePageMark AS _UNSIGNED _BYTE"
    PRINT #2, "    ReservedJunk1 AS STRING * 2"
    PRINT #2, "END TYPE"
    PRINT #2, ""
    PRINT #2, ""

    PRINT #2, "TYPE DATE_FORMAT"
    PRINT #2, "    Year AS STRING * 4"
    PRINT #2, "    Month AS STRING * 2"
    PRINT #2, "    Day AS STRING * 2"
    PRINT #2, "END TYPE"
    PRINT #2, ""
    PRINT #2, ""

    PRINT #2, "TYPE DataType"
    PRINT #2, "    VALID AS _BYTE"
    FOR i = 1 TO UBOUND(DataFS)
        temp$ = DataFS(i).FieldName + " AS "
        SELECT CASE DataFS(i).FieldType
            CASE "C"
                'C is for Characters, or basically STRING characters.
                temp$ = temp$ + "STRING * " + STR$(DataFS(i).FieldLength) + " ' A basic Character field"
            CASE "G"
                temp$ = temp$ + "STRING * " + STR$(DataFS(i).FieldLength) + " ' OLE Info Field."
            CASE "N"
                temp$ = temp$ + "STRING * " + STR$(DataFS(i).FieldLength) + " ' A Numberic Field, with " + STR$(DataFS(i).FieldDecimal) + " Decimal Places"
            CASE "F"
                temp$ = temp$ + "STRING * " + STR$(DataFS(i).FieldLength) + " ' A Floating Field, with " + STR$(DataFS(i).FieldDecimal) + " Decimal Places"
            CASE "0"
                '0 is for Null Flags, which I have no clue what they're for.  I'm basically reading them here as worthless characters until I learn otherwise.
                temp$ = temp$ + "STRING * " + STR$(DataFS(i).FieldLength) + " ' A Null Flag.  No idea what these are actually for, but they're part of the data structure."
            CASE "M"
                temp$ = temp$ + "STRING * " + STR$(DataFS(i).FieldLength) + " ' Memo Field, which is stored in a different DBT file.  What we have here is the block number of the memo location in that file, stored as a simple set of characters."
            CASE "D"
                'D is for Date fields.
                'Dates are stored as a string, in the format YYYYMMDD
                temp$ = temp$ + "DATE_FORMAT"
            CASE "Y"
                'Y is for currency, which is an _INTEGER 64, with an implied 4 spaces for decimal built in.
                temp$ = temp$ + "_INTEGER64 ' This is actually a currency field.  Divide this by 1000 to get your actual data, as dBase doesn't store the decimal."
            CASE "L"
                'L is our logical operator.  Basically, it's simply True or False Boolean logic
                temp$ = temp$ + "_BYTE"
            CASE "@"
                '@ are Timestamps, which I'm too lazy to fully support at the moment.
                'They are 8 bytes - two longs, first for date, second for time.
                'The date is the number of days since  01/01/4713 BC.
                'Time is hours * 3600000L + minutes * 60000L + Seconds * 1000L
                'All I'm going to do is read both longs as a single _Integer64 and then write that data to the disk.
                'Be certain to convert it as needed to make use of the Timestamp.
                'I'm just lazy and don't wanna convert anything right now!  :P
                temp$ = temp$ + "LONG" + CHR$(13) + DataFS(i).FieldName + "1 AS LONG ' Timestamps here and above.  First is the number of days since  01/01/4713 BC, Second is hours * 3600000L + minutes * 60000L + Seconds * 1000L "
            CASE "O"
                'O are double long integers -- basically Integer 64s.
                temp$ = temp$ + "_INTEGER 64"
            CASE "I", "+"
                'Long Integers.  Basically 4 byte numbers
                '+ are auto-increments.  Stored the same way as a Long.
                temp$ = temp$ + "LONG"
        END SELECT
        IF LEFT$(temp$, 1) = "_" THEN temp$ = RIGHT$(temp$, LEN(temp$) - 1)
        temp$ = "    " + temp$
        PRINT #2, UCASE$(temp$)
    NEXT
    PRINT #2, "END TYPE"
    PRINT #2, ""
    PRINT #2, "DIM DBH AS DB_Header"
    PRINT #2, "DIM DB AS DataType"
    PRINT #2, ""
    temp$ = "OPEN " + CHR$(34) + file$ + CHR$(34) + " FOR BINARY AS #1 LEN = " + STR$(DataH.RecordLength)
    PRINT #2, temp$
    temp$ = "GET #1, 1, DBH"
    PRINT #2, temp$

    temp$ = "FirstRecord = DBH.FirstRecord +1 ' Add one for QB64 file counting offset"
    PRINT #2, temp$
    temp$ = "RecordLength = DBH.RecordLength"
    PRINT #2, temp$
    temp$ = "TotalRecordNumber = DBH.RecordNumber"
    PRINT #2, temp$
    PRINT #2, ""
    PRINT #2, "'    SEEK #1, FirstRecord 'Use this and the next remark if you prefer sequental reads."
    PRINT #2, "FOR i = 1 to TotalRecordNumber"
    PRINT #2, "    'GET #1, , DB 'Use this and the previous remark if you prefer sequental reads."
    PRINT #2, "    GET #1,FirstRecord + (i-1) * RecordLength, DB 'Remark this line out, if you use the other two for sequental input."
    PRINT #2, ""
    PRINT #2, "    'insert code to do stuff here with your data."
    PRINT #2, ""
    PRINT #2, "    'Remember to update DBH.RecordNumber if you add any extra records, so that they'll be available in use in your other dBase programs."
    PRINT #2, "    'Do this with DBH.RecordNumber = ###, where the ### is the total number of records."
    PRINT #2, "    'And then PUT #1, 1, DBH"
    PRINT #2, ""
    PRINT #2, "NEXT"
    CLOSE
END SUB


