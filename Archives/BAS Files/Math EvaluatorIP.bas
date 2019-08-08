'Math Evaluator
'Written Oct/2013 in QB64-BASIC
'Updated Aug/2015 in QB64
SCREEN _NEWIMAGE(1280, 720, 256)
REDIM SHARED OName(0) AS STRING 'Operation Name
REDIM SHARED PL(0) AS INTEGER 'Priority Level
DIM SHARED QuickReturn AS INTEGER

Set_OrderOfOperations 'This will also make certain our directories are valid, and if not make them.
DO
    INPUT x$
    IF LTRIM$(RTRIM$(x$)) = "" THEN SYSTEM
    y$ = Evaluate_Expression(x$)
    PRINT "EVALUATED TO: "; y$
    PRINT "STRING VALUE of "; x$; " = "; y$
    _CLIPBOARD$ = y$
    PRINT "NUMERIC VALUE of "; x$; " = "; VAL(y$)
LOOP


FUNCTION Evaluate_Expression$ (e$)
t$ = e$ 'So we preserve our original data, we parse a temp copy of it

QuickReturn = 0
PreParse t$
IF QuickReturn THEN Evaluate_Expression$ = t$: EXIT FUNCTION

IF LEFT$(t$, 5) = "ERROR" THEN Evaluate_Expression$ = t$: EXIT FUNCTION

'Deal with brackets first
exp$ = "(" + t$ + ")" 'Starting and finishing brackets for our parse routine.
DO
    e = INSTR(exp$, ")")
    IF e > 0 THEN
        c = 0
        DO UNTIL e - c <= 0
            c = c + 1
            IF e THEN
                IF MID$(exp$, e - c, 1) = "(" THEN EXIT DO
            END IF
        LOOP
        s = e - c + 1
        IF s < 1 THEN PRINT "ERROR -- BAD () Count": END
        eval$ = " " + MID$(exp$, s, e - s) + " " 'pad with a space before and after so the parser can pick up the values properly.
        ParseExpression eval$
        eval$ = LTRIM$(RTRIM$(eval$))
        IF LEFT$(eval$, 5) = "ERROR" THEN Evaluate_Expression$ = eval$: EXIT SUB
        exp$ = DWD(LEFT$(exp$, s - 2) + eval$ + MID$(exp$, e + 1))
        IF MID$(exp$, 1, 1) = "N" THEN MID$(exp$, 1) = "-"
    END IF
LOOP UNTIL e = 0

c = 0
DO
    c = c + 1
    SELECT CASE MID$(exp$, c, 1)
        CASE "0" TO "9", ".", "-" 'At this point, we should only have number values left.
        CASE ELSE: Evaluate_Expression$ = "ERROR - Unknown Diagnosis: (" + exp$ + ") ": EXIT SUB
    END SELECT
LOOP UNTIL c >= LEN(exp$)

Evaluate_Expression$ = exp$
END FUNCTION



SUB ParseExpression (exp$)
DIM num(10) AS STRING
'We should now have an expression with no () to deal with
FOR J = 1 TO 250
    lowest = 0
    DO UNTIL lowest = LEN(exp$)
        lowest = LEN(exp$): OpOn = 0
        FOR P = 1 TO UBOUND(OName)
            'Look for first valid operator
            IF J = PL(P) THEN 'Priority levels match
                IF LEFT$(exp$, 1) = "-" THEN op = INSTR(2, exp$, OName(P)) ELSE op = INSTR(exp$, OName(P))
                IF op > 0 AND op < lowest THEN lowest = op: OpOn = P
            END IF
        NEXT
        IF OpOn = 0 THEN EXIT DO 'We haven't gotten to the proper PL for this OP to be processed yet.
        IF LEFT$(exp$, 1) = "-" THEN op = INSTR(2, exp$, OName(OpOn)) ELSE op = INSTR(exp$, OName(OpOn))
        numset = 0

        '*** SPECIAL OPERATION RULESETS
        IF OName(OpOn) = "-" THEN 'check for BOOLEAN operators before the -
            SELECT CASE MID$(exp$, op - 3, 3)
                CASE "NOT", "XOR", "AND", "EQV", "IMP"
                    EXIT DO 'Not an operator, it's a negative
            END SELECT
            IF MID$(exp$, op - 3, 2) = "OR" THEN EXIT DO 'Not an operator, it's a negative
        END IF

        IF op THEN
            c = LEN(OName(OpOn)) - 1
            DO
                SELECT CASE MID$(exp$, op + c + 1, 1)
                    CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ".": numset = -1 'Valid digit
                    CASE "-" 'We need to check if it's a minus or a negative
                        IF OName(OpOn) = "PI" OR numset THEN EXIT DO
                    CASE ELSE 'Not a valid digit, we found our separator
                        EXIT DO
                END SELECT
                c = c + 1
            LOOP UNTIL op + c >= LEN(exp$)
            e = op + c

            c = 0
            DO
                c = c + 1
                SELECT CASE MID$(exp$, op - c, 1)
                    CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ".", "N" 'Valid digit
                    CASE "-" 'We need to check if it's a minus or a negative
                        c1 = c
                        bad = 0
                        DO
                            c1 = c1 + 1
                            SELECT CASE MID$(exp$, op - c1, 1)
                                CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."
                                    bad = -1
                                    EXIT DO 'It's a minus sign
                                CASE ELSE
                                    'It's a negative sign and needs to count as part of our numbers
                            END SELECT
                        LOOP UNTIL op - c1 <= 0
                        IF bad THEN EXIT DO 'We found our seperator
                    CASE ELSE 'Not a valid digit, we found our separator
                        EXIT DO
                END SELECT
            LOOP UNTIL op - c <= 0
            s = op - c
            num(1) = N2S(MID$(exp$, s + 1, op - s - 1)) 'Get our first number
            num(2) = N2S(MID$(exp$, op + LEN(OName(OpOn)), e - op - LEN(OName(OpOn)) + 1)) 'Get our second number
            IF MID$(num(1), 1, 1) = "N" THEN MID$(num(1), 1) = "-"
            IF MID$(num(2), 1, 1) = "N" THEN MID$(num(2), 1) = "-"
            num(3) = N2S(EvaluateNumbers(OpOn, num()))
            IF MID$(num(3), 1, 1) = "-" THEN MID$(num(3), 1) = "N"
            'PRINT "*************"
            'PRINT num(1), OName(OpOn), num(2), num(3) ', exp$
            IF LEFT$(num(3), 5) = "ERROR" THEN exp$ = num(3): EXIT SUB
            exp$ = LTRIM$(N2S(DWD(LEFT$(exp$, s) + RTRIM$(LTRIM$(num(3))) + MID$(exp$, e + 1))))
            'PRINT exp$
        END IF
        op = 0
    LOOP
NEXT

END SUB



SUB Set_OrderOfOperations
'PL sets our priortity level. 1 is highest to 65535 for the lowest.
'I used a range here so I could add in new priority levels as needed.
'OName ended up becoming the name of our commands, as I modified things.... Go figure!  LOL!

'Constants get evaluated first, with a Priority Level of 1
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_PI"
REDIM _PRESERVE PL(i): PL(i) = 10
'I'm not certain where exactly percentages should go.  They kind of seem like a special case to me.  COS10% should be COS.1 I'd think...
'I'm putting it here for now, and if anyone knows someplace better for it in our order of operations, let me know.
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "%"
REDIM _PRESERVE PL(i): PL(i) = 5
'Then Functions with PL 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ACOS"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ASIN"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ARCSEC"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ARCCSC"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ARCCOT"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "SECH"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "CSCH"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "COTH"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "COS"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "SIN"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "TAN"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "LOG"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "EXP"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ATN"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_D2R"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_D2G"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_R2D"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_R2G"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_G2D"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_G2R"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ABS"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "SGN"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "INT"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "_ROUND"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "FIX"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "SEC"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "CSC"
REDIM _PRESERVE PL(i): PL(i) = 10
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "COT"
REDIM _PRESERVE PL(i): PL(i) = 10
'Exponents with PL 20
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "^"
REDIM _PRESERVE PL(i): PL(i) = 20
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "SQR"
REDIM _PRESERVE PL(i): PL(i) = 20
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "ROOT"
REDIM _PRESERVE PL(i): PL(i) = 20
'Multiplication and Division PL 30
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "*"
REDIM _PRESERVE PL(i): PL(i) = 30
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "/"
REDIM _PRESERVE PL(i): PL(i) = 30
'Integer Division PL 40
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "\"
REDIM _PRESERVE PL(i): PL(i) = 40
'MOD PL 50
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "MOD"
REDIM _PRESERVE PL(i): PL(i) = 50
'Addition and Subtraction PL 60
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "+"
REDIM _PRESERVE PL(i): PL(i) = 60
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "-"
REDIM _PRESERVE PL(i): PL(i) = 60

'Relational Operators =, >, <, <>, <=, >=   PL 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "<>"
REDIM _PRESERVE PL(i): PL(i) = 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "><" 'These next three are just reversed symbols as an attempt to help process a common typo
REDIM _PRESERVE PL(i): PL(i) = 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "<="
REDIM _PRESERVE PL(i): PL(i) = 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = ">="
REDIM _PRESERVE PL(i): PL(i) = 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "=<" 'I personally can never keep these things straight.  Is it < = or = <...
REDIM _PRESERVE PL(i): PL(i) = 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "=>" 'Who knows, check both!
REDIM _PRESERVE PL(i): PL(i) = 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = ">"
REDIM _PRESERVE PL(i): PL(i) = 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "<"
REDIM _PRESERVE PL(i): PL(i) = 70
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "="
REDIM _PRESERVE PL(i): PL(i) = 70
'Logical Operations PL 80+
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "NOT"
REDIM _PRESERVE PL(i): PL(i) = 80
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "AND"
REDIM _PRESERVE PL(i): PL(i) = 90
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "OR"
REDIM _PRESERVE PL(i): PL(i) = 100
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "XOR"
REDIM _PRESERVE PL(i): PL(i) = 110
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "EQV"
REDIM _PRESERVE PL(i): PL(i) = 120
i = i + 1: REDIM _PRESERVE OName(i): OName(i) = "IMP"
REDIM _PRESERVE PL(i): PL(i) = 130
END SUB

FUNCTION EvaluateNumbers$ (p, num() AS STRING)
DIM n1 AS _FLOAT, n2 AS _FLOAT, n3 AS _FLOAT
SELECT CASE OName(p) 'Depending on our operator..
    CASE "_PI"
        IF num(2) = "" THEN num(2) = "1"
        n1 = _PI * VAL(num(2)) 'Future compatable in case something ever stores extra digits for PI
    CASE "%" 'Note percent is a special case and works with the number BEFORE the % command and not after
        IF num(1) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get percent of NULL string": EXIT FUNCTION
        n1 = (VAL(num(1))) / 100
    CASE "_ACOS"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCOS of NULL string": EXIT FUNCTION
        n1 = VAL(num(2))
        IF n1 > 1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCOS from value >1, which is Invalid": EXIT FUNCTION
        IF n1 < -1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCOS from value <-1, which is Invalid": EXIT FUNCTION
        IF n1 = 1 THEN EvaluateNumbers$ = "0": EXIT FUNCTION
        n1 = _ACOS(n1)
    CASE "_ASIN"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get _ASIN of NULL string": EXIT FUNCTION
        n1 = VAL(num(2))
        IF n1 > 1 THEN EvaluateNumbers$ = "ERROR - Attemping to get _ASIN from value >1, which is Invalid": EXIT FUNCTION
        IF n1 < -1 THEN EvaluateNumbers$ = "ERROR - Attemping to get _ASIN from value <-1, which is Invalid": EXIT FUNCTION
        n1 = _ASIN(n1)
    CASE "_ARCSEC"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSEC of NULL string": EXIT FUNCTION
        n1 = VAL(num(2))
        IF n1 > 1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSEC from value > 1, which is Invalid": EXIT FUNCTION
        IF n1 < -1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSEC from value < -1, which is Invalid": EXIT FUNCTION
        n1 = _ARCSEC(n1)
    CASE "ARCCSC"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCSC of NULL string": EXIT FUNCTION
        n1 = VAL(num(2))
        IF n1 > 1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCSC from value >=1, which is Invalid": EXIT FUNCTION
        IF n1 < -1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCSC from value <-1, which is Invalid": EXIT FUNCTION
        n1 = _ARCCSC(n1)
    CASE "ARCCOT"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCOT of NULL string": EXIT FUNCTION
        n1 = VAL(num(2))
        n1 = _ARCCOT(n1)
    CASE "SECH"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SECH of NULL string": EXIT FUNCTION
        n1 = VAL(num(2))
        IF n1 > 88.02969 OR (EXP(n1) + EXP(-n1)) = 0 THEN EvaluateNumbers$ = "ERROR - Bad SECH command": EXIT FUNCTION
        n1 = _SECH(n1)
    CASE "CSCH"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get CSCH of NULL string": EXIT FUNCTION
        n1 = VAL(num(2))
        IF n1 > 88.02969 OR (EXP(n1) - EXP(-n1)) = 0 THEN EvaluateNumbers$ = "ERROR - Bad CSCH command": EXIT FUNCTION
        n1 = _CSCH(n1)
    CASE "COTH"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get COTH of NULL string": EXIT FUNCTION
        n1 = VAL(num(2))
        IF 2 * n1 > 88.02969 OR EXP(2 * n1) - 1 = 0 THEN EvaluateNumbers$ = "ERROR - Bad COTH command": EXIT FUNCTION
        n1 = _COTH(n1)
    CASE "COS"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get COS of NULL string": EXIT FUNCTION
        n1 = COS(VAL(num(2)))
    CASE "SIN"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SIN of NULL string": EXIT FUNCTION
        n1 = SIN(VAL(num(2)))
    CASE "TAN"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get TAN of NULL string": EXIT FUNCTION
        n1 = TAN(VAL(num(2)))
    CASE "LOG"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get LOG of NULL string": EXIT FUNCTION
        n1 = LOG(VAL(num(2)))
    CASE "EXP"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get EXP of NULL string": EXIT FUNCTION
        n1 = EXP(VAL(num(2)))
    CASE "ATN"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ATN of NULL string": EXIT FUNCTION
        n1 = ATN(VAL(num(2)))
    CASE "_D2R"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Radian of NULL Degree value": EXIT FUNCTION
        n1 = _R2D(VAL(num(2)))
    CASE "_D2G"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Grad of NULL Degree string": EXIT FUNCTION
        n1 = _D2G(VAL(num(2)))
    CASE "_R2D"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Degree of NULL Radian string": EXIT FUNCTION
        n1 = _R2D(VAL(num(2)))
    CASE "_R2G"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Grad of NULL Radian string": EXIT FUNCTION
        n1 = _R2G(VAL(num(2)))
    CASE "_G2D"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Degree of NULL Gradian string": EXIT FUNCTION
        n1 = _G2D(VAL(num(2)))
    CASE "_G2R"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Radian of NULL Grad string": EXIT FUNCTION
        n1 = _G2R(VAL(num(2)))
    CASE "ABS"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ABS of NULL string": EXIT FUNCTION
        n1 = ABS(VAL(num(2)))
    CASE "SGN"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SGN of NULL string": EXIT FUNCTION
        n1 = SGN(VAL(num(2)))
    CASE "INT"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get INT of NULL string": EXIT FUNCTION
        n1 = INT(VAL(num(2)))
    CASE "_ROUND"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to _ROUND a NULL string": EXIT FUNCTION
        n1 = _ROUND(VAL(num(2)))
    CASE "FIX"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to FIX a NULL string": EXIT FUNCTION
        n1 = FIX(VAL(num(2)))
    CASE "SEC"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SEC of NULL string": EXIT FUNCTION
        n1 = COS(VAL(num(2)))
        IF n1 = 0 THEN EvaluateNumbers$ = "ERROR - COS value is 0, thus SEC is 1/0 which is Invalid": EXIT FUNCTION
        n1 = 1 / n1
    CASE "CSC"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get CSC of NULL string": EXIT FUNCTION
        n1 = SIN(VAL(num(2)))
        IF n1 = 0 THEN EvaluateNumbers$ = "ERROR - SIN value is 0, thus CSC is 1/0 which is Invalid": EXIT FUNCTION
        n1 = 1 / n1
    CASE "COT"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get COT of NULL string": EXIT FUNCTION
        n1 = TAN(VAL(num(2)))
        IF n1 = 0 THEN EvaluateNumbers$ = "ERROR - TAN value is 0, thus COT is 1/0 which is Invalid": EXIT FUNCTION
        n1 = 1 / n1
    CASE "^"
        IF num(1) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to raise NULL string to exponent": EXIT FUNCTION
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to raise number to NULL exponent": EXIT FUNCTION
        n1 = VAL(num(1)) ^ VAL(num(2))
    CASE "SQR"
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SQR of NULL string": EXIT FUNCTION
        IF VAL(num(2)) < 0 THEN EvaluateNumbers$ = "ERROR - Cannot take take SQR of numbers < 0.  I'm a computer, I have a poor imagination.": EXIT FUNCTION
        n1 = SQR(VAL(num(2)))
    CASE "ROOT"
        IF num(1) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ROOT of a NULL string": EXIT FUNCTION
        IF num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get NULL ROOT of a string": EXIT FUNCTION
        n1 = VAL(num(1)): n2 = VAL(num(2))
        IF n2 = 1 THEN EvaluateNumbers$ = RTRIM$(LTRIM$(STR$(n1))): EXIT FUNCTION
        IF n2 = 0 THEN EvaluateNumbers$ = "ERROR - There is no such thing as a 0 ROOT of a number": EXIT FUNCTION
        IF n1 < 0 AND n2 MOD 2 = 0 AND n2 > 1 THEN EvaluateNumbers$ = "ERROR - Cannot take take an EVEN ROOT of numbers < 0.  I'm a computer, I have a poor imagination.": EXIT FUNCTION
        IF n1 < 0 AND n2 >= 1 THEN sign = -1: n1 = -n1 ELSE sign = 1
        n3 = 1## / n2
        IF n3 <> INT(n3) AND n2 < 1 THEN sign = SGN(n1): n1 = ABS(n1)
        n1 = sign * (n1 ^ n3)
    CASE "*"
        IF num(1) = "" OR num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to multiply NULL string ": EXIT FUNCTION
        n1 = VAL(num(1)) * VAL(num(2))
    CASE "/":
        IF num(1) = "" OR num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to divide NULL string ": EXIT FUNCTION
        IF VAL(num(2)) = 0 THEN EvaluateNumbers$ = "ERROR - Division by 0": EXIT FUNCTION
        n1 = VAL(num(1)) / VAL(num(2))
    CASE "\"
        IF num(1) = "" OR num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to divide NULL string ": EXIT FUNCTION
        IF VAL(num(2)) = 0 THEN EvaluateNumbers$ = "ERROR - Division by 0": EXIT FUNCTION
        n1 = VAL(num(1)) \ VAL(num(2))
    CASE "MOD"
        IF num(1) = "" OR num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to MOD with NULL string ": EXIT FUNCTION
        IF VAL(num(2)) = 0 THEN EvaluateNumbers$ = "ERROR - Division by 0": EXIT FUNCTION
        n1 = VAL(num(1)) MOD VAL(num(2))
    CASE "+": n1 = VAL(num(1)) + VAL(num(2))
    CASE "-": n1 = VAL(num(1)) - VAL(num(2))
    CASE "=": n1 = VAL(num(1)) = VAL(num(2))
    CASE ">": n1 = VAL(num(1)) > VAL(num(2))
    CASE "<": n1 = VAL(num(1)) < VAL(num(2))
    CASE "<>", "><": n1 = VAL(num(1)) <> VAL(num(2))
    CASE "<=", "=<": n1 = VAL(num(1)) <= VAL(num(2))
    CASE ">=", "=>": n1 = VAL(num(1)) >= VAL(num(2))
    CASE "NOT": n1 = NOT VAL(num(2))
    CASE "AND": n1 = VAL(num(1)) AND VAL(num(2))
    CASE "OR": n1 = VAL(num(1)) OR VAL(num(2))
    CASE "XOR": n1 = VAL(num(1)) XOR VAL(num(2))
    CASE "EQV": n1 = VAL(num(1)) EQV VAL(num(2))
    CASE "IMP": n1 = VAL(num(1)) IMP VAL(num(2))
    CASE ELSE
        EvaluateNumbers$ = "ERROR - Bad operation (We shouldn't see this)" 'Let's say we're bad...
END SELECT
EvaluateNumbers$ = RTRIM$(LTRIM$(STR$(n1)))
END FUNCTION

FUNCTION DWD$ (exp$) 'Deal With Duplicates
'To deal with duplicate operators in our code.
'Such as --  becomes a +
'++ becomes a +
'+- becomes a -
'-+ becomes a -
t$ = exp$
DO
    bad = 0
    DO
        l = INSTR(t$, "++")
        IF l THEN t$ = LEFT$(t$, l - 1) + "+" + MID$(t$, l + 2): bad = -1
    LOOP UNTIL l = 0
    DO
        l = INSTR(t$, "+-")
        IF l THEN t$ = LEFT$(t$, l - 1) + "-" + MID$(t$, l + 2): bad = -1
    LOOP UNTIL l = 0
    DO
        l = INSTR(t$, "-+")
        IF l THEN t$ = LEFT$(t$, l - 1) + "-" + MID$(t$, l + 2): bad = -1
    LOOP UNTIL l = 0
    DO
        l = INSTR(t$, "--")
        IF l THEN t$ = LEFT$(t$, l - 1) + "+" + MID$(t$, l + 2): bad = -1
    LOOP UNTIL l = 0
LOOP UNTIL NOT bad
DWD$ = t$
VerifyString t$
END FUNCTION

SUB PreParse (e$)
DIM f AS _FLOAT

t$ = e$
IF QuickReturn THEN EXIT SUB
'First strip all spaces
t$ = ""
FOR i = 1 TO LEN(e$)
    IF MID$(e$, i, 1) <> " " THEN t$ = t$ + MID$(e$, i, 1)
NEXT

t$ = UCASE$(t$)
IF t$ = "" THEN e$ = "ERROR -- NULL string; nothing to evaluate"

'ERROR CHECK by counting our brackets
l = 0
DO
    l = INSTR(l + 1, t$, "("): IF l THEN c = c + 1
LOOP UNTIL l = 0
l = 0
DO
    l = INSTR(l + 1, t$, ")"): IF l THEN c1 = c1 + 1
LOOP UNTIL l = 0
IF c <> c1 THEN e$ = "ERROR -- Bad Parenthesis:" + STR$(c) + "( vs" + STR$(c1) + ")": EXIT SUB

'Modify so that NOT will process properly
l = 0
DO
    l = INSTR(l + 1, t$, "NOT")
    IF l THEN
        'We need to work magic on the statement so it looks pretty.
        ' 1 + NOT 2 + 1 is actually processed as 1 + (NOT 2 + 1)
        'Look for something not proper
        l1 = INSTR(l + 1, t$, "AND")
        IF l1 = 0 OR (INSTR(l + 1, t$, "OR") > 0 AND INSTR(l + 1, t$, "OR") < l1) THEN l1 = INSTR(l + 1, t$, "OR")
        IF l1 = 0 OR (INSTR(l + 1, t$, "XOR") > 0 AND INSTR(l + 1, t$, "XOR") < l1) THEN l1 = INSTR(l + 1, t$, "XOR")
        IF l1 = 0 OR (INSTR(l + 1, t$, "EQV") > 0 AND INSTR(l + 1, t$, "EQV") < l1) THEN l1 = INSTR(l + 1, t$, "EQV")
        IF l1 = 0 OR (INSTR(l + 1, t$, "IMP") > 0 AND INSTR(l + 1, t$, "IMP") < l1) THEN l1 = INSTR(l + 1, t$, "IMP")
        IF l1 = 0 THEN l1 = LEN(t$) + 1
        t$ = LEFT$(t$, l - 1) + "(" + MID$(t$, l, l1 - l) + ")" + MID$(t$, l + l1 - l)
        l = l + 3
        'PRINT t$
    END IF
LOOP UNTIL l = 0

'Check for bad operators before a ( bracket
l = 0
DO
    l = INSTR(l + 1, t$, "(")
    IF l AND l > 2 THEN 'Don't check the starting bracket; there's nothing before it.
        good = 0
        FOR i = 1 TO UBOUND(OName)
            IF MID$(t$, l - LEN(OName(i)), LEN(OName(i))) = OName(i) AND PL(i) > 1 AND PL(i) <= 250 THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
        NEXT
        IF NOT good THEN e$ = "ERROR - Improper operations before (.": EXIT SUB
        l = l + 1
    END IF
LOOP UNTIL l = 0

'Check for bad operators after a ) bracket
l = 0
DO
    l = INSTR(l + 1, t$, ")")

    IF l AND l < LEN(t$) THEN
        good = 0
        FOR i = 1 TO UBOUND(OName)
            IF MID$(t$, l + 1, LEN(OName(i))) = OName(i) AND PL(i) > 1 AND PL(i) <= 250 THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
        NEXT
        IF MID$(t$, l + 1, 1) = ")" THEN good = -1
        IF NOT good THEN e$ = "ERROR - Improper operations after ).": EXIT SUB
        l = l + 1
    END IF
LOOP UNTIL l = 0 OR l = LEN(t$) 'last symbol is a bracket

'Turn all &H (hex) numbers into decimal values for the program to process properly
l = 0
DO
    l = INSTR(t$, "&H")
    IF l THEN
        e = l + 1: finished = 0
        DO
            e = e + 1
            comp$ = MID$(t$, e, 1)
            SELECT CASE comp$
                CASE "0" TO "9", "A" TO "F" 'All is good, our next digit is a number, continue to add to the hex$
                CASE ELSE
                    good = 0
                    FOR i = 1 TO UBOUND(OName)
                        IF MID$(t$, e, LEN(OName(i))) = OName(i) AND PL(i) > 1 AND PL(i) <= 250 THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
                    NEXT
                    IF NOT good THEN e$ = "ERROR - Improper &H value. (" + comp$ + ")": EXIT SUB
                    e = e - 1
                    finished = -1
            END SELECT
        LOOP UNTIL finished OR e = LEN(t$)
        t$ = LEFT$(t$, l - 1) + LTRIM$(RTRIM$(STR$(VAL(MID$(t$, l, e - l + 1))))) + MID$(t$, e + 1)
    END IF
LOOP UNTIL l = 0

'Turn all &B (binary) numbers into decimal values for the program to process properly
l = 0
DO
    l = INSTR(t$, "&B")
    IF l THEN
        e = l + 1: finished = 0
        DO
            e = e + 1
            comp$ = MID$(t$, e, 1)
            SELECT CASE comp$
                CASE "0", "1" 'All is good, our next digit is a number, continue to add to the hex$
                CASE ELSE
                    good = 0
                    FOR i = 1 TO UBOUND(OName)
                        IF MID$(t$, e, LEN(OName(i))) = OName(i) AND PL(i) > 1 AND PL(i) <= 250 THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
                    NEXT
                    IF NOT good THEN e$ = "ERROR - Improper &B value. (" + comp$ + ")": EXIT SUB
                    e = e - 1
                    finished = -1
            END SELECT
        LOOP UNTIL finished OR e = LEN(t$)
        t$ = LEFT$(t$, l - 1) + LTRIM$(RTRIM$(STR$(VAL(MID$(t$, l, e - l + 1))))) + MID$(t$, e + 1)
    END IF
LOOP UNTIL l = 0


'turn all scientific notation into strings
l = 0
DO
    l = INSTR(t$, "D+")
    IF l = 0 THEN l = INSTR(t$, "D-")
    IF l = 0 THEN l = INSTR(t$, "E+")
    IF l = 0 THEN l = INSTR(t$, "E-")
    IF l THEN
        before = 1: after = LEN(t$) + 1
        FOR i = l - 1 TO 1 STEP -1
            'the only things that should come before are numbers and a single period
            SELECT CASE MID$(t$, i, 1)
                CASE "0" TO "9", ".": 'it's good, keep counting backwards
                CASE ELSE
                    before = i + 1: EXIT FOR
            END SELECT
        NEXT
        FOR i = l + 3 TO LEN(t$)
            'the only things that should come before are numbers and a single period
            SELECT CASE MID$(t$, i, 1)
                CASE "0" TO "9", ".": 'it's good, keep counting backwards
                CASE ELSE
                    after = i: EXIT FOR
            END SELECT
        NEXT
        m$ = MID$(t$, before, after - before)
        t$ = LEFT$(t$, before - 1) + N2S$(m$) + MID$(t$, after)
        'PRINT m$, t$
    END IF
LOOP UNTIL l = 0

VerifyString t$

e$ = t$
END SUB


SUB VerifyString (t$)
'ERROR CHECK for unrecognized operations
j = 1
DO
    comp$ = MID$(t$, j, 1)
    SELECT CASE comp$
        CASE "0" TO "9", ".", "(", ")": j = j + 1
        CASE ELSE
            good = 0
            FOR i = 1 TO UBOUND(OName)
                IF MID$(t$, j, LEN(OName(i))) = OName(i) THEN good = -1: EXIT FOR 'We found an operator after our ), and it's not a CONST (like PI)
            NEXT
            IF NOT good THEN t$ = "ERROR - Bad Operational value. (" + comp$ + ")": EXIT SUB
            j = j + LEN(OName(i))
    END SELECT
LOOP UNTIL j > LEN(t$)
END SUB


FUNCTION N2S$ (exp$) 'scientific Notation to String
t$ = LTRIM$(RTRIM$(exp$))
IF LEFT$(t$, 1) = "-" THEN sign$ = "-": t$ = MID$(t$, 2)

dp = INSTR(t$, "D+"): dm = INSTR(t$, "D-")
ep = INSTR(t$, "E+"): em = INSTR(t$, "E-")
check1 = SGN(dp) + SGN(dm) + SGN(ep) + SGN(em)
IF check1 < 1 OR check1 > 1 THEN N2S = exp$: EXIT SUB 'If no scientic notation is found, or if we find more than 1 type, it's not SN!

SELECT CASE l 'l now tells us where the SN starts at.
    CASE IS < dp: l = dp
    CASE IS < dm: l = dm
    CASE IS < ep: l = ep
    CASE IS < em: l = em
END SELECT

l$ = LEFT$(t$, l - 1) 'The left of the SN
r$ = MID$(t$, l + 1): r&& = VAL(r$) 'The right of the SN, turned into a workable long

IF INSTR(l$, ".") THEN 'Location of the decimal, if any
    IF r&& > 0 THEN
        r&& = r&& - LEN(l$) + 2
    ELSE
        r&& = r&& + 1
    END IF
    l$ = LEFT$(l$, 1) + MID$(l$, 3)
END IF

SELECT CASE r&&
    CASE 0 'what the heck? We solved it already?
        'l$ = l$
    CASE IS < 0
        FOR i = 1 TO -r&&
            l$ = "0" + l$
        NEXT
        l$ = "0." + l$
    CASE ELSE
        FOR i = 1 TO r&&
            l$ = l$ + "0"
        NEXT
END SELECT

N2S$ = sign$ + l$
END SUB

