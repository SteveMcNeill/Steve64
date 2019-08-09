DIM x AS LONG, y AS LONG

DO
    PRINT
    PRINT
    PRINT "Give me your fraction:"
    PRINT
    INPUT "Number above the division sign: "; x
    IF x = 0 THEN END
    INPUT "Number below the division sign: "; y
    IF y = 0 THEN END
    PRINT
    PRINT
    PRINT x; "/"; y; "=";
    SimFrac x, y
    PRINT x; "/"; y
LOOP

SUB SimFrac (top AS LONG, bottom AS LONG)
    CONST PrimeArrayLimit = 6550
    STATIC init AS INTEGER, prime(1 TO PrimeArrayLimit) AS _UNSIGNED LONG
    DIM counter AS _UNSIGNED _INTEGER64
    DIM limit AS SINGLE

    'Let's generate a list of prime numbers and save them as a static array for use in factoring our numbers
    IF NOT init THEN
        init = -1 'We initialize our prime array
        prime(1) = 2: prime(2) = 3: counter = 3
        FOR j = 3 TO PrimeArrayLimit
            DO
                IsPrime = -1
                counter = counter + 1
                limit = SQR(counter)
                FOR i = 1 TO PrimeArrayLimit
                    IF prime(i) > limit THEN EXIT FOR 'It's a prime.
                    IF counter MOD prime(i) = 0 THEN IsPrime = 0: EXIT FOR 'It's not a prime
                NEXT
                IF IsPrime THEN prime(j) = counter: EXIT DO
            LOOP
        NEXT
    END IF


    DO
        finished = -1
        IF top > bottom THEN limit = SQR(top) ELSE limit = SQR(bottom)
        FOR i = 1 TO PrimeArrayLimit
            IF prime(i) > prime(limit) THEN EXIT FOR 'We're finished as we've checked all the primes we need to check
            t = top MOD prime(i): b = bottom MOD prime(i)
            IF t = 0 AND b = 0 THEN
                top = top \ prime(i): bottom = bottom \ prime(i)
                finished = 0
                EXIT FOR
            END IF
        NEXT
    LOOP UNTIL finished
END SUB

