FUNCTION GetDay$ (mm, dd, yyyy) 'use 4 digit year
    'From Zeller's congruence: https://en.wikipedia.org/wiki/Zeller%27s_congruence
    IF mm < 3 THEN mm = mm + 12: yyyy = yyyy - 1
    century = yyyy MOD 100
    zerocentury = yyyy \ 100
    result = (dd + INT(13 * (mm + 1) / 5) + century + INT(century / 4) + INT(zerocentury / 4) + 5 * zerocentury) MOD 7
    SELECT CASE result
        CASE 0: Day$ = "Saturday"
        CASE 1: Day$ = "Sunday"
        CASE 2: Day$ = "Monday"
        CASE 3: Day$ = "Tuesday"
        CASE 4: Day$ = "Wednesday"
        CASE 5: Day$ = "Thursday"
        CASE 6: Day$ = "Friday"
    END SELECT
END FUNCTION

