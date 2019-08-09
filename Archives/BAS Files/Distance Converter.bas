_TITLE "QB64 Distance Converter"

PRINT ConvertLength(1, "mile", "meter") 'convert 1 mile to meter units
PRINT ConvertLength(1, "mile", "foot") 'convert 1 mile to foot units (feet)



FUNCTION ConvertLength## (value1 AS _FLOAT, unit1 AS STRING, unit2 AS STRING)
    IF unit1 = unit2 THEN ConvertLength = value1: EXIT FUNCTION
    ConvertLength## = value1 * LengthValue(unit2) / LengthValue(unit1)
END FUNCTION

FUNCTION LengthValue## (unit$)
    'Length is measured by the standard Mile as it seemed a nice midpoint
    SELECT CASE LTRIM$(RTRIM$(LCASE$(unit$)))
        CASE "centimeter": LengthValue = 160934.4
        CASE "foot": LengthValue = 5280
        CASE "inch": LengthValue = 63360
        CASE "kilometer": LengthValue = 1.609344
        CASE "league": LengthValue = 0.3333333303
        CASE "nautical.league": LengthValue = 0.289658747
        CASE "meter": LengthValue = 1609.344
        CASE "microinch": LengthValue = 63360000000
        CASE "mile": LengthValue = 1
        CASE "millimeter": LengthValue = 1609344
        CASE "yard": LengthValue = 1760
        CASE "cable": LengthValue = 7.3333333333
        CASE "click": LengthValue = 1.609344
        CASE "cubit": LengthValue = 3520
        CASE "decimeter": LengthValue = 16093.44
        CASE "digit": LengthValue = 84702.315789
        CASE "fathom": LengthValue = 880
        CASE "finger": LengthValue = 14080
        CASE "fist": LengthValue = 16093.44
        CASE "lightyear": LengthValue = 1.7022430721E-13
        CASE "parsec": LengthValue = 5.215528863E-14
    END SELECT
END FUNCTION

