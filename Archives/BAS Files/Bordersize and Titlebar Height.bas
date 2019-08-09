DECLARE LIBRARY
    FUNCTION glutGet& (BYVAL what&)
END DECLARE

_SCREENMOVE 200, 200 'We moved our screen to 200,200 so we know these values are correct
BorderWidth = glutGet(506)
TitleBarHeight = glutGet(507)
ActualScreenX = glutGet(100)
ActualScreenY = glutGet(101)

PRINT "Actual Top Left of screen is at ("; _SCREENX; ","; _SCREENY; ")"
PRINT "The border is "; BorderWidth; "pixels"
PRINT "And the title bar is"; TitleBarHeight; " pixels"
PRINT "So your actual display starts at ("; ActualScreenX; ","; ActualScreenY; ")"

