DECLARE LIBRARY
    FUNCTION glutGet& (BYVAL what&)
END DECLARE

SLEEP 1: _SCREENMOVE 200, 200
ScreenX1 = glutGet(100) '- glutGet(506)
ScreenY1 = glutGet(101) '- glutGet(507) - glutGet(506)
_SCREENMOVE ScreenX1, ScreenY1
ScreenX2 = glutGet(100) '- glutGet(506)
ScreenY2 = glutGet(101) '- glutGet(507) - glutGet(506)
BorderWidth = ScreenX2 - ScreenX1
BorderHeight = ScreenY2 - ScreenY1
PRINT "Your border dimensions = "; BorderWidth, BorderHeight
SCREENX = ScreenX1 - BorderWidth
SCREENY = ScreenY1 - BorderHeight
_SCREENMOVE SCREENX, SCREENY 'And this should put us back where we started exactly.
PRINT "Current position is:"; SCREENX, SCREENY



