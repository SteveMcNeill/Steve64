ts = _NEWIMAGE(800, 6000, 32)
ws = _NEWIMAGE(800, 600, 32)
SCREEN ws

_DEST ts
COLOR _RGB32(255, 255, 0)
CenterText 10, "Steve's Basic SIN/COS Demo/Tutorial for Programming"
COLOR -1
LOCATE 3, 1
PRINT "First, let's go over what SIN and COS actually are.  A lot of people are completely mystified by"
PRINT "these two basic math commands."
PRINT
PRINT "SIN and COS are nothing more than than a RATIO of the sides of a right triangle"
PRINT
PRINT "For example, take a look at the triangle below"
LINE (200, 150)-(0, 250), _RGB32(255, 0, 0) 'Red
LINE (0, 250)-(200, 250), _RGB32(0, 255, 0) 'Green
LINE (200, 250)-(200, 150), _RGB32(0, 0, 255) 'Blue

COLOR _RGB32(255, 0, 0)
LOCATE 11, 40: PRINT "The RED Line is the Hypotenuse"
COLOR _RGB32(0, 255, 0)
LOCATE 13, 40: PRINT "The GREEN Line is Adjacent to our angle"
COLOR _RGB32(0, 0, 255)
LOCATE 15, 40: PRINT "The BLUE Line is Opposite from our angle"
COLOR -1

LOCATE 18, 1
PRINT "If we look at the triangle above, we want to use the angle from the far left corner to get our SIN"
PRINT "and COS ratios."
PRINT
PRINT "SINE (SIN) is basically nothing more than the ratio of the length of the side that is opposite that"
PRINT "angle to the length of the longest side of the triangle (the Hypotenuse)."
PRINT
PRINT "Now, that might sound like a complex mouthful, but it's not as complicated as it seems."
PRINT "Basically, in the image above, if we were to measure the length of the BLUE line and then DIVIDE it"
PRINT "By the length of the RED line, we'd have the SINE value of our angle."
PRINT
PRINT "Opposite/ Hypotenuse = SINE  (or in this case, length of BLUE / length of RED = SINE)"
PRINT
PRINT
PRINT "Now, image taking this triangle and making it bigger or larger.  DON'T change the angle, but expand"
PRINT "the length and height of it in your imagination."
PRINT
PRINT "No matter how large you scale it, the RATIO of that opposite side and hypotenuse is ALWAYS going to"
PRINT "be the same."
PRINT
PRINT
COLOR _RGB32(255, 255, 0)
PRINT "That RATIO of opposite side divided by hypotenuse IS what SINE (SIN) is."
COLOR -1
PRINT
PRINT "And COSINE (COS) is nothing more than the ratio of the ADJACENT side divided by the hypotenuse."
PRINT "Which, in the above triangle would be nothing more than the length of the GREEN line, divied by"
PRINT "the length of the RED line."
PRINT
COLOR _RGB32(255, 255, 0)
PRINT "That RATIO of adjacent side divided by hypotenuse IS what COSINE (COS) is."
COLOR -1
PRINT
PRINT
PRINT "No matter how much you scale that triangle, that RATIO between those sides is going to stay the same"
PRINT "for that angle"
PRINT
PRINT "Now, let's say that my triangle has sides which are 3, 4, and 5 (units) long.  (If you're American,"
PRINT "they can be inches.  Elsewhere, centimeters...)."
PRINT
PRINT "Now, obviously looking at my (not to scale) triangle above, the Opposite side would have to be the"
PRINT "shortest at a length of 3.  The Adjacent side would have to be a little longer at a length of 4."
PRINT "And the Hypotenuse would be the longest side with the height of 5)"
PRINT
PRINT "(Red Line = 5, Green Line = 3, Blue Line = 4 on the diagram above)"
PRINT
PRINT
COLOR _RGB32(255, 255, 0)
PRINT "So what would our SIN and COS be for the above triangle (with the imaginary figures provided)?"
COLOR -1
PRINT
PRINT "SIN = Opposite / Hypotenuse, so that's 3/5 in this case (.6)"
PRINT "COS = Adjacent / Hypotenuse, so that's 4/5 in this case (.8)"
PRINT
PRINT
PRINT "And if we take a protractor (remember those from school?), we can measure the angle (for that"
PRINT "imaginary triangle), and see that it is roughly 36.87 degrees."
PRINT
PRINT "So, if we use the computer to check our math, this is what it tells us:"
PRINT "SIN = "; SIN(_D2R(36.87))
PRINT "COS = "; COS(_D2R(36.87))
PRINT
PRINT "As you can see, our numbers aren't an EXACT match, but that's because I didn't use an EXACT figure"
PRINT "for that angle.  The angle formed by a 3-4-5 triangle is actually closer to 36.86989764582773"
PRINT "degrees, but who's counting that close?  (Except for the computer?) :P"
PRINT
PRINT
PRINT
COLOR _RGB32(255, 0, 255)
CenterText 1250, "Press the <<RIGHT ARROW>> to go on to PART II: Using SIN/COS In Triangles"
COLOR -1
PRINT
PRINT
PRINT
PRINT
COLOR _RGB32(255, 255, 0)
CenterText 1320, "PART II: Usine SIN/COS in triangles"
COLOR -1
PRINT
PRINT
PRINT "To briefly recap:"
PRINT "SIN(angle) = Opposite / Hypotenuse"
PRINT "COS(angle) = Adjacent / Hypotenuse"
PRINT
PRINT
PRINT "Which all sounds good and such.  We now know HOW we get sine and cosine from an angle, but how"
PRINT "the heck do we make use of it?"
PRINT
PRINT
PRINT "First, let's look at a basic right triangle again.  (A right triangle is defined as having one"
PRINT "side which is 90 degrees, as illustrated again below.)"
LINE (400, 1650)-STEP(-20, -20), _RGB32(185, 185, 0), B
LINE (400, 1550)-(300, 1650)
LINE (300, 1650)-(400, 1650)
LINE (400, 1550)-(400, 1650)
COLOR _RGB32(255, 255, 0)
_PRINTSTRING (396, 1530), "A"
_PRINTSTRING (286, 1642), "B"
_PRINTSTRING (404, 1642), "C"
COLOR -1
LOCATE 106, 1
PRINT "As you can see, the angle at Point C is 90 degrees, so this is indeed a right triangle."
PRINT "(I'd hate to work with wrong ones.  Last I heard, they got several years jail time!)"
PRINT
PRINT
PRINT "So given two pieces of information for this triangle, can you figure out the rest?"
PRINT
PRINT "For example, let's pretend that this is a triangle with the longest side (hyptoenuse) being"
PRINT "200 units long, and our angle at point B is 45 degrees."
PRINT
PRINT "What would the length of the other 2 sides be?"
PRINT
PRINT
PRINT "Now, even though we all hate word problems, let's take a look at this one before we give up."
PRINT
PRINT "First, what's the side opposite of our given angle?"
PRINT "(Looking at the illustration above, it'd be the line from A to C, correct?)"
PRINT
PRINT "Open up your calculator, and get ready to do some math!  (Or else you'll never learn...)"
PRINT
PRINT "Remember what we said about what SINE and COSINE was?"
PRINT "SIN(angle) = Opposite / Hypotenuse"
PRINT "COS(angle) = Adjacent / Hypotenuse"
PRINT
PRINT "So to find that side opposite our given angle, we'd need to multiple both sides by the"
PRINT "Hypotenuse, so that we'd end up with basically the following:"
PRINT "SIN(angle) * Hypotenuse = Opposite"
PRINT "COS(angle) * Hypotenuse = Adjacent"
PRINT
PRINT
COLOR _RGB32(125, 125, 125)
PRINT "Remember why this works?  Think of our formula with actual numbers:"
PRINT "6 = x / 2  <-- let's pretend this is our formula."
PRINT "Now, if we multiply each side by 2, we'd end up getting:"
PRINT " 6 * 2 = x / 2 * 2"
PRINT "When you divide by a number and multiply by the same number, you get 1..."
PRINT "Which simplifies down to:    6 * 2 = x (or our answer of 12 in this case)"
COLOR -1
PRINT
PRINT
PRINT "In this case, we can just plug in the 2 numbers we know and get the last one."
PRINT "Our angle is 45 degrees."
PRINT "Our Hypotenuse is 100 units in length."
PRINT
PRINT "What would the length of the opposite side be? See if you can figure it out with what we've went"
PRINT "over above."
PRINT
PRINT
PRINT
PRINT
COLOR _RGB32(125, 125, 125)
PRINT "SIN(angle) * Hypotenuse = Opposite"
PRINT "SIN(45) * 100 = Opposite"
PRINT SIN(_D2R(45)) * 100; " = Opposite"
PRINT
PRINT "COS(angle) * Hypotenuse = Adjacent"
PRINT "COS(45) * 100 = Adjacent"
PRINT COS(_D2R(45)) * 100; " = Adjacent"
COLOR -1
PRINT
PRINT
PRINT
PRINT
PRINT "Does the numbers you have look similar to the ones above?"
PRINT
PRINT "Seems simple enough, but think of what we've just did..."
PRINT "We took nothing more than a given angle and a side, and calculated the other sides of our triangle!"
PRINT
PRINT
PRINT "CONGRATULATIONS!!  You're well on the way to understanding SIN and COS."
PRINT "It's really not as complicated as some folks like to make it seem.  It's basically just"
PRINT "a relationship between 3 things, and using 2 of those things to figure out the third..."
PRINT
PRINT
PRINT
COLOR _RGB32(255, 0, 255)
CenterText 2770, "Press the <<RIGHT ARROW>> to go on to PART III: Using SIN/COS For Circles and Arcs"
COLOR _RGB32(255, 255, 0)
PRINT
CenterText 2820, "PART III: Using SIN/COS for Circles and Arcs"
COLOR -1
PRINT
PRINT
PRINT "Now that we've discussed the basics about how we can use SIN and COS in triangles, just how the heck"
PRINT "do we make them useful for circles?"
PRINT
PRINT
CIRCLE (400, 3000), 100, _RGB32(255, 255, 0)
PSET (400, 3000), _RGB32(255, 255, 0)
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT "This is a standard QB64 circle."
PRINT "The command to make this is:"
PRINT "CIRCLE (x,y), radius, color"
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT "Using the information above, we know a heck of a lot of information about this circle."
PRINT "If you look at the source code for this program, you'll see our circle command is:"
PRINT "CIRCLE (400,3000), 100, _RGB32(255,255,0)"
PRINT
PRINT "Which breaks down to the center being at the point 400,3000, the radius being 100 pixels, and the"
PRINT "color being yellow."
PRINT
PRINT
PRINT "But could we draw this circle manually, using SIN and COS?"
PRINT
PRINT "Of course!"
PRINT
PRINT
PRINT "But how??"
PRINT
PRINT
PRINT "Think back on our previous lesson..."
PRINT "We take an angle and a side, and we calculate the length of the other 2 sides."
PRINT "To make a circle, we basically make a 360 degree arc, correct?"
PRINT
PRINT "So this would give us a nice loop so we can plot a pixel at each degree:"
PRINT "FOR angle = 1 to 360"
PRINT
PRINT
PRINT "And we know what our radius would be, correct?  (It was 100, if you remember...)"
PRINT
PRINT
PRINT "And using what we learned previously, what would the length of the opposite and adjacent sides"
PRINT "of a triangle be with 45 degrees and a 100 unit hypotenuse??"
COLOR _RGB32(125, 125, 125)
PRINT "SIN(angle) * Hypotenuse = Opposite"
PRINT "COS(angle) * Hypotenuse = Adjacent"
COLOR -1
PRINT
PRINT "And how does this pertain to our CIRCLE, you ask...."
PRINT
PRINT "As you can see, we have a 45"
PRINT "degree angle in this circle,"
PRINT "as illustrated here."
PRINT
PRINT
CIRCLE (400, 3750), 100, _RGB32(255, 255, 255)
LINE (250, 3750)-STEP(300, 0), _RGB32(125, 125, 0)
LINE (400, 3600)-STEP(0, 300), _RGB32(125, 125, 0)
x = SIN(_D2R(45)) * 100
y = -COS(_D2R(45)) * 100
COLOR _RGB32(255, 0, 0)
LINE (400, 3750)-STEP(x, y)
LINE STEP(0, 0)-STEP(0, -y)
LINE STEP(0, 0)-STEP(-x, 0)
_PRINTSTRING (390, 3740), "A"
_PRINTSTRING (420, 3700), "R"
COLOR -1
_PRINTSTRING (435, 3752), "x"
_PRINTSTRING (480, 3708), "y"
PRINT
PRINT "Now, for Angle A, with Radius R"
PRINT "What is the length of the other"
PRINT "two sides?"
PRINT
COLOR _RGB32(125, 125, 125)
PRINT "SIN(angle) * Hypotenuse = Opposite"
PRINT "COS(angle) * Hypotenuse = Adjacent"
COLOR -1
PRINT
PRINT
PRINT "So, let's plug in what we know to these 2 sets of equations:"
PRINT "x is ADJACENT to our angle A."
PRINT "y is OPPOSITE to our angle A."
PRINT "Our Hypotenuse is 100 pixels in size.  (R)"
PRINT "The angle A that we're using here is 45 degrees"
PRINT
PRINT "SIN(45) * 100 = Opposite (or y in this case)"
PRINT "COS(45) * 100 = Adjacent (or x in this case)"
PRINT
PRINT "X = "; INT(SIN(_D2R(45)) * 100)
PRINT "Y = "; INT(COS(_D2R(45)) * 100)
PRINT
PRINT
PRINT "And what does this tell us about this single point on our circle??"
PRINT
PRINT "That when we have a radius of 100 pixels, the point at 45 degrees is going to be 70 pixels above"
PRINT "and 70 pixels right of the center!"
PRINT
PRINT
PRINT "But this just gives us a single point on the circle, for where 45 degrees would be..."
PRINT
PRINT "But never fear!  We can apply the exact same logic and math to get ANY point on our circle!!"
PRINT
PRINT "Try it out yourself, in your head.   Change that angle to 30 degrees.  The radius is going to stay"
PRINT "the same, as it's consistent with a circle (or else you don't have a circle), but using the"
PRINT "above method, we can find ANY point on our circle..."
PRINT
PRINT
PRINT "So to do a complete circle, we might code the following:"
PRINT
PRINT "FOR angle = 1 to 360 '1 point on the circle for each degree"
PRINT "    x = SIN(_D2R(angle)) * Radius 'we have to convert degree to radian for computer math, thus _D2R"
PRINT "    y = COS(_D2R(angle)) * Radius"
PRINT "    PSET (x,y)"
PRINT "NEXT"
PRINT
PRINT
COLOR _RGB32(255, 0, 0)
PRINT "And we don't make a circle!!"
COLOR -1
PRINT
PRINT "ARGH!!  Why the heck is he teaching us how to NOT make a circle?"
PRINT "WHY didn't that make a circle??"

clip$ = "SCREEN 12" + CHR$(10) + "COLOR 4" + CHR$(10) + "Radius = 50" + CHR$(10)
clip$ = clip$ + "FOR angle = 1 TO 360 '1 point on the circle for each degree" + CHR$(10)
clip$ = clip$ + "x = SIN(_D2R(angle)) * Radius 'we have to convert degree to radian for computer math, thus _D2R" + CHR$(10)
clip$ = clip$ + "y = COS(_D2R(angle)) * Radius" + CHR$(10)
clip$ = clip$ + "PSET (x, y)" + CHR$(10)
clip$ = clip$ + "NEXT"
_CLIPBOARD$ = clip$
PRINT
PRINT
PRINT "If you're using a Windows machine to run this, open a second version of QB64 and test it out"
PRINT "for yourself."
PRINT "I've made the process as simple as possible:  Just open a new QB64 window, hit Ctrl-V to paste"
PRINT "and then compile and run.  I've already loaded your clipboard with a sample program all set to run!"
PRINT
PRINT
PRINT "For you Linux folks (whom clipboard support doesn't work fully for yet with GL), or for those"
PRINT "who don't want to test the results themselves..."
PRINT
PRINT "The reason this fails is simple:"
PRINT "We just calculated our circle, but forgot to plot it RELATIVE TO THE CENTER!"
PRINT
PRINT
PRINT "x and y were how far right and up we had to go from the CENTER to draw our circle (or arc for a"
PRINT "partial segment) -- and we didn't calculate for that."
PRINT
PRINT "What we'd want to do is change the PSET line to include that center coordinate."
PRINT "Something like this should work:  PSET (Xcenter + x, Ycenter + y)"
PRINT
PRINT "Then we just set that Xcenter and YCenter to wherever we want the center point of our circle to be"
PRINT "and we can draw it onto our screen!"
PRINT
PRINT
PRINT
PRINT "Now, that wasn't TOO terrible was it?"
PRINT
PRINT "I feel like I've rushed part of this, and have skipped over several things that would potentially be"
PRINT "very useful to people in the long run (like Opposite ^ 2 + Adjacent ^ 2 = Hypotenuse ^ 2), but I"
PRINT "wanted to toss together the very basics of SIN/COS, what they are, and how we use them for a circle"
PRINT "in our programming."
PRINT
PRINT
PRINT "I'm still going to be busy for the next few weeks, but I hope this helps people get a start on"
PRINT "learning what these commands are, and how they relate to our circles for us."
PRINT
PRINT
PRINT "Keep an eye open, and once my time frees up once more, I'll expand this even more and try and add"
PRINT "some interactive demostrations for everyone to play around with."
PRINT
PRINT "Like most projects, I don't know if it'll ever get finished."
PRINT
PRINT
PRINT "Look for the next update...."
PRINT
PRINT "SOON(tm) !!"


y = 0: Part = 1

_DEST ws
DO
    _LIMIT 60
    _PUTIMAGE (0, 0)-(800, 600), ts, ws, (0, y)-STEP(800, 600)
    k = _KEYHIT
    Button = MouseClick: MK = 0
    SELECT CASE Button
        CASE 1: k = 19712: MK = -1
        CASE 2: k = 19200: MK = -1
        CASE 4: k = 18432: MK = -1
        CASE 5: k = 20480: MK = -1
    END SELECT
    IF k = 27 THEN SYSTEM
    IF MK THEN Scroll = 25 ELSE Scroll = 10
    SELECT CASE Part
        CASE 1
            SELECT CASE k
                CASE 18432: y = y - Scroll: IF y < 0 THEN y = 0
                CASE 20480: y = y + Scroll: IF y > 700 THEN y = 700
                CASE 19712: Part = 2: y = 1300
            END SELECT
        CASE 2
            SELECT CASE k
                CASE 18432: y = y - Scroll: IF y < 1300 THEN y = 1300
                CASE 20480: y = y + Scroll: IF y > 2200 THEN y = 2200
                CASE 19712: Part = 3: y = 2800
                CASE 19200: Part = 1: y = 0
            END SELECT
        CASE 3
            SELECT CASE k
                CASE 18432: y = y - Scroll: IF y < 2800 THEN y = 2800
                CASE 20480: y = y + Scroll: IF y > 5000 THEN y = 5000
                CASE 19200: Part = 2: y = 1300
            END SELECT

    END SELECT

    _DISPLAY
    CLS
LOOP



SUB CenterText (y, text$)
    l = _PRINTWIDTH(text$)
    w = _WIDTH
    x = (w - l) \ 2
    _PRINTSTRING (x, y), text$
END SUB

FUNCTION MouseClick%
    DO WHILE _MOUSEINPUT 'check mouse status
        scroll% = scroll% + _MOUSEWHEEL ' if scrollwheel changes, watch the change here
        IF _MOUSEBUTTON(1) THEN 'left mouse pushed down
            speedup = 1
        ELSEIF _MOUSEBUTTON(2) THEN 'right mouse pushed down
            speedup = 2
        ELSEIF _MOUSEBUTTON(3) THEN 'middle mouse pushed down
            speedup = 3
        END IF
        IF speedup THEN 'buton was pushed down
            mouseclickxxx1% = _MOUSEX: mouseclickyyy1% = _MOUSEY 'see where button was pushed down at
            DO WHILE _MOUSEBUTTON(speedup) 'while button is down
                i% = _MOUSEINPUT
            LOOP 'finishes when button is let up
            IF mouseclickxxx1% >= _MOUSEX - 2 AND mouseclickxxx1% <= _MOUSEX + 2 AND mouseclickyyy1% >= _MOUSEY - 2 AND mouseclickyyy1% <= _MOUSEY + 2 THEN 'if mouse hasn't moved (ie  clicked down, dragged somewhere, then released)
                MouseClick% = speedup
            ELSE
                MouseClick% = 0
            END IF
        END IF
    LOOP
    IF scroll% < 0 THEN MouseClick% = 4
    IF scroll% > 0 THEN MouseClick% = 5
END FUNCTION

