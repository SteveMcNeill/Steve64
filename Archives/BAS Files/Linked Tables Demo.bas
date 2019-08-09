'$INCLUDE:'color32.bi'
CONST True = -1, False = 0
CONST Left = 1, Right = 2, Middle = 3, Center = 3
CONST None = 0, Alpha = 1, Numeric = 2, NoCase = 4, Reverse = 8
CONST LeftClick = 1, RightClick = 2, LeftDown = 4, RightDown = 8, Hover = 16

TYPE MenuType
    Valid AS _BYTE
    Visible AS _BYTE
    ScrollBarHidden AS _BYTE
    Top AS INTEGER
    Left AS INTEGER
    Width AS INTEGER
    Height AS INTEGER
    Frame AS _BYTE
    BorderColor AS _UNSIGNED LONG
    BackgroundColor AS _UNSIGNED LONG
    Header AS _BYTE
    Caption AS STRING * 255
    CC AS _UNSIGNED LONG 'caption color
    CBG AS _UNSIGNED LONG 'caption background color
    HighLightColor AS _UNSIGNED LONG
    Exit AS _BYTE
    Entries AS INTEGER
    TopEntry AS INTEGER
    ListColor AS _UNSIGNED LONG
    ListBackground AS _UNSIGNED LONG
    ListJustify AS _BYTE
END TYPE

DIM SHARED MenusActive AS LONG
REDIM SHARED Menu(10) AS MenuType
REDIM SHARED MenuList(32767, 10) AS STRING 'Up to 32,767 items max in our list.
REDIM SHARED MenuListDisabled(32767, 10) AS _BYTE
REDIM SHARED MenuDisplayOrder(32767, 10) AS INTEGER
TYPE LinkType
    one AS LONG
    another AS LONG
END TYPE
REDIM SHARED LinkedTo(1000) AS LinkType
DIM SHARED ScrollDelay AS _FLOAT
DIM SHARED MouseScroll AS INTEGER

'Before here goes BI file content
'After here goes working program

DEFLNG A-Z
SCREEN _NEWIMAGE(800, 600, 32)
_SCREENMOVE _MIDDLE

MainMenu = GetMenuHandle
SetMenuSize MainMenu, 200, 150
SetMenuPosition MainMenu, 100, 100
SetMenuFrame MainMenu, True, Red, Yellow
SetMenuVisible MainMenu, True
SetMenuCaption MainMenu, True, "Name", Black, White, True
SetMenuListProperties MainMenu, Black, 0, Center 'Right 'Left
SetMenuHighLightColor MainMenu, Red
FOR i = 1 TO 23
    READ n$
    AddMenuItem MainMenu, n$
NEXT

DATA Steve,Pete,Bob,Joe,Fred
DATA Sam,One,Two,Three,Four
DATA Five,Six,Seven,Eight,Nine
DATA These,are,all,my,names
DATA "Aren't",they,grand

SecondMenu = GetMenuHandle
SetMenuSize SecondMenu, 100, 150
SetMenuPosition SecondMenu, 300, 100
SetMenuFrame SecondMenu, True, Red, Yellow
SetMenuVisible SecondMenu, True
SetMenuCaption SecondMenu, True, "Age", Black, White, True
SetMenuListProperties SecondMenu, Black, 0, Left
SetMenuHighLightColor SecondMenu, Red

FOR i = 1 TO 23
    READ n$
    AddMenuItem SecondMenu, n$
NEXT

DATA 12,23,34,45,56
DATA 67,78,89,90,1
DATA 9,98,87,76,65
DATA 54,43,32,21,10
DATA 42,55,12

sortmode = 0: linked = -1: menuon = 1

HideMenuScrollBar MainMenu
LinkMenus MainMenu, SecondMenu

DisableItem MainMenu, 5
ScrollDelay = .25
DO
    CLS
    LOCATE 20, 1: PRINT "Press <H> to hide the menu."
    PRINT "Press <S> to show the menu."
    PRINT "Press <N> for No Sort order."
    PRINT "Press <A> for Alphabetic Sort order."
    PRINT "Press <#> for Numeric Sort order."
    PRINT "Press <C> to toggle case sorting."
    PRINT "Press <R> to toggle reverse sorting."
    PRINT "Press <L> to link the menus."
    PRINT "Press <U> to unlink the menus."
    PRINT "Press <TAB> to swap between menus."
    PRINT "<ESC> to quit"
    PRINT
    PRINT "Currently: ";
    IF sortmode AND 1 THEN
        PRINT "ALPHA SORT";
        IF kase THEN PRINT ", CASE-SENSITIVE";
        IF reversed THEN PRINT ", REVERSE-ORDER" ELSE PRINT
    ELSEIF sortmode AND 2 THEN
        PRINT "NUMERIC SORT";
        IF reversed THEN PRINT ", REVERSE-ORDER" ELSE PRINT
    ELSE
        PRINT "NOT SORTING"
    END IF
    LOCATE 5, 25
    IF linked THEN PRINT "LINKED LISTS" ELSE PRINT "UNLINKED LISTS"
    LOCATE 6, 15: PRINT "MENU ASSOCIATED WITH KEYBOARD: "; menuon

    MouseScroll = 0
    WHILE _MOUSEINPUT
        MouseScroll = MouseScroll + _MOUSEWHEEL
    WEND

    k = _KEYHIT
    SELECT CASE k
        CASE ASC("L"), ASC("l"): LinkMenus MainMenu, SecondMenu: linked = -1
        CASE ASC("U"), ASC("u"): UnLinkMenus MainMenu, SecondMenu: linked = 0
        CASE ASC("H"), ASC("h"): HideMenu menuon
        CASE ASC("S"), ASC("s"): ShowMenu menuon
        CASE ASC("N"), ASC("n"): sortmode = None: changed = -1: reversed = 0: kase = 0
        CASE ASC("A"), ASC("a"): sortmode = Alpha: changed = -1
        CASE ASC("#"), ASC("3"): sortmode = Numeric: changed = -1
        CASE ASC("C"), ASC("c"): kase = NOT kase: changed = -1
        CASE ASC("R"), ASC("r"): reversed = NOT reversed: changed = -1
        CASE 9: menuon = menuon + 1: IF menuon = 3 THEN menuon = 1
        CASE 27: SYSTEM
    END SELECT
    IF changed THEN
        IF sortmode <> 0 THEN
            IF kase THEN sortmode = sortmode OR NoCase ELSE sortmode = sortmode AND NOT NoCase
            IF reversed THEN sortmode = sortmode OR Reverse ELSE sortmode = sortmode AND NOT Reverse
        END IF
        MenuDisplaySort menuon, sortmode
        changed = 0
    END IF
    DisplayMenus
    CheckMenus MouseStatus, MenuSelected, OptionSelected
    IF MouseStatus <> 0 AND MenuSelected <> 0 THEN
        IF MouseStatus AND LeftClick THEN
            LOCATE 1, 1
            PRINT "You LEFT CLICKED Option #"; OptionSelected; " in Menu #"; MenuSelected
            PRINT "Which was: "; GetListItem(MenuSelected, OptionSelected)
            PRINT
            IF linked THEN
                PRINT "Since our lists are linked, we get the following items:"; GetListItem(1, OptionSelected), GetListItem(2, OptionSelected)
            ELSE
                PRINT "Since our lists are unlinked, we get the following items:"; GetListItem(MenuSelected, OptionSelected)
            END IF
            _DISPLAY
            _DELAY 2 'give it time to pop up
        ELSEIF MouseStatus AND RightClick THEN
            LOCATE 1, 1
            PRINT "You RIGHT CLICKED Option #"; OptionSelected; " in Menu #"; MenuSelected
            PRINT "Which was: "; GetListItem(MenuSelected, OptionSelected)
            PRINT
            IF linked THEN
                PRINT "Since our lists are linked, we get the following items:"; GetListItem(1, OptionSelected), GetListItem(2, OptionSelected)
            ELSE
                PRINT "Since our lists are unlinked, we get the following items:"; GetListItem(MenuSelected, OptionSelected)
            END IF
            _DISPLAY
            _DELAY 2 'give it time to pop up
        END IF
        COLOR Yellow
        IF MouseStatus AND LeftDown THEN LOCATE 35, 1: PRINT "LEFT MOUSE DOWN over Option #"; OptionSelected; " in Menu #"; MenuSelected
        IF MouseStatus AND RightDown THEN LOCATE 35, 1: PRINT "RIGHT MOUSE DOWN over Option #"; OptionSelected; " in Menu #"; MenuSelected
        COLOR Purple
        IF MouseStatus AND Hover THEN LOCATE 36, 1: PRINT "HOVERING over Option #"; OptionSelected; " in Menu #"; MenuSelected;
        COLOR White

    END IF
    _LIMIT 30
    _DISPLAY
LOOP
'And here goes the BM routines



SUB LinkMenus (handle1, handle2)
    IF handle1 = 0 OR handle2 = 0 THEN ERROR 5: EXIT SUB
    IF handle1 = handle2 THEN EXIT SUB 'Why the heck are we linking one list to itself?!
    IF Menu(handle1).Valid AND Menu(handle2).Valid THEN
        LinkMax = LinkedTo(0).one 'I'm using the very first entry into my array to store the number of link entries I have
        'First check to see if the two menus are already linked
        FOR i = 1 TO LinkMax
            found = 0
            IF handle1 = LinkedTo(i).one OR handle1 = LinkedTo(i).another THEN found = found + 1
            IF handle2 = LinkedTo(i).one OR handle2 = LinkedTo(i).another THEN found = found + 1
            IF found = 2 THEN EXIT SUB 'the two lists are already linked
            IF handle1 = 0 AND handle2 = 0 AND openspot = 0 THEN openspot = i 'we found a spot where a link was freed before; let's use it
        NEXT
        MenuDisplaySort handle1, None: MenuDisplaySort handle2, None 'unsort the lists to begin with.
        Menu(handle1).TopEntry = 1: Menu(handle2).TopEntry = 1 'and then reset them to their topmost position

        IF openspot THEN
            LinkedTo(openspot).one = handle1
            LinkedTo(openspot).another = handle2
        ELSE
            LinkMax = LinkMax + 1: LinkedTo(0).one = LinkMax
            LinkedTo(LinkMax).one = handle1
            LinkedTo(LinkMax).another = handle2
        END IF
    ELSE
        ERROR 5
    END IF
END SUB

SUB UnLinkMenus (handle1, handle2)
    IF handle1 = 0 OR handle2 = 0 THEN ERROR 5: EXIT SUB 'no list should be linked to 0.  0 is nothing...  Can't free a link to nothing.
    IF handle1 = handle2 THEN EXIT SUB 'We can't unlink a list from itself!
    IF Menu(handle1).Valid AND Menu(handle2).Valid THEN
        FOR i = 1 TO LinkedTo(0).one
            IF handle1 = LinkedTo(i).one OR handle1 = LinkedTo(i).another THEN found = found + 1
            IF handle2 = LinkedTo(i).one OR handle2 = LinkedTo(i).another THEN found = found + 1
            IF found = 2 THEN LinkedTo(i).one = 0: LinkedTo(i).another = 0 'unlink them!
        NEXT
    ELSE
        ERROR 5
    END IF
END SUB

SUB DisableItem (handle, item)
    IF Menu(handle).Valid THEN MenuListDisabled(item, handle) = -1 ELSE ERROR 5
END SUB

SUB EnableItem (handle, item)
    IF Menu(handle).Valid THEN MenuListDisabled(item, handle) = 0 ELSE ERROR 5
END SUB

SUB ShowMenu (Handle)
    IF Menu(Handle).Valid THEN Menu(Handle).Visible = -1 ELSE ERROR 5
END SUB

SUB HideMenu (Handle)
    IF Menu(Handle).Valid THEN Menu(Handle).Visible = 0 ELSE ERROR 5
END SUB

SUB ShowMenuScrollBar (Handle)
    IF Menu(Handle).Valid THEN Menu(Handle).ScrollBarHidden = 0 ELSE ERROR 5
END SUB

SUB HideMenuScrollBar (Handle)
    IF Menu(Handle).Valid THEN Menu(Handle).ScrollBarHidden = -1 ELSE ERROR 5
END SUB



FUNCTION GetListItem$ (Handle, Item)
    IF Menu(Handle).Valid THEN
        IF Item < 0 OR Item > Menu(Handle).Entries THEN ERROR 5: EXIT SUB
        GetListItem$ = LTRIM$(RTRIM$(MenuList(Item, Handle)))
    ELSE
        ERROR 5
    END IF
END FUNCTION



SUB AddMenuItem (Handle, Item$)
    IF Menu(Handle).Valid THEN
        Menu(Handle).Entries = Menu(Handle).Entries + 1
        MenuList(Menu(Handle).Entries, Handle) = Item$
        MenuDisplayOrder(Menu(Handle).Entries, Handle) = Menu(Handle).Entries
    ELSE
        ERROR 5
    END IF
END SUB


SUB SetMenuListProperties (Handle, ListColor AS _UNSIGNED LONG, ListBackground AS _UNSIGNED LONG, ListJustify AS _BYTE)
    IF Menu(Handle).Valid THEN
        Menu(Handle).ListColor = ListColor
        Menu(Handle).ListBackground = ListBackground
        Menu(Handle).ListJustify = ListJustify
    ELSE
        ERROR 5
    END IF
END SUB

SUB SetMenuHighLightColor (Handle, HighLightColor AS _UNSIGNED LONG)
    IF Menu(Handle).Valid THEN
        Menu(Handle).HighLightColor = HighLightColor
    ELSE
        ERROR 5
    END IF
END SUB


SUB SetMenuCaption (Handle, Header, Caption AS STRING * 255, CaptionColor AS _UNSIGNED LONG, CaptionBackground AS _UNSIGNED LONG, Xit)
    IF Menu(Handle).Valid THEN
        Menu(Handle).Header = Header
        Menu(Handle).Caption = Caption
        Menu(Handle).CC = CaptionColor
        Menu(Handle).CBG = CaptionBackground
        Menu(Handle).Exit = Xit
    ELSE
        ERROR 5
    END IF
END SUB


SUB SetMenuFrame (Handle, HaveFrame, FrameColor AS _UNSIGNED LONG, FrameBackGround AS _UNSIGNED LONG)
    IF Menu(Handle).Valid THEN
        Menu(Handle).Frame = HaveFrame
        Menu(Handle).BorderColor = FrameColor
        Menu(Handle).BackgroundColor = FrameBackGround
    ELSE
        ERROR 5
    END IF
END SUB



SUB SetMenuPosition (Handle, Left, Top)
    IF Menu(Handle).Valid THEN
        'some basic error checking
        IF Top < 0 THEN ERROR 5: EXIT SUB 'Let's try and keep the menu on the screen, why don't we
        IF Left < 0 THEN ERROR 5: EXIT SUB
        IF Left > _WIDTH THEN ERROR 5: EXIT SUB
        IF Top > _HEIGHT THEN ERROR 5: EXIT SUB
        Menu(Handle).Left = Left
        Menu(Handle).Top = Top
    ELSE
        ERROR 5 'toss a generic error if the handle is bad
        'I can add a custom error pop up routine later with appropiate messages
    END IF
END SUB


SUB SetMenuVisible (Handle, Visible)
    IF Menu(Handle).Valid THEN Menu(Handle).Visible = Visible ELSE ERROR 5
END SUB

SUB SetMenuSize (Handle, Width, Height)
    IF Menu(Handle).Valid THEN
        'some basic error checking
        IF Width < _FONTWIDTH THEN ERROR 5: EXIT SUB 'Can't we at least make a menu which will hold a single character?!
        IF Height < _FONTHEIGHT THEN ERROR 5: EXIT SUB
        IF Width > _WIDTH THEN ERROR 5: EXIT SUB 'And let's not make it generally larger than our screen, why don't we?!
        IF Height > _HEIGHT THEN ERROR 5: EXIT SUB
        Menu(Handle).Width = Width
        Menu(Handle).Height = Height
    ELSE
        ERROR 5 'toss a generic error if the handle is bad
        'I can add a custom error pop up routine later with appropiate messages
    END IF
END SUB

FUNCTION GetMenuHandle&
    FOR i = 1 TO MenusActive
        IF Menu(i).Valid = 0 THEN found = i: EXIT FOR
    NEXT
    IF NOT found THEN
        MenusActive = MenusActive + 1
        found = MenusActive
        u = UBOUND(menu)
        DO UNTIL MenusActive < u
            REDIM _PRESERVE Menu(u + 10) AS MenuType
            REDIM _PRESERVE MenuList(32767, u + 10) AS STRING
            REDIM _PRESERVE MenuDisplayOrder(32767, u + 10) AS INTEGER
            REDIM _PRESERVE MenuListDisabled(32767, u + 10) AS _BYTE
            u = UBOUND(menu)
        LOOP
    END IF
    GetMenuHandle& = found
    Menu(found).Valid = -1 'and let's make this a valid handle
END FUNCTION


SUB CheckMenus (MouseStatus AS LONG, MenuSelected AS LONG, OptionSelected AS LONG)

    MenuSelected = 0: OptionSelected = 0
    FOR i = 1 TO MenusActive
        IF Menu(i).Visible AND Menu(i).Valid THEN
            IF startnum = 0 THEN startnum = i
            ProcessMenu i, startnum, MouseStatus, MenuSelected, OptionSelected
            IF MenuSelected THEN EXIT SUB
        END IF
    NEXT
END SUB


SUB DisplayMenus
    FC = _DEFAULTCOLOR: BG = _BACKGROUNDCOLOR
    FOR Whichone = 1 TO MenusActive
        IF Menu(Whichone).Visible THEN
            'Get the starting limits of where menu/list text can appear
            x1 = Menu(Whichone).Left: x2 = x1 + Menu(Whichone).Width
            y1 = Menu(Whichone).Top: y2 = Menu(Whichone).Top + Menu(Whichone).Height
            caption$ = LTRIM$(RTRIM$(Menu(Whichone).Caption)) 'strip unneeded spaces from the caption (WhichOnef any)

            'clear the background
            LINE (Menu(Whichone).Left, Menu(Whichone).Top)-STEP(Menu(Whichone).Width, Menu(Whichone).Height), Menu(Whichone).BackgroundColor, BF
            'draw the frame; adjust text limits
            IF Menu(Whichone).Frame THEN
                LINE (Menu(Whichone).Left, Menu(Whichone).Top)-STEP(Menu(Whichone).Width, Menu(Whichone).Height), Menu(Whichone).BorderColor, B
                x1 = x1 + 1: y1 = y1 + 1
                x2 = x2 - 1: y2 = y2 - 1
            END IF
            IF Menu(Whichone).Header THEN
                temp = x2 - x1 + 1
                LINE (x1, y1)-(x2, y1 + _FONTHEIGHT), Menu(Whichone).CBG, BF
                IF Menu(Whichone).Exit THEN
                    temp = temp - _FONTWIDTH * 2
                    ex1 = x2 - 1 - _FONTWIDTH: ex2 = ex1 + _FONTWIDTH
                    ey1 = y1 + 1: ey2 = ey1 + _FONTHEIGHT - 3
                    LINE (ex1, ey1)-(ex2, ey2), Red, BF
                    LINE (ex1, ey1)-(ex2, ey2), Black
                    LINE (ex1, ey2)-(ex2, ey1), Black
                END IF
                DO UNTIL _PRINTWIDTH(caption$) <= temp
                    caption$ = LEFT$(caption$, LEN(caption$) - 1)
                LOOP
                COLOR Menu(Whichone).CC, Menu(Whichone).CBG
                _PRINTSTRING (x1 + (temp - _PRINTWIDTH(caption$)) \ 2, y1), caption$
                y1 = y1 + _FONTHEIGHT
                IF Menu(Whichone).Frame THEN
                    LINE (x1, y1)-(x2, y1), Menu(Whichone).BorderColor
                    y1 = y1 + 2
                END IF
            END IF 'end header creation

            IF Menu(Whichone).Entries > 0 THEN 'We have items in our list to display!
                IF Menu(Whichone).TopEntry < 1 THEN Menu(Whichone).TopEntry = 1 'check to make certain we're displaying from the first entry on at least
                IF Menu(Whichone).TopEntry > Menu(Whichone).Entries THEN Menu(Whichone).TopEntry = Menu(Whichone).Entries
                printlimit = (x2 - x1 + 1) \ _FONTWIDTH
                limitfound = 1 + (y2 - y1 + 1) \ _FONTHEIGHT - 1
                IF limitfound > Menu(Whichone).Entries THEN
                    limitfound = Menu(Whichone).Entries
                ELSE
                    scrollneeded = -1
                    printlimit = printlimit - 1
                END IF
                COLOR Menu(Whichone).ListColor, Menu(Whichone).ListBackground
                IF Menu(Whichone).ScrollBarHidden = -1 THEN scrollneeded = 0
                DIM r AS _UNSIGNED _BYTE, g AS _UNSIGNED _BYTE, b AS _UNSIGNED _BYTE
                DIM CC AS INTEGER

                r = _RED32(Menu(Whichone).BackgroundColor)
                g = _GREEN32(Menu(Whichone).BackgroundColor)
                b = _BLUE32(Menu(Whichone).BackgroundColor)
                Fade& = _RGBA32(r, g, b, 180)

                SELECT CASE Menu(Whichone).ListJustify
                    CASE Left
                        FOR j = 1 TO limitfound
                            CC = MenuDisplayOrder(Menu(Whichone).TopEntry + j - 1, Whichone) 'currentchoice
                            graybox = 0
                            t$ = RTRIM$(LTRIM$(MenuList(CC, Whichone)))
                            IF MenuListDisabled(CC, Whichone) THEN graybox = -1
                            FOR ii = 1 TO LinkedTo(0).one
                                IF Whichone = LinkedTo(ii).one AND MenuListDisabled(CC, LinkedTo(ii).another) THEN graybox = -1
                                IF Whichone = LinkedTo(ii).another AND MenuListDisabled(CC, LinkedTo(ii).one) THEN graybox = -1
                            NEXT
                            t$ = LEFT$(t$, printlimit)
                            _PRINTSTRING (x1, y1 + (j - 1) * _FONTHEIGHT), t$
                            IF graybox THEN LINE (x1, y1 + (j - 1) * _FONTHEIGHT)-(x2, y1 + (j) * _FONTHEIGHT), Fade&, BF
                        NEXT
                    CASE Right
                        FOR j = 1 TO limitfound
                            CC = MenuDisplayOrder(Menu(Whichone).TopEntry + j - 1, Whichone) 'currentchoice
                            graybox = 0
                            t$ = RTRIM$(LTRIM$(MenuList(MenuDisplayOrder(Menu(Whichone).TopEntry + j - 1, Whichone), Whichone)))
                            IF MenuListDisabled(CC, Whichone) THEN graybox = -1
                            FOR ii = 1 TO LinkedTo(0).one
                                IF Whichone = LinkedTo(ii).one AND MenuListDisabled(CC, LinkedTo(ii).another) THEN graybox = -1
                                IF Whichone = LinkedTo(ii).another AND MenuListDisabled(CC, LinkedTo(ii).one) THEN graybox = -1
                            NEXT

                            t$ = LTRIM$(LEFT$(t$, printlimit))
                            p = _PRINTWIDTH(t$)
                            IF scrollneeded THEN
                                _PRINTSTRING (x2 - p - _FONTWIDTH, y1 + (j - 1) * _FONTHEIGHT), t$
                            ELSE
                                _PRINTSTRING (x2 - p, y1 + (j - 1) * _FONTHEIGHT), t$
                            END IF
                            IF graybox THEN LINE (x1, y1 + (j - 1) * _FONTHEIGHT)-(x2, y1 + (j) * _FONTHEIGHT), Fade&, BF
                        NEXT
                    CASE Center
                        FOR j = 1 TO limitfound
                            CC = MenuDisplayOrder(Menu(Whichone).TopEntry + j - 1, Whichone) 'currentchoice
                            graybox = 0
                            t$ = LTRIM$(MenuList(MenuDisplayOrder(Menu(Whichone).TopEntry + j - 1, Whichone), Whichone))
                            IF MenuListDisabled(CC, Whichone) THEN graybox = -1
                            FOR ii = 1 TO LinkedTo(0).one
                                IF Whichone = LinkedTo(ii).one AND MenuListDisabled(CC, LinkedTo(ii).another) THEN graybox = -1
                                IF Whichone = LinkedTo(ii).another AND MenuListDisabled(CC, LinkedTo(ii).one) THEN graybox = -1
                            NEXT
                            t$ = LTRIM$(RTRIM$(LEFT$(t$, printlimit)))
                            p = _PRINTWIDTH(t$)
                            _PRINTSTRING ((x2 - x1 + 1) - p \ 2, y1 + (j - 1) * _FONTHEIGHT), t$
                            IF graybox THEN LINE (x1, y1 + (j - 1) * _FONTHEIGHT)-(x2, y1 + (j) * _FONTHEIGHT), Fade&, BF
                        NEXT
                    CASE ELSE
                        FOR j = 1 TO limitfound
                            CC = MenuDisplayOrder(Menu(Whichone).TopEntry + j - 1, Whichone) 'currentchoice
                            graybox = 0
                            t$ = RTRIM$(LTRIM$(MenuList(CC, Whichone)))
                            IF MenuListDisabled(CC, Whichone) THEN graybox = -1
                            FOR ii = 1 TO LinkedTo(0).one
                                IF Whichone = LinkedTo(ii).one AND MenuListDisabled(CC, LinkedTo(ii).another) THEN graybox = -1
                                IF Whichone = LinkedTo(ii).another AND MenuListDisabled(CC, LinkedTo(ii).one) THEN graybox = -1
                            NEXT
                            t$ = LEFT$(t$, printlimit)
                            _PRINTSTRING (x1, y1 + (j - 1) * _FONTHEIGHT), t$
                            IF graybox THEN LINE (x1, y1 + (j - 1) * _FONTHEIGHT)-(x2, y1 + (j) * _FONTHEIGHT), Fade&, BF
                        NEXT
                        Menu(Whichone).ListJustify = Left 'If it's not specified for some reason, let's make it left justified as default
                END SELECT
            END IF 'end of displaying items
            IF scrollneeded THEN 'then we need a vertical scroll bar
                barx1 = x2 - _FONTWIDTH - 1
                barx2 = barx1 + _FONTWIDTH
                LINE (barx1, y1)-(barx2, y2), LightGray, BF
                COLOR Black, DarkGray
                _PRINTSTRING (barx1, y1), ""
                _PRINTSTRING (barx1, y2 - _FONTHEIGHT), ""
            END IF
        END IF
    NEXT
    COLOR FC, BG
END SUB



SUB ProcessMenu (WhichOne AS LONG, StartNum AS LONG, MouseStatus AS LONG, MenuSelected AS LONG, OptionSelected AS LONG)
    STATIC OldMouse AS _BYTE, ElapsedTimer AS _FLOAT, Click AS _BYTE
    STATIC ScrollAble AS _BYTE, OldMouse2 AS _BYTE, Click2 AS _BYTE
    MX = _MOUSEX: MY = _MOUSEY: MB = _MOUSEBUTTON(1): MB2 = _MOUSEBUTTON(2)
    IF ScrollDelay < 0 THEN ScrollDelay = 0

    'Get the starting limits of where menu/list text can appear
    x1 = Menu(WhichOne).Left: x2 = x1 + Menu(WhichOne).Width
    y1 = Menu(WhichOne).Top: y2 = Menu(WhichOne).Top + Menu(WhichOne).Height
    IF WhichOne = StartNum THEN
        IF OldMouse = 0 AND MB = -1 THEN Click = -1 ELSE Click = 0
        IF OldMouse2 = 0 AND MB2 = -1 THEN Click2 = -1 ELSE Click2 = 0
        OldMouse = MB: OldMouse2 = MB2
        IF ElapsedTimer + ScrollDelay < TIMER(0.01) THEN
            ElapsedTimer = TIMER(0.01)
            ScrollAble = -1
        ELSE
            ScrollAble = 0
        END IF
    END IF




    IF Menu(WhichOne).Frame THEN
        LINE (Menu(WhichOne).Left, Menu(WhichOne).Top)-STEP(Menu(WhichOne).Width, Menu(WhichOne).Height), Menu(WhichOne).BorderColor, B
        x1 = x1 + 1: y1 = y1 + 1
        x2 = x2 - 1: y2 = y2 - 1
    END IF
    IF Menu(WhichOne).Header THEN
        temp = x2 - x1 + 1
        IF Menu(WhichOne).Exit THEN
            temp = temp - _FONTWIDTH * 2
            ex1 = x2 - 1 - _FONTWIDTH: ex2 = ex1 + _FONTWIDTH
            ey1 = y1 + 1: ey2 = ey1 + _FONTHEIGHT - 3
        END IF
        y1 = y1 + _FONTHEIGHT
        IF Menu(WhichOne).Frame THEN y1 = y1 + 2
    END IF 'end header creation

    IF Menu(WhichOne).Entries > 0 THEN 'We have items in our list to display!
        IF Menu(WhichOne).TopEntry < 1 THEN Menu(WhichOne).TopEntry = 1 'check to make certain we're displaying from the first entry on at least
        IF Menu(WhichOne).TopEntry > Menu(WhichOne).Entries THEN Menu(WhichOne).TopEntry = Menu(WhichOne).Entries
        printlimit = (x2 - x1 + 1) \ _FONTWIDTH
        limitfound = 1 + (y2 - y1 + 1) \ _FONTHEIGHT - 1
        IF limitfound > Menu(WhichOne).Entries THEN
            limitfound = Menu(WhichOne).Entries
        ELSE
            scrollneeded = -1
            printlimit = printlimit - 1
        END IF
    END IF 'end of displaying items

    IF Menu(WhichOne).ScrollBarHidden = -1 THEN scrollneeded = 0

    IF scrollneeded THEN 'then we need a vertical scroll bar
        barx1 = x2 - _FONTWIDTH - 1
        barx2 = barx1 + _FONTWIDTH
    END IF


    SELECT CASE MY 'let's determine which line we clicked the mouse on
        CASE IS < ey1 'do nothing as it's too far up the screen to be a click in this box
        CASE IS > y2 'do nothing again as it's too far down the screen to be a click in this box
        CASE ey1 TO ey2 'we've clicked on the line where the EXIT button might exist
        CASE y1 TO y2
    END SELECT



    SELECT CASE MY 'let's determine which line we clicked the mouse on
        CASE IS < ey1 'do nothing as it's too far up the screen to be a click in this box
        CASE IS > y2 'do nothing again as it's too far down the screen to be a click in this box
        CASE ey1 TO ey2 'we've clicked on the line where the EXIT button might exist
            IF Click AND Menu(WhichOne).Exit THEN
                IF MX >= ex1 AND MX <= ex2 THEN Menu(WhichOne).Visible = False 'If the exit button is available, and we click it, it closes the menu/list
            END IF
        CASE y1 TO y2
            done = 0
            IF barx1 > 0 THEN p2 = barx1 - 1 ELSE p2 = x2
            IF MX >= x1 AND MX <= p2 THEN 'highlight the choice the user is over
                yPOS = ((MY - y1 + 1) \ _FONTHEIGHT) * _FONTHEIGHT + y1
                IF yPOS + _FONTHEIGHT <= y2 THEN LINE (x1, yPOS)-(p2, yPOS + _FONTHEIGHT), Menu(WhichOne).HighLightColor, B
            END IF

            IF MouseScroll THEN
                IF MX >= x1 AND MX <= x2 THEN
                    Menu(WhichOne).TopEntry = Menu(WhichOne).TopEntry + MouseScroll
                    IF Menu(WhichOne).TopEntry < 1 THEN Menu(WhichOne).TopEntry = 1
                    IF Menu(WhichOne).TopEntry > Menu(WhichOne).Entries - limitfound + 1 THEN Menu(WhichOne).TopEntry = Menu(WhichOne).Entries - limitfound + 1
                    FOR i = 1 TO LinkedTo(0).one
                        IF WhichOne = LinkedTo(i).one THEN Menu(LinkedTo(i).another).TopEntry = Menu(WhichOne).TopEntry
                        IF WhichOne = LinkedTo(i).another THEN Menu(LinkedTo(i).one).TopEntry = Menu(WhichOne).TopEntry
                    NEXT
                END IF
            END IF

            IF scrollneeded THEN
                IF MY >= y1 AND MY <= y1 + _FONTHEIGHT AND MX >= barx1 AND MX <= barx2 AND MB <> 0 THEN 'it's the top scroll bar
                    IF ScrollAble THEN Menu(WhichOne).TopEntry = Menu(WhichOne).TopEntry - 1
                    IF Menu(WhichOne).TopEntry < 1 THEN Menu(WhichOne).TopEntry = 1
                    done = -1
                    FOR i = 1 TO LinkedTo(0).one
                        IF WhichOne = LinkedTo(i).one THEN Menu(LinkedTo(i).another).TopEntry = Menu(WhichOne).TopEntry
                        IF WhichOne = LinkedTo(i).another THEN Menu(LinkedTo(i).one).TopEntry = Menu(WhichOne).TopEntry
                    NEXT
                ELSEIF MY >= y2 - _FONTHEIGHT AND MY <= y2 AND MX >= barx1 AND MX <= barx2 AND MB <> 0 THEN 'it's the bottom scroll bar
                    IF ScrollAble THEN Menu(WhichOne).TopEntry = Menu(WhichOne).TopEntry + 1
                    IF Menu(WhichOne).TopEntry > Menu(WhichOne).Entries - limitfound + 1 THEN Menu(WhichOne).TopEntry = Menu(WhichOne).Entries - limitfound + 1
                    done = -1
                    FOR i = 1 TO LinkedTo(0).one
                        IF WhichOne = LinkedTo(i).one THEN Menu(LinkedTo(i).another).TopEntry = Menu(WhichOne).TopEntry
                        IF WhichOne = LinkedTo(i).another THEN Menu(LinkedTo(i).one).TopEntry = Menu(WhichOne).TopEntry
                    NEXT
                ELSEIF MX >= barx1 AND MX <= barx2 AND MB <> 0 THEN
                    MenuLimit = Menu(WhichOne).Entries - limitfound + 2
                    ylimit = y2 - y1 - _FONTHEIGHT * 2 + 1
                    yPOS = MY - y1 - _FONTHEIGHT + 1
                    Menu(WhichOne).TopEntry = (MenuLimit - (ylimit - yPOS) / ylimit * MenuLimit)
                    IF Menu(WhichOne).TopEntry >= MenuLimit THEN Menu(WhichOne).TopEntry = MenuLimit - 1
                    done = -1
                    FOR i = 1 TO LinkedTo(0).one
                        IF WhichOne = LinkedTo(i).one THEN Menu(LinkedTo(i).another).TopEntry = Menu(WhichOne).TopEntry
                        IF WhichOne = LinkedTo(i).another THEN Menu(LinkedTo(i).one).TopEntry = Menu(WhichOne).TopEntry
                    NEXT
                END IF
            END IF

            IF NOT done THEN 'if we've processed a scrollbar event, we're finished
                IF MX >= x1 AND MX <= x2 THEN
                    MenuSelected = WhichOne
                    OptionSelected = MenuDisplayOrder((MY - y1 + 1) \ _FONTHEIGHT + Menu(WhichOne).TopEntry, WhichOne)
                    invalidate = 0
                    IF MenuListDisabled(OptionSelected, WhichOne) THEN invalidate = -1
                    FOR ii = 1 TO LinkedTo(0).one
                        IF WhichOne = LinkedTo(ii).one AND MenuListDisabled(OptionSelected, LinkedTo(ii).another) THEN invalidate = -1
                        IF WhichOne = LinkedTo(ii).another AND MenuListDisabled(OptionSelected, LinkedTo(ii).one) THEN invalidate = -1
                    NEXT
                    IF barx1 <> 0 AND MX > barx1 THEN invalidate = -1
                    IF invalidate THEN MenuSelected = 0: OptionSelected = 0
                END IF
            END IF
    END SELECT


    MouseStatus = 0
    MouseStatus = MouseStatus OR -Click 'leftclick
    MouseStatus = MouseStatus OR Click2 * -2 'rightclick
    MouseStatus = MouseStatus OR _MOUSEBUTTON(1) * -4 'leftdown
    MouseStatus = MouseStatus OR _MOUSEBUTTON(2) * -8 'rightdown
    MouseStatus = MouseStatus OR (MenuSelected <> 0) * 16 'If we're over the menu, we're hovering

END SUB


SUB MenuDisplaySort (handle AS LONG, sortmethod AS _BYTE)
    gap = Menu(handle).Entries

    IF sortmethod AND Alpha THEN
        IF sortmethod AND NoCase THEN
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    t$ = UCASE$(LTRIM$(RTRIM$(MenuList(MenuDisplayOrder(i, handle), handle))))
                    t1$ = UCASE$(LTRIM$(RTRIM$(MenuList(MenuDisplayOrder(i + gap, handle), handle))))
                    IF t$ > t1$ THEN
                        SWAP MenuDisplayOrder(i, handle), MenuDisplayOrder(i + gap, handle)
                        FOR ii = 1 TO LinkedTo(0).one
                            IF handle = LinkedTo(ii).one THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).another), MenuDisplayOrder(i + gap, LinkedTo(ii).another)
                            IF handle = LinkedTo(ii).another THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).one), MenuDisplayOrder(i + gap, LinkedTo(ii).one)
                        NEXT
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > Menu(handle).Entries
            LOOP UNTIL gap = 1 AND swapped = 0
        ELSE
            DO
                gap = 10 * gap \ 13
                IF gap < 1 THEN gap = 1
                i = 0
                swapped = 0
                DO
                    t$ = LTRIM$(RTRIM$(MenuList(MenuDisplayOrder(i, handle), handle)))
                    t1$ = LTRIM$(RTRIM$(MenuList(MenuDisplayOrder(i + gap, handle), handle)))
                    IF t$ > t1$ THEN
                        SWAP MenuDisplayOrder(i, handle), MenuDisplayOrder(i + gap, handle)
                        FOR ii = 1 TO LinkedTo(0).one
                            IF handle = LinkedTo(ii).one THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).another), MenuDisplayOrder(i + gap, LinkedTo(ii).another)
                            IF handle = LinkedTo(ii).another THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).one), MenuDisplayOrder(i + gap, LinkedTo(ii).one)
                        NEXT
                        swapped = -1
                    END IF
                    i = i + 1
                LOOP UNTIL i + gap > Menu(handle).Entries
            LOOP UNTIL gap = 1 AND swapped = 0
        END IF
        IF sortmethod AND Reverse THEN
            FOR i = 1 TO Menu(handle).Entries \ 2
                SWAP MenuDisplayOrder(i, handle), MenuDisplayOrder(Menu(handle).Entries - i + 1, handle)
                FOR ii = 1 TO LinkedTo(0).one
                    IF handle = LinkedTo(ii).one THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).another), MenuDisplayOrder(Menu(handle).Entries - i + 1, LinkedTo(ii).another)
                    IF handle = LinkedTo(ii).another THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).one), MenuDisplayOrder(Menu(handle).Entries - i + 1, LinkedTo(ii).one)
                NEXT
            NEXT
        END IF
    ELSEIF sortmethod AND Numeric THEN
        DO
            gap = 10 * gap \ 13
            IF gap < 1 THEN gap = 1
            i = 0
            swapped = 0
            DO
                IF VAL(MenuList(MenuDisplayOrder(i, handle), handle)) > VAL(MenuList(MenuDisplayOrder(i + gap, handle), handle)) THEN
                    SWAP MenuDisplayOrder(i, handle), MenuDisplayOrder(i + gap, handle)
                    FOR ii = 1 TO LinkedTo(0).one
                        IF handle = LinkedTo(ii).one THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).another), MenuDisplayOrder(i + gap, LinkedTo(ii).another)
                        IF handle = LinkedTo(ii).another THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).one), MenuDisplayOrder(i + gap, LinkedTo(ii).one)
                    NEXT
                    swapped = -1
                END IF
                i = i + 1
            LOOP UNTIL i + gap > Menu(handle).Entries
        LOOP UNTIL gap = 1 AND swapped = 0
        IF sortmethod AND Reverse THEN
            FOR i = 1 TO Menu(handle).Entries \ 2
                SWAP MenuDisplayOrder(i, handle), MenuDisplayOrder(Menu(handle).Entries - i + 1, handle)
                FOR ii = 1 TO LinkedTo(0).one
                    IF handle = LinkedTo(ii).one THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).another), MenuDisplayOrder(Menu(handle).Entries - i + 1, LinkedTo(ii).another)
                    IF handle = LinkedTo(ii).another THEN SWAP MenuDisplayOrder(i, LinkedTo(ii).one), MenuDisplayOrder(Menu(handle).Entries - i + 1, LinkedTo(ii).one)
                NEXT
            NEXT
        END IF
    ELSE
        FOR i = 1 TO Menu(handle).Entries
            MenuDisplayOrder(i, handle) = i
            FOR ii = 1 TO LinkedTo(0).one
                IF handle = LinkedTo(ii).one THEN MenuDisplayOrder(i, LinkedTo(ii).another) = i
                IF handle = LinkedTo(ii).another THEN MenuDisplayOrder(i, LinkedTo(ii).one) = i
            NEXT
        NEXT
    END IF

END SUB

