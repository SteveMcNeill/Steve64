DEFLNG A-Z 'default to long instead of single
TYPE TextArea
    InUse AS INTEGER
    x1 AS LONG 'left
    y1 AS LONG 'top
    w AS LONG 'width
    h AS LONG 'height
    FrameColor AS _UNSIGNED LONG
    BackColor AS _UNSIGNED LONG
    Xpos AS INTEGER
    Ypos AS INTEGER
    VerticalAlignment AS INTEGER
    Justification AS INTEGER
    UpdateMethod AS INTEGER
    TextColor AS _UNSIGNED LONG
    TextBackgroundColor AS _UNSIGNED LONG
    SavedBackground AS INTEGER
    HideFrame AS INTEGER
    ScreenX AS INTEGER
    ScreenY AS INTEGER
    Font AS LONG 'NEW! Change fonts for each independent font area
END TYPE

REDIM SHARED TextHandles(0) AS TextArea

CONST True = -1, False = 0
CONST LeftJustify = -1, CenterJustify = -2, RightJustify = -3, NoJustify = 0
CONST OnLine = 0, CenterLine = -1, TopLine = 1, BottomLine = -2
CONST NoUpdate = 0, DoUpdate = 1, NewLine = 2
'********************************************************
'* Text Frames before this line
'********************************************************


$COLOR:32



$CONSOLE
_CONSOLE ON 'for debugging purposes while making/testing things

TYPE Damage_Type
    Low AS INTEGER
    High AS INTEGER
END TYPE

TYPE Light_Type
    Reach AS INTEGER
    Left AS INTEGER
END TYPE

TYPE Weapon_Type
    Identified AS _UNSIGNED _BYTE
    Name AS STRING * 20
    DisplayedName AS STRING * 20
    Reach AS INTEGER
    Damage AS Damage_Type
    HitBonus AS INTEGER
    DamageBonus AS INTEGER
    Left AS INTEGER 'life left on the weapon, AKA "durability", but easier and cheaper to spell
    IconX AS LONG
    IconY AS LONG
    Light AS Light_Type
END TYPE

TYPE Armor_Type
    Identified AS _UNSIGNED _BYTE
    Name AS STRING * 20
    DisplayedName AS STRING * 20
    PD AS INTEGER 'Passive Defense (dodge)
    DR AS INTEGER 'Damage Resistance (absorption)
    Left AS INTEGER 'life left on the weapon, AKA "durability", but easier and cheaper to spell
    IconX AS LONG
    IconY AS LONG
END TYPE

TYPE Food_Type
    Identified AS _UNSIGNED _BYTE
    Name AS STRING * 20
    DisplayedName AS STRING * 20
    HungerFill AS SINGLE 'how much said food fills one's stomach
    HungerRate AS SINGLE 'how fast it digests before we get hunry again
    IconX AS LONG
    IconY AS LONG
END TYPE


TYPE Hero_Type
    Name AS STRING * 20
    X AS _UNSIGNED _BYTE
    Y AS _UNSIGNED _BYTE
    Life AS Damage_Type
    Mana AS Damage_Type
    Level AS _UNSIGNED _BYTE
    EXP_Earned AS LONG
    EXP_Needed AS LONG
    Light AS Light_Type
    Weapon1 AS Weapon_Type
    Weapon2 AS Weapon_Type
    Armor AS Armor_Type
    HealingRate AS INTEGER 'number of turns before the hero heals a point
    Hunger AS SINGLE
    HungerRate AS SINGLE
END TYPE

TYPE Treasure_TYPE
    Chance AS SINGLE
    Type AS _UNSIGNED _BYTE '1 weapon, 2 armor, 3 food, 4 item
    Index AS INTEGER 'the number of the type... Weapon(0) = "Bare Fist", so tyoe = 1, index = 0.
END TYPE

TYPE Monster_TYPE
    Name AS STRING * 20
    Life AS Damage_Type
    Level AS INTEGER
    ExpBonus AS INTEGER
    Sight AS INTEGER
    Hearing AS INTEGER
    Detection AS INTEGER 'in case it has some sort of magic "sixth sense" to detect characters, not related to sight nor sound.
    Weapon1 AS Weapon_Type
    Weapon2 AS Weapon_Type
    Armor AS Armor_Type
    Found AS INTEGER
    IconX AS LONG
    IconY AS LONG
    Loot1 AS Treasure_TYPE
    Loot2 AS Treasure_TYPE
    loot3 AS Treasure_TYPE
END TYPE

TYPE Encounter_TYPE
    Active AS INTEGER
    X AS INTEGER
    Y AS INTEGER
    Type AS INTEGER '0 monster, 1 weapon, 2 armor, 3 food, 4 item
    Index AS INTEGER
    Life AS INTEGER
END TYPE

TYPE Inventory_Type
    Weapon AS Weapon_Type
    Armor AS Armor_Type
    Food AS Food_Type
    'Item as item_type 'to come later, once general items get added
END TYPE


REDIM SHARED Monster(100) AS Monster_TYPE
REDIM SHARED Encounter(1000) AS Encounter_TYPE, EncounterLimit AS INTEGER

'These are all base items and weapons.  The sets which the character actually carries will vary significantly from wear/tear, enchantments, and other in-game factors.
REDIM SHARED Weapon(100) AS Weapon_Type
REDIM SHARED Armor(100) AS Armor_Type
REDIM SHARED Food(100) AS Food_Type

DIM SHARED Hero AS Hero_Type
REDIM SHARED Inventory(103) AS Inventory_Type '103 slots each to carry weapons/armor/food/items
DIM SHARED WeaponsOwned AS _UNSIGNED _BYTE, ArmorOwned AS _UNSIGNED _BYTE, FoodOwned AS _UNSIGNED _BYTE, ItemsOwned AS _UNSIGNED _BYTE 'a count of each which we own.  Should always be a value between 0 and 103.
DIM SHARED Level AS _UNSIGNED _BYTE: Level = 1
DIM SHARED XL, XH, YL, YH 'the map X/Y low/high array limits.
DIM SHARED PrintArea AS LONG 'the handle to our text frame print area for game results.
DIM SHARED Scale AS _FLOAT, WorkScreen AS LONG, DisplayScreen AS LONG, Icons AS LONG
DIM SHARED TextFont AS LONG, StepsTaken AS _UNSIGNED _INTEGER64

WorkScreen = _NEWIMAGE(3200, 2400, 32)
DisplayScreen = _NEWIMAGE(800, 700, 32)
SCREEN DisplayScreen
Scale = 2
_FONT 8

REDIM SHARED MapArray(0, 0) AS _UNSIGNED _BYTE
'1 map is illuminated
'2 map is uncovered
'4 map is a wall
'8 map is a pathway
'16 map is a stairway
'32 map is simply blocked (perhaps with a monster?)
'64 map is secret (can not be uncovered)

REDIM SHARED Distance(0, 0) AS _UNSIGNED _BYTE
REDIM SHARED Temp(0, 0) AS _UNSIGNED _BYTE

RANDOMIZE TIMER
_CONTROLCHR OFF

Init
CreateMap 99, 74, 10
DO
    DrawMap
    DisplayCharacter
    _DISPLAY
    GetInput
    MonstersTurn
    CheckForHeroGrowth
LOOP

SUB Init
    D = _DEST
    Hero.Name = "Steve The Tester!"
    Hero.Life.Low = 10: Hero.Life.High = 10: Hero.Level = 1
    Hero.Mana.Low = 10: Hero.Mana.High = 10
    Hero.EXP_Earned = 0: Hero.EXP_Needed = 2
    Hero.Weapon1.Name = "Bare Fist"
    Hero.Weapon1.Reach = 1: Hero.Weapon1.Damage.Low = 1: Hero.Weapon1.Damage.High = 2
    Hero.Weapon1.HitBonus = 0: Hero.Weapon1.DamageBonus = 0
    Hero.Weapon1.Left = -1 'your fist is indestructible!
    Hero.Weapon1.IconX = 2 * 32: Hero.Weapon1.IconY = 47 * 32
    Hero.Weapon2.Name = "Magic Candle"
    Hero.Weapon2.Reach = 0: Hero.Weapon2.Damage.Low = 0: Hero.Weapon2.Damage.High = 0
    Hero.Weapon2.HitBonus = 0: Hero.Weapon2.DamageBonus = 0
    Hero.Weapon2.Left = 0 'you can't attack with a candle
    Hero.Weapon2.IconX = 52 * 32: Hero.Weapon2.IconY = 42 * 32
    Hero.Weapon2.Light.Reach = 2: Hero.Weapon2.Light.Left = -1 'infinite
    Hero.Armor.Name = "Naked"
    Hero.Armor.PD = 0: Hero.Armor.DR = 0: Hero.Armor.Left = -1 'you might be naked, but at least you can't break your armor!
    Hero.Armor.IconX = 46 * 32: Hero.Armor.IconY = 42 * 32
    Hero.HealingRate = 20 'the hero heals 1 point of health for every 20 valid turns naturally
    Hero.HungerRate = 0.1 'Let's start heros out with a full belly and a low hunger growth rate

    PrintArea = NewTextArea(230, 601, 799, 699, False)
    ColorTextArea PrintArea, _RGB32(255, 255, 255), _RGB32(0, 0, 128)
    SetTextFont PrintArea, "courbd.ttf", 24
    DrawTextArea PrintArea
    SetPrintPositionX PrintArea, CenterJustify
    SetPrintUpdate PrintArea, NewLine
    PrintOut PrintArea, "WELCOME TO (almost) ROGUE"
    SetTextFont PrintArea, "courbd.ttf", 18
    PrintOut PrintArea, "created by STEVE!"
    PrintOut PrintArea, ""
    SetPrintPositionX PrintArea, LeftJustify
    SetTextFont PrintArea, "courbd.ttf", 12
    TextFont = 12

    temp = _NEWIMAGE(480, 480, 32)
    _DEST temp: _CONTROLCHR OFF
    COLOR &HFFFFFF00, 0 'Yellow Hero
    _PRINTSTRING (0, 0), CHR$(1) 'the hero
    COLOR &HFFFF0000, 0 'Red Question Mark
    _PRINTSTRING (16, 0), "?" 'a question mark
    Icons = _LOADIMAGE("Sprites.png", 32)
    _PUTIMAGE (2016, 1504)-STEP(32, 32), temp, Icons, (0, 0)-STEP(_FONTWIDTH, _FONTHEIGHT)
    _PUTIMAGE (1984, 1504)-STEP(32, 32), temp, Icons, (16, 0)-STEP(_FONTWIDTH, _FONTHEIGHT)
    '        SCREEN Icons: DO: SLEEP: LOOP
    _DEST D

    'Init some basic weapons which we can find

    'These first two should always be available for us to use/enjoy.
    Weapon(0).Name = "Bare Fist"
    Weapon(0).Reach = 1: Weapon(0).Damage.Low = 1: Weapon(0).Damage.High = 2
    Weapon(0).HitBonus = 0: Weapon(0).DamageBonus = 0
    Weapon(0).Left = -1 'your fist is indestructible!
    Weapon(0).IconX = 2 * 32: Weapon(0).IconY = 47 * 32
    Weapon(0).Light.Reach = 0: Weapon(0).Light.Left = 0
    Weapon(1).Name = "Magic Candle"
    Weapon(1).Reach = 0: Weapon(1).Damage.Low = 0: Weapon(1).Damage.High = 0
    Weapon(1).HitBonus = 0: Weapon(1).DamageBonus = 0
    Weapon(1).Left = 0 'you can't attack with a candle
    Weapon(1).IconX = 52 * 32: Weapon(1).IconY = 42 * 32
    Weapon(1).Light.Reach = 2: Weapon(1).Light.Left = -1 'infinite

    Armor(0).Name = "Naked"
    Armor(0).PD = 0: Armor(0).DR = 0: Armor(0).Left = -1 'you might be naked, but at least you can't break your armor!
    Armor(0).IconX = 46 * 32: Armor(0).IconY = 42 * 32

    Food(1).Name = "Bat Meat"
    Food(1).HungerFill = 10: Food(1).HungerRate = .1
    Food(1).IconX = 36 * 32: Food(1).IconY = 23 * 32

    Food(2).Name = "Rat Meat"
    Food(2).HungerFill = 10: Food(2).HungerRate = .1
    Food(2).IconX = 36 * 32: Food(2).IconY = 23 * 32

    Food(3).Name = "Snake Meat"
    Food(3).HungerFill = 10: Food(3).HungerRate = .1
    Food(3).IconX = 36 * 32: Food(3).IconY = 23 * 32

    WeaponsOwned = 2
    Inventory(0).Weapon.Identified = -1
    Inventory(0).Weapon.Name = "Bare Fist"
    Inventory(0).Weapon.Reach = 1: Inventory(0).Weapon.Damage.Low = 1: Inventory(0).Weapon.Damage.High = 2
    Inventory(0).Weapon.HitBonus = 0: Inventory(0).Weapon.DamageBonus = 0
    Inventory(0).Weapon.Left = -1 'your fist is indestructible!
    Inventory(0).Weapon.IconX = 2 * 32: Inventory(0).Weapon.IconY = 47 * 32
    Inventory(0).Weapon.Light.Reach = 0: Inventory(0).Weapon.Light.Left = -1
    Inventory(1).Weapon.Identified = -1
    Inventory(1).Weapon.Name = "Magic Candle"
    Inventory(1).Weapon.Reach = 0: Inventory(1).Weapon.Damage.Low = 0: Inventory(1).Weapon.Damage.High = 0
    Inventory(1).Weapon.HitBonus = 0: Inventory(1).Weapon.DamageBonus = 0
    Inventory(1).Weapon.Left = -1 'magic candle is indestructible
    Inventory(1).Weapon.IconX = 52 * 32: Inventory(1).Weapon.IconY = 42 * 32
    Inventory(1).Weapon.Light.Reach = 2: Inventory(1).Weapon.Light.Left = -1 'infinite

    ArmorOwnded = 1
    Inventory(0).Armor.Identified = -1
    Inventory(0).Armor.Name = "Naked"
    Inventory(0).Armor.PD = 0: Inventory(0).Armor.DR = 0: Inventory(0).Armor.Left = -1 'you might be naked, but at least you can't break your armor!
    Inventory(0).Armor.IconX = 46 * 32: Inventory(0).Armor.IconY = 42 * 32

    InitMonsters
END SUB

SUB InitMonsters

    Monster(1).Name = "Bat": Monster(1).Life.Low = 1: Monster(1).Life.High = 4: Monster(1).Level = 1: Monster(1).ExpBonus = 0
    Monster(1).Sight = 2: Monster(1).Hearing = 4: Monster(1).Detection = 0
    Monster(1).Weapon1.Name = "Bite": Monster(1).Weapon1.Reach = 1
    Monster(1).Weapon1.Damage.Low = 1: Monster(1).Weapon1.Damage.High = 2
    Monster(1).IconX = 44 * 32: Monster(1).IconY = 3 * 32 'position 44,3 on the sprite sheet
    Monster(1).Loot1.Chance = 95: Monster(1).Loot1.Type = 3: Monster(1).Loot1.Index = 1
    'Monster(1).Weapon1.HitBonus = 0: Monster(1).Weapon1.DamageBonus = 0: Monster(1).Weapon1.Left = 0
    'Monster(1).Weapon2.Name = "": Monster(1).Weapon2.Reach = 0
    'Monster(1).Weapon2.Damage.Low = 0: Monster(1).Weapon2.Damage.High = 0
    'Monster(1).Weapon2.HitBonus = 0: Monster(1).Weapon2.DamageBonus = 0: Monster(1).Weapon2.Left = 0
    'Monster(1).Armor.Name = ""
    'Monster(1).Armor.PD = 0: Monster(1).Armor.DR = 0: Monster(1).Armor.Left = 0
    Monster(2).Name = "Rat": Monster(2).Life.Low = 1: Monster(2).Life.High = 4
    Monster(2).Level = 1: Monster(2).ExpBonus = 0
    Monster(2).Sight = 2: Monster(2).Hearing = 4: Monster(2).Detection = 0
    Monster(2).Weapon1.Name = "Bite": Monster(2).Weapon1.Reach = 1
    Monster(2).Weapon1.Damage.Low = 1: Monster(2).Weapon1.Damage.High = 2
    Monster(2).IconX = 23 * 32: Monster(2).IconY = 4 * 32 'position 44,3 on the sprite sheet
    Monster(2).Loot1.Chance = 25: Monster(2).Loot1.Type = 3: Monster(2).Loot1.Index = 2
    Monster(3).Name = "Snake": Monster(3).Life.Low = 1: Monster(3).Life.High = 4
    Monster(3).Level = 1: Monster(3).ExpBonus = 0
    Monster(3).Sight = 2: Monster(3).Hearing = 4: Monster(3).Detection = 0
    Monster(3).Weapon1.Name = "Bite": Monster(3).Weapon1.Reach = 1
    Monster(3).Weapon1.Damage.Low = 1: Monster(3).Weapon1.Damage.High = 2
    Monster(3).IconX = 37 * 32: Monster(3).IconY = 4 * 32 'position 44,3 on the sprite sheet
    Monster(3).Loot1.Chance = 25: Monster(3).Loot1.Type = 3: Monster(3).Loot1.Index = 3
END SUB


SUB CheckForHeroGrowth
    IF Hero.Life.Low < 1 THEN 'first, let's check to see if we died...
        CLS
        PRINT "YOU DIED!  HAHAHAHA!! (Better ending coming later...)"
        _DISPLAY
        BEEP
        _DELAY 5
        SYSTEM
    END IF
    IF Hero.EXP_Earned >= Hero.EXP_Needed THEN 'let's check to see if the hero has leveled up
        PrintOut PrintArea, "Congratulations!  You have gained a level!"
        DO
            r = INT(RND * 6) + 1
            lifegained = lifegained + r
        LOOP UNTIL r <> 6
        Hero.Life.Low = Hero.Life.Low + r
        Hero.Life.High = Hero.Life.High + r
        Hero.EXP_Earned = 0
        Hero.Level = Hero.Level + 1
        Hero.EXP_Needed = Hero.EXP_Needed + Hero.Level + 1
    END IF
    IF StepsTaken MOD Hero.HealingRate = 0 THEN 'heal the hero naturally over time
        IF Hero.Life.Low < Hero.Life.High THEN Hero.Life.Low = Hero.Life.Low + 1
    END IF
    Hero.Hunger = Hero.Hunger + Hero.HungerRate
    IF Hero.Weapon1.Light.Left > -1 THEN Hero.Weapon1.Light.Left = Hero.Weapon1.Light.Left - 1 'durability on our light sources wear down over time
    IF Hero.Weapon2.Light.Left > -1 THEN Hero.Weapon2.Light.Left = Hero.Weapon2.Light.Left - 1

END SUB


SUB DisplayCharacter
    LINE (0, 601)-(229, 799), &HFF000000, BF
    COLOR -1, 0
    Box 0, 601, 229, 62, 0, 0, "", Silver, 0
    Box 0, 601, 229, 12, Black, 0, _TRIM$(Hero.Name), Silver, 0
    Box 0, 626, 229 * Hero.Life.Low / Hero.Life.High, 12, 0, 0, "", Red, Black
    Box 0, 639, 229 * Hero.Mana.Low / Hero.Mana.High, 12, 0, 0, "", Blue, Black
    Box 0, 652, 229 * Hero.EXP_Earned / Hero.EXP_Needed, 12, 0, 0, "", Green, Black
    _PRINTSTRING (10, 616), "LEVEL:" + STR$(Hero.Level)
    _PRINTSTRING (10, 629), "LIFE :" + STR$(Hero.Life.Low) + " / " + _TRIM$(STR$(Hero.Life.High))
    _PRINTSTRING (10, 642), "MANA :" + STR$(Hero.Mana.Low) + " / " + _TRIM$(STR$(Hero.Mana.High))
    _PRINTSTRING (10, 655), "EXP  :" + STR$(Hero.EXP_Earned) + " (" + _TRIM$(STR$(Hero.EXP_Needed)) + ")"
    FOR i = 0 TO 5 'six boxes for information : left hand, right hand, armor, and 3 more for later....
        Box 36 * i + 8, 665, 34, 34, 0, 0, "", Black, Silver
    NEXT
    _PUTIMAGE (9, 666)-STEP(32, 32), Icons, DisplayScreen, (Hero.Weapon1.IconX, Hero.Weapon1.IconY)-STEP(32, 32)
    _PUTIMAGE (45, 666)-STEP(32, 32), Icons, DisplayScreen, (Hero.Weapon2.IconX, Hero.Weapon2.IconY)-STEP(32, 32)
    _PUTIMAGE (81, 666)-STEP(32, 32), Icons, DisplayScreen, (Hero.Armor.IconX, Hero.Armor.IconY)-STEP(32, 32)
END SUB

SUB ManageInventory
    STATIC Header AS LONG, MainFont AS LONG, ItemNameFont AS LONG
    STATIC Selection AS INTEGER, Page AS INTEGER, Item AS INTEGER
    PCOPY 0, 1
    D = _DEST: S = _SOURCE
    F = _FONT
    IF Header = 0 THEN Header = _LOADFONT("courbd.ttf", 24, "monospace")
    IF MainFont = 0 THEN MainFont = _LOADFONT("courbd.ttf", 14, "monospace")
    IF ItemNameFont = 0 THEN ItemNameFont = _LOADFONT("courbd.ttf", 18, "monospace")
    IF Selection = 0 THEN Selection = 1 'there's 4 categories which we can choose from (Weapon, Armor, Food, Item)
    'Page = 0 'there's 4 pages of info for each category, from 0 to 3
    'Item = 0 'and there's 26 items on each page, from 0 to 25.
    DO
        _LIMIT 30
        valid = -1
        LINE (50, 50)-STEP(700, 500), SkyBlue, BF 'erase the background
        LINE (300, 106)-(725, 450), LightGray, BF
        _FONT Header
        SELECT CASE Selection 'redraw the selection we're working with
            CASE 1
                Box 51, 51, 174, 50, Black, 0, "Weapons", SkyBlue, SkyBlue
                Box 225, 51, 174, 50, Black, 0, "Armors", LightGray, Black
                Box 400, 51, 174, 50, Black, 0, "Food", LightGray, Black
                Box 575, 51, 174, 50, Black, 0, "Items", LightGray, Black
                _FONT MainFont
                FOR i = 26 * Page TO 26 * Page + 25
                    out$ = CHR$(65 + i MOD 26) + ")" + Inventory(i).Weapon.Name
                    _PRINTSTRING (56, 106 + _FONTHEIGHT * (i MOD 26)), out$
                NEXT
                S = Item MOD 26
                Box 54 + _FONTWIDTH * 2, 106 + _FONTHEIGHT * Item, _FONTWIDTH * 20 + 4, _FONTHEIGHT, 0, 0, "", LightGray, Black
                out$ = CHR$(65 + S) + ")" + Inventory(Item).Weapon.Name
                _PRINTSTRING (56, 106 + _FONTHEIGHT * S), out$

                COLOR Black, 0
                _FONT ItemNameFont
                IF Inventory(Item).Weapon.Identified THEN
                    Box 300, 110, 375, 28, Black, 0, _TRIM$(Inventory(Item).Weapon.Name), 0, 0
                    _FONT 8
                    _PRINTSTRING (305, 145), "Reach  :" + STR$(Inventory(Item).Weapon.Reach)
                    _PRINTSTRING (305, 165), "Min Dmg:" + STR$(Inventory(Item).Weapon.Damage.Low)
                    _PRINTSTRING (305, 175), "Max Dmg:" + STR$(Inventory(Item).Weapon.Damage.High)
                    _PRINTSTRING (305, 185), "Hit Mod:" + STR$(Inventory(Item).Weapon.HitBonus)
                    _PRINTSTRING (305, 195), "Dmg Mod:" + STR$(Inventory(Item).Weapon.DamageBonus)
                    _PRINTSTRING (305, 205), "Light  :" + STR$(Inventory(Item).Weapon.Light.Reach)
                ELSE
                    Box 350, 110, 375, 28, Black, 0, _TRIM$(Inventory(Item).Weapon.DisplayedName), 0, 0
                    _FONT 8
                    _PRINTSTRING (305, 145), "Reach  : ???"
                    _PRINTSTRING (305, 165), "Min Dmg: ???"
                    _PRINTSTRING (305, 175), "Max Dmg: ???"
                    _PRINTSTRING (305, 185), "Hit Mod: ???"
                    _PRINTSTRING (305, 195), "Dmg Mod: ???"
                    _PRINTSTRING (305, 205), "Light  : ???"
                END IF
                out$ = "Cond   : "
                SELECT CASE Inventory(Item).Weapon.Left 'the durability left on a weapon
                    CASE -1: out$ = out$ + "Ind."
                    CASE 0: out$ = out$ + "Broken"
                    CASE IS < 26: out$ = out$ + "Worn"
                    CASE IS < 50: out$ = out$ + "Used"
                    CASE IS < 75: out$ = out$ + "Good"
                    CASE IS < 90: out$ = out$ + "Mint"
                    CASE ELSE: out$ = out$ + "New"
                END SELECT
                _PRINTSTRING (305, 155), out$
                LINE (422, 134)-STEP(130, 130), Black, BF
                _PUTIMAGE (423, 135)-STEP(128, 128), Icons, DisplayScreen, (Inventory(Item).Weapon.IconX, Inventory(Item).Weapon.IconY)-STEP(31, 31)
            CASE 2
                Box 225, 51, 174, 50, Black, 0, "Armors", SkyBlue, SkyBlue
                Box 51, 51, 174, 50, Black, 0, "Weapons", LightGray, Black
                Box 400, 51, 174, 50, Black, 0, "Food", LightGray, Black
                Box 575, 51, 174, 50, Black, 0, "Items", LightGray, Black
                _FONT MainFont
                FOR i = 0 TO 25
                    out$ = CHR$(65 + i) + ")" + Inventory(i).Armor.Name
                    _PRINTSTRING (56, 106 + _FONTHEIGHT * i), out$
                NEXT
                Box 54 + _FONTWIDTH * 2, 106 + _FONTHEIGHT * Item, _FONTWIDTH * 20 + 4, _FONTHEIGHT, 0, 0, "", LightGray, Black
                out$ = CHR$(65 + Item MOD 26) + ")" + Inventory(Item).Armor.Name
                _PRINTSTRING (56, 106 + _FONTHEIGHT * (Item MOD 26)), out$

                COLOR Black, 0
                _FONT ItemNameFont
                IF Inventory(Item).Armor.Identified THEN
                    Box 300, 110, 375, 28, Black, 0, _TRIM$(Inventory(Item).Armor.Name), 0, 0
                    _FONT 8
                    _PRINTSTRING (305, 145), "PD   :" + STR$(Inventory(Item).Armor.PD)
                    _PRINTSTRING (305, 155), "DR   :" + STR$(Inventory(Item).Armor.DR)
                ELSE
                    Box 350, 110, 375, 28, Black, 0, _TRIM$(Inventory(Item).Armor.DisplayedName), 0, 0
                    _FONT 8
                    _PRINTSTRING (305, 145), "PD   : ???"
                    _PRINTSTRING (305, 155), "DR   : ???"
                END IF
                out$ = "Cond : "
                SELECT CASE Inventory(Item).Armor.Left 'the durability left on armor
                    CASE -1: out$ = out$ + "Ind."
                    CASE 0: out$ = out$ + "Broken"
                    CASE IS < 26: out$ = out$ + "Worn"
                    CASE IS < 50: out$ = out$ + "Used"
                    CASE IS < 75: out$ = out$ + "Good"
                    CASE IS < 90: out$ = out$ + "Mint"
                    CASE ELSE: out$ = out$ + "New"
                END SELECT
                _PRINTSTRING (305, 175), out$
                LINE (422, 134)-STEP(130, 130), Black, BF
                _PUTIMAGE (423, 135)-STEP(128, 128), Icons, DisplayScreen, (Inventory(Item).Armor.IconX, Inventory(Item).Armor.IconY)-STEP(31, 31)
            CASE 3
                Box 400, 51, 174, 50, Black, 0, "Food", SkyBlue, SkyBlue
                Box 51, 51, 174, 50, Black, 0, "Weapons", LightGray, Black
                Box 225, 51, 174, 50, Black, 0, "Armors", LightGray, Black
                Box 575, 51, 174, 50, Black, 0, "Items", LightGray, Black
                _FONT MainFont
                FOR i = 0 TO 25
                    out$ = CHR$(65 + i) + ")" + Inventory(i).Food.Name
                    _PRINTSTRING (56, 106 + _FONTHEIGHT * i), out$
                NEXT
                Box 54 + _FONTWIDTH * 2, 106 + _FONTHEIGHT * Item, _FONTWIDTH * 20 + 4, _FONTHEIGHT, 0, 0, "", LightGray, Black
                out$ = CHR$(65 + Item MOD 26) + ")" + Inventory(Item).Food.Name
                _PRINTSTRING (56, 106 + _FONTHEIGHT * (Item MOD 26)), out$

                COLOR Black, 0
                _FONT ItemNameFont
                IF Inventory(Item).Food.Identified THEN
                    Box 300, 110, 375, 28, Black, 0, _TRIM$(Inventory(Item).Food.Name), 0, 0
                ELSE
                    Box 350, 110, 375, 28, Black, 0, _TRIM$(Inventory(Item).Food.DisplayedName), 0, 0
                END IF
                LINE (422, 134)-STEP(130, 130), Black, BF
                _PUTIMAGE (423, 135)-STEP(128, 128), Icons, DisplayScreen, (Inventory(Item).Food.IconX, Inventory(Item).Food.IconY)-STEP(31, 31)
            CASE 4
                Box 575, 51, 174, 50, Black, 0, "Items", SkyBlue, SkyBlue
                Box 51, 51, 174, 50, Black, 0, "Weapons", LightGray, Black
                Box 225, 51, 174, 50, Black, 0, "Armors", LightGray, Black
                Box 400, 51, 174, 50, Black, 0, "Food", LightGray, Black
                _FONT MainFont
                FOR i = 0 TO 25
                    out$ = CHR$(65 + i) + ")" '+ Inventory(i).Item.Name 'not defined yet
                    _PRINTSTRING (56, 106 + _FONTHEIGHT * i), out$
                NEXT
                Box 54 + _FONTWIDTH * 2, 106 + _FONTHEIGHT * Item, _FONTWIDTH * 20 + 4, _FONTHEIGHT, 0, 0, "", LightGray, Black
                out$ = CHR$(65 + Item MOD 26) + ")" '+ Inventory(Item).Item.Name
                _PRINTSTRING (56, 106 + _FONTHEIGHT * (Item MOD 26)), out$
        END SELECT
        _FONT MainFont
        Box 55, 485, 95, 50, Black, 0, "Page" + STR$(Page + 1), 0, 0
        Box 170, 485, 95, 50, Black, 0, "(D)rop", LightGray, Black
        Box 285, 485, 95, 50, Black, 0, "(E)quip", LightGray, Black
        Box 410, 485, 95, 50, Black, 0, "(U)se", LightGray, Black
        Box 525, 485, 95, 50, Black, 0, "nothing", LightGray, Black
        Box 640, 485, 95, 50, Black, 0, "(C)lose", LightGray, Black
        k = _KEYHIT
        SELECT CASE k
            CASE 18432 'up
                Item = Item - 1
                IF Item < 0 THEN Item = 25
            CASE 19200 'left
                IF _KEYDOWN(100303) OR _KEYDOWN(100304) THEN 'shift arrow
                    Page = Page - 1
                    IF Page < 0 THEN Page = 3
                ELSE
                    Selection = Selection - 1
                    IF Selection < 1 THEN Selection = 4
                END IF
            CASE 20480 'down
                Item = Item + 1
                IF Item > 25 THEN Item = 0
            CASE 19712 'right
                IF _KEYDOWN(100303) OR _KEYDOWN(100304) THEN 'shift arrow
                    Page = Page + 1
                    IF Page > 3 THEN Page = 0
                ELSE
                    Selection = Selection + 1
                    IF Selection > 4 THEN Selection = 1
                END IF
            CASE ASC("C"), ASC("c"), 27
                EXIT DO
            CASE ELSE
                valid = 0
        END SELECT
        _DISPLAY
        PCOPY 1, 0
    LOOP

    _FONT F
    _DEST D: _SOURCE S
END SUB




SUB GetInput
    DO
        k = _KEYHIT: valid = -1
        SELECT CASE k
            CASE 18432 'up
                IF _KEYDOWN(100303) OR _KEYDOWN(100304) THEN 'shift arrow
                    SELECT CASE Scale
                        CASE 1.5: Scale = 2 'It's as small as we go
                        CASE 2: Scale = 3
                        CASE 3: Scale = 4
                        CASE 4: Scale = 6
                        CASE 6: Scale = 8
                        CASE 8: Scale = 12
                    END SELECT
                ELSE
                    IF Hero.Y > YL THEN MoveHero 0, -1 'if we can move up
                END IF
            CASE 19200: 'left
                IF _KEYDOWN(100303) OR _KEYDOWN(100304) THEN 'shift arrow
                    TextFont = TextFont - 1
                    IF TextFont < 8 THEN TextFont = 8
                    SetTextFont PrintArea, "courbd.ttf", TextFont
                    ClearTextArea PrintArea
                    SetPrintPosition PrintArea, 1, 1
                    PrintOut PrintArea, "Font Size Changed"
                ELSE
                    IF Hero.X > XL THEN MoveHero -1, 0 'if we can move left
                END IF
            CASE 20480: 'down
                IF _KEYDOWN(100303) OR _KEYDOWN(100304) THEN 'shift arrow
                    SELECT CASE Scale
                        CASE 2: Scale = 1.5 'It's as small as we go
                        CASE 3: Scale = 2
                        CASE 4: Scale = 3
                        CASE 6: Scale = 4
                        CASE 8: Scale = 6
                        CASE 12: Scale = 8
                    END SELECT
                ELSE
                    IF Hero.Y < YH THEN MoveHero 0, 1 'if we can move down
                END IF
            CASE 19712: 'right
                IF _KEYDOWN(100303) OR _KEYDOWN(100304) THEN 'shift arrow
                    TextFont = TextFont + 1
                    IF TextFont > 48 THEN TextFont = 48
                    SetTextFont PrintArea, "courbd.ttf", TextFont
                    ClearTextArea PrintArea
                    SetPrintPosition PrintArea, 1, 1
                    PrintOut PrintArea, "Font Size Changed"
                ELSE
                    IF Hero.X < XH THEN MoveHero 1, 0 'if we can move right
                END IF
            CASE 32 'space to just wait and skip a turn
            CASE 60 ' "<" key
                IF MapArray(Hero.X, Hero.Y) AND 16 THEN
                    Level = Level + 1
                    CreateMap 99, 74, 10
                    PathFind
                END IF
            CASE ASC("I"), ASC("i")
                ManageInventory
            CASE ASC("+"), ASC("=")
                IF Hero.Weapon2.Light.Reach < 25 THEN Hero.Weapon2.Light.Reach = Hero.Weapon2.Light.Reach + 1
            CASE ASC("-"), ASC("_")
                IF Hero.Weapon2.Light.Reach > -1 THEN Hero.Weapon2.Light.Reach = Hero.Weapon2.Light.Reach - 1
            CASE ELSE
                valid = 0 'it's a key press which we don't recognize.  Ignore it
        END SELECT
        _LIMIT 60
    LOOP UNTIL k AND valid
    _KEYCLEAR 'one keystroke at a time
    StepsTaken = StepsTaken + 1
END SUB

SUB Box (X, Y, Wide, High, FontColor as _unsigned long, _
         FontBackGround as _unsigned long, Caption AS STRING, Kolor AS _UNSIGNED LONG, BackGround AS _UNSIGNED LONG)
    DC = _DEFAULTCOLOR: BG = _BACKGROUNDCOLOR
    COLOR FontColor, FontBackGround
    LINE (X, Y)-STEP(Wide, High), Kolor, BF
    LINE (X, Y)-STEP(Wide, High), BackGround, B
    pw = _PRINTWIDTH(Caption): ph = _FONTHEIGHT
    _PRINTSTRING (X + (Wide - pw + 1) \ 2, Y + (High - ph + 1) \ 2), Caption
    COLOR DC, BG
END SUB


SUB MoveHero (MoveX, MoveY)
    TestX = Hero.X + MoveX: TestY = Hero.Y + MoveY
    IF MapArray(TestX, TestY) AND (4 OR 8) THEN 'and it's a room or passageway
        IF (MapArray(TestX, TestY) AND 32) = 0 THEN 'and it's not blocked for some reason
            MapArray(Hero.X, Hero.Y) = MapArray(Hero.X, Hero.Y) AND NOT 32 'unblock where the hero is
            IF MoveX THEN Hero.X = Hero.X + MoveX
            IF MoveY THEN Hero.Y = Hero.Y + MoveY
            MapArray(Hero.X, Hero.Y) = MapArray(Hero.X, Hero.Y) OR 32 'and block where the hero is now that he moved
            PathFind
        ELSE
            'chances are it's blocked by a monster.  Since we're one step away from it, let's see which monster it is and attack it!
            FOR i = 1 TO EncounterLimit
                IF Encounter(i).Active THEN 'Check for active/alive monsters only
                    MX = Encounter(i).X: MY = Encounter(i).Y 'monster x, monster y position
                    IF MX = TestX AND MY = TestY THEN 'yep, we found our monster!
                        Swing 0, i 'hero swings at the monster
                    END IF
                END IF
            NEXT
        END IF
    END IF
END SUB

SUB Swing (Who, AtWhom)

    BaseChancetohit = 10 'base 10 chance to hit
    IF Who = 0 THEN 'it's the hero attacking, add his attack bonuses
        M = Encounter(AtWhom).Index
        IF Hero.Weapon1.Reach >= Distance(Encounter(AtWhom).X, Encounter(AtWhom).Y) THEN 'it's a weapon and not an utility object being held.
            Chancetohit = BaseChancetohit + Hero.Weapon1.HitBonus 'add in the weapon's hit bonus
            Chancetohit = Chancetohit - Monster(AtWhom).Armor.PD 'subtract the monster's armor/ natural dodge
            totalroll = 0
            DO
                roll = INT(RND * 20) + 1
                IF roll = 1 THEN totalroll = totalroll - 20 'critical failure
                IF roll = 20 THEN totalroll = totalroll + 20
                totalroll = totalroll + roll
            LOOP UNTIL roll <> 1 AND roll <> 20
            damage = INT(RND * (Hero.Weapon1.Damage.High - Hero.Weapon1.Damage.Low + 1)) + Hero.Weapon1.Damage.Low 'random damage for the hit
            damage = damage + Hero.Weapon1.DamageBonus 'add in the weapon's damage bonus
            out$ = _TRIM$(Hero.Name)
            IF totalroll < Chancetohit - 20 THEN 'you critically failed!
                SetTextColor PrintArea, &HFFF000F0, 0
                out$ = out$ + " CRITICALLY FAILED attacking.  They hit themselves for"
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Hero.Weapon1.Name) + "!"
                Hero.Life.Low = Hero.Life.Low - damage
            ELSEIF totalroll < Chancetohit THEN
                SetTextColor PrintArea, &HFFF0F000, 0
                out$ = out$ + " missed " + _TRIM$(Monster(M).Name) + ", with " + _TRIM$(Hero.Weapon1.Name) + "!"
            ELSEIF totalroll > Chancetohit + 20 THEN
                SetTextColor PrintArea, &HFF00FF00, 0
                out$ = out$ + " CRITICALLY hit " + _TRIM$(Monster(M).Name) + " for"
                damage = damage * (totalroll / 20 + 1)
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Hero.Weapon1.Name) + "!"
                Encounter(AtWhom).Life = Encounter(AtWhom).Life - damage
            ELSEIF totalroll >= Chancetohit THEN
                SetTextColor PrintArea, &HFF00FF00, 0
                out$ = out$ + " hit " + _TRIM$(Monster(M).Name) + " for"
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Hero.Weapon1.Name) + "."
                Encounter(AtWhom).Life = Encounter(AtWhom).Life - damage
            END IF
        ELSEIF Hero.Weapon1.Reach > 0 THEN
            SetTextColor PrintArea, &HFFF000F0, 0
            out$ = _TRIM$(Monster(M).Name) + " is too far away to attack with a " + _TRIM$(Hero.Weapon1.Name) + "!"
        ELSE
            out$ = ""
        END IF
        IF out$ <> "" THEN PrintOut PrintArea, out$
        IF Hero.Weapon2.Reach >= Distance(Encounter(AtWhom).X, Encounter(AtWhom).Y) THEN 'it's a weapon and not an utility object being held.
            Chancetohit = BaseChancetohit + Hero.Weapon2.HitBonus 'add in the weapon's hit bonus
            Chancetohit = Chancetohit - Monster(AtWhom).Armor.PD 'subtract the monster's armor/ natural dodge
            totalroll = 0
            DO
                roll = INT(RND * 20) + 1
                IF roll = 1 THEN totalroll = totalroll - 20 'critical failure
                IF roll = 20 THEN totalroll = totalroll + 20
                totalroll = totalroll + roll
            LOOP UNTIL roll <> 1 AND roll <> 20
            damage = INT(RND * (Hero.Weapon2.Damage.High - Hero.Weapon2.Damage.Low + 1)) + Hero.Weapon2.Damage.Low 'random damage for the hit
            damage = damage + Hero.Weapon2.DamageBonus 'add in the weapon's damage bonus
            out$ = _TRIM$(Hero.Name)
            IF totalroll < Chancetohit - 20 THEN 'you critically failed!
                SetTextColor PrintArea, &HFFF000F0, 0
                out$ = out$ + " CRITICALLY FAILED attacking.  They hit themselves for"
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Hero.Weapon2.Name) + "!"
                damage = damage - Hero.Armor.PD: IF damage < 0 THEN damage = 0 'armor absorbs some damage for us
                Hero.Life.Low = Hero.Life.Low - damage
            ELSEIF totalroll < Chancetohit THEN
                SetTextColor PrintArea, &HFFF0F000, 0
                out$ = out$ + " missed " + _TRIM$(Monster(M).Name) + ", with " + _TRIM$(Hero.Weapon1.Name) + "!"
            ELSEIF totalroll > Chancetohit + 20 THEN
                SetTextColor PrintArea, &HFF00FF00, 0
                out$ = out$ + " CRITICALLY hit " + _TRIM$(Monster(M).Name) + " for"
                damage = damage * (totalroll / 20 + 1)
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Hero.Weapon2.Name) + "!"
                damage = damage - Monster(M).Armor.PD: IF damage < 0 THEN damage = 0 'armor absorbs some damage for us"
                Encounter(AtWhom).Life = Encounter(AtWhom).Life - damage
            ELSEIF totalroll >= Chancetohit THEN
                SetTextColor PrintArea, &HFF00FF00, 0
                out$ = out$ + " hit " + _TRIM$(Monster(M).Name) + " for"
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Hero.Weapon2.Name) + "."
                damage = damage - Monster(M).Armor.PD: IF damage < 0 THEN damage = 0 'armor absorbs some damage for us"
                Encounter(AtWhom).Life = Encounter(AtWhom).Life - damage
            END IF
        ELSEIF Hero.Weapon2.Reach > 0 THEN
            SetTextColor PrintArea, &HFFF000F0, 0
            out$ = _TRIM$(Monster(M).Name) + " is too far away to attack with a " + _TRIM$(Hero.Weapon2.Name) + "!"
        ELSE
            out$ = ""
        END IF
        IF out$ <> "" THEN PrintOut PrintArea, out$
        IF Encounter(AtWhom).Life <= 0 THEN MonsterDied (AtWhom) 'the monster died!
    ELSE 'it's a monster attacking
        M = Encounter(Who).Index
        IF Monster(M).Weapon1.Reach >= Distance(Encounter(Who).X, Encounter(Who).Y) THEN 'it's a weapon and not an utility object being held.
            Chancetohit = BaseChancetohit + Monster(M).Weapon1.HitBonus 'add in the weapon's hit bonus
            Chancetohit = Chancetohit - Hero.Armor.PD 'subtract the hero's armor/ natural dodge
            totalroll = 0
            DO
                roll = INT(RND * 20) + 1
                IF roll = 1 THEN totalroll = totalroll - 20 'critical failure
                IF roll = 20 THEN totalroll = totalroll + 20
                totalroll = totalroll + roll
            LOOP UNTIL roll <> 1 AND roll <> 20
            damage = INT(RND * (Monster(M).Weapon1.Damage.High - Monster(M).Weapon1.Damage.Low + 1)) + Monster(M).Weapon1.Damage.Low 'random damage for the hit
            damage = damage + Monster(M).Weapon1.DamageBonus 'add in the weapon's damage bonus
            out$ = _TRIM$(Monster(M).Name)
            IF totalroll < Chancetohit - 20 THEN 'you critically failed!
                SetTextColor PrintArea, &HFFF000F0, 0
                out$ = out$ + " CRITICALLY FAILED attacking.  They hit themselves for"
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Monster(M).Weapon1.Name) + "!"
                Monster(M).Life.Low = Monster(M).Life.Low - damage
            ELSEIF totalroll < Chancetohit THEN
                SetTextColor PrintArea, &HFFF0F000, 0
                out$ = out$ + " missed " + _TRIM$(Hero.Name) + ", with " + _TRIM$(Monster(M).Weapon1.Name) + "!"
            ELSEIF totalroll > Chancetohit + 20 THEN
                SetTextColor PrintArea, &HFF00FFFF, 0
                out$ = out$ + " CRITICALLY hit " + _TRIM$(Hero.Name) + " for"
                damage = damage * (totalroll / 20 + 1)
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Monster(M).Weapon1.Name) + "!"
                Hero.Life.Low = Hero.Life.Low - damage
            ELSEIF totalroll >= Chancetohit THEN
                SetTextColor PrintArea, &HFF00FFFF, 0
                out$ = out$ + " hit " + _TRIM$(Hero.Name) + " for"
                out$ = out$ + STR$(damage) + " damage, with " + _TRIM$(Monster(M).Weapon1.Name) + "."
                Hero.Life.Low = Hero.Life.Low - damage
            END IF
        ELSEIF Monster(M).Weapon1.Reach > 0 THEN
            SetTextColor PrintArea, &HFFF000F0, 0
            out$ = _TRIM$(Monster(M).Name) + " is too far away to attack with a " + _TRIM$(Monster(M).Weapon2.Name) + "!"
        ELSE
            out$ = ""
        END IF
        IF out$ <> "" THEN PrintOut PrintArea, out$
    END IF
END SUB

SUB MonsterDied (Who)
    M = Encounter(Who).Index
    SetTextColor PrintArea, &HFFFF0000, 0
    out$ = _TRIM$(Monster(M).Name) + " died!  You earned " + _TRIM$(STR$(Monster(M).Level + Monster(M).ExpBonus)) + " experience."
    PrintOut PrintArea, out$
    Encounter(Who).Active = 0
    Hero.EXP_Earned = Hero.EXP_Earned + Monster(M).Level + Monster(M).ExpBonus
    MapArray(Encounter(Who).X, Encounter(Who).Y) = MapArray(Encounter(Who).X, Encounter(Who).Y) AND NOT 32 'the way is no longer blocked once we kill the monster!
    IF Monster(M).Found = 0 THEN
        Monster(M).Found = -1 'it's a first time kill!
        SetTextColor PrintArea, &HFFFFFF00, &HFFFF0000
        out$ = "Congratulations!  You killed a " + _TRIM$(Monster(M).Name) + " for the first time!"
        PrintOut PrintArea, out$
    END IF

    'monster loot!!
    IF UBOUND(Encounter) <= EncounterLimit THEN REDIM _PRESERVE Encounter(EncounterLimit + 100) AS Encounter_TYPE 'make certain our array is large enough to hold all the loot on the map.
    'if the player starts dumping items onto the ground, we could concievably fill the screen with tons of loots.
    R# = RND * 100
    IF R# < Monster(M).Loot1.Chance THEN
        GOSUB addloot
        Encounter(E).Type = Monster(M).Loot1.Type
        Encounter(E).Index = Monster(M).Loot1.Index
    ELSEIF R# < Monster(M).Loot2.Chance THEN
        GOSUB addloot
    ELSEIF R# < Monster(M).loot3.Chance THEN
        GOSUB addloot
    END IF

    EXIT SUB

    addloot: 'a small sub proceedure to reduce copy/paste code
    EncounterLimit = EncounterLimit + 1
    E = EncounterLimit 'just for ease of typing below
    Encounter(E).Active = -1
    Encounter(E).X = Encounter(Who).X
    Encounter(E).Y = Encounter(Who).Y
    PrintOut PrintArea, _TRIM$(Monster(M).Name) + " left something behind."
    RETURN

END SUB

FUNCTION MoveMonster (Monster, MoveX, MoveY)
    MX = Encounter(Monster).X: MY = Encounter(Monster).Y 'monster x, monster y position
    D = Distance(MX, MY) 'distance from monster to the hero
    E = Encounter(i).Index 'the actual monster in question
    IF D > Distance(MX + MoveX, MY + MoveY) THEN
        IF (MapArray(MX + MoveX, MY + MoveY) AND 32) = 0 THEN 'where we're trying to move isn't blocked
            MapArray(MX, MY) = MapArray(MX, MY) AND NOT 32 'unblock where the monster is
            Encounter(Monster).X = Encounter(Monster).X + MoveX
            Encounter(Monster).Y = Encounter(Monster).Y + MoveY
            MapArray(MX + MoveX, MY + MoveY) = MapArray(MX + MoveX, MY + MoveY) OR 32 'block where the monster moved to
            MoveMonster = -1
        END IF
    END IF
END SUB



SUB MonstersTurn
    FOR i = 1 TO EncounterLimit
        IF Encounter(i).Active AND (Encounter(i).Type = 0) THEN 'Only if it's a monster, and the monster is still alive and active do we need to actually do anything else.
            MX = Encounter(i).X: MY = Encounter(i).Y 'monster x, monster y position
            D = Distance(MX, MY) 'distance from monster to the hero
            E = Encounter(i).Index 'the actual monster in question
            IF D < Monster(E).Sight OR D <= Monster(E).Hearing OR D <= Monster(E).Detection THEN
                attack = 0
                IF D <= Monster(E).Weapon1.Reach THEN 'we're in reach for the monster to attack with their main hand.
                    'insert attack code here
                    Swing i, 0
                    _CONTINUE
                END IF
                'if the monster didn't attack, it can now move towards the hero.
                IF MX > 0 THEN 'check to see if moving left moves us towards the hero.
                    IF D > Distance(MX - 1, MY) THEN
                        IF MoveMonster(i, -1, 0) THEN _CONTINUE 'move left
                    END IF
                END IF
                IF MY > 0 THEN 'check to see if moving up moves us towards the hero.
                    IF D > Distance(MX, MY - 1) THEN
                        IF MoveMonster(i, 0, -1) THEN _CONTINUE 'move up
                    END IF
                END IF
                IF MX < XH THEN 'check to see if moving right moves us towards the hero.
                    IF D > Distance(MX + 1, MY) THEN
                        IF MoveMonster(i, 1, 0) THEN _CONTINUE 'move right
                    END IF
                END IF
                IF MY < YH THEN 'check to see if moving down moves us towards the hero.
                    IF D > Distance(MX, MY + 1) THEN
                        IF MoveMonster(i, 0, 1) THEN _CONTINUE 'move down
                    END IF
                END IF
            END IF
        END IF
    NEXT
END SUB



SUB DrawMap
    _DEST WorkScreen
    CLS
    'LINE (0, 0)-(3200, 2400), &HFF000000, BF 'clear the map
    IF Hero.Weapon1.Light.Reach > Hero.Weapon2.Light.Reach THEN LightReach = Hero.Weapon1.Light.Reach ELSE LightReach = Hero.Weapon2.Light.Reach
    FOR Y = 0 TO YH
        FOR X = 0 TO XH
            IF Distance(X, Y) <= LightReach THEN 'It's close enough to check for illumination
                IF MapArray(X, Y) <> 0 THEN MapArray(X, Y) = MapArray(X, Y) OR 1 OR 2
            END IF
            IF MapArray(X, Y) AND 2 THEN 'It's an uncovered part of the map, draw it
                IF MapArray(X, Y) AND 4 THEN 'it's a visible room
                    '                    LINE (X * 32, Y * 32)-STEP(32, 32), &HFF303030, BF
                    _PUTIMAGE (X * 32, Y * 32)-STEP(32, 32), Icons, WorkScreen, (6 * 32, 18 * 32)-STEP(31, 31)
                END IF
                IF MapArray(X, Y) AND 8 THEN 'it's a visible path
                    _PUTIMAGE (X * 32, Y * 32)-STEP(32, 32), Icons, WorkScreen, (36 * 32, 13 * 32)-STEP(31, 31)
                    '                    LINE (X * 32, Y * 32)-STEP(32, 32), &HFF707070, BF
                END IF
                IF MapArray(X, Y) AND 16 THEN 'it's the stairs to the next level
                    _PUTIMAGE (X * 32, Y * 32)-STEP(32, 32), Icons, WorkScreen, (4 * 32, 45 * 32)-STEP(31, 31)
                END IF
            END IF
            'note: highlighting for the light should come AFTER the map is drawn
            IF MapArray(X, Y) AND 1 THEN 'it's currently illuminated by the lightsource
                LINE (X * 32, Y * 32)-STEP(32, 32), &H40FFFF00, BF
                MapArray(X, Y) = MapArray(X, Y) - 1
                FOR I = 1 TO EncounterLimit
                    IF X = Encounter(I).X AND Y = Encounter(I).Y AND Encounter(I).Active = -1 THEN
                        E = Encounter(I).Index
                        T = Encounter(I).Type
                        SELECT CASE T
                            CASE 0 'it's a monster
                                IF Monster(E).Found THEN
                                    _PUTIMAGE (X * 32, Y * 32)-STEP(32, 32), Icons, WorkScreen, (Monster(E).IconX, Monster(E).IconY)-STEP(31, 31)
                                ELSE
                                    _PUTIMAGE (X * 32, Y * 32)-STEP(32, 32), Icons, WorkScreen, (1984, 1504)-STEP(31, 31)
                                END IF
                            CASE 1 'weapon
                                _PUTIMAGE (X * 32, Y * 32)-STEP(32, 32), Icons, WorkScreen, (Weapon(E).IconX, Weapon(E).IconY)-STEP(31, 31)
                            CASE 2 'armor
                                _PUTIMAGE (X * 32, Y * 32)-STEP(32, 32), Icons, WorkScreen, (Armor(E).IconX, Armor(E).IconY)-STEP(31, 31)
                            CASE 3 'food
                                _PUTIMAGE (X * 32, Y * 32)-STEP(32, 32), Icons, WorkScreen, (Food(E).IconX, Food(E).IconY)-STEP(31, 31)
                            CASE 4 'item
                        END SELECT
                    END IF
                NEXT

            END IF
        NEXT
    NEXT
    COLOR &HFFFFFF00, 0 'Yellow Hero
    _PUTIMAGE (Hero.X * 32, Hero.Y * 32)-STEP(32, 32), Icons, WorkScreen, (2016, 1504)-STEP(31, 31)
    XOffset## = 1600 / Scale
    YOffset## = 1200 / Scale
    CenterX = Hero.X * 32 'convert hero coordinate to grid coordinate
    CenterY = Hero.Y * 32
    _DEST DisplayScreen
    LINE (0, 0)-(800, 600), &HFF000000, BF 'clear the map
    _PUTIMAGE (0, 0)-(800, 600), WorkScreen, DisplayScreen, (CenterX - XOffset##, CenterY - YOffset##)-(CenterX + XOffset##, CenterY + YOffset##)
END SUB





SUB CreateMap (XLimit, YLimit, Rooms)
    ERASE MapArray 'clear the old map and reset everything to 0
    REDIM MapArray(XLimit, YLimit) AS _UNSIGNED _BYTE
    REDIM Distance(XLimit, YLimit) AS _UNSIGNED _BYTE
    REDIM Temp(XLimit, YLimit) AS _UNSIGNED _BYTE
    XL = 0: XH = XLimit: YL = 0: YH = YLimit 'global values to pass along our map ultimate dimensions

    DIM RoomCenterX(Rooms) AS _UNSIGNED _BYTE, RoomCenterY(Rooms) AS _UNSIGNED _BYTE

    StairRoom = INT(RND * Rooms) + 1
    FOR i = 1 TO Rooms
        DO
            RoomSize = INT(RND * 9) + 2
            RoomX = INT(RND * (XLimit - RoomSize))
            RoomY = INT(RND * (YLimit - RoomSize))
            'test for positioning
            good = -1 'it's good starting out
            FOR Y = 0 TO RoomSize: FOR X = 0 TO RoomSize
                    IF MapArray(RoomX + X, RoomY + Y) = 4 THEN good = 0: EXIT FOR 'don't draw a room on a room
            NEXT X, Y
        LOOP UNTIL good
        FOR Y = 0 TO RoomSize: FOR X = 0 TO RoomSize
                MapArray(RoomX + X, RoomY + Y) = 4 'go ahead and draw a room
        NEXT X, Y
        RoomCenterX(i) = RoomX + .5 * RoomSize
        RoomCenterY(i) = RoomY + .5 * RoomSize
        IF i = 1 THEN 'place the hero in the first room  (which can be anywhere randomly on our map)
            Hero.X = RoomX + INT(RND * RoomSize)
            Hero.Y = RoomY + INT(RND * RoomSize)
            MapArray(Hero.X, Hero.Y) = MapArray(Hero.X, Hero.Y) OR 32 'block the map where the hero stands
        END IF
        IF i = StairRoom THEN 'place the stairs in one of the random rooms
            DO 'But lets not place the stairs directly on top of the hero to begin with
                StairX = RoomX + INT(RND * RoomSize)
                StairY = RoomY + INT(RND * RoomSize)
            LOOP UNTIL StairX <> Hero.X AND StairY <> Hero.Y
            MapArray(StairX, StairY) = MapArray(StairX, StairY) OR 16
        END IF
    NEXT
    FOR i = 1 TO Rooms - 1
        StartX = RoomCenterX(i): StartY = RoomCenterY(i)
        EndX = RoomCenterX(i + 1): EndY = RoomCenterY(i + 1)
        DO UNTIL StartX = EndX AND StartY = EndY
            CoinToss = INT(RND * 100) 'Coin toss to move left/right or up/down, to go towards room, or wander a bit.
            Meander = 10
            IF CoinToss MOD 2 THEN 'even or odd, so we only walk vertical or hortizontal and not diagional
                IF CoinToss < 100 - Meander THEN 'Lower values meander less and go directly to the target.
                    XChange = SGN(EndX - StartX) '-1,0,1, drawn always towards the mouse
                    Ychange = 0
                ELSE
                    XChange = INT(RND * 3) - 1 '-1, 0, or 1, drawn in a random direction to let the lightning wander
                    Ychange = 0
                END IF
            ELSE
                IF CoinToss < 100 - Meander THEN 'Lower values meander less and go directly to the target.
                    Ychange = SGN(EndY - StartY)
                    XChange = 0
                ELSE
                    Ychange = INT(RND * 3) - 1
                    XChange = 0
                END IF
            END IF
            StartX = StartX + XChange
            StartY = StartY + Ychange
            IF StartX < 0 THEN StartX = 0 'Make certain we move inside the bounds of our map dimensions
            IF StartY < 0 THEN StartY = 0
            IF StartX > XH THEN StartX = XH
            IF StartY > YH THEN StartY = YH
            IF MapArray(StartX, StartY) = 0 THEN MapArray(StartX, StartY) = 8 'place a path where we moved to
        LOOP
    NEXT
    PathFind
    EncounterLimit = INT(RND * 6) + 5
    FOR i = 1 TO EncounterLimit
        Encounter(i).Type = 0 'type 0 is a monster
        Encounter(i).Index = RandomMonster
        Encounter(i).Active = -1
        M = Encounter(i).Index
        Encounter(i).Life = INT(RND * Monster(M).Life.High - Monster(M).Life.Low + 1) + Monster(M).Life.Low
        valid = -1: EndlessLoopExit = 0
        DO
            EndlessLoopExit = EndlessLoopExit + 1
            Encounter(i).X = INT(RND * XLimit + 1)
            Encounter(i).Y = INT(RND * YLimit + 1)
            IF MapArray(Encounter(i).X, Encounter(i).Y) AND 32 THEN valid = 0 'the spot where we're wanting to place our monster is invalid.  (Another monster or the hero is probably there.)
            IF EndlessLoopExit = 1000 THEN EXIT DO 'if we can't place the monster in a room after 1000 tries, just place it wherever and call it a "wandering monster".
            'Of course, "wandering monsters" may end up inside a wall, in which case they simply become "lost monsters" and do nothing to affect the level.  It's the same as if they never existed at all.
            'BUT, we *should* generally be able to place a monster after 1000 tries.  This segment is just in the off-chance that the Random Number Gods are out to get us and to prevent any chance for an endless loop.
        LOOP UNTIL MapArray(Encounter(i).X, Encounter(i).Y) AND 4 AND valid 'monsters only spawn in rooms to begin with.
        MapArray(Encounter(i).X, Encounter(i).Y) = MapArray(Encounter(i).X, Encounter(i).Y) OR 32
    NEXT
    LootLimit = 0 'no loot on the map at this time.  Too bad for joo!
END SUB

SUB PathFind
    $CHECKING:OFF
    STATIC m AS _MEM, m1 AS _MEM 'no need to keep initializing and freeing these blocks over and over.  Just reuse them...
    DIM pass AS _UNSIGNED _BYTE
    m = _MEM(Distance()): m1 = _MEM(Temp())
    _MEMFILL m1, m1.OFFSET, m1.SIZE, 255 AS _UNSIGNED _BYTE 'flush distance with 255 values until we see how far things actually are from the hero
    _MEMFILL m, m.OFFSET, m.SIZE, 255 AS _UNSIGNED _BYTE
    Temp(Hero.X, Hero.Y) = 0
    pass = 0
    DO
        changed = 0
        y = 0
        DO
            x = 0
            DO
                IF Distance(x, y) = 255 AND MapArray(x, y) <> 0 THEN
                    IF x < XH THEN
                        IF Temp(x + 1, y) = pass THEN Distance(x, y) = pass + 1: changed = -1
                    END IF
                    IF x > 0 THEN
                        IF Temp(x - 1, y) = pass THEN Distance(x, y) = pass + 1: changed = -1
                    END IF
                    IF y < YH THEN
                        IF Temp(x, y + 1) = pass THEN Distance(x, y) = pass + 1: changed = -1
                    END IF
                    IF y > 0 THEN
                        IF Temp(x, y - 1) = pass THEN Distance(x, y) = pass + 1: changed = -1
                    END IF
                END IF
                x = x + 1
            LOOP UNTIL x > XH
            y = y + 1
        LOOP UNTIL y > YH
        _MEMCOPY m, m.OFFSET, m.SIZE TO m1, m1.OFFSET
        pass = pass + 1
    LOOP UNTIL changed = 0 OR pass = 255 'if we're more than 255 steps from the hero, we don't need to know where the hell we're at.  We're off the map as far as the hero is concerned!
    Distance(Hero.X, Hero.Y) = 0
    $CHECKING:ON
END SUB

FUNCTION RandomMonster
    SELECT CASE Level 'the level we're on
        CASE 1 TO 3: MC = 3 'the monster count which we can randomly run into and battle from on the current floor
        CASE ELSE: MC = 3 'since there's only 3 whole monsters in our monster database at the moment, don't expect to find a ton of them to choose from yet!
    END SELECT
    RandomMonster = INT(RND * MC) + 1
END SUB


' ----------------------------------------------------------------------------------------------------------------------------------------------------------- '
'# SUBroutines and FUNCTIONs below #'
' ----------------------------------------------------------------------------------------------------------------------------------------------------------- '
SUB PrintOut (WhichHandle AS INTEGER, What AS STRING)
    u = UBOUND(TextHandles)
    Handle = WhichHandle
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    Where = TextHandles(Handle).VerticalAlignment
    How = TextHandles(Handle).Justification
    UpdatePrintPosition = TextHandles(Handle).UpdateMethod
    PlaceText Handle, Where, How, What, UpdatePrintPosition
END SUB


SUB PlaceText (WhichHandle AS INTEGER, Where AS INTEGER, How AS INTEGER, What AS STRING, UpdatePrintPosition AS INTEGER)
    'WhichHandle is the handle which designates which text area we want to use
    'Where is where we want it to go in that text area
    '  -- Online prints the text to the current print position line in that text area.
    '  -- CenterLine centers the text to the center of that text area.
    '  -- any other value will print to that line positon in that particular box.
    'How tells us how we want to place that text (LeftJustified, RightJustified,CenterJustified, or NoJustify)
    'What is the text that we want to print in our text area
    'UpdatePrintPosition lets us know if we need to move to a newline or stay on the same line.  (Think PRINT with a semicolon vs PRINT without a semicolon).

    DIM FG AS _UNSIGNED LONG, BG AS _UNSIGNED LONG
    FG = _DEFAULTCOLOR: BG = _BACKGROUNDCOLOR
    D = _DEST: S = _SOURCE
    OldFont = _FONT

    u = UBOUND(TextHandles)
    Handle = WhichHandle

    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).HideFrame THEN
        _DEST TextHandles(Handle).SavedBackground
        _SOURCE TextHandles(Handle).SavedBackground
    END IF
    _FONT TextHandles(Handle).Font
    fh = _FONTHEIGHT: pw = _PRINTWIDTH(What)
    IF _FONTWIDTH = 0 THEN
        FOR i = 1 TO 255
            IF _PRINTWIDTH(CHR$(i)) > fw THEN fw = _PRINTWIDTH(CHR$(i))
        NEXT
    ELSE
        fw = _FONTWIDTH
    END IF

    h = TextHandles(Handle).h - 4: w = TextHandles(Handle).w - 4

    SELECT CASE Where
        CASE BottomLine
            y = h \ fh
        CASE OnLine
            y = TextHandles(Handle).Ypos
            IF y = 0 THEN y = 1
        CASE CenterLine
            linesused = 0
            tpw = pw: tw = w: tWhat$ = What
            DO UNTIL tpw <= tw
                textallowed = WordBreak(LEFT$(tWhat$, w \ fw))
                text$ = RTRIM$(LEFT$(tWhat$, textallowed))
                linesused = linesused + 1
                tWhat$ = MID$(tWhat$, textallowed + 1)
                tpw = _PRINTWIDTH(tWhat$)
            LOOP
            linesused = linesused + 1
            py = (h - linesused * fh) \ 2
            y = py \ fh + 1
            IF y < 1 THEN y = 1
        CASE ELSE
            y = Where
    END SELECT

    'IF y < 1 THEN ERROR 5: EXIT FUNCTION 'We don't print above the allocated text area.
    blend = _BLEND
    _DONTBLEND

    DO UNTIL y * fh < h 'We need to scroll the text area up, if someone is trying to print below it.
        'first let's get a temp image handle for the existing area of the screen.
        x1 = TextHandles(Handle).x1 + 2
        y1 = TextHandles(Handle).y1 + 2
        x2 = TextHandles(Handle).x1 + w
        y2 = TextHandles(Handle).y1 + h
        nh = y2 - y1 + 1 - fh
        nw = x2 - x1 + 1
        tempimage = _NEWIMAGE(nw, nh, 32) 'Really, I should swap this to a routine to pick which screen mode the user is in, but I'll come back to that later.
        _PUTIMAGE , , tempimage, (x1, y1 + fh)-(x2, y2)
        DrawTextArea Handle
        _PUTIMAGE (x1, y1)-(x2, y2 - fh), tempimage
        y = y - 1
    LOOP

    IF blend THEN _BLEND

    COLOR TextHandles(Handle).TextColor, TextHandles(Handle).TextBackgroundColor

    SELECT CASE How
        CASE LeftJustify
            x = 0
            IF pw > w THEN
                textallowed = WordBreak(LEFT$(What, w \ fw))
                text$ = RTRIM$(LEFT$(What, textallowed))
                _PRINTSTRING (x + 2 + TextHandles(Handle).x1, (y - 1) * fh + TextHandles(Handle).y1 + 2), text$
                PlaceText Handle, y + 1, LeftJustify, MID$(What, textallowed + 1), 0
            ELSE
                _PRINTSTRING (x + 2 + TextHandles(Handle).x1, (y - 1) * fh + TextHandles(Handle).y1 + 2), What
                finished = -1
            END IF
        CASE CenterJustify
            IF pw > w THEN
                textallowed = WordBreak(LEFT$(What, w \ fw))
                text$ = RTRIM$(LEFT$(What, textallowed))
                x = (w - _PRINTWIDTH(text$)) \ 2
                _PRINTSTRING (x + 2 + TextHandles(Handle).x1, (y - 1) * fh + TextHandles(Handle).y1 + 2), text$
                PlaceText Handle, y + 1, CenterJustify, MID$(What, textallowed + 1), NoUpdate
            ELSE
                x = (w - pw) \ 2
                _PRINTSTRING (x + 2 + TextHandles(Handle).x1, (y - 1) * fh + TextHandles(Handle).y1 + 2), What
                finished = -1
            END IF
        CASE RightJustify
            IF pw > w THEN
                textallowed = WordBreak(LEFT$(What, w \ fw))
                text$ = RTRIM$(LEFT$(What, textallowed))
                x = w - _PRINTWIDTH(text$)
                _PRINTSTRING (x + 2 + TextHandles(Handle).x1, (y - 1) * fh + TextHandles(Handle).y1 + 2), text$
                PlaceText Handle, y + 1, RightJustify, MID$(What, textallowed + 1), 0
            ELSE
                x = w - pw
                _PRINTSTRING (x + 2 + TextHandles(Handle).x1, (y - 1) * fh + TextHandles(Handle).y1 + 2), What
                finished = -1
            END IF
        CASE NoJustify
            x = TextHandles(Handle).Xpos
            firstlinelimit = (w - x) \ fw 'the limit of characters on the first line
            IF LEN(What) > firstlinelimit THEN
                textallowed = WordBreak(LEFT$(What, firstlinelimit))
                text$ = RTRIM$(LEFT$(What, textallowed))
                _PRINTSTRING (x + 2 + TextHandles(Handle).x1, (y - 1) * fh + TextHandles(Handle).y1 + 2), text$
                PlaceText Handle, y + 1, LeftJustify, MID$(What, textallowed + 1), 0 'After the first line we start printing over on the left, after a line break
            ELSE
                _PRINTSTRING (x + 2 + TextHandles(Handle).x1, (y - 1) * fh + TextHandles(Handle).y1 + 2), What
                finished = -1
            END IF
    END SELECT
    IF finished THEN
        SELECT CASE TextHandles(Handle).UpdateMethod
            CASE NoUpdate 'We don't update the position at all.
            CASE DoUpdate
                TextHandles(Handle).Xpos = x + pw
                TextHandles(Handle).Ypos = y
            CASE NewLine
                TextHandles(Handle).Ypos = y + 1
                TextHandles(Handle).Xpos = 1
        END SELECT
        _FONT OldFont
        _DEST D: _SOURCE S
        COLOR FG, BG
    END IF
END SUB

SUB SetTextForeground (Handle AS INTEGER, Foreground AS _UNSIGNED LONG)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    TextHandles(Handle).TextColor = Foreground
END SUB


SUB SetTextBackground (Handle AS INTEGER, Background AS _UNSIGNED LONG)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    TextHandles(Handle).TextBackgroundColor = Background
END SUB

SUB SetTextFont (Handle AS INTEGER, FontName AS STRING, FontSize AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    SELECT CASE TextHandles(Handle).Font
        CASE 8, 9, 14, 15, 16, 17 'In built QB64 fonts.  We don't need to free them.
        CASE IS > 1
            'we have the font already in use
            'REMOVE THIS CONDITION IF NECESSARY, AND MANUALLY FREE/RELEASE FONTS AS ABLE!!!
            _FREEFONT TextHandles(Handle).Font 'if it's in use elsewhere, this *WILL* toss an error.
    END SELECT

    temp = _LOADFONT(FontName, FontSize, "MONOSPACE")
    IF temp > 1 THEN
        TextHandles(Handle).Font = temp
    ELSE
        TextHandles(Handle).Font = 16 'default to font 16, in case
    END IF

END SUB

SUB SetTextColor (Handle AS INTEGER, Foreground AS _UNSIGNED LONG, Background AS _UNSIGNED LONG)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    TextHandles(Handle).TextColor = Foreground
    TextHandles(Handle).TextBackgroundColor = Background
END SUB


SUB SetPrintUpdate (Handle AS INTEGER, Method AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    IF Method < 0 OR Method > 2 THEN ERROR 5: EXIT FUNCTION
    TextHandles(Handle).UpdateMethod = Method
END SUB


SUB SetPrintPosition (Handle AS INTEGER, X AS INTEGER, Y AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    SELECT CASE Y
        CASE BottomLine
            TextHandles(Handle).VerticalAlignment = -2
        CASE CenterLine
            TextHandles(Handle).VerticalAlignment = -1
        CASE ELSE
            TextHandles(Handle).VerticalAlignment = 0
    END SELECT
    IF X < 1 AND X > -4 THEN
        TextHandles(Handle).Justification = X
    ELSE
        TextHandles(Handle).Xpos = X
    END IF
    IF Y < 1 THEN EXIT SUB
    TextHandles(Handle).Ypos = Y
END SUB

SUB SetPrintPositionX (Handle AS INTEGER, X AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    IF X < 1 AND X > -4 THEN
        TextHandles(Handle).Justification = X
    ELSE
        TextHandles(Handle).Xpos = X
    END IF
END SUB

SUB SetPrintPositionY (Handle AS INTEGER, Y AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    SELECT CASE Y
        CASE BottomLine
            TextHandles(Handle).VerticalAlignment = -2
        CASE CenterLine
            TextHandles(Handle).VerticalAlignment = -1
        CASE ELSE
            TextHandles(Handle).VerticalAlignment = 0
    END SELECT
    IF Y < 1 THEN EXIT SUB
    TextHandles(Handle).Ypos = Y
END SUB


FUNCTION GetPrintPositionY (Handle AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    GetPrintPositionY = TextHandles(Handle).Ypos
END FUNCTION

FUNCTION GetPrintPositionX (Handle AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    GetPrintPositionX = TextHandles(Handle).Xpos
END FUNCTION



FUNCTION WordBreak (text$)
    CONST Breaks = " ;,.?!-"
    FOR i = LEN(text$) TO 0 STEP -1
        IF INSTR(Breaks, MID$(text$, i, 1)) THEN EXIT FOR
        loopcount = loopcount + 1
    NEXT
    IF i = 0 THEN i = LEN(text$)
    WordBreak = i
END FUNCTION



SUB ClearTextArea (Handle AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).SavedBackground THEN
        w = TextHandles(Handle).w
        h = TextHandles(Handle).h
        x1 = TextHandles(Handle).ScreenX
        y1 = TextHandles(Handle).ScreenY
        x2 = x1 + w - 1
        y2 = y1 + h - 1
        blend = _BLEND
        _DONTBLEND
        _PUTIMAGE (x1, y1)-(x2, y2), TextHandles(Handle).SavedBackground
        IF blend THEN _BLEND
    END IF
    DrawTextArea Handle
END SUB



SUB DrawTextArea (Handle AS INTEGER)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    w = TextHandles(Handle).w
    h = TextHandles(Handle).h
    x1 = TextHandles(Handle).ScreenX
    y1 = TextHandles(Handle).ScreenY
    x2 = x1 + w - 1
    y2 = y1 + h - 1

    LINE (x1, y1)-(x2, y2), TextHandles(Handle).BackColor, BF
    LINE (x1, y1)-(x2, y2), TextHandles(Handle).FrameColor, B
END SUB



SUB ColorTextArea (Handle AS INTEGER, FrameColor AS _UNSIGNED LONG, BackColor AS _UNSIGNED LONG)
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    TextHandles(Handle).FrameColor = FrameColor
    TextHandles(Handle).BackColor = BackColor
END SUB



FUNCTION NewTextArea% (tx1 AS INTEGER, ty1 AS INTEGER, tx2 AS INTEGER, ty2 AS INTEGER, SaveBackground AS INTEGER)
    x1 = tx1: y1 = ty1 'We pass temp variables to the function so we can swap values if needed without altering user variables
    x2 = tx2: y2 = ty2
    IF x1 > x2 THEN SWAP x1, x2
    IF y1 > y2 THEN SWAP y1, y2
    w = x2 - x1 + 1
    h = y2 - y1 + 1
    IF w = 0 AND h = 0 THEN ERROR 5: EXIT FUNCTION 'Illegal Function Call if the user tries to define an area with no size
    'Error checking for if the user sends coordinates which are off the screen
    IF x1 < 0 OR x2 > _WIDTH - 1 THEN ERROR 5: EXIT FUNCTION
    IF y1 < 0 OR y2 > _HEIGHT - 1 THEN ERROR 5: EXIT FUNCTION

    u = UBOUND(TextHandles)
    FOR i = 1 TO u 'First let's check to see if we have an open handle from where one was freed earlier
        IF TextHandles(i).InUse = False THEN Handle = i: EXIT FOR
    NEXT
    IF Handle = 0 THEN 'We didn't have an open spot, so we need to add one to our list
        Handle = u + 1
        REDIM _PRESERVE TextHandles(Handle) AS TextArea
    END IF
    TextHandles(Handle).x1 = x1
    TextHandles(Handle).y1 = y1
    TextHandles(Handle).w = w: TextHandles(Handle).h = h
    TextHandles(Handle).InUse = True
    TextHandles(Handle).Xpos = 0
    TextHandles(Handle).Ypos = 1
    TextHandles(Handle).UpdateMethod = NewLine
    TextHandles(Handle).TextColor = _RGB32(255, 255, 255) 'White text
    TextHandles(Handle).TextBackgroundColor = _RGB32(0, 0, 0) 'Black background

    IF SaveBackground THEN
        imagehandle = _NEWIMAGE(w, h, 32)
        _PUTIMAGE , 0, imagehandle, (x1, y1)-(x2, y2)
        TextHandles(Handle).SavedBackground = imagehandle
    END IF
    TextHandles(Handle).ScreenX = x1
    TextHandles(Handle).ScreenY = y1
    TextHandles(Handle).Font = 16 'default to font 16
    NewTextArea% = Handle
END FUNCTION

SUB FreeTextArea (Handle AS INTEGER)
    IF Handle > 0 AND Handle <= UBOUND(TextHandles) THEN
        IF TextHandles(Handle).InUse THEN
            TextHandles(Handle).InUse = False
            IF TextHandles(Handle).SavedBackground THEN
                IF TextHandles(Handle).HideFrame = 0 THEN 'If the frame isn't hidden, then restore what's supposed to be beneath it
                    w = TextHandles(Handle).w
                    h = TextHandles(Handle).h
                    x1 = TextHandles(Handle).ScreenX
                    y1 = TextHandles(Handle).ScreenY
                    x2 = x1 + w - 1
                    y2 = y1 + h - 1
                    blend = _BLEND
                    _DONTBLEND
                    _PUTIMAGE (x1, y1)-(x2, y2), TextHandles(Handle).SavedBackground
                    IF blend THEN _BLEND
                END IF
                'Even if it is hidden though, if we're going to free that frame, we need to free the stored image held with it to reduce memory usage.
                _FREEIMAGE TextHandles(Handle).SavedBackground
            END IF
        ELSE
            ERROR 258 'Invalid handle if the user tries to free a handle which has already been freed.
        END IF
    ELSE
        ERROR 5 'Illegal function call if the user tries to free a handle that doesn't exist at all.
    END IF
END SUB

SUB HideFrame (Handle AS INTEGER)
    IF TextHandles(Handle).HideFrame = 0 THEN 'only if the frame isn't hidden, can we hide it.
        TextHandles(Handle).HideFrame = -1
        w = TextHandles(Handle).w
        h = TextHandles(Handle).h
        x1 = TextHandles(Handle).ScreenX
        y1 = TextHandles(Handle).ScreenY
        x2 = x1 + w - 1
        y2 = y1 + h - 1
        imagehandle = _NEWIMAGE(TextHandles(Handle).w, TextHandles(Handle).h, 32)
        _PUTIMAGE , 0, imagehandle, (x1, y1)-(x2, y2)
        IF TextHandles(Handle).SavedBackground THEN
            blend = _BLEND
            _DONTBLEND
            _PUTIMAGE (x1, y1)-(x2, y2), TextHandles(Handle).SavedBackground
            _FREEIMAGE TextHandles(Handle).SavedBackground
            IF blend THEN _BLEND
        END IF
        TextHandles(Handle).SavedBackground = imagehandle
        TextHandles(Handle).x1 = 0 'When the frames are hidden, we calculate our print position based off the hidden image
        TextHandles(Handle).y1 = 0 'So we'd start at point (0,0) as being top left
    END IF
END SUB

SUB RestoreFrame (Handle AS INTEGER)
    IF TextHandles(Handle).HideFrame THEN 'only if we have a hidden frame do we have to worry about restoring it
        TextHandles(Handle).HideFrame = 0
        w = TextHandles(Handle).w
        h = TextHandles(Handle).h
        x1 = TextHandles(Handle).ScreenX
        y1 = TextHandles(Handle).ScreenY
        x2 = x1 + w - 1
        y2 = y1 + h - 1
        imagehandle = _NEWIMAGE(TextHandles(Handle).w, TextHandles(Handle).h, 32)
        blend = _BLEND
        _DONTBLEND
        _PUTIMAGE , 0, imagehandle, (x1, y1)-(x2, y2)
        _PUTIMAGE (x1, y1)-(x2, y2), TextHandles(Handle).SavedBackground ', 0, (0, 0)-(w, h)
        _FREEIMAGE TextHandles(Handle).SavedBackground
        IF blend THEN _BLEND
        TextHandles(Handle).SavedBackground = imagehandle
        TextHandles(Handle).x1 = x1 'When the frames are restored, we need to recalculate our print position
        TextHandles(Handle).y1 = y1 'as we're no longer going over the image cooridinates, but the screen location of the top left corner instead.
    END IF
END SUB

SUB MoveFrame (Handle AS INTEGER, x1 AS INTEGER, y1 AS INTEGER)
    'Only two coordinates here, so we'll be positioning our frames new movement by the top left corner.
    u = UBOUND(TextHandles)
    IF Handle < 1 OR Handle > u THEN ERROR 5: EXIT FUNCTION
    IF TextHandles(Handle).InUse = False THEN ERROR 5: EXIT FUNCTION
    HideFrame Handle
    TextHandles(Handle).ScreenX = x1
    TextHandles(Handle).ScreenY = y1
    RestoreFrame Handle
END SUB

