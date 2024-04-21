DECLARE SUB SaveScreen ()
DECLARE SUB RestoreScreen ()
DECLARE SUB LaOSNet ()
DECLARE SUB InetOPEN (Datei$, DateiModus$, DateiNummer%)
DECLARE SUB SortCombo (Nr%, ab%)
DECLARE SUB ComboLoadPrograms (runbas%)
'LaOS 6.1
'Copyright (C) 2000, BCO corp.

'Entwickelt unter QBASIC von Urs Langmeier
'Idee & Programmierung: Urs Langmeier

'Alle Urheber- und Leistungsschutzrechte vorbehalten. Der Code darf nicht un-
'erlaubt kopiert oder verkauft werden.

'Alleiniger Inhaber aller Rechte:

'Urs Langmeier
'Gartenstrasse 37
'8154 Oberglatt

DECLARE SUB Progman (Ordner$, Datei$, Symbol$)
DECLARE FUNCTION TrashVoll% ()
DECLARE SUB StatusBar (tex$, Nxt%, klein%)
DECLARE FUNCTION aDateiInfo$ (Datei$, InOrdner$)
DECLARE FUNCTION ComboBox$ (Nr!, z!, s!, zu!, su!)
DECLARE FUNCTION Format$ (FormatString AS STRING)
DECLARE SUB PartFormat (Text$, Setting$)
DECLARE SUB echo (Txt$)
DECLARE FUNCTION MenuInt$ (Item$, Trennzeichen$)
DECLARE SUB DFile (FileName$, IsNPS AS INTEGER)
DECLARE SUB Start (Programm$)
DECLARE SUB PruefeShortCut (taste$)
DECLARE FUNCTION IsInRange% (wert%, min!, max!)
DECLARE FUNCTION ShortCutName$ (taste$)
DECLARE FUNCTION GetProgName$ (dosprog$)
DECLARE FUNCTION IsItNPS$ (Prog$)
DECLARE SUB DrawSchloss ()
DECLARE FUNCTION Inv& (Zahl1 AS LONG)
DECLARE FUNCTION DosFile$ (Kennung$, InOrdner$, IsNPS AS INTEGER)
DECLARE SUB NPSUnLock (File$, OutFile$)
DECLARE SUB FileChoose (Art1$)
DECLARE SUB DeleteBox (Box$)
DECLARE SUB Frame ()
DECLARE SUB Info (Ctrl$, InfoText$)
DECLARE SUB MakeWindow (a!, B!, c!, d!)
DECLARE SUB MenuBox (Zeilen!, Spalten!, Caption$)
DECLARE SUB NewBox (Name$)
DECLARE FUNCTION DriveList$ (Nr!)
DECLARE SUB switchdrive (lw$)
DECLARE SUB ExitBox (x!, y!)
DECLARE FUNCTION exist! (p$)
DECLARE SUB TextBox (Zeilen!, Spalten!, Text$, Break!)
DECLARE SUB Register (z!, s!, zu!, su!, Text$)
DECLARE SUB FrameWindow (a!, B!, c!, d!, del!)
DECLARE SUB echo (Text$)
DECLARE SUB GetInp ()
DECLARE SUB Control (Zeilen!, Spalten!, Text$)
DECLARE SUB CLW ()
DECLARE FUNCTION Gerade! (Zahl!)
DECLARE FUNCTION IsExist! (Datei$, InOrdner$)
DECLARE FUNCTION ComboBox$ (Nr!, z!, s!, zu!, su!)
DECLARE FUNCTION IsEmptyBox! (Ordner$)
DECLARE FUNCTION IsExistBox! (Box$)
DECLARE FUNCTION Trim$ (Variable$, Trennzeichen$, a!)
DECLARE FUNCTION MenuInt$ (Item$, Trennzeichen$)
DECLARE SUB Dialog (Caption$)
DECLARE SUB LoadSymbol (Name$, Kennung$)
DECLARE SUB ShowSymbols ()
DECLARE SUB Menuact (Menu$)
DECLARE FUNCTION SDate$ ()
DECLARE SUB TimeLock (Status!)
DECLARE SUB StoppUhrBeep (Uhr!)
DECLARE SUB Symbact (Symbol$)
DECLARE FUNCTION Ltext$ (Text$, ab!)
DECLARE SUB ShowInfo ()
DECLARE FUNCTION GetKey$ ()
DECLARE SUB ClearControls (Connect!)
DECLARE SUB NextControl ()
DECLARE SUB LastControl ()
DECLARE SUB Activate (Ctrl$)
DECLARE FUNCTION InputBox$ (Zeilen!, Spalten!, Text$, Break!, MaxDown!)

ON ERROR GOTO fehlernext

SCREEN 9, , 0, 0
VIEW PRINT 1 TO 25
'    -> 640 x 350 Grafik
PALETTE 8, 0

DIM SHARED Hintergrund(456) AS LONG, DateiInfo AS STRING
DIM SHARED AlteZeit(1 TO 4) AS SINGLE
DIM SHARED TextScale AS INTEGER
DIM SHARED TextBreak AS INTEGER
DIM SHARED Activ AS STRING
DIM SHARED ControlNr AS INTEGER
DIM SHARED ControlList(40) AS STRING
DIM SHARED Break AS INTEGER
DIM SHARED EBox.left AS INTEGER, EBox.top AS INTEGER
DIM SHARED Left AS INTEGER, Top AS INTEGER, Right AS INTEGER, Down AS INTEGER
DIM SHARED NextBreak AS INTEGER
DIM SHARED QuickInfos(40) AS STRING
DIM SHARED ControlConnect AS INTEGER 'Zur Unterscheidung der 3 Eingabecontrols
DIM SHARED Symbollist(501) AS STRING, SymbolNr AS INTEGER
DIM SHARED Symb.left AS INTEGER, Symb.top AS INTEGER, Symb.total AS INTEGER
DIM SHARED Symb.ipp AS INTEGER 'IPP=Icons par page (misst die letzte volle Seite)
DIM SHARED Symb.count 'ZÑhlt die Symbole in der aktuellen Seite
DIM SHARED DimY AS LONG, DimNow AS INTEGER, Symb.pnr'Nr. in der aktuellen Seite
DIM SHARED MenuList(40) AS STRING, MenuNr AS INTEGER
DIM SHARED Focus AS STRING, keyb AS STRING, Changed AS INTEGER
DIM SHARED Change(2) AS INTEGER
DIM SHARED FileBox AS STRING 'In FileBox ist der aktuelle Ordner gespeichert.
DIM SHARED errorlevel AS INTEGER
DIM SHARED Choice AS INTEGER
DIM SHARED Combo(1 TO 2, 1 TO 100) AS STRING
DIM SHARED Pictures(4) AS STRING
DIM SHARED ActivItem AS STRING, DriveName AS STRING
DIM SHARED HaltOnKeyboard AS INTEGER, key$
DIM SHARED StoppUhr(1 TO 4) AS SINGLE '1 ist Reserviert!
DIM SHARED InDrive AS INTEGER, lw$


FesteEinstellungen:
WIDTH 80, 25

Einstellungen:

DIM SHARED Fensterfarbe AS INTEGER, Randfarbe AS INTEGER, Schattenfarbe AS INTEGER, Controlfarbe AS INTEGER
DIM SHARED Symbolactiv AS INTEGER, SymbactStil AS INTEGER, SymbolFrame AS INTEGER, SymbolColor AS INTEGER
DIM SHARED Hintergrundfarbe  AS INTEGER, Farbe AS INTEGER, IconVerticalSpace AS INTEGER
DIM SHARED InfoTime AS INTEGER, InfoColor  AS INTEGER, InfoFrame, Datumsformat AS INTEGER
DIM SHARED ShowFileInfo%

CLOSE 1: OPEN "c:\laos\config.lcg" FOR RANDOM AS 1
GET 1, 1, Schema$
GET 1, 2, ShowFileInfo%
DIM m(1 TO 15)

IF Schema$ <> "" THEN

CLOSE 1: OPEN "c:\laos\" + Schema$ + ".scl" FOR INPUT AS 1

IF LOF(1) <> 0 THEN
LINE INPUT #1, a$
FOR i = 1 TO 15
INPUT #1, m(i)
NEXT
ELSE m(10) = 15: m(1) = 14
END IF

ELSE m(10) = 15: m(1) = 14
END IF

Fensterfarbe = m(1)'Fensterfarben
Randfarbe = m(2)
Schattenfarbe = m(3)
Controlfarbe = m(4)  'SchaltflÑchenfarbe
Symbolactiv = m(5) 'Umrahmungsfarbe des aktiven Symbols
SymbactStil = m(6) 'Umrahmungs-Stil des aktivierten Symbols (-1=voll)

SymbolFrame = m(7) 'Farbe des Symbolrahmens
SymbolColor = m(8) 'Farbe des Symbolhintergrundes

Hintergrundfarbe = m(9) 'Bildschirmfarben
Farbe = m(10) '= Vordergrundfarbe

IconVerticalSpace = m(11) 'Setzt den Vertikalen Standard-Abstand der Symbole
                            'Auf Null gesetzt ist der Abstand genÅgend.
InfoTime = m(12)'Setzt die zu verstreichende Zeit in Sekunden, bis die Quick-
                  'Info erscheint. (0=Ausgeschaltet)
InfoColor = m(13) 'Setzt die Farbe, in der der Quick-Info-Text erscheint.
InfoFrame = m(14) 'Setzt die Farbe des Quick-Info-Rahmens.

Datumsformat = m(15)

ERASE m

CONST fe1 = 6, fe2 = 4, fe3 = 75 'Startfensterkoordinaten
DIM SHARED fe4
IF IconVerticalSpace = 0 THEN fe4 = 20 ELSE fe4 = 21

StoppUhr(2) = 60 '2.Stoppuhr: Bildschirmschoner


VorStart:
CLOSE
VIEW
COLOR Farbe, Hintergrundfarbe
switchdrive "c"

Code = 0 'Legt die Ereignisverfolgung fÅr die Alt-Taste fest
FOR i = 15 TO 19
KEY i, CHR$(Code) + CHR$(56)
ON KEY(i) GOSUB AltKey
Code = Code + 32
NEXT i
Code = 0: i = 0

Start:
TYPE handlertype
    Vorgangsname AS STRING * 30
    Anwendung AS STRING * 12
    SubID AS STRING * 8
END TYPE


StoppUhr(3) = 0

CLOSE
IF exist("runtmp") THEN KILL "runtmp"
IF exist("runtmp.LAO") THEN KILL "runtmp.LAO"
IF exist("execdrv.tmp") THEN KILL "execdrv.tmp"

DIM SHARED ord$

DO
CLW

ExitBox -1, -1
ClearControls (3)

MenuBox 2, 73, "System"

FileChoose ""
LOOP

SYSTEM
TimeEreignis:
TIMER OFF
ShowInfo
RETURN


AltKey:
IF MenuList(1) <> "" THEN Activate CHR$(0): ControlConnect = 1: Activate MenuList(1): MenuNr = 1
RETURN

fehlernext:
errorlevel = 1
RESUME NEXT

SUB Activate (Ctrl$)
SELECT CASE ControlConnect
CASE 0 'Controls
i = 0
DO
i = i + 1
a$ = ControlList(i)
IF a$ = "" THEN EXIT SUB
'ControlNr = i
Activ = Ctrl$
Zeilen = VAL(MID$(a$, 1, 2))
Spalten = VAL(MID$(a$, 3, 2))
Texta$ = MID$(a$, 5)
Control Zeilen, Spalten, Texta$
LOOP UNTIL a$ = ""
CASE 1 'Menus
i = 0
Activ = Ctrl$
DO
i = i + 1
a$ = MenuList(i)
IF a$ = "" THEN EXIT DO
Menuact a$
LOOP UNTIL a$ = ""



CASE 2 'Symbole
i = 0
Activ = Ctrl$
'i = Symb.total * Symb.page
DO
i = i + 1
a$ = Symbollist(i)
IF MID$(a$, 19) = Activ THEN Activ = a$
IF a$ = "" THEN EXIT DO
Symbact a$
LOOP UNTIL a$ = ""
END SELECT
END SUB

FUNCTION aDateiInfo$ (Datei$, InOrdner$)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a

IF HaltOnKeyboard THEN
key$ = INKEY$
IF key$ <> "" THEN EXIT FUNCTION
END IF

IF RTRIM$(OsName$) = Datei$ AND RTRIM$(Ordner$) = InOrdner$ THEN
    DateiInfo$ = FileInfo$
END IF
LOOP UNTIL Dateiname$ = STRING$(11, 0)

END FUNCTION

SUB ClearControls (Connect)
SELECT CASE Connect
CASE 0: ControlNr = 0: ERASE ControlList: ERASE QuickInfos
CASE 1: MenuNr = 0: ERASE MenuList: ERASE QuickInfos
CASE 2: SymbolNr = 0: ERASE Symbollist: Symb.left = 0: Symb.top = 0: Symb.total = 0
CASE 3 'Alle lîschen
ControlNr = 0: ERASE ControlList: ERASE QuickInfos
MenuNr = 0: ERASE MenuList: ERASE QuickInfos
SymbolNr = 0: ERASE Symbollist: Symb.left = 0: Symb.top = 0: Symb.total = 0
END SELECT

END SUB

SUB CLW
'Fenster:
LINE (2, 2)-(637, 347), 0, BF
LINE (0, 0)-(638, 348), Fensterfarbe, B
'Schatten:
LINE (1, 1)-(637, 1), Schattenfarbe
LINE (1, 1)-(1, 347), Schattenfarbe
'Rand:
LINE (639, 349)-(0, 349), Randfarbe
LINE (639, 349)-(639, 0), Randfarbe

TextScale = 2 '=Zeile
TextBreak = 2 '=Spalte
EBox.left = 603: EBox.top = 321
Left = 2: Top = 2: Right = 79: Down = 24
END SUB

FUNCTION ComboBox$ (Nr, z, s, zu, su)

IF Combo(Nr, 1) = "" THEN Combo(Nr, 1) = " "
DIM Trenner(1 TO 5)


MaxItem% = 30000

IF z = 0 THEN z = Top
IF s = 0 THEN s = Left
IF zu = 0 THEN zu = Down
IF su = 0 THEN su = Right

MaxLenth = su - s + 1
MaxDown = zu - z + 1

DO
i = i + 1
IF Combo(Nr, i) = "-" THEN Combo(Nr, i) = STRING$(MaxLenth, 196): tr = tr + 1: Trenner(tr) = Standort + i
LOOP UNTIL Combo(Nr, i) = ""


a = s * 8 - 11
B = z * 14 - 17
c = su * 8 + 1
d = zu * 14 + 1


FrameWindow a, B, c, d, 0

ItemPos = 1
ItemPage = 1

IF ActivItem <> "" THEN act$ = ActivItem: zrl = 1: GOTO aktivieren
weiter2:

DO

 Standort = ItemPage * MaxDown - MaxDown + 1
LOCATE z

 FOR i = 0 TO MaxDown - 1
  IF Standort + i <= MaxItem% THEN
  IF LEN(Combo(Nr, (Standort + i))) <= MaxLenth THEN
  Eintrag$ = Combo(Nr, Standort + i) + SPACE$(MaxLenth - LEN(Combo(Nr, Standort + i)))
  ELSE
  Eintrag$ = LEFT$(Combo(Nr, Standort + i), MaxLenth)
  END IF
  ELSE Eintrag$ = SPACE$(MaxLenth)
  END IF

    IF INSTR(Eintrag$, CHR$(0)) > 0 THEN Eintrag$ = LEFT$(Eintrag$, INSTR(Eintrag$, CHR$(0)) - 1)

   IF Standort + i = ItemPos THEN COLOR InfoColor ELSE COLOR Farbe

   LOCATE , s: PRINT Eintrag$
IF i = 0 THEN LINE (s * 8 - 9, (z + i) * 14 - 15)-(su * 8, (z + i) * 14), 0, B ELSE LINE (s * 8 - 9, (z + i) * 14 - 14)-(su * 8, (z + i) * 14), 0, B

 NEXT i

i = ItemPos - Standort
LINE (s * 8 - 9, (z + i) * 14 - 15)-(su * 8, (z + i) * 14), InfoFrame, B

SELECT CASE GetKey
CASE "up"
ItemPos = ItemPos - 1

FOR i = 1 TO UBOUND(Trenner)
IF Trenner(i) = ItemPos THEN ItemPos = ItemPos - 1
NEXT
IF ItemPos < Standort THEN ItemPage = ItemPage - 1


CASE "down"
IF ItemPos < MaxItem% THEN

IF Combo(Nr, ItemPos + 1) <> "" THEN

ItemPos = ItemPos + 1

FOR i = 1 TO UBOUND(Trenner)
IF Trenner(i) = ItemPos THEN ItemPos = ItemPos + 1
NEXT


IF ItemPos > Standort + MaxDown - 1 THEN ItemPage = ItemPage + 1

END IF
END IF

CASE "home"
    ItemPos = 1
    ItemPage = 1
    IF Trenner(1) = 1 THEN ItemPos = ItemPos + 1

CASE "end"
    DO
        ItemPos = ItemPos + 1
        Standort = ItemPage * MaxDown - MaxDown + 1
        IF ItemPos > Standort + MaxDown THEN ItemPage = ItemPage + 1
    LOOP UNTIL Combo(Nr, ItemPos) = ""
        ItemPos = ItemPos - 1

    FOR i = 1 TO UBOUND(Trenner)
        IF Trenner(i) = ItemPos THEN ItemPos = ItemPos - 1
    NEXT

CASE "pageup"
IF ItemPos - MaxDown > 0 THEN
    ItemPage = ItemPage - 1
    ItemPos = ItemPos - MaxDown
END IF
CASE "pagedown"
IF ItemPos + MaxDown < MaxItem% + 1 THEN
IF Combo(Nr, ItemPos + MaxDown) <> "" THEN
ItemPage = ItemPage + 1
ItemPos = ItemPos + MaxDown
END IF
END IF
CASE "esc"
    Choice = 0: ComboBox = "": GOTO ExitBox

CASE CHR$(13), "tab"
    LINE (s * 8 - 9, (z + i) * 14 - 15)-(su * 8, (z + i) * 14), 0, B
    Choice = ItemPos: ComboBox = Combo(Nr, ItemPos): GOTO ExitBox

CASE ELSE
IF LEN(keyb) = 1 THEN
act$ = keyb
aktivieren:

AltePosition = ItemPos
AlteSeite = ItemPage

DO
 ItemPos = ItemPos + 1
 IF ItemPos = AltePosition THEN ItemPage = AlteSeite: EXIT DO
 Standort = ItemPage * MaxDown - MaxDown + 1
 IF ItemPos > Standort + MaxDown - 1 THEN ItemPage = ItemPage + 1

   Eintrag$ = LEFT$(Combo(Nr, ItemPos), MaxLenth)
   IF Eintrag$ = "" OR ItemPos > MaxItem% - 1 THEN ItemPos = 0: ItemPage = 1

LOOP UNTIL LCASE$(LEFT$(Eintrag$, LEN(act$))) = LCASE$(act$)
GOTO weiter2

END IF

END SELECT

FOR i = 1 TO UBOUND(Trenner)
IF Trenner(i) = ItemPos THEN ItemPos = ItemPos + 1
NEXT

IF ItemPos < 1 THEN ItemPos = 1: ItemPage = 1
IF Combo(Nr, ItemPos) = "" THEN ItemPos = ItemPos - 1
IF ItemPos < 1 THEN ItemPos = 1: ItemPage = 1

LOOP

ExitBox:
COLOR Farbe
END FUNCTION

SUB ComboLoadPrograms (runbas%)
echo ""
echo ""
echo ""
echo "    Bitte warten."
echo "    Programmliste wird erstellt."

IF Combo(1, 1) <> "" THEN EXIT SUB

TimeLock 1
SHELL "dir c:\laos\*.lao /B >c:\laos\flist.tmp"
CLOSE 2

OPEN "c:\laos\flist.tmp" FOR INPUT AS 2
ERASE Combo


TimeLock 1
i = 0
IF runbas% THEN Combo(1, 1) = "Keine Anwendung (LaOS-HauptmenÅ)": i = 1

nz = TIMER + .5
DO WHILE NOT EOF(2)
IF TIMER > nz THEN nz = TIMER + .5: TimeLock 1
i = i + 1
LINE INPUT #2, a$
IF i > 98 THEN Dialog "Es kînnen nicht alle Programme angezeigt werden.": EXIT DO
B$ = GetProgName(a$)
IF B$ <> "" THEN Combo(1, i) = B$ + SPACE$(30) + CHR$(0) + a$ ELSE i = i - 1
LOOP

IF runbas% THEN SortCombo 1, 2 ELSE SortCombo 1, 1

TimeLock 0

END SUB

SUB Control (Zeilen, Spalten, Text$)

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Zeilen = 0 THEN Zeilen = TextScale
IF Spalten = 0 THEN Spalten = TextBreak
IF Zeilen > Down THEN Zeilen = Top: Spalten = NextBreak + 1: TextScale = Top: TextBreak = NextBreak

IF LEN(Text$) + Spalten + 2 > NextBreak THEN NextBreak = LEN(Text$) + Spalten + 2


a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + LEN(Text$)
EndZeile = Zeilen + INT(LEN(Text$) / 80 + 1)
IF LEN(Text$) = 80 THEN EndZeile = EndZeile - 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

LOCATE Zeilen, Spalten: COLOR Farbe: PRINT Text$
'LINE (a, B)-(c, d), Farbe, B
'PAINT (a + 1, B + 1), Controlfarbe, Farbe
LINE (a, B)-(c, d), Controlfarbe, B

'Schatten: (Wird angezeigt, falls das Control aktiviert ist.
IF Activ = Text$ THEN
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
ELSE
LINE (a + 1, B + 1)-(c - 1, B + 1), 0
LINE (a + 1, B + 1)-(a + 1, d - 1), 0
END IF
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe


Zeilen$ = MID$(STR$(Zeilen), 2)
Spalten$ = MID$(STR$(Spalten), 2)
IF LEN(Zeilen$) < 2 THEN Zeilen$ = "0" + Zeilen$
IF LEN(Spalten$) < 2 THEN Spalten$ = "0" + Spalten$

'Control eintragen:
a$ = Zeilen$ + Spalten$ + Text$
i = 0
DO: i = i + 1
IF ControlList(i) = a$ THEN GOTO 2
LOOP UNTIL ControlList(i) = ""
ControlList(i) = a$
ControlNr = 1


2 Activ = Text$: ControlConnect = 0: TextScale = Zeilen + 2
END SUB

SUB DeleteBox (Box$)

CLOSE 1: OPEN "ORDNER.BNK" FOR RANDOM AS 1
i = 0
DO
i = i + 1
GET 1, i, a$
IF a$ = Box$ THEN
DO
plus = plus + 1
GET 1, i + plus, a$
PUT 1, i + plus - 1, a$
LOOP WHILE a$ <> ""
errorlevel = 0: EXIT SUB
END IF
LOOP UNTIL a$ = ""

errorlevel = 1 'Ordner nicht gefunden.
END SUB

SUB DFile (FileName$, IsNPS AS INTEGER)

'abgewandelt: Gibt ISNPS zurÅck, anhand eines Dos-Dateinamens
'benîtigt, um herauszufinden, ob eine Datei geschÅtzt ist, oder nicht.


CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF HaltOnKeyboard THEN
key$ = INKEY$
IF key$ <> "" THEN EXIT SUB
END IF

IF FileName$ = RTRIM$(Dateiname$) THEN
                IF Attribut$ = "L" THEN IsNPS = -1 ELSE IsNPS = 0
                CLOSE 1: EXIT SUB
END IF
LOOP UNTIL Dateiname$ = STRING$(11, 0)

errorlevel = 1
CLOSE 1

END SUB

SUB Dialog (Caption$)
'Formatoptionen: gleich wie echo. Standard: "c"=zentriert
PartFormat Caption$, fmt$

IF LEN(Caption$) < 22 THEN Caption$ = STRING$(INT((22 - LEN(Kennung$)) / 2), " ") + Caption$ + STRING$(CINT((22 - LEN(Kennung$)) / 2 + .1), " ")

IF LEN(Caption$) > 303 THEN Caption$ = LEFT$(Caption$, 300) + "..."
IF LEN(Caption$) > 76 THEN Length = 76 ELSE Length = LEN(Caption$)
Spalten = INT(40 - Length / 2 + 1)
Zeilen = 11 - INT(LEN(Caption$) / 76) + 1

x1! = Spalten * 8 - 11
y1! = Zeilen * 14 - 17
EndSpalten = 40 + INT(Length / 2)
EndZeilen = 14 + INT(LEN(Caption$) / 76)
IF INT(LEN(Caption$) / 76) > 1 THEN EndZeilen = EndZeilen - (INT(LEN(Caption$) / 76) - 1)
IF LEN(Caption$) < 76 THEN EndZeilen = EndZeilen + 1
x2! = EndSpalten * 8 + 4
y2! = EndZeilen * 14 - 4
Groesse = 4 + INT(((PMAP(x2!, 0) - PMAP(x1!, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y2!, 1) - PMAP(y1!, 1) + 1)
DIM Background(Groesse) AS INTEGER


GET (x1!, y1!)-(x2!, y2!), Background

FrameWindow x1!, y1!, x2! - 1, y2! - 1, 1

a% = TextBreak: B% = TextScale: c% = Right: d% = Down
    TextBreak = Spalten
    TextScale = Zeilen
    Right = EndSpalten
    Down = EndZeilen

        IF fmt$ = "" THEN fmt$ = "c"
        echo Caption$ + Format(fmt$)

Zeilen = TextScale
TextBreak = a%: TextScale = B%: Right = c%: Down = d%

TextBox Zeilen + 1, 33, "      OK", 14

DO
B$ = a$
a$ = GetKey
LOOP UNTIL a$ = "esc" OR a$ = CHR$(13)
IF a$ = "esc" THEN keyb = "esc" ELSE keyb = B$
PUT (x1!, y1!), Background, PSET: x1! = 0: y1! = 0: EXIT SUB


END SUB

FUNCTION DosFile$ (Kennung$, InOrdner$, IsNPS AS INTEGER)
'Gibt die DOS-Datei eines LaOS-Files zurÅck.
'Beispiel: OPEN DosFile("LaOS Draw","Zubehîr") FOR INPUT AS 1

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF HaltOnKeyboard THEN
key$ = INKEY$
IF key$ <> "" THEN EXIT FUNCTION
END IF

IF Kennung$ = RTRIM$(OsName$) AND InOrdner$ = RTRIM$(Ordner$) THEN
                IF Attribut$ = "L" THEN IsNPS = -1 ELSE IsNPS = 0
                DateiInfo = FileInfo$
                DosFile = RTRIM$(Dateiname$): CLOSE 1: EXIT FUNCTION
END IF
LOOP UNTIL Dateiname$ = STRING$(11, 0)
errorlevel = 1
CLOSE 1

END FUNCTION

SUB DrawSchloss
VIEW (540, 290)-(580, 342)
LINE (5.352941, 20.47059)-(33.58823, 44), 8, B
PAINT (19.47059, 29.88235), 7, 8
CIRCLE (19.47059, 20.47059), 14.11765, 8, 6.283185, 3.141593, 1.5
CIRCLE (19.47059, 20.47059), 16.47059, 8, 6.283185, 3.141593, 1.4
PAINT (29.35294, 15.29412), 8, 8
PAINT (9, 16), 8, 8
PAINT (19, 5), 8, 8
PAINT (16, 6), 8, 8
PAINT (13.5, 8), 8, 8
PAINT (11.5, 10.5), 8, 8
PAINT (28, 11.5), 8, 8
PAINT (25.5, 8), 8, 8
PAINT (28, 12), 8, 8
PAINT (23, 7), 8, 8
PAINT (11, 11), 8, 8
PAINT (22, 7), 8, 8
LINE (22, 6)-(23, 6), 8
LINE (13, 9)-(14, 7), 8
PSET (13, 8), 8
PSET (24, 7), 8
PSET (25, 8), 8
CIRCLE (19.125, 27.3), 2.5, 8, , , 1
LINE (19.125, 29.8)-(19.125, 39.8), 8
LINE (19.125, 39.8)-(16.625, 39.8), 8
LINE (19.125, 38.55)-(17.875, 38.55), 8
LINE (17.875, 38.55)-(17.875, 36.05), 8
LINE (17.875, 36.05)-(19.125, 36.05), 8
LINE (19.125, 34.8)-(17.875, 34.8), 8

END SUB

FUNCTION DriveList$ (Nr)
STATIC LastNr
IF Nr < 1 THEN Nr = LastNr + 1

CLOSE 1: OPEN "c:\laos\drvlist.bnk" FOR RANDOM AS 1
GET 1, Nr, a$

LastNr = Nr
DriveList = LEFT$(a$, 1)
CLOSE 1
END FUNCTION

SUB echo (Txt$)
PartFormat Txt$, fmt$

'Format: "l", "c", "r": LinksbÅndig, zentriert, rechtsbÅndig

SELECT CASE fmt$
    CASE "l", "": Align% = 0
    CASE "c": Align% = 1
    CASE "r": Align% = 2
END SELECT

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF TextScale > Down THEN TextScale = Top

'10.11.1999: Echo mit Zeilenumbruch (C) Urs Langmeier.

Text$ = Txt$ + " "

DIM s AS INTEGER, z AS INTEGER, t AS INTEGER, brk AS INTEGER


s = 1
z = Right - TextBreak + 2'ZeilenlÑnge
dwn = 0

p1% = s
p2% = p1%
DO
    t = t + 1
        IF t = p1% THEN p2% = p1%'alten Retten
        p1% = INSTR(t, Text$, " ")
        IF p1% = 0 OR p1% = LEN(Text$) THEN
            IF p1% <= s + z - 1 THEN p2% = LEN(Text$): brk = -1
        END IF

        IF p1% > s + z - 1 OR brk THEN 'nehme alten, weil neuen Åber die Zeile ragt.
            'Zeile ausgeben:
            p1% = p2%
            IF TextScale + dwn > Down THEN EXIT DO
                IF p1% - s <= 0 THEN 'falls Wort zu lange (nicht in die Zeile passt:)

                    p1% = s + z - 1
                    a$ = MID$(Text$, s, p1% - s)
                    t = p1% - 1
                    s = t + 1

                ELSE

                    a$ = MID$(Text$, s, p1% - s)
                    t = p1% + 1
                    s = t
                END IF

            'Ausrichtung:
            SELECT CASE Align%
                CASE 0: x% = TextBreak
                CASE 1: x% = TextBreak + FIX((Right - TextBreak - LEN(a$) + 1) / 2)
                CASE 2: x% = Right - LEN(a$) + 1
            END SELECT

            LOCATE TextScale + dwn, x%
            PRINT a$;

            dwn = dwn + 1
        END IF
LOOP UNTIL t >= LEN(Text$)
TextScale = TextScale + dwn


END SUB

FUNCTION exist (p$)

errorlevel = 0
f% = FREEFILE
OPEN p$ FOR INPUT AS f%
IF errorlevel = 0 THEN exist = -1 ELSE exist = 0
CLOSE f%

END FUNCTION

SUB ExitBox (x, y)
IF x = -1 THEN x = EBox.left
IF y = -1 THEN y = EBox.top
LINE (x, y)-(x + 26, y + 20), Fensterfarbe, B
LINE (x + 1, y + 1)-(x + 25, y + 1), Schattenfarbe
LINE (x + 1, y + 1)-(x + 1, y + 19), Schattenfarbe
LINE (x, y + 21)-(x + 26, y + 21), Randfarbe
LINE (x + 27, y + 21)-(x + 27, y), Randfarbe

LINE (x + 2, y + 19)-(x + 24, y + 2), Fensterfarbe
LINE (x + 3, y + 19)-(x + 25, y + 2), Schattenfarbe
LINE (x + 2, y + 2)-(x + 24, y + 19), Fensterfarbe
LINE (x + 3, y + 2)-(x + 25, y + 19), Schattenfarbe

END SUB

SUB FileChoose (Art1$)
'LÑsst den Benutzer eine Datei auswÑhlen. Art1$: Die Art-Spalte. Leer = alle.

301
nz = TIMER + .5
CLOSE
ord$ = ""
InDrive = 0
SHELL "c:": CHDIR "\laos"
DriveName = ""
Register fe2, fe1, fe4, fe3, ""
ClearControls 2
TimeLock 1
Datics = 0

'Laufwerke laden:
ON ERROR GOTO fehlernext
CLOSE 1: OPEN "drvlist.bnk" FOR RANDOM AS 1
nf = FREEFILE
lmf = nf + 1
CLOSE lmf: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS lmf

i = 0
DO
i = i + 1
errorlevel = 0

GET 1, i, LNam$
IF LNam$ = "" THEN EXIT DO
lw$ = LEFT$(LNam$, 1): OPEN lw$ + ":\nul" FOR BINARY AS nf: CLOSE nf
IF errorlevel = 0 THEN 'Laufwerk vorhanden:
IF MID$(LNam$, 4) <> DriveName$ THEN
OPEN lw$ + ":\laos\medname.inf" FOR RANDOM AS nf
GET nf, 1, B$
IF B$ = "" THEN B$ = MID$(LNam$, 4)
errorlevel = 0
CLOSE nf: OPEN lw$ + ":\laos\drvicon.lsb" FOR BINARY AS nf
Datics = Datics + 1

PUT lmf, , lw$

IF LOF(nf) = 0 THEN
LoadSymbol "drvicon", B$
ELSE
'Symbol des DatentrÑgers verwenden:
IF TIMER > nz THEN TimeLock 1: nz = TIMER + .5
KILL "~otdric" + lw$ + ".lsb"
f = FREEFILE
OPEN "~otdric" + lw$ + ".lsb" FOR BINARY AS f
DIM g AS SINGLE
DIM Befehle(5) AS SINGLE
DO
FOR Nr = 0 TO 5
GET nf, , Befehle(Nr)
PUT f, , Befehle(Nr)
NEXT
LOOP WHILE Befehle(0) <> 0
CLOSE f
LoadSymbol "~otdric" + lw$, B$
END IF
CLOSE nf
LNam$ = ""
B$ = ""
END IF
END IF
LOOP
CLOSE lmf
TimeLock 0


'Ordner laden:
304
CLOSE 1: OPEN "ordner.bnk" FOR RANDOM AS 1
i = 0
DO
i = i + 1
GET 1, i, a$
IF a$ <> "" THEN LoadSymbol "ordner", a$
LOOP UNTIL a$ = ""

305
ShowSymbols

307
IF vv = 1 THEN vv = 0: GOTO 301
GetInp
IF keyb = "insert" AND SymbolNr > Datics THEN Progman Focus, "", ""
IF keyb = "exit" THEN 301
IF Focus = "System" THEN GOSUB Menu: GOTO 307
IF keyb = "esc" AND InDrive = 0 THEN 307
IF keyb = "esc" THEN DriveName = "": GOTO 301
IF LCASE$(Focus) = "laos internet" THEN LaOSNet

IF SymbolNr <= Datics THEN 'Laufwerk angewÑhlt:
Datics = 0
f = FREEFILE: OPEN "c:\laos\lwmap.tmp" FOR RANDOM AS f
GET f, SymbolNr, lw$: CLOSE f
switchdrive lw$
IF NOT exist("\laos\nul") THEN Dialog "Dieser DatentrÑger enthÑlt keine Bezeichnung und Symbol! Gehen Sie im Ordner 'Zubehîr' auf 'Laufwerke' und wÑhlen Sie dort das MenÅ 'Bezeichnung & Symbol...' aus.": GOTO 301
ClearControls (2)
Register fe2, fe1, fe4, fe3, DriveName
InDrive = 1
GOTO 304
END IF

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS InOrdner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

Register fe2, fe1, fe4, fe3, Focus
TimeLock 1
Az = TIMER
ClearControls (2)
DO
GET 1
IF RTRIM$(InOrdner$) = Focus AND RTRIM$(Art$) = Art1$ OR Art1$ = "" AND RTRIM$(InOrdner$) = Focus THEN
LoadSymbol RTRIM$(Symbol$), RTRIM$(OsName$)
END IF
IF TIMER - Az > .5 THEN TimeLock (1): Az = TIMER
LOOP UNTIL Dateiname$ = STRING$(11, 0)

TimeLock (0)
ord$ = Focus

ShowSymbols

306
IF vv = 1 THEN vv = 0: GOTO 301
GetInp
IF Focus = "System" THEN GOSUB Menu: GOTO 306
IF keyb = "esc" AND InDrive = 0 THEN ord$ = "": GOTO 301
IF keyb = "esc" THEN ord$ = "": DriveName = "": ClearControls 2: Register fe2, fe1, fe4, fe3, DriveName: GOTO 304
IF keyb = "insert" THEN Progman ord$, Focus, MID$(Symbollist(SymbolNr), 11, 8)
IF keyb = "exit" THEN 301
Programm$ = DosFile(Focus, ord$, IsNPS%)

startnow:
'prÅfen, ob NPS-Decodiert:

DocFile$ = ""
IF IsNPS% THEN
        ext$ = ""
        IF exist(Programm$ + ".lao") THEN ext$ = ".lao"
        NPSUnLock Programm$ + ext$, "runtmp" + ext$

        InDrive = 0: DocFile$ = Programm$: Programm$ = "runtmp"

        IF errorlevel OR keyb = "esc" THEN 301
END IF

IF exist(Programm$ + ".lao") THEN
IF exist("c:\laos\linkway.tmp") THEN KILL "c:\laos\linkway.tmp"
CLOSE : CHAIN Programm$ + ".lao"
ELSE
CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS InOrdner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

TimeLock 1
AlteZeit = TIMER
DO
GET 1
IF RTRIM$(Dateiname$) = Programm$ OR RTRIM$(Dateiname$) = DocFile$ THEN
IF RTRIM$(Link$) = "" THEN
Dialog "Diese Datei ist mit keinem Programm verknÅpft. Sie kann nicht alleine ge-   startet werden.": GOTO 350
ELSE
'linken:
DocServer$ = IsItNPS(RTRIM$(Link$))
IF errorlevel OR keyb = "esc" THEN 301

IF exist(DocServer$ + ".lao") THEN
CLOSE 2: OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 2

IF InDrive = 1 THEN Programm$ = lw$ + ":\laos\" + Programm$
PUT 2, 1, Programm$: CLOSE 2
CLOSE : CHAIN DocServer$ + ".lao"
ELSE
IF exist("C:\LAOS\" + DocServer$ + ".lao") THEN

CLOSE 2: OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 2

IF InDrive = 1 THEN Programm$ = lw$ + ":\laos\" + Programm$
PUT 2, 1, Programm$: CLOSE 2

CLOSE : CHAIN "C:\LAOS\" + DocServer$ + ".lao"
ELSE
Dialog "Falsche VerknÅpfung. Die Datei kann nicht gestartet werden.": GOTO 350
END IF
END IF
END IF
END IF
IF TIMER - AlteZeit > .5 THEN TimeLock (1): AlteZeit = TIMER
LOOP UNTIL Dateiname$ = STRING$(11, 0)

Dialog "Es wurde versucht, ein Programm zu laden, welches nicht existiert."
END IF

350
TimeLock 0
GOTO 306

EXIT SUB



Menu:
IF keyb = "esc" THEN Activate CHR$(0): ControlConnect = 2: RETURN
a$ = MenuInt("Neu...˘Ordner wegwerfen˘-˘Zuletzt erstellt...˘-˘Einstellungen...˘-˘Beenden", "˘")
Activate CHR$(0)
ControlConnect = 2

SELECT CASE Choice
CASE 0: RETURN
CASE 1

DIM handler AS handlertype

f% = FREEFILE
OPEN "c:\laos\handler.rsc" FOR RANDOM AS f%
B$ = ""
DO: GET f%, , handler
IF handler.Vorgangsname = STRING$(30, CHR$(0)) OR RTRIM$(handler.Vorgangsname) = "" THEN EXIT DO
    B$ = B$ + RTRIM$(handler.Vorgangsname$) + "˘"
LOOP
CLOSE f%

a$ = MenuInt("Ordner            ˘-˘" + B$, "˘")
    SELECT CASE Choice
    CASE 0: RETURN
    CASE 1
        DO
            Register 8, 10, 13, 50, "Neuer Ordner einrichten"
                ExitBox -1, -1
                echo "Geben Sie den Namen ein:"
                a$ = InputBox(11, 0, "", 32, 1)
                IF keyb = "esc" THEN GOTO 312
                NewBox RTRIM$(a$)
        LOOP WHILE errorlevel = 1
        GOTO 311
    CASE IS >= 3
        OPEN "c:\laos\handler.rsc" FOR RANDOM AS f%
            DO: GET f%, , handler
            IF RTRIM$(handler.Vorgangsname$) = a$ THEN

CLOSE
OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 1
IF RTRIM$(handler.SubID) <> "" THEN a$ = "/" + RTRIM$(handler.SubID) ELSE a$ = ""
PUT 1, 1, a$
IF LCASE$(RIGHT$(RTRIM$(handler.Anwendung), 4)) = ".lao" THEN Start LEFT$(RTRIM$(handler.Anwendung), LEN(RTRIM$(handler.Anwendung)) - 4) ELSE CHAIN RTRIM$(handler.Anwendung)
EXIT DO

            END IF
            LOOP UNTIL handler.Vorgangsname$ = STRING$(30, CHR$(0)) OR RTRIM$(handler.Vorgangsname) = ""
        Dialog "Fehler beim AusfÅhren des Vorganges. FÅhren Sie DriveClean im Ordner 'Zubehîr' aus."
        CLOSE f%

    END SELECT

CASE 2
IF ord$ = "" THEN
Box$ = MID$(Symbollist(SymbolNr), 19)
IF IsExistBox(Box$) = 0 THEN Dialog "Es ist kein Ordner angewÑhlt.": Activate CHR$(0): ControlConnect = 2: RETURN
ELSE Box$ = ord$
END IF
IF IsEmptyBox(Box$) = 0 THEN Dialog "Der angewÑhlte Ordner ist nicht leer. Lîschen Sie zuerst alle Dokumente im  Ordner.":  Activate CHR$(0): ControlConnect = 2: RETURN

Register 8, 10, 16, 50, ""' "Ordner wegwerfen"
ExitBox -1, -1
echo "Sind Sie sicher, dass Sie den Ordner"
COLOR InfoColor: echo Box$
COLOR Farbe: echo "wegwerfen wollen?"
a$ = InputBox(13, 0, "", 4, 1)
IF keyb = "esc" THEN GOTO 312
IF LCASE$(a$) = "ja" THEN
DeleteBox (Box$)
ord$ = ""
GOTO 311
END IF

CASE 4

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a% = 0
DO
a% = a% + 1
GET 1, a%
LOOP UNTIL Dateiname$ = STRING$(11, 0)

ClearControls 3
Register fe2, fe1, fe4, fe3, DriveName

f = FREEFILE
OPEN "fn.tmp" FOR RANDOM AS f
i = 0
DO
i = i + 1
a% = a% - 1
GET 1, a%
a$ = Dateiname$ + Ordner$
PUT #f, , a$
LoadSymbol RTRIM$(Symbol$), RTRIM$(OsName$)
LOOP UNTIL (a% < 2) OR i > 49

ShowSymbols

DO
GetInp
LOOP UNTIL keyb = CHR$(13) OR keyb = "esc" OR keyb = "insert"
IF keyb = "esc" THEN EXIT SUB

GET f, SymbolNr, a$
CLOSE f

IF keyb = "insert" THEN Progman RTRIM$(MID$(a$, 12)), Focus, MID$(Symbollist(SymbolNr), 11, 8)

Start LEFT$(a$, 11)

CASE 6
ord$ = ""
a$ = "#rnstat0"
B$ = "#rnstat1"

a% = ShowFileInfo%
    IF a% THEN SWAP a$, B$

SymbolNr = 1

55520
i = SymbolNr

    ClearControls 3
    Register fe2, fe1, fe4, fe3, "Einstellungen"
    LoadSymbol a$, "Datei-Info anzeigen"
    LoadSymbol "tastatur", "Kurztasten einrichten..."
    LoadSymbol "timedata", "Zeit und Datum..."
    LoadSymbol "autostrt", "Standard-Anwendung"
    IF TrashVoll THEN LoadSymbol "trash1", "Papierkorb" ELSE LoadSymbol "trash0", "Papierkorb"
    ShowSymbols

        SymbolNr = 1
        DO WHILE i <> SymbolNr
        NextControl
        LOOP

    GetInp

    CLOSE 1: OPEN "c:\laos\config.lcg" FOR RANDOM AS 1

IF keyb = "esc" THEN
        ShowFileInfo% = a%
    CLOSE 1: EXIT SUB
END IF

        SELECT CASE SymbolNr
            CASE 2: GOTO 4551
            CASE 1: SWAP a$, B$: a% = 1 - a%: PUT 1, 2, a%
            CASE 3
t25:
                    Register 6, 8, 16, 40, "Zeit und Datum setzen"
                    ExitBox -1, -1
                    echo ""
                    echo "Zeit eingeben:"
                    echo ""
                    t$ = InputBox(0, 0, TIME$, 8, 1)
                    IF keyb = "esc" THEN 55520
                    errorlevel = 0
                    TIME$ = t$
                    IF errorlevel THEN Dialog "UngÅltige Zeiteingabe! Geben Sie die Zeit in folgendem Format ein: hh:mm": GOTO t25
t26:
                    TextScale = 12
                    echo "Datum eingeben:"
                    echo ""
                    t$ = InputBox(0, 0, MID$(DATE$, 4, 2) + "." + LEFT$(DATE$, 2) + "." + RIGHT$(DATE$, 4), 10, 1)
                    IF keyb = "esc" THEN 55520
                    errorlevel = 0
                    DATE$ = MID$(t$, 4, 2) + "-" + LEFT$(t$, 2) + "-" + MID$(t$, 7)
                    IF errorlevel THEN Dialog "UngÅltige Datumseingabe! Geben Sie das Datum in folgendem Format ein: tt.mm.jjjj": GOTO t26

            CASE 4

Register 6, 7, 20, 72, "Standard-Anwendung definieren..."
ExitBox -1, -1
echo ""
echo "WÑhlen Sie aus der folgenden Liste ein Programm aus, welches beim Start von LaOS geladen werden soll:"

ComboLoadPrograms -1

c$ = ComboBox(1, 11, 9, 19, 66)

IF keyb <> "esc" THEN
CLOSE 1: OPEN "c:\laos\config.lcg" FOR RANDOM AS 1
IF Choice = 1 THEN File$ = "": PUT 1, 3, File$: GOTO 55520

File$ = MID$(c$, INSTR(c$, CHR$(0)) + 1)
f% = FREEFILE
PUT 1, 3, File$

END IF

            CASE 5: Progman "Papierkorb", "", ""
        END SELECT

GOTO 55520

CASE 8
Dialog "LaOS wird jetzt beendet. Vielen Dank fÅr Ihren Besuch."
IF keyb = "esc" THEN Activate CHR$(0): ControlConnect = 2: RETURN
SYSTEM
END SELECT

311
vv = 1: RETURN


312
Activate CHR$(0)
Register fe2, fe1, fe4, fe3, DriveName
SymbolNr = 0
ShowSymbols
RETURN

4551
ClearControls 0
Register 3, 4, 21, 76, "Kurztasten einrichten"
ExitBox -1, -1
echo ""
echo "Kurztasten, oder auch ShortCuts erlauben Ihnen den schnellen Start von"
echo "Programmen Åber einen Tastaturbefehl wie z.B. Shift + F1"
echo ""
echo "WÑhlen Sie bitte die gewÅnschte Option:"
echo ""
Control 0, 6, "Neuer ShortCut einrichten"
Control 0, 6, "Vorhandener ShortCut lîschen"
GetInp

IF keyb = "esc" THEN EXIT SUB
SELECT CASE ControlNr
CASE 2
Register 3, 4, 21, 76, "Kurztasten lîschen"
echo ""
echo "WÑhlen Sie aus der folgenden Liste den Shortcut aus, den Sie lîschen"
echo "wollen, und drÅcken Sie die Eingabetaste."
TimeLock 1
nz = TIMER + .5
CLOSE 2: OPEN "c:\laos\scuts.dat" FOR RANDOM AS 2 LEN = 10
FIELD 2, 8 AS f$, 2 AS s$
ERASE Combo
i = 0
DO
IF TIMER > nz THEN nz = TIMER + .5: TimeLock 1

i = i + 1
GET 2, i
IF i > 99 THEN Dialog "Es kînnen nicht alle Shortcuts angezeigt werden.": EXIT DO

IF f$ <> STRING$(8, 0) THEN Combo(1, i) = GetProgName(RTRIM$(f$) + ".lao") + ", " + ShortCutName(RTRIM$(s$))

LOOP UNTIL f$ = STRING$(8, 0)

CLOSE 1, 2
TimeLock 0

B$ = ComboBox(1, 8, 6, 20, 74)
ERASE Combo
IF keyb = "esc" THEN 4551


errorlevel = 0
CLOSE 1, 2: OPEN "c:\laos\scuts.dat" FOR RANDOM AS 1 LEN = 10
FIELD 1, 8 AS fa$, 2 AS sa$
OPEN "c:\laos\runsc.tmp" FOR RANDOM AS 2 LEN = 10
FIELD 2, 8 AS f1$, 2 AS s1$

i = 0: u = 0
DO
i = i + 1
GET 1, i
IF i <> Choice AND fa$ <> STRING$(8, 0) THEN
        u = u + 1
        LSET f1$ = fa$
        LSET s1$ = sa$
        PUT 2, u
END IF
LOOP UNTIL fa$ = STRING$(8, 0)
IF errorlevel = 0 THEN CLOSE : KILL "c:\laos\scuts.dat": NAME "c:\laos\runsc.tmp" AS "c:\laos\scuts.dat" ELSE Dialog "Fehler beim Lîschen."


CASE 1
Register 3, 4, 21, 76, "Kurztasten einrichten"
echo ""
echo "WÑhlen Sie aus der folgenden Liste ein Programm aus, fÅr welches"
echo "Sie einen ShortCut erstellen wollen:"

ComboLoadPrograms 0
ExitBox -1, -1

B$ = ComboBox(1, 8, 6, 20, 67)
IF keyb = "esc" THEN 4551

File$ = MID$(B$, INSTR(B$, CHR$(0)) + 1)


Register 3, 4, 21, 76, "Kurztasten einrichten"
ExitBox -1, -1
echo ""
echo "DrÅcken Sie die Taste, oder die Tastenkombination, welche Sie als"
echo "Kurztaste fÅr das ausgewÑhle Programm verwenden wollen."

TextBox 8, 8, "", 12
TextBox 12, 8, "     OK", 12

DO
k$ = ""
WHILE INKEY$ <> "": WEND
DO: k$ = INKEY$: LOOP WHILE k$ = ""
IF k$ = CHR$(27) THEN EXIT SUB
IF k$ <> CHR$(13) THEN sc$ = k$: TextBox 8, 8, ShortCutName(sc$), 12
LOOP UNTIL k$ = CHR$(13)

'Enter gedrÅckt:
CLOSE 1: OPEN "c:\laos\scuts.dat" FOR RANDOM AS 1 LEN = 10
File$ = LEFT$(File$, LEN(File$) - 4) '.lao wegnehmen
FIELD 1, 8 AS f$, 2 AS s$

i = 0
DO
i = i + 1
GET 1, i
LOOP UNTIL f$ = STRING$(8, 0)
LSET f$ = File$
LSET s$ = sc$
PUT 1, i
CLOSE 1


CASE 2
END SELECT
GOTO 4551

END SUB

FUNCTION Format$ (FormatString AS STRING)
    Format$ = CHR$(0) + FormatString
END FUNCTION

SUB Frame
'Frame kann ausgefÅhrt werden, um ein Rahmen um das Fenster zu machen,
'und dabei aber die Left- Top- usw.-Kordinaten zu behalten.
'einzig die TextScale- und TextBreak- Koordinaten wechseln nach oben links.
'-> Siehe auch die Sub "CLW".

FrameWindow 0, 0, 638, 348, 0

'EBox.left = 603: EBox.top = 321
'Left = 2: Top = 2: Right = 79: Down = 24
TextScale = 2: TextBreak = 2
END SUB

SUB FrameWindow (a, B, c, d, del)
'Erstellt ein Fenster, ohne dabei die Umgebungsvariablen zu verÑndern
'Wahlweise wird der Inhalt des Fensters gelîscht (del = 1), oder nicht.

'Fenster:
LINE (a, B)-(c, d), Fensterfarbe, B
IF del = 1 THEN LINE (a + 2, B + 2)-(c - 1, d - 1), 0, BF
'Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe

END SUB

FUNCTION Gerade (Zahl)
IF Zahl / 2 - FIX(Zahl / 2) = 0 THEN Gerade = 1:  ELSE Gerade = 0
END FUNCTION

SUB GetInp
DIM kbd AS STRING
'Aktiviere das Control mit dem Fokus:
SELECT CASE ControlConnect
CASE 0: Activate MID$(ControlList(ControlNr), 5)
CASE 1: Activate MenuList(MenuNr)
CASE 2: Activate Symbollist(SymbolNr)
END SELECT


DO
FOR i = 15 TO 19: KEY(i) ON: NEXT i
IF InfoTime > 0 THEN
TIMER ON
ON TIMER(InfoTime) GOSUB TimeEreignis
END IF
  FOR i = 1 TO 4
    AlteZeit(i) = TIMER
  NEXT i
WHILE INKEY$ = CHR$(13) OR INKEY$ = CHR$(27): WEND
kbd = ""

StoppUhrBeep 5

WHILE kbd = ""
  kbd = INKEY$

  IF key$ <> "" THEN kbd = key$: key$ = ""


   FOR i = 1 TO 4
     IF StoppUhr(i) > 0 THEN
       IF TIMER - AlteZeit(i) > StoppUhr(i) THEN StoppUhrBeep (i): AlteZeit(i) = TIMER
END IF
   NEXT i
 WEND
TIMER OFF: FOR i = 15 TO 19: KEY(i) OFF: NEXT i

PruefeShortCut kbd

SELECT CASE kbd
CASE CHR$(0) + "G": DO: a$ = Activ: LastControl: LOOP UNTIL a$ = Activ OR INKEY$ <> ""
CASE CHR$(0) + "O": DO: a$ = Activ: NextControl: LOOP UNTIL a$ = Activ OR INKEY$ <> ""
CASE CHR$(0) + "?": 'F5
                    keyb = "exit"
                    GOTO 25

CASE CHR$(0) + "R": 'insert
                    keyb = "insert"
                    GOTO 25

CASE CHR$(0) + "^": Dialog "LaOS 6.1, 21.10.2002 (C) 1998-2002, Urs Oliver Langmeier"
CASE CHR$(0) + "T": ShowInfo 'Manueller Q-Info-Abruf mit Shift+F1
CASE CHR$(13)
keyb = kbd
25
SELECT CASE ControlConnect
CASE 0: Focus = MID$(ControlList(ControlNr), 5)
CASE 1: Focus = MID$(MenuList(MenuNr), 5)
CASE 2: Focus = MID$(Symbollist(SymbolNr), 19)
END SELECT
EXIT SUB
CASE CHR$(9) 'Tab
STATIC ch AS INTEGER

Activate CHR$(0) 'Aktiviere ein Control, das gar nicht existiert, um alle
                 'anderen zu deaktivieren.
ControlConnect = ControlConnect + 1
IF ControlConnect > 2 THEN ControlConnect = 0
FOR i = 1 TO 3
SELECT CASE ControlConnect
CASE 0: IF ControlList(1) = "" THEN ControlConnect = 1
CASE 1: IF Change(ch + 1) <> 0 THEN ControlConnect = 0: EXIT FOR
        IF MenuList(1) = "" THEN ControlConnect = 2
CASE 2: IF Symbollist(1) = "" THEN ControlConnect = 0
END SELECT
NEXT i
SELECT CASE ControlConnect
CASE 0
ch = ch + 1
IF Change(ch) = 0 THEN ch = 0
IF ch <> 0 THEN ControlNr = Change(ch)
Activate MID$(ControlList(ControlNr), 5)
CASE 1: Activate MenuList(MenuNr)
CASE 2: Activate Symbollist(SymbolNr)
END SELECT
CASE CHR$(0) + "H", CHR$(0) + "K": LastControl
CASE CHR$(0) + "P", CHR$(0) + "M": NextControl
CASE CHR$(27), CHR$(8)
SELECT CASE ControlConnect
CASE 0: Focus = MID$(ControlList(ControlNr), 5)
CASE 1: Focus = MID$(MenuList(MenuNr), 5)
CASE 2: Focus = MID$(Symbollist(SymbolNr), 19)
END SELECT
keyb = "esc": EXIT SUB'Minimize
END SELECT
LOOP

END SUB

FUNCTION GetKey$

WHILE INKEY$ <> "": WEND

a$ = "": WHILE a$ = "": a$ = INKEY$: WEND

SELECT CASE MID$(a$, 2)
CASE "H": a$ = "up"
CASE "P": a$ = "down"
CASE "K": a$ = "left"
CASE "M": a$ = "right"
CASE "R": a$ = "insert"
CASE "S": a$ = "delete"
CASE "G": a$ = "home"
CASE "O": a$ = "end"
CASE "I": a$ = "pageup"
CASE "Q": a$ = "pagedown"
END SELECT

SELECT CASE ASC(a$)
CASE 27: a$ = "esc"
CASE 9: a$ = "tab"
END SELECT

GetKey = a$
keyb = a$
END FUNCTION

FUNCTION GetProgName$ (dosprog1$)
dosprog$ = LEFT$(dosprog1$, LEN(dosprog1$) - 4) '.lao von Datei wegnehmen.

CLOSE 1: OPEN "C:\LAOS\DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a

IF LCASE$(RTRIM$(Dateiname$)) = LCASE$(dosprog$) THEN GetProgName$ = OsName$: EXIT FUNCTION

LOOP UNTIL Dateiname$ = STRING$(11, 0)

errorlevel = 1
CLOSE 1


END FUNCTION

SUB GraphicView (a, B, c, d)
'Diese Sub Ñndert die Umgebungsvariablen (Left, Top, usw.) fÅr einen bestimmt-
'en Fenster-Bereich. Die darauffolgenden Grafikausgaben erfolgen in diesem
'Bereich.
'LINE (a, B)-(c, d), Randfarbe, B



EBox.left = c - 31: EBox.top = d - 24
Left = INT(a / 8) + 2: Top = INT(B / 14) + 2: Right = INT((c + 8) / 8) - 1: Down = INT((d + 14) / 14) - 1
TextBreak = Left
TextScale = Top
END SUB

'siehe auch: PushInetFile
'bsp: InetOpen "dateien.bnk", "random#400", 1
'îffnet die Datei dateien.bnk im Random-Modus mit der DatensatzlÑnge 400
'als Datei 1.
SUB InetOPEN (Datei$, DateiModus$, DateiNummer%)

SaveScreen
'SCREEN 0

    IF INSTR(DateiModus$, "#") > 0 THEN
        lenth = VAL(MID$(DateiModus$, INSTR(DateiModus$, "#") + 1))
        DateiModus$ = LEFT$(DateiModus$, INSTR(DateiModus$, "#") - 1)
    END IF

    CLOSE DateiNummer%
    IF DateiNummer% = 0 THEN DateiNummer% = FREEFILE
    DateiModus$ = LCASE$(DateiModus$)

    SELECT CASE DateiModus$
    CASE "input", "binary", "random", "append" 'ok
    CASE "output"
        OPEN "c:\laos\ftp\" + Datei$ FOR APPEND AS DateiNummer%
        EXIT SUB 'Muss zuerst downloaded werden (Datei ist zu Beginn des Schreibens leer)
    CASE ELSE
        Dialog "UngÅltiger Dateimodus angegeben!"
    END SELECT

    OPEN "c:\laos\ftp\transfer.ftp" FOR OUTPUT AS DateiNummer%

    PRINT #DateiNummer%, "laosoft.ch"
    PRINT #DateiNummer%, "mba"
    PRINT #DateiNummer%, "verbose"
    PRINT #DateiNummer%, "cd laos"
    PRINT #DateiNummer%, "lcd c:\laos\ftp"

    'FTP-Dateimodus einstellen:
    SELECT CASE DateiModus$
    CASE "input", "output", "append"
        PRINT #DateiNummer%, "ascii"
    CASE ELSE
        PRINT #DateiNummer%, "bin"
    END SELECT

    SELECT CASE DateiModus$
    CASE "input", "binary", "random", "append"
        PRINT #DateiNummer%, "GET " + UCASE$(Datei$)
    END SELECT
    PRINT #DateiNummer%, "quit"
    CLOSE DateiNummer%

    'SHELL "ftp -w:128 -i -s:c:\laos\ftp\transfer.ftp laosoft.ch >transfer.log"
    'SHELL "ftp -w:128 -s:c:\laos\ftp\transfer.ftp laosoft.ch >transfer.log"
    PRINT "ftp -w:128 -s:c:\laos\ftp\transfer.ftp laosoft.ch >transfer.log"
    SHELL

    'ok!

    'zum Schluss Datei lokal îffnen: (im gewÅnschten Modus)

    SELECT CASE DateiModus$
    CASE "input"
        IF lenth > 0 THEN
            OPEN "c:\laos\ftp\" + Datei$ FOR INPUT AS DateiNummer% LEN = lenth
        ELSE
            OPEN "c:\laos\ftp\" + Datei$ FOR RANDOM AS DateiNummer%
        END IF

    CASE "binary"
        IF lenth > 0 THEN
            OPEN "c:\laos\ftp\" + Datei$ FOR BINARY AS DateiNummer% LEN = lenth
        ELSE
            OPEN "c:\laos\ftp\" + Datei$ FOR RANDOM AS DateiNummer%
        END IF
    CASE "random"
        IF lenth > 0 THEN
            OPEN "c:\laos\ftp\" + Datei$ FOR RANDOM AS DateiNummer% LEN = lenth
        ELSE
            OPEN "c:\laos\ftp\" + Datei$ FOR RANDOM AS DateiNummer%
        END IF

    CASE "append"
        IF lenth > 0 THEN
            OPEN "c:\laos\ftp\" + Datei$ FOR APPEND AS DateiNummer% LEN = lenth
        ELSE
            OPEN "c:\laos\ftp\" + Datei$ FOR RANDOM AS DateiNummer%
        END IF

    END SELECT

'SCREEN 9
COLOR Farbe, Hintergrundfarbe

'SCREEN 9
RestoreScreen

END SUB

SUB Info (Ctrl$, InfoText$)
SHARED QuickInfos() AS STRING
i = 0
DO
i = i + 1
IF MID$(ControlList(i), 5) = Ctrl$ THEN EXIT DO
IF MID$(MenuList(i), 5) = Ctrl$ THEN EXIT DO
LOOP
QuickInfos(i) = InfoText$
END SUB

FUNCTION InputBox$ (Zeilen, Spalten, Text$, Break, MaxDown)
'Break=Zeilenumbruch | MaxDown=Maximale Zeilen
IF Text$ = CHR$(0) THEN HidePasswordChar = -1: Text$ = ""

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Zeilen = 0 THEN Zeilen = TextScale
IF Spalten = 0 THEN Spalten = TextBreak + 1

IF Break = 0 OR Break > Right - Spalten THEN Break = Right - Spalten
IF MaxDown = 0 THEN MaxDown = 24 - Zeilen
TotalBreak = Break
Texta$ = Text$: a$ = ""
GOTO Deletefirst

lesen:
a$ = GetKey

IF a$ = CHR$(13) THEN InputBox$ = Texta$: GOTO delcur
IF a$ = "up" OR a$ = "down" OR a$ = "left" OR a$ = "right" OR a$ = "tab" THEN InputBox$ = Texta$: GOTO delcur
IF a$ = "esc" THEN InputBox$ = Texta$: GOTO delcur
IF a$ = CHR$(8) THEN
IF LEN(Texta$) > 0 THEN Texta$ = LEFT$(Texta$, LEN(Texta$) - 1)
LINE (a, B)-(c + 1, d + 1), 0, BF
a$ = ""
END IF
IF dwn > MaxDown THEN a$ = "": nocur = 1

IF LEN(a$) = 1 THEN Texta$ = Texta$ + a$
Break = LEN(Texta$) + 1

Deletefirst:
Break = LEN(Texta$) + 1
dwn = INT(LEN(Texta$) / TotalBreak) + 1
IF Break > TotalBreak THEN Break = TotalBreak: nocur = 1
IF LEN(Texta$) < (INT(LEN(Texta$) / TotalBreak) + 1) * TotalBreak AND LEN(Texta$) > INT(LEN(Texta$) / TotalBreak) * TotalBreak THEN nocur = 0

a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + Break
EndZeile = Zeilen + INT(LEN(Texta$) / Break + 1)
IF dwn > MaxDown AND MaxDown > 1 THEN EndZeile = EndZeile - 1
IF Break = LEN(Texta$) THEN EndZeile = EndZeile - 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

FrameWindow a, B, c, d, 1


ecr = TotalBreak * -1 + 1
FOR i = 1 TO INT(LEN(Texta$) / TotalBreak) + 1
LOCATE Zeilen + i - 1, Spalten
ecr = ecr + TotalBreak
IF HidePasswordChar THEN PRINT MID$(STRING$(LEN(Texta$), "*"), ecr, TotalBreak);  ELSE PRINT MID$(Texta$, ecr, TotalBreak);
NEXT i
ecr = 0

IF nocur = 0 THEN LINE (POS(0) * 8, CSRLIN * 14)-(POS(0) * 8 - 8, CSRLIN * 14 - 14), InfoColor, BF ELSE nocur = 0
GOTO lesen

delcur:
LINE (POS(0) * 8, CSRLIN * 14)-(POS(0) * 8 - 8, CSRLIN * 14 - 14), 0, BF

END FUNCTION

FUNCTION Inv& (Zahl1 AS LONG)
'Inv: kehrt die Ziffernfolge einer Zahl um und gibt diese als Ergebnis zurÅck

Zahl = Zahl1 'ByVal-Effekt simulieren

'Startwerte definieren:
durch = 10000
m = 1
NeueZahl = 0
too = 0

DO
wert = FIX(Zahl / durch)

IF too OR wert > 0 THEN
        NeueZahl = NeueZahl + (wert * m)
        m = m * 10

        Zahl = Zahl - (wert * durch)
        too = -1
END IF

durch = durch / 10

LOOP UNTIL durch < 1

Inv = NeueZahl

END FUNCTION

FUNCTION IsEmptyBox (Ordner$)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS InOrdner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

TimeLock (1)
Az = TIMER
DO
GET 1
IF RTRIM$(InOrdner$) = Ordner$ THEN IsEmptyBox = 0: TimeLock (0): EXIT FUNCTION

IF TIMER - Az > .5 THEN TimeLock (1): Az = TIMER
LOOP UNTIL Dateiname$ = STRING$(11, 0)

TimeLock (0)

IsEmptyBox = 1'Box ist leer
END FUNCTION

FUNCTION IsExist (Datei$, InOrdner$)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF Datei$ = RTRIM$(OsName$) AND InOrdner$ = RTRIM$(Ordner$) THEN IsExist = 1: EXIT FUNCTION
IF Datei$ = RTRIM$(OsName$) AND InOrdner$ = "" THEN IsExist = 1: EXIT FUNCTION
LOOP UNTIL Dateiname$ = STRING$(11, 0)

IsExist = 0


END FUNCTION

FUNCTION IsExistBox (Box$)

CLOSE 1: OPEN "ORDNER.BNK" FOR RANDOM AS 1
i = 0
DO
i = i + 1
GET 1, i, a$
IF a$ = Box$ THEN IsExistBox = 1: EXIT FUNCTION
LOOP UNTIL a$ = ""

IsExistBox = 0
END FUNCTION

FUNCTION IsInRange% (wert%, min, max)
'PrÅft, ob der wert% im Bereich von min bis max liegt. (Bereichsgrenze inklusive min und max)

IF wert% >= min AND wert% <= max THEN IsInRange% = -1 ELSE IsInRange% = 0

END FUNCTION

FUNCTION IsItNPS$ (Prog$)
'prÅft, ob Prog$ ein NPS-Programm ist, und gibt falls ja runtmp zurÅck,
'falls nein, Prog$
'falls erforderlich, wird das Programm automatisch entschlÅsselt
errorlevel = 0
DIM NPS AS STRING
STATIC stack AS INTEGER


NPS = ""
CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF LCASE$(RTRIM$(Dateiname$)) = LCASE$(Prog$) THEN
IF Attribut$ = "L" THEN
        NPSUnLock Prog$ + ".lao", "runtmp.lao"
        NPS = "runtmp"
        InDrive = 0

ELSE NPS = Prog$
END IF
EXIT DO
END IF
LOOP UNTIL Dateiname$ = STRING$(11, 0)

IF NPS = "" AND stack = 0 THEN
        'Falls Docserver nicht existiert, das ganze fÅr C: Probieren:
        stack = 1
        switchdrive "c"
        NPS = IsItNPS(Prog$)
END IF

IsItNPS = NPS

CLOSE 1
stack = 0

END FUNCTION

SUB LaOSNet
    Dialog "Willkommen im LaOS-Internet!"

CLEAR
CLOSE
ClearControls 3
801

'Ordner laden:
804
InetOPEN "ordner.bnk", "RANDOM", 1
i = 0
DO
i = i + 1
GET 1, i, a$
IF a$ <> "" THEN LoadSymbol "ordner", a$
LOOP UNTIL a$ = ""

805
ShowSymbols

807
GetInp
IF keyb = "insert" THEN Progman Focus, "", ""
IF Focus = "System" THEN GOSUB Menu2: GOTO 806
IF keyb = "esc" THEN 807
IF LCASE$(Focus) = "laos internet" THEN LaOSNet

InetOPEN "dateien.bnk", "RANDOM#400", 1
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS InOrdner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

Register fe2, fe1, fe4, fe3, Focus
TimeLock 1
Az = TIMER
ClearControls (2)
DO
GET 1
IF RTRIM$(InOrdner$) = Focus AND RTRIM$(Art$) = Art1$ OR Art1$ = "" AND RTRIM$(InOrdner$) = Focus THEN
LoadSymbol RTRIM$(Symbol$), RTRIM$(OsName$)
END IF
IF TIMER - Az > .5 THEN TimeLock (1): Az = TIMER
LOOP UNTIL Dateiname$ = STRING$(11, 0)

TimeLock (0)
ord$ = Focus

ShowSymbols

806
GetInp
IF Focus = "System" THEN GOSUB Menu2: GOTO 806
IF keyb = "esc" THEN ord$ = "": DriveName = "": ClearControls 2: Register fe2, fe1, fe4, fe3, DriveName: GOTO 804
IF keyb = "insert" THEN Progman ord$, Focus, MID$(Symbollist(SymbolNr), 11, 8)
Programm$ = DosFile(Focus, ord$, IsNPS%)

startnow2:

IF exist(Programm$ + ".lao") THEN
IF exist("c:\laos\linkway.tmp") THEN KILL "c:\laos\linkway.tmp"
CLOSE : CHAIN Programm$ + ".lao"
ELSE
InetOPEN "dateien.bnk", "RANDOM#400", 1
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS InOrdner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

TimeLock 1
AlteZeit = TIMER
DO
GET 1
IF RTRIM$(Dateiname$) = Programm$ OR RTRIM$(Dateiname$) = DocFile$ THEN
IF RTRIM$(Link$) = "" THEN
Dialog "Diese Datei ist mit keinem Programm verknÅpft. Sie kann nicht alleine ge-   startet werden.": GOTO 850
ELSE
'linken:
DocServer$ = IsItNPS(RTRIM$(Link$))
IF errorlevel OR keyb = "esc" THEN 801

IF exist(DocServer$ + ".lao") THEN
CLOSE 2: OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 2

IF InDrive = 1 THEN Programm$ = lw$ + ":\laos\" + Programm$
PUT 2, 1, Programm$: CLOSE 2
CLOSE : CHAIN DocServer$ + ".lao"
ELSE
IF exist("C:\LAOS\" + DocServer$ + ".lao") THEN

CLOSE 2: OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 2

IF InDrive = 1 THEN Programm$ = lw$ + ":\laos\" + Programm$
PUT 2, 1, Programm$: CLOSE 2

CLOSE : CHAIN "C:\LAOS\" + DocServer$ + ".lao"
ELSE
Dialog "Falsche VerknÅpfung. Die Datei kann nicht gestartet werden.": GOTO 850
END IF
END IF
END IF
END IF
IF TIMER - AlteZeit > .5 THEN TimeLock (1): AlteZeit = TIMER
LOOP UNTIL Dateiname$ = STRING$(11, 0)

Dialog "Es wurde versucht, ein Programm zu laden, welches nicht existiert."
END IF

850
TimeLock 0
GOTO 806

EXIT SUB


Menu2:
a$ = MenuInt("ZurÅck,-,LaOS Beenden", "")
RETURN

END SUB

SUB LastControl
'Wird von GetInp verwendet.

SELECT CASE ControlConnect
CASE 0
ControlNr = ControlNr - 1
IF ControlNr < 1 THEN ControlNr = 1
i = 0
DO
i = i + 1
a$ = ControlList(i)
Activ = MID$(ControlList(ControlNr), 5)
Zeilen = VAL(MID$(a$, 1, 2))
Spalten = VAL(MID$(a$, 3, 2))
Texta$ = MID$(a$, 5)
IF a$ = "" THEN EXIT DO
Control Zeilen, Spalten, Texta$
LOOP UNTIL a$ = ""
CASE 1
MenuNr = MenuNr - 1
IF MenuNr < 1 THEN MenuNr = 1
Activate MenuList(MenuNr)
CASE 2
Symb.pnr = Symb.pnr - 1
SymbolNr = SymbolNr - 1
IF SymbolNr < 1 THEN SymbolNr = 1: Symb.pnr = 1
IF Symb.pnr < 1 THEN

pagerows% = FIX((Down - Top - 4) / (IconVerticalSpace + 4)) + 1

i = SymbolNr
rows% = 0
y2 = VAL(MID$(Symbollist(i), 6, 5))
DO
    i = i - 1
    y = VAL(MID$(Symbollist(i), 6, 5))
    IF y < y2 THEN
        y2 = y
        rows% = rows% + 1
    END IF

LOOP UNTIL rows% >= pagerows%

DimNow = 1
SymbolNr.old = SymbolNr
SymbolNr = SymbolNr - ABS(i - SymbolNr)
IF SymbolNr < 0 THEN SymbolNr = 0
LINE (Left * 8 - 8, Top * 14 - 14)-(Right * 8, Down * 14), 0, BF
ShowSymbols
SymbolNr = SymbolNr.old
Symb.pnr = Symb.count
END IF
Activate Symbollist(SymbolNr)
END SELECT
END SUB

SUB LoadProgram (Ordner$, Programm$)

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS InOrdner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

AlteZeit = TIMER
ds = 0
DO
ds = ds + 1
GET 1, ds
IF RTRIM$(InOrdner$) = Ordner$ AND RTRIM$(OsName$) = Programm$ THEN EXIT DO
IF Dateiname$ = STRING$(11, 0) THEN Dialog "Es wurde versucht, ein Programm zu laden, das nicht existiert.": errorlevel = 1: EXIT SUB
IF TIMER - AlteZeit > .5 THEN TimeLock (1): AlteZeit = TIMER
LOOP

TimeLock (0)

'Program = Dateiname$

CLOSE 1
errorlevel = 0
END SUB

SUB LoadSymbol (Name$, Kennung$)
Symb.total = Symb.total + 1

IF LEN(Kennung$) < 4 THEN Kennung$ = Kennung$ + SPACE$(4 - LEN(Kennung$))


IF Symb.left = 0 THEN Symb.left = Left
IF Symb.top = 0 THEN Symb.top = Top

x = (Symb.left - 2 + LEN(Kennung$) / 2) * 8
y = Symb.top * 14

IF (INT(x / 8)) + 4 - CINT(LEN(Kennung$) / 2) + LEN(Kennung$) > Right THEN
x = (Left - 2 + LEN(Kennung$) / 2) * 8: Symb.left = Left: Symb.top = Symb.top + 4 + IconVerticalSpace: y = Symb.top * 14
END IF

Symb.left = (INT(x / 8)) + 4 - CINT(LEN(Kennung$) / 2) + LEN(Kennung$)

'Symbol eintragen:

x$ = MID$(STR$(x), 2)
IF LEN(x$) < 5 THEN x$ = STRING$(5 - LEN(x$), "0") + x$

y$ = MID$(STR$(y), 2)
IF LEN(y$) < 5 THEN y$ = STRING$(5 - LEN(y$), "0") + y$

IF LEN(Name$) < 8 THEN Name$ = Name$ + STRING$(8 - LEN(Name$), " ")

a$ = x$ + y$ + Name$ + RTRIM$(Kennung$)

i = 0
DO: i = i + 1: LOOP UNTIL Symbollist(i) = ""
Symbollist(i) = a$
SymbolNr = 0
END SUB

SUB MakeWindow (a, B, c, d)

'Fenster:
LINE (a, B)-(c, d), Fensterfarbe, B
LINE (a + 2, B + 2)-(c - 1, d - 1), 0, BF
'Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe

TextScale = INT(B / 14) + 2 '=Zeile
TextBreak = INT(a / 8) + 2 '=Spalte


EBox.left = c - 31: EBox.top = d - 24
Left = INT(a / 8) + 2: Top = INT(B / 14) + 2: Right = INT((c + 8) / 8) - 1: Down = INT((d + 14) / 14) - 1

END SUB

SUB Menuact (Menu$)
Zeilen = VAL(LEFT$(Menu$, 2))
Spalten = VAL(MID$(Menu$, 3, 2))
Caption$ = MID$(Menu$, 5)
a = Spalten * 8 - 11: B = Zeilen * 14 - 17
c = (Spalten + LEN(Caption$)) * 8 - 5: d = (Zeilen + 1) * 14 - 11


IF Activ = Menu$ THEN
LOCATE Zeilen, Spalten: PRINT Caption$
Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
ELSE
LINE (a + 1, B + 1)-(c - 1, B + 1), 0
LINE (a + 1, B + 1)-(a + 1, d - 1), 0
END IF
END SUB

SUB MenuBox (Zeilen, Spalten, Caption$)
STATIC NMZeilen AS INTEGER, NMSpalten AS INTEGER
IF NMZeilen = 0 THEN NMZeilen = Top
IF NMSpalten = 0 THEN NMSpalten = Left
IF Zeilen = 0 THEN Zeilen = NMZeilen
IF Spalten = 0 THEN Spalten = NMSpalten

IF Spalten + LEN(Caption$) - 1 > Right THEN Spalten = Left: Zeilen = Zeilen + 2
IF Zeilen > Down THEN Zeilen = Top: Spalten = Left

a = Spalten * 8 - 11: B = Zeilen * 14 - 17: c = (Spalten + LEN(Caption$)) * 8 - 5
d = (Zeilen + 1) * 14 - 11

'Lîschen:
LINE (a, B)-(c, d), 0, BF
LOCATE Zeilen, Spalten: PRINT Caption$
'Fenster:
LINE (a, B)-(c, d), Fensterfarbe, B
'Schatten:
'LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
'LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe



NMSpalten = Spalten + LEN(Caption$) + 2
NMZeilen = Zeilen
IF NMSpalten > Right THEN NMSpalten = Left: NMZeilen = Zeilen + 2
IF NMZeilen > Down THEN NMZeilen = Top: NMSpalten = Left

'Menu eintragen:
Zeilen$ = MID$(STR$(Zeilen), 2)
Spalten$ = MID$(STR$(Spalten), 2)
IF LEN(Zeilen$) < 2 THEN Zeilen$ = "0" + Zeilen$
IF LEN(Spalten$) < 2 THEN Spalten$ = "0" + Spalten$

a$ = Zeilen$ + Spalten$ + Caption$
i = 0
DO: i = i + 1
IF MenuList(i) = a$ THEN EXIT SUB
LOOP UNTIL MenuList(i) = ""
MenuList(i) = a$
MenuNr = 1


TextScale = Zeilen + 2
END SUB

FUNCTION MenuInt$ (Item$, Trennzeichen$)

Item$ = Item$ + Trennzeichen$

MaxLenth = 1
FOR i = 1 TO UBOUND(Combo, 2)
    IF Combo(2, i) <> "" THEN Combo(2, i) = "" ELSE EXIT FOR
NEXT

i = 0

DO
i = i + 1

a$ = Trim(Item$, Trennzeichen$, i)

IF LEN(a$) > 77 THEN a$ = LEFT$(a$, 77)
IF LEN(a$) > MaxLenth THEN MaxLenth = LEN(a$)

IF a$ <> "" THEN Combo(2, i) = a$


LOOP WHILE a$ <> ""

Zeilen = VAL(LEFT$(MenuList(MenuNr), 2)) + 1
Spalten = VAL(MID$(MenuList(MenuNr), 3, 2))
IF Spalten + MaxLenth > 79 THEN Spalten = 79 - MaxLenth
IF Zeilen + i - 1 > 25 THEN Zeilen = 25 - i + 1
PCOPY 0, 1

a$ = ComboBox(2, Zeilen, Spalten, Zeilen + i - 2, Spalten + MaxLenth)


DO

SELECT CASE keyb
CASE CHR$(13): MenuInt$ = a$: EXIT DO
CASE "esc", "tab": MenuInt$ = "": Choice = 0: keyb = "esc": EXIT DO
END SELECT
LOOP

PCOPY 1, 0
Activate CHR$(0)

END FUNCTION

SUB NewBox (Name$) 'Richtet einen neuen Ordner ein.

CLOSE 1: OPEN "ORDNER.BNK" FOR RANDOM AS 1
i = 0
DO
i = i + 1
GET 1, i, a$
IF a$ = Name$ THEN SOUND 198, 1: Dialog "Es wurde versucht, einen Ordner zu erstellen, welcher schon existiert.": errorlevel = 1: EXIT SUB
LOOP UNTIL a$ = ""

PUT 1, i, Name$
errorlevel = 0

END SUB

SUB NextControl
SELECT CASE ControlConnect
CASE 0
tC = 0
DO
tC = tC + 1
LOOP WHILE ControlList(tC) <> ""
tC = tC - 1

ControlNr = ControlNr + 1
IF ControlNr > tC THEN ControlNr = tC
i = 0
DO
i = i + 1
a$ = ControlList(i)
Activ = MID$(ControlList(ControlNr), 5)
Zeilen = VAL(MID$(a$, 1, 2))
Spalten = VAL(MID$(a$, 3, 2))
Texta$ = MID$(a$, 5)
IF a$ = "" THEN EXIT DO
Control Zeilen, Spalten, Texta$
LOOP UNTIL a$ = ""
CASE 1
tC = 0: DO: tC = tC + 1: LOOP WHILE MenuList(tC) <> ""
MenuNr = MenuNr + 1
IF MenuNr > tC - 1 THEN MenuNr = tC - 1
Activate MenuList(MenuNr)
CASE 2
tC = 0
DO
tC = tC + 1
LOOP WHILE Symbollist(tC) <> ""
SymbolNr = SymbolNr + 1: Symb.pnr = Symb.pnr + 1

IF SymbolNr > tC - 1 THEN
SymbolNr = tC - 1: Symb.pnr = Symb.pnr - 1
ELSE
IF Symb.pnr > Symb.count THEN
DimNow = 1
SymbolNr = SymbolNr - 1: Symb.pnr = Symb.pnr - 1
LINE (Left * 8 - 8, Top * 14 - 14)-(Right * 8, Down * 14), 0, BF
ShowSymbols
END IF
END IF
Activate Symbollist(SymbolNr)
END SELECT
END SUB

SUB NPSUnLock (File$, OutFile$)
'EntschlÅsselt File$ nach OutFile$
'NPS 3. MadeDate 3. Mai 1999 (!)
'Implementiert am 3. Mai 1999

502
Register 8, 8, 14, 50, "GeschÅtzte Datei"
ExitBox -1, -1
echo "Passwort:"
passwort$ = InputBox(11, 10, CHR$(0), 15, 1)
IF keyb = "esc" THEN EXIT SUB


IF LEN(passwort$) > 15 THEN Dialog "Passwort zu lang": GOTO 502
IF LEN(passwort$) < 15 THEN passwort$ = passwort$ + STRING$(15 - LEN(passwort$), " ")

errorlevel = 0
CLOSE

OPEN File$ FOR BINARY AS 1

OPEN "c:\laos\" + OutFile$ FOR BINARY AS 2
CLOSE 2: KILL "c:\laos\" + OutFile$
OPEN "c:\laos\" + OutFile$ FOR BINARY AS 2

DIM pw AS LONG

FOR i = 1 TO LEN(passwort$)
pw = pw + ASC(MID$(passwort$, i, 1)) * i
NEXT

DIM Tresor AS LONG

GET 1, 1, Tresor

IF Tresor <> pw * Inv(pw) THEN Dialog "Passwort falsch.": errorlevel = 1: EXIT SUB


DIM a AS LONG
DIM B AS INTEGER

Register 8, 8, 14, 50, "EntschlÅsseln von " + Focus
echo "Datei wird geschrieben..."

DO WHILE NOT EOF(1)
GET 1, , a
B = a / pw
PUT 2, , B
LOOP

CLOSE

IF errorlevel = 1 THEN Dialog "Ein Fehler ist aufgetreten"

END SUB

SUB PartFormat (Text$, Setting$)

IF INSTR(Text$, CHR$(0)) THEN
    Setting$ = MID$(Text$, INSTR(Text$, CHR$(0)) + 1)
    Text$ = LEFT$(Text$, INSTR(Text$, CHR$(0)) - 1)
ELSE
    Setting$ = ""
END IF

END SUB

SUB Progman (Ordner$, Datei$, Symbol$)
'Dialog Ordner$ + Datei$ + Symbol$

CLOSE
OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 1
PUT 1, 1, Ordner$
PUT 1, 2, Datei$
PUT 1, 3, Symbol$
CLOSE
a$ = IsItNPS$("progman")
IF InDrive THEN SHELL lw$ + ":"
IF errorlevel = 0 THEN CHAIN "c:\laos\" + a$ + ".lao" ELSE keyb = "exit"
END SUB

SUB PruefeShortCut (taste$)
'prÅft taste$, ob sie ein Shortcut ist, und startet in diesem Fall das
'zugehîrige Programm.

CLOSE 1: OPEN "c:\laos\scuts.dat" FOR RANDOM AS 1 LEN = 10
FIELD 1, 8 AS f$, 2 AS s$
i = 0
DO
i = i + 1
GET 1, i

IF RTRIM$(s$) = taste$ THEN
d$ = f$
CLOSE
IF InDrive THEN
    OPEN "c:\laos\execdrv.tmp" FOR RANDOM AS 1
    PUT 1, 1, lw$
END IF
    switchdrive "c"
    Start d$
    EXIT SUB
END IF

LOOP UNTIL f$ = STRING$(8, 0)
CLOSE 1


END SUB

'LÑdt eine zuvor geÑnderte File wieder hinauf zum FTP
SUB PushInetFile (Datei$, DateiModus$, DateiNummer%)

    CLOSE DateiNummer%
    IF DateiNummer% = 0 THEN DateiNummer% = FREEFILE
    DateiModus$ = LCASE$(DateiModus$)

    SELECT CASE DateiModus$
    CASE "input", "binary", "random", "append", "output" 'ok
    CASE "input"
        EXIT SUB 'Muss zuerst gepusht werden (Datei war "Nur lesen" geîffnet)
    CASE ELSE
        Dialog "UngÅltiger Dateimodus angegeben!"
    END SELECT

    OPEN "c:\laos\ftp\transfer.ftp" FOR OUTPUT AS DateiNummer%

    PRINT #DateiNummer%, "laosoft.ch"
    PRINT #DateiNummer%, "mba"
    PRINT #DateiNummer%, "cd LAOS"
    PRINT #DateiNummer%, "lcd c:\LAOS\ftp"

    'FTP-Dateimodus einstellen:
    SELECT CASE DateiModus$
    CASE "output", "append"
        PRINT #DateiNummer%, "ascii"
    CASE ELSE
        PRINT #DateiNummer%, "bin"
    END SELECT

    SELECT CASE DateiModus$
    CASE "binary", "random", "append", "output"
        PRINT #DateiNummer%, "PUT " + UCASE$(Datei$)
    END SELECT
    PRINT #DateiNummer%, "quit"
    CLOSE DateiNummer%

    SHELL "ftp -s:transfer.ftp laosoft.ch"

    'ok!

END SUB

SUB Register (z, s, zu, su, Text$)
IF z = 0 THEN z = 1
IF s = 0 THEN s = 1
IF zu = 0 THEN zu = 25
IF su = 0 THEN su = 80
IF s + LEN(Text$) > su - 1 AND su - s > 2 THEN Text$ = LEFT$(Text$, su - s - 3) + ".."
a = s * 8 - 4
B = z * 14 - 9
c = su * 8 - 7
d = zu * 14 - 7
'Fenster:
LINE (a, B - 4)-(c, d), 0, BF
LINE (a, B)-(c, d), Fensterfarbe, B
'Schatten:
LINE (a + 1, B + 1)-(c - 1, B + 1), Schattenfarbe
LINE (a + 1, B + 1)-(a + 1, d - 1), Schattenfarbe
'Rand:
LINE (c + 1, d + 1)-(a, d + 1), Randfarbe
LINE (c + 1, d + 1)-(c + 1, B), Randfarbe


TextScale = INT(B / 14) + 2 '=Zeile
TextBreak = INT(a / 8) + 2 '=Spalte
LOCATE INT(B / 14) + 1, TextBreak
PRINT Text$
Break = su - 1
IF z = 1 THEN LINE (0, 0)-(638, 0), Fensterfarbe: LINE (1, 1)-(637, 1), 0: LINE (1, 1)-(1, 347), 0
EBox.left = c - 31: EBox.top = d - 24
Left = s + 1: Top = z + 1: Right = su - 1: Down = zu - 1
END SUB

SUB RestoreScreen

'  *****************    Lade nun das Bild neu
' Methode von Thomas Antoni

pic$ = "temp"

    DEF SEG = &HA000
    FOR i% = 0 TO 3
        OUT &H3C4, 2
        OUT &H3C5, 2 ^ i%
        BLOAD "c:\laos\" + pic$ + CHR$(49 + i%), 0
    NEXT

END SUB

SUB SaveScreen

pic$ = "temp"

bytes = 28000  'Screen 12: 38400, Screen 9: 640*350/2/4 (2 Pixel in 4 Bit Farbe = 1 Byte) = 28000

' *********************  Hier wird das Bild gespeichert
IF bytes THEN
  DEF SEG = &HA000
    FOR i% = 0 TO 3
      OUT &H3CE, 4
      OUT &H3CF, i%
      BSAVE "c:\laos\" + pic$ + CHR$(49 + i%), 0, bytes
   NEXT
  DEF SEG
END IF

END SUB

FUNCTION SDate$ 'Function fÅr das System-Datum
SELECT CASE Datumsformat
CASE 0'TT.MM.JJJJ
SDate$ = MID$(DATE$, 4, 2) + "." + LEFT$(DATE$, 2) + "." + RIGHT$(DATE$, 4)
CASE 1'MM-TT-JJJJ
SDate$ = DATE$
CASE 2'JJJJ/MM/TT
SDate$ = RIGHT$(DATE$, 4) + "/" + LEFT$(DATE$, 2) + "/" + MID$(DATE$, 4, 2)
END SELECT
END FUNCTION

FUNCTION ShortCutName$ (t$)
taste$ = t$ 'ByVal-Effekt simulieren

IF LEN(taste$) = 2 THEN
'erweiterte Tastaturcodes: (#0+xxx)

taste$ = MID$(taste$, 2)
t% = ASC(taste$)


'f?
IF IsInRange(t%, 59, 68) THEN a$ = "F" + LTRIM$(STR$(t% - 58))
IF IsInRange(t%, 133, 134) THEN a$ = "F" + LTRIM$(STR$(10 + t% - 132))


'Shift + f?
IF IsInRange(t%, 84, 93) THEN a$ = "Shift + F" + LTRIM$(STR$(t% - 83))
IF IsInRange(t%, 135, 136) THEN a$ = "Shift + F" + LTRIM$(STR$(10 + t% - 134))


'Ctrl + f?
IF IsInRange(t%, 94, 103) THEN a$ = "Ctrl + F" + LTRIM$(STR$(t% - 93))
IF IsInRange(t%, 137, 138) THEN a$ = "Ctrl + F" + LTRIM$(STR$(10 + t% - 136))


'Alt + f?
IF IsInRange(t%, 104, 113) THEN a$ = "Alt + F" + LTRIM$(STR$(t% - 103))
IF IsInRange(t%, 139, 140) THEN a$ = "Alt + F" + LTRIM$(STR$(10 + t% - 138))

IF a$ = "" THEN
SELECT CASE t%
CASE 115: a$ = "Ctrl + Linkspfeil"
CASE 116: a$ = "Ctrl + Rechtspfeil"
CASE 72: a$ = "Pfeil nach Oben"
CASE 80: a$ = "Pfeil nach Unten"
CASE 75: a$ = "Pfeil nach Links"
CASE 77: a$ = "Pfeil nach Rechts"
CASE 127: a$ = "Ctrl + Backspace"
CASE 82: a$ = "Insert"
CASE 83: a$ = "Delete"
CASE 71: a$ = "Home"
CASE 79: a$ = "End"
CASE 73: a$ = "Page Up"
CASE 81: a$ = "Page Down"
CASE 15: a$ = "Shift + Tabulator"

CASE ELSE: a$ = STR$(ASC(taste$))
END SELECT



END IF

ELSE
'einfache Zeichen:

SELECT CASE ASC(taste$)
CASE 27: a$ = "Escape"
CASE 13: a$ = "Eingabetaste"
CASE 32: a$ = "Leertaste"
CASE 9: a$ = "Tabulator"
CASE 8: a$ = "RÅcktaste"

CASE ELSE: a$ = taste$
END SELECT

END IF


ShortCutName$ = a$

END FUNCTION

SUB ShowInfo
IF ControlConnect = 2 THEN EXIT SUB
i = 0
DO
i = i + 1
IF i > 500 THEN EXIT SUB
IF MID$(ControlList(i), 5) = Activ OR MenuList(i) = Activ THEN EXIT DO
LOOP
IF QuickInfos(i) = "" THEN EXIT SUB
IF Activ = MenuList(i) THEN
z = VAL(LEFT$(MenuList(i), 2)) + 1 'Menu
s = VAL(MID$(MenuList(i), 3, 2)) + 1
ELSE
z = VAL(LEFT$(ControlList(i), 2)) + 1 'Control
s = VAL(MID$(ControlList(i), 3, 2)) + 1
END IF
IF s > Right THEN s = Right - LEN(MID$(ControlList(i), 5))

IF z + INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1 > Down THEN z = Down - (INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1) + 1
IF z < Top THEN
s = Left + 1
z = Down - INT(LEN(QuickInfos(i)) / (Right - s + 1))
END IF

REDIM BG(4 * 8 * 14 * 5) AS DOUBLE
GET (s * 8 - 9, z * 14 - 15)-(s * 8 + (Right - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1) * 14), BG
LINE (s * 8 - 9, z * 14 - 15)-(s * 8 + (Right - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1) * 14), 0, BF
LINE (s * 8 - 9, z * 14 - 15)-(s * 8 + (Right - s) * 8, z * 14 - 14 + (INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1) * 14), InfoFrame, B


ecr = (Right - s + 1) * -1 + 1
FOR a = 1 TO INT(LEN(QuickInfos(i)) / (Right - s + 1)) + 1
LOCATE z + a - 1, s
ecr = ecr + Right - s + 1
COLOR InfoColor: PRINT MID$(QuickInfos(i), ecr, Right - s + 1)
NEXT
COLOR Farbe
ecr = 0
a = 0
i = 0
SLEEP
PUT (s * 8 - 9, z * 14 - 15), BG, PSET
END SUB

SUB ShowSymbols
Symb.count = 0
Symbol = SymbolNr
DIM Befehle(2500, 5) AS SINGLE
DIM i AS INTEGER
DIM Nr AS INTEGER
DIM LastSymbol AS STRING


IF Symbol = 0 THEN DimY = 0

DO
Symbol = Symbol + 1
Symb.count = Symb.count + 1

a$ = Symbollist(Symbol)
IF a$ = "" THEN EXIT DO

Name$ = MID$(a$, 11, 8)
Kennung$ = MID$(a$, 19)

IF LEN(Kennung$) < 4 THEN Kennung$ = STRING$(INT((4 - LEN(Kennung$)) / 2), " ") + Kennung$ + STRING$(CINT((4 - LEN(Kennung$)) / 2 + .1), " ")

x = VAL(LEFT$(a$, 5))
y = VAL(MID$(a$, 6, 5))
IF DimNow = 1 THEN DimY = y - Top * 14: DimNow = 0
y = y - DimY
IF y > Down * 14 - 46 THEN DimNow = 1: EXIT DO'DimY = DimY + y - Top * 14: EXIT DO

VIEW (x, y)-(x + 40, y + 40), SymbolColor, SymbolFrame
Spalten = (INT(x / 8)) + 4 - CINT(LEN(Kennung$) / 2)
IF Spalten <= 1 THEN Spalten = 2
IF Spalten + LEN(Kennung$) > Right THEN Spalten = Right - LEN(Kennung$)
IF Spalten <= Left THEN Kennung$ = LEFT$(Kennung$, LEN(Kennung$) - ((Left - Spalten) + 1)): Spalten = Left + 1
LOCATE INT((y + 40) / 14) + 2, Spalten: PRINT Kennung$
Symb.left = Spalten + LEN(Kennung$)

IF LastSymbol <> Name$ THEN
LastSymbol = Name$

TA = 0
CLOSE 1: OPEN RTRIM$(Name$) + ".lsb" FOR BINARY AS 1: GET 1, 1, TA
IF TA = 0 THEN
    Name$ = "c:\laos\" + Name$
    CLOSE 1: OPEN RTRIM$(Name$) + ".lsb" FOR BINARY AS 1: GET 1, 1, TA
    IF TA = 0 THEN Name$ = "c:\laos\sys"
END IF

CLOSE 1: OPEN Name$ + ".lsb" FOR BINARY AS 1

i = 0
DO
i = i + 1
FOR Nr = 0 TO 5
GET 1, , Befehle(i, Nr)
NEXT
LOOP WHILE Befehle(i, 0) <> 0

i = 0
DO
i = i + 1
SELECT CASE CHR$(Befehle(i, 0))
CASE "p": PSET (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), Befehle(i, 3)
CASE "m": PAINT (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), Befehle(i, 3), Befehle(i, 4)
CASE "l": LINE (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus)-(Befehle(i, 3) + xplus, Befehle(i, 4) + yplus), Befehle(i, 5)
CASE "r": LINE (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus)-(Befehle(i, 3) + xplus, Befehle(i, 4) + yplus), Befehle(i, 5), B
CASE "b": LINE (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus)-(Befehle(i, 3) + xplus, Befehle(i, 4) + yplus), Befehle(i, 5), BF
CASE "k": CIRCLE (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), Befehle(i, 3), Befehle(i, 4), , , Befehle(i, 5)
CASE "x": CIRCLE (Befehle(i, 1) + xplus, Befehle(i, 2) + yplus), Befehle(i, 3), Befehle(i, 4), Befehle(i + 1, 1), Befehle(i + 1, 2), Befehle(i, 5)
          i = i + 1
END SELECT
LOOP WHILE Befehle(i, 0) <> 0
i = 0
    GET (0, 0)-(40, 40), Hintergrund
ELSE
    PUT (0, 0), Hintergrund, PSET
END IF

VIEW 'Setzt das Darstellungsfeld wieder auf den ganzen Bildschirm.
LOOP
IF a$ <> "" THEN Symb.ipp = Symb.count - 1
Symb.total = Symbol - 1
Symb.count = Symb.count - 1

ControlConnect = 2 'Setzt den Verweis auf das Symbol-Steuerelement
SymbolNr = Symb.total - Symb.count + 1
Symb.pnr = 1
END SUB

SUB SortCombo (Nr%, ab%)

Anz% = 0
FOR Anz% = 1 TO UBOUND(Combo, 2)
    IF Combo(Nr%, Anz%) = "" THEN EXIT FOR
NEXT
Anz% = Anz% - 1

FOR a% = ab% TO Anz% - 1
    FOR B% = a% + 1 TO Anz%
        IF Combo(Nr%, a%) > Combo(Nr%, B%) THEN SWAP Combo(Nr%, a%), Combo(Nr%, B%)
    NEXT
NEXT



END SUB

SUB Start (Programm$)
Programm$ = RTRIM$(Programm$)

DFile Programm$, IsNPS%

'prÅfen, ob NPS-Decodiert:

DocFile$ = ""
IF IsNPS% THEN
        ext$ = ""
        IF exist(Programm$ + ".lao") THEN ext$ = ".lao"
        NPSUnLock Programm$ + ext$, "runtmp" + ext$

        InDrive = 0: DocFile$ = Programm$: Programm$ = "runtmp"

        IF errorlevel OR keyb = "esc" THEN keyb = "tab": EXIT SUB
END IF

IF exist(Programm$ + ".lao") THEN
IF exist("c:\laos\linkway.tmp") THEN KILL "c:\laos\linkway.tmp"
CLOSE : CHAIN Programm$ + ".lao"
ELSE
CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS InOrdner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

TimeLock 1
AlteZeit = TIMER
DO
GET 1
IF RTRIM$(Dateiname$) = Programm$ OR RTRIM$(Dateiname$) = DocFile$ THEN
IF RTRIM$(Link$) = "" THEN
Dialog "Diese Datei ist mit keinem Programm verknÅpft. Sie kann nicht alleine ge-   startet werden.": GOTO 750
ELSE
'linken:
DocServer$ = IsItNPS(RTRIM$(Link$))
IF errorlevel OR keyb = "esc" THEN keyb = "tab": EXIT SUB

IF exist(DocServer$ + ".lao") THEN
CLOSE 2: OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 2

IF InDrive = 1 THEN Programm$ = lw$ + ":\laos\" + Programm$
PUT 2, 1, Programm$: CLOSE 2
CLOSE : CHAIN DocServer$ + ".lao"
ELSE
IF exist("C:\LAOS\" + DocServer$ + ".lao") THEN

CLOSE 2: OPEN "c:\laos\linkway.tmp" FOR RANDOM AS 2

IF InDrive = 1 THEN Programm$ = lw$ + ":\laos\" + Programm$
PUT 2, 1, Programm$: CLOSE 2

CLOSE : CHAIN "C:\LAOS\" + DocServer$ + ".lao"
ELSE
Dialog "Falsche VerknÅpfung. Die Datei kann nicht gestartet werden.": GOTO 750
END IF
END IF
END IF
END IF
IF TIMER - AlteZeit > .5 THEN TimeLock (1): AlteZeit = TIMER
LOOP UNTIL Dateiname$ = STRING$(11, 0)

Dialog "Es wurde versucht, ein Programm zu laden, welches nicht existiert."
END IF

750
TimeLock 0
EXIT SUB


END SUB

SUB StatusBar (tex$, Nxt%, klein%)
STATIC eval%

IF Nxt% = 0 THEN eval% = 1: Nxt% = 1 ELSE eval% = eval% + 1

DIM Text AS STRING * 72

IF eval% > LEN(RTRIM$(tex$)) THEN eval% = 1: StoppUhr(3) = 2

IF klein% AND LEN(RTRIM$(tex$)) <= 65 THEN
    eval% = 1
ELSEIF NOT klein% AND LEN(RTRIM$(tex$)) <= 72 THEN
    eval% = 1
END IF

Text = MID$(tex$, eval%, 72)

IF RTRIM$(tex$) = "" THEN LINE (13, 320)-(595, 337), 0, BF: EXIT SUB

LOCATE 24, 3


IF klein% THEN

    LINE (13, 320)-(539, 337), Farbe, B

    PRINT LEFT$(Text$, 65);

    'Ecke:
        LINE STEP(-6, 0)-(539, 330), Farbe
        LINE (531, 337)-(539, 329), Farbe
        PAINT (537, 335), InfoColor, Farbe

ELSE

    LINE (13, 320)-(595, 337), Farbe, B

    PRINT Text$;

    'Ecke:
        LINE STEP(-6, 0)-(595, 330), Farbe
        LINE (587, 337)-(595, 329), Farbe
        PAINT (593, 335), InfoColor, Farbe

END IF

END SUB

SUB StoppUhrBeep (Uhr)
STATIC IsNPS%

SELECT CASE Uhr
CASE 1: TimeLock (1)
CASE 2: SCREEN , , 1, 1: COLOR , 0: CLS
a$ = "": DO: a$ = INKEY$: LOOP WHILE a$ = ""
SCREEN , , 0, 0: COLOR Farbe, Hintergrundfarbe

CASE 3
IF ControlConnect = 2 AND ShowFileInfo% THEN
    IF ord$ <> "" AND RTRIM$(DateiInfo) <> "" THEN StoppUhr(3) = .25: StatusBar DateiInfo, 1, IsNPS%
END IF

CASE 5

IF ord$ <> "" THEN
HaltOnKeyboard = 1


a$ = DosFile(MID$(Symbollist(SymbolNr), 19), ord$, IsNPS%)

StatusBar "", 0, 0
IF IsNPS% THEN DrawSchloss ELSE VIEW (540, 290)-(580, 342), 0

VIEW

IF ShowFileInfo% AND RTRIM$(DateiInfo) <> "" THEN StatusBar DateiInfo, 0, IsNPS%: StoppUhr(3) = 2

ELSE
    StatusBar "", 0, 0
    VIEW (540, 290)-(580, 342), 0
    VIEW
END IF
END SELECT
END SUB

SUB switchdrive (lw$)
IF lw$ = "c" THEN SHELL "c:": CHDIR "\laos": EXIT SUB

ON ERROR GOTO fehlernext
errorlevel = 0
SHELL lw$ + ":"
CHDIR "\laos"

IF errorlevel = 0 THEN

CLOSE 1: OPEN "c:\laos\drvlist.bnk" FOR RANDOM AS 1
   
    i = 0
    DO
        i = i + 1
        GET 1, i, a$
    LOOP UNTIL LEFT$(a$, 1) = lw$ OR a$ = ""

CLOSE 1: OPEN lw$ + ":\laos\medname.inf" FOR RANDOM AS 1
GET 1, 1, DriveName
IF DriveName = "" THEN DriveName = MID$(a$, 4)

END IF

END SUB

SUB Symbact (Symbol$) 'Aktiviert ein Symbol
x = VAL(MID$(Symbol$, 1, 5))
y = VAL(MID$(Symbol$, 6, 5))
IF DimNow = 1 THEN DimY = DimY + (y - Top * 14): DimNow = 0
y = y - DimY

IF Activ = Symbol$ THEN
LINE (x - 2, y - 2)-(x + 42, y + 42), Symbolactiv, B, SymbactStil
ELSE
IF y >= Top * 14 AND y < Down * 14 - 46 THEN LINE (x - 2, y - 2)-(x + 42, y + 42), 0, B, SymbactStil
END IF
END SUB

SUB TextBox (Zeilen, Spalten, Text$, Break)
'Break heisst hier: Hier wird der Text gebrochen (Zeilenumbruch)

IF TextScale = 0 THEN TextScale = Top
IF TextBreak = 0 THEN TextBreak = Left
IF Zeilen = 0 THEN Zeilen = TextScale
IF Spalten = 0 THEN Spalten = TextBreak + 1

IF Zeilen > 24 THEN Zeilen = 24

IF Spalten + LEN(Text$) > Right AND Break = 0 THEN Break = Right - Spalten
IF Break = 0 THEN Break = LEN(Text$)
IF Break = 0 THEN Spalten = Spalten - 1: Break = 1


a = Spalten * 8 - 11
B = Zeilen * 14 - 17
EndSpalte = Spalten + Break
EndZeile = Zeilen + INT(LEN(Text$) / Break + 1)
IF Break = LEN(Text$) THEN EndZeile = EndZeile - 1
c = EndSpalte * 8 - 5
d = EndZeile * 14 - 11

FrameWindow a, B, c, d, 1

ecr = Break * -1 + 1
FOR i = 1 TO INT(LEN(Text$) / Break) + 1
LOCATE Zeilen + i - 1, Spalten
ecr = ecr + Break
PRINT MID$(Text$, ecr, Break)
NEXT i
ecr = 0


TextScale = EndZeile + 1
END SUB

SUB TimeLock (Status)
STATIC On.too
STATIC tm
Radius = 14 * 2
Farbe1 = 12
Farbe2 = 9
x = 319 - Radius
y = 174 - Radius

SELECT CASE Status
CASE 1:
'Falls es das erste Mal ist, muss der Hintergrund noch gespeichert werden:
IF On.too = 0 THEN On.too = 1: GET (319 - Radius, 174 - Radius)-(319 + Radius, 174 + Radius), Hintergrund

LINE (x + Radius - Radius, y + Radius - Radius)-(x + Radius + Radius, y + Radius + Radius), 0, BF
IF tm = 1 THEN tm = 0 ELSE tm = 1

IF tm = 0 THEN
LINE (x + Radius / 3 * 2, y + Radius / 3 * 2)-(x + Radius / 3 * 4, y + Radius / 3 * 4), Farbe1
LINE (x + Radius / 3 * 2, y + Radius / 3 * 4)-(x + Radius / 3 * 4, y + Radius / 3 * 2), Farbe1
CIRCLE (x + Radius, y + Radius), Radius / 2, Farbe1, , , .9
PAINT (x + Radius + 1, y + Radius), Farbe1
PAINT (x + Radius - 1, y + Radius), Farbe1
END IF


IF tm = 1 THEN
LINE (x + Radius / 3 * 2, y + Radius / 3 * 2)-(x + Radius / 3 * 4, y + Radius / 3 * 4), Farbe2
LINE (x + Radius / 3 * 2, y + Radius / 3 * 4)-(x + Radius / 3 * 4, y + Radius / 3 * 2), Farbe2
CIRCLE (x + Radius, y + Radius), Radius / 2, Farbe2, , , .9
PAINT (x + Radius, y + Radius + 1), Farbe2
PAINT (x + Radius, y + Radius - 1), Farbe2
END IF
CASE 0: IF On.too <> 0 THEN On.too = 0: PUT (319 - Radius, 174 - Radius), Hintergrund, PSET: StoppUhr(1) = 0
END SELECT
END SUB

FUNCTION TrashVoll%

TrashVoll% = 0

CLOSE 1: OPEN "DATEIEN.BNK" FOR RANDOM AS #1 LEN = 400
FIELD 1, 11 AS Dateiname$, 16 AS Art$, 11 AS Symbol$, 32 AS OsName$, 32 AS Ordner$, 11 AS Link$, 10 AS Datum$, 5 AS Zeit$, 1 AS Attribut$, 256 AS FileInfo$

a = 0
DO
a = a + 1
GET 1, a
IF HaltOnKeyboard THEN
key$ = INKEY$
IF key$ <> "" THEN EXIT FUNCTION
END IF

IF RTRIM$(Ordner$) = "Papierkorb" THEN TrashVoll% = -1
LOOP UNTIL Dateiname$ = STRING$(11, 0)

END FUNCTION

FUNCTION Trim$ (Variable$, Trennzeichen$, a)

mcr = 0
gef = 0
FOR i = 1 TO LEN(Variable$)
mcr = mcr + 1
a$ = MID$(Variable$, mcr)
IF LEFT$(a$, 1) = Trennzeichen$ THEN gef = gef + 1
IF gef = a - 1 THEN EXIT FOR
NEXT
IF LEFT$(a$, 1) = Trennzeichen$ THEN a$ = MID$(a$, 2) ELSE a$ = a$

mcr = 0
FOR i = 1 TO LEN(a$)
mcr = mcr + 1
IF MID$(a$, mcr, 1) = Trennzeichen$ THEN a$ = LEFT$(a$, mcr - 1): EXIT FOR
NEXT

Trim$ = a$

END FUNCTION

