DECLARE SUB zEditorHandler (sGroup$, iCtrl%, iRefresh%, dblClicked%)
DECLARE SUB Editor (sGroup$, sName$, file$, x%, y%, x2%, y2%, w%, h%)
DECLARE FUNCTION SaveDialog$ (PrgStarter$, icon$)
'Lege hier die maximale Anzahl Controls und Strings (Editboxen) fest:
CONST MaxControls = 20, MaxStrings = 6

'Snowflake GUI v.0.2.7
'28.12.2003
'Entwickelt fÅr LaOS 7
'Dies ist OpenSource
'Programmierer, die an diesem Code mitgearbeitet haben, schreiben sich bitte
'hier ein, wenn mîglich mit kurzer Beschreibung der Arbeit:
'------------------------------------------------------------
'Urs Langmeier (Snowflake Standard-GUI)

CONST MaxIntArraySize = 50    'Legt die maximale Integer-Datenfeldgrîsse fest
                              'Standard = 4456

DECLARE SUB IconList (sGroup$, sName$, x%, y%, x2%, y2%, iScrollBars%)
DECLARE SUB RaiseKeyPress (iCtrl%, sKey AS STRING)
DECLARE FUNCTION DosSaveDialog$ (DateiSuffix$)
DECLARE FUNCTION DosOpenDialog$ (DateiSuffix$)
DECLARE SUB ProgressBar (Text$, Stat AS SINGLE)
DECLARE SUB Menu (sGroup$, caption$)
DECLARE SUB Main ()
DECLARE SUB CloseFrame (sGroup$)
DECLARE SUB RaiseMouseUp (iCtrl%, name$, iButton%)
DECLARE SUB zDrawMouse (x%, y%)
DECLARE SUB zSwapFocus (iAlt%)
DECLARE SUB SavePicture (x%, y%, x2%, y2%, file$)
DECLARE FUNCTION LinkedFile$ ()
DECLARE FUNCTION GetCtrl% (name$)
DECLARE FUNCTION OpenDialog$ (PrgStarter$)
DECLARE FUNCTION ColorChoice% (col%)
DECLARE FUNCTION ChooseIcon$ (MarkedIcon$)
DECLARE FUNCTION IsNull% (Text$)
DECLARE SUB zHandleControls (iCtrl%, sGroup$, iChanges%)
DECLARE FUNCTION MenuInt$ (items AS STRING)
DECLARE SUB RaiseMouseMove (iCtrl%, name$, iButton%)
DECLARE SUB RaiseGotFocus (iOldCtrl%, iCtrl%, name$)
DECLARE SUB AddIcon (sName$, Index%, IconFile$, caption$, Value%)
DECLARE SUB RaiseToolTipText (iCtrl%, name$)
DECLARE FUNCTION zQuitKey% ()
DECLARE SUB RaisePopupMenu (iCtrl%, name$, ExitInput%)
DECLARE SUB zRspm (iCtrl%)
DECLARE SUB Frame (sGroup$, caption$, x%, y%, x2%, y2%)
DECLARE SUB zDoEvents ()
DECLARE SUB FullScreen ()
DECLARE SUB ClearList (sName$)
DECLARE SUB ExitBox (sGroup$, x%, y%)
DECLARE SUB EditBox (sGroup$, sName$, x%, y%, iAnzahlSpalten%, iAnzahlZeilen%, iMaxZeichen%)
DECLARE SUB CLW (sGroup$)
DECLARE FUNCTION exists% (FilePath$)
DECLARE SUB TimeLock (modus%)
DECLARE SUB zDoMouse ()
DECLARE SUB ScrollBar (sGroup$, sName$, x%, y%, w%, h%, Min%, max%, stp%, iCtrl%)
DECLARE SUB zEventDblClick (iCtrl%)
DECLARE SUB ListAdd (sName$, Index%, Text AS STRING, Value%)
DECLARE SUB zGetMouse (b%, x%, y%)
DECLARE FUNCTION SysDate$ (d$)
DECLARE SUB Label (x%, y%, x2%, y2%, Text AS STRING, Alignment%, bgColor%)
DECLARE SUB zToolTipText (Text$)
DECLARE SUB zScreenSaver ()
DECLARE SUB zDrawCtrl (iCtrl%, MouseDown%, Refresh%)
DECLARE SUB RaiseScroll (iCtrl%, name$)
DECLARE SUB zReplace (Text$, find$, replce$)
DECLARE SUB zSetMouseCursor (sCursor$)
DECLARE SUB PutValue (objekt$, wert%)
DECLARE SUB zEditBoxHandler (sGroup$, sName$, iCtrl%, x1%, y1%, x2%, y2%, iAnzSpalten%, iAnzZeilen%, Text AS STRING, Refresh%)
DECLARE SUB zIconListHandler (sGroup$, iCtrl%, Refresh%)
DECLARE SUB zRessourceSet (sGroup$, Index%, Text$, Value%)
DECLARE SUB ListBox (sGroup$, sName$, x%, y%, x2%, y2%, WithCheckBoxes%)
DECLARE SUB zRessourceGet (sGroup$, Index%, Text$, Value%)
DECLARE SUB zCreateControl (iCtrlType%, sGroup$, sName$, caption$, Value%, x%, y%, x2%, y2%, w%, h%, tag$, iSelfHandler%)
DECLARE SUB zSetOrigPalette ()
DECLARE FUNCTION TextLen% (Text AS STRING)
DECLARE SUB GetValue (objekt$, wert%)
DECLARE SUB MouseClicker (sGroup$, sName$, x%, y%, x2%, y2%, sSimulateKey AS STRING, iWithShadow%)
DECLARE FUNCTION TextHeight% (Text AS STRING)
DECLARE SUB zListboxHandler (sGroup$, iCtrl%, x%, y%, x2%, y2%, iAnzSpalten%, iAnzZeilen%, RefreshOnly%)
DECLARE FUNCTION TextWidth% (Text AS STRING)
DECLARE SUB zRGBPalette (slot%, r%, g%, b%)
DECLARE SUB ShowPicture (xx%, yy%, sFileName AS STRING)
DECLARE SUB RestoreScreen ()
DECLARE SUB zEventMouseMove (iCtrl%, iButton%)
DECLARE SUB SilentScreen ()
DECLARE SUB zEventClick (iCtrl%, iButton%)
DECLARE SUB zEventMouseDown (iCtrl%, iButton%)
DECLARE FUNCTION zLauscheEvents% (sGroup$)
DECLARE FUNCTION zSelfHandleControl% (sGroup$, iCtrlType%)
DECLARE FUNCTION zGK$ (sGroup$)
DECLARE SUB PutString (objekt$, Text$)
DECLARE SUB GetString (objekt$, Text$)
DECLARE SUB zTimeEventHandler (nr%)
DECLARE SUB zMoveScreen (x1%, y1%, x2%, y2%, ToX%, ToY%)
DECLARE SUB ClearCtrls (sGroup$)
DECLARE SUB zDrawBox (x1%, y1%, x2%, y2%)
DECLARE SUB DrawCtrls (sGroup$)
DECLARE SUB Button (sGroup$, caption$, x%, y%)
DECLARE SUB EchoXY (x%, y%, Text AS STRING)
DECLARE SUB zNextCtrl (sGroup$)
DECLARE SUB zLastControl (sGroup$)
DECLARE SUB Echo (Text AS STRING)
DECLARE SUB GetInp (sGroup$)
DECLARE SUB Dialog (Text AS STRING)

'Initialisierungen:
'$STATIC
DEFINT A-Z
CLEAR , , 8000
CLOSE
ON ERROR GOTO errHandler
SCREEN 9, , 0, 0
zSetOrigPalette
VIEW PRINT 1 TO 25
WIDTH 80, 25
VIEW

'GUI-Konfigurationsvariablen:
DIM SHARED zShowTimeExitBox

DIM SHARED CurrentX, CurrentY   'Die aktuelle Cursor-Position
DIM SHARED choice               'Die Wahl des Benutzers (MenuInt)

'Datentypen fÅr die LaOS-GUI:
TYPE zControlType
    Value       AS INTEGER      'Der Wert
    caption     AS STRING * 40  'Die Beschreibung des Controls
    Group       AS STRING * 8   'Die Steuerelement-Gruppe, zu der das Control gehîrt
    x           AS INTEGER      'X-Position
    y           AS INTEGER      'Y-Position
    x2          AS INTEGER      'Position
    y2          AS INTEGER      'Position
    w           AS INTEGER      'Kann fÅr die Anz Zeichen (horizontal) verwendet werden
    h           AS INTEGER      'Kann fÅr die Anz Zeichen (vertikal) verwendet werden
    tag         AS STRING * 12  'Unterschiedliche Verwendung
    CtrlType    AS INTEGER      'Der Control-Typ (zwingend!)
    SelfHandler AS INTEGER      '-1 = Das Control handelt sich selbst! (aufwÑndige Controls)
END TYPE

TYPE zRessourceType
    Value       AS INTEGER
    Text        AS STRING * 80  'Text
END TYPE

TYPE zMouseType
    x           AS INTEGER
    y           AS INTEGER
    Cursor      AS INTEGER '0 = normal, 1 = Textcursor
    Speed       AS INTEGER
    MouseOn     AS INTEGER
    Clicked     AS INTEGER '1 = linke Taste geclickt, 2 = rechte Taste
    Down        AS INTEGER '1 = linke Taste unten, 2 = rechte Taste
    Oldx        AS INTEGER
    Oldy        AS INTEGER
    time        AS SINGLE  'Die Zeit, zu der die letzte Bewegung gemacht wurde
    ClickX      AS INTEGER 'Letzter Click X
    ClickY      AS INTEGER 'Letzter Click Y
    dx          AS INTEGER 'In RaiseMouseMove abrufbar:
    dy          AS INTEGER 'Koordinaten des MouseDown
END TYPE

TYPE zTiEvType
  inc AS SINGLE
  nextime AS SINGLE
END TYPE

TYPE startype
    x AS INTEGER
    y AS INTEGER
    col AS INTEGER
    float AS SINGLE 'Grîsse
END TYPE

'Konstanten fÅr CtrlType (Steuerelement-Typen):
'Controls mit Tabstopp:
CONST zMenu = 1
CONST zButton = 2
CONST zRadioButton = 3
CONST zCheckBox = 4
CONST zListBox = 5
CONST zEditBox = 6
CONST zIconList = 7
CONST zExitFocus = 8
CONST zEditor = 9

'Controltypen ohne Tabstopp sind hîher als zControlTypeCount!
CONST zControlTypeCount = 9

'Controls ohne Tabstop:
CONST zScrollBar = 97
CONST zMouseClicker = 98

'Variablen und Datenfelder fÅr die LaOS-Technlologien:
DIM SHARED laosdir            AS STRING   'Das LaOS-Heimatverzeichnis
DIM SHARED tempdir            AS STRING   'Das Verzeichnis fÅr temporÑre Dateien

'Variablen und Datenfelder fÅr die LaOS-GUI:
'Maximale Mauszeigerdimensionen: 15x18 Pixels!
DIM SHARED Controls(1 TO MaxControls) AS zControlType, zStrings(1 TO MaxStrings) AS STRING, zTimeEvent(1) AS zTiEvType, zMCS AS STRING, zMHG(80), iMS(45), Mouse AS zMouseType, SilentScreenMode, SaveScreenMode, zSSMStop, left, Top, Right, Down,  _
TextAlignment, zMouseDown

DIM SHARED NextCtrl
  'Das jeweils nÑchste Control
DIM SHARED keyb AS STRING
  'Speichert die zuletzt gedrÅckte Taste
DIM SHARED Focus AS STRING
  'Das Control mit dem Fokus
DIM SHARED crlf AS STRING * 2
  'WagenrÅcklauf & Zeilenvorschub
DIM SHARED ActiveCtrl
  'Die ControlNr des aktivierten Steuerelementes
DIM SHARED DefaultCtrl, errorlevel
DIM SHARED ShortKeys AS STRING

'Config:
DIM SHARED MouseOn, Dateformat, zsson
OPEN "c:\laos7\cfg.cfg" FOR INPUT AS 1
INPUT #1, MouseOn, Mouse.Speed, Dateformat, zsson, tempdir, laosdir
CLOSE 1

'Variablen fÅr das Farbschema:
DIM SHARED Farbe          'lokal, aktive Farbe
DIM SHARED InversFarbe    'lokal, aktive inverse HG-Farbe
DIM SHARED tcol, tcol2, tcol3, focol, bcol, mfcol, mtcol, wincol, shdcol, frmcol, icfcol, btfcol, btscol, btrcol, btlcol, bttcol, lbicol, lbacol, tbfcol, tbtcol, sbbcol, sbfcol, sbscol, sbrcol, ttfcol, ttrcol, tttcol, frmlcol, lbfcol, tbbcol, lbtcol _
, stcol, sfcol

'Farbschema laden und setzen:
errorlevel = 0
OPEN laosdir + "colors.cfg" FOR INPUT AS 1
INPUT #1, a$, tcol, tcol2, tcol3, focol, bcol, mfcol, mtcol, wincol, shdcol, frmcol, icfcol, btfcol, btscol, btrcol, btlcol, bttcol, lbicol, lbacol, tbfcol, tbtcol, sbbcol, sbfcol, sbscol, sbrcol, ttfcol, ttrcol, tttcol, frmlcol, lbfcol, tbbcol,  _
lbtcol, stcol, sfcol
CLOSE 1

'Index der Farbwerte:
'
'   Hauptfarben
'   -----------
'   tcol        Allgemeine Textfarbe
'   tcol2       Alternative Textfarbe
'   tcol3           "           "     (in Kontrast zu tcol2)
'   bcol        Hintergrundfarbe
'   shdcol      Schattenfarbe
'   focol       FokusFarbe

'   Fensterrahmen
'   -------------
'   frmcol      Rahmenfarbe
'   wincol      Fensterfarbe
'   frmlcol     Farbe der Titellinie

'   Editbox
'   -------
'   tbfcol      Textbox-Hintergrundfarbe
'   tbbcol      Textbox-Rahmenfarbe
'   tbtcol      TextBox-Textfarbe

'   Listbox
'   -------
'   lbicol      Inaktiver Eintrag
'   lbacol      Aktiver Eintrag
'   lbtcol      Text des aktivierten Eintrages
'   lbfcol      FÅllfarbe der Listbox

'   Symbole
'   -------
'   icfcol      Symbol-Hintergrundfarbe

'   Buttons
'   -------
'   btfcol      Buttonfarbe
'   btscol      Buttonschattenfarbe
'   btrcol      Buttonrandfarbe
'   btlcol      Buttonlinienfarbe
'   bttcol      ButtonTextfarbe

'   MenÅ
'   ----
'   mfcol       Menu-Hintergrundfarbe
'   mtcol       MenÅ-Textfarbe

'   Scrollbar
'   ----------
'   sbfcol      FÅllfarbe der Scrollbar
'   sbbcol      Farbe der Scrollbarbuttons
'   sbscol      Farbe des Sliderbuttons der Scrollbar (-1 = 3D)
'   sbrcol      Farbe des Scrollbarrahmens

'   Tooltiptext
'   -------------
'   ttfcol      FÅllfarbe des Tooltiptextes
'   ttrcol      Rahmenfarbe des Tooltiptextes
'   tttcol      Tooltiptextfarbe

'   Statusbar
'   ---------
'   stcol       Textfarbe der Statusbar
'   sfcol       FÅllfarbe der Statusbar

'Allgemeine Konstanten:
CONST True = -1
CONST False = 0

CONST AlignLeft = 1
CONST AlignCenter = 2
CONST AlignRight = 3

CONST PunkteLinie = 21845

CONST AutoVal = -32768      'Ermîglicht das automatische umschalten von RessourceneintrÑgen
CONST RscFile = 255         'Dateinummer fÅr die Ressourcen-Auslagerungsdatei

crlf = CHR$(13) + CHR$(10)  'Zeilenvorschub & WagenrÅcklauf

1 Farbe = tcol
COLOR Farbe, bcol
TextAlignment = AlignLeft
NextCtrl = 1
Mouse.x = 319
Mouse.y = 174
zSetMouseCursor laosdir + "mouse"
zTimeEvent(1).inc = 1
zTimeEvent(0).inc = zsson

'LaOS XT Initialization
'///////////////////////

'Filesystem:
TYPE zFileType
    relpos AS INTEGER
    id AS INTEGER
    docint AS INTEGER
    caption AS STRING * 40
    SysType AS STRING * 1
    Created AS STRING * 10
    time AS STRING * 5
    Attrib AS STRING * 1
    info AS STRING * 256
    PrgStarter AS STRING * 40
    icon AS STRING * 12
END TYPE

zShowTimeExitBox = -1
'-1 = Zeigt die Uhr und Exitbox im Hauptfenster an.

Programm:
'********************
'Schreibe in der Sub Main dein Programm!
'Wenn du globale Variablen brauchst kannst du sie hier deklarieren.

Main

'Beenden:
CLEAR
CHAIN "c:\laos7\run.bas"
SYSTEM

errHandler:
  errorlevel = ERR
RESUME NEXT

zMouseData:
DATA 55,8b,ec,56,57,8b,76,0c,8b,04,8b,76,0a,8b,1c,8b,76,08,8b,0c
DATA 8b,76,06,8b,14,cd,21,8b,76,0c,89,04,8b,76,0a,89,1c,8b,76,08,89,0c
DATA 8b,76,06,89,14,5f,5e,5d,ca,08,00,-

zColors:
DATA 0,0,0,0,0,42,0,42,0,0,42,42,42,0,0,42,0,42,21,21,21,42,42,42,0,0,0,21,21,63,21,63,21,21,63,63,63,21,21,63,21,63,63,63,21,63,63,63

'FÅgt ein Symbol zu einer Symbolliste hinzu. (Mit ->IconList erstellt man die
'Symbolliste)
'sName$ = Der Name der Symbolliste
'Index = Der Index des Eintrages (erforderlich: muss grîsser als 0 sein!)
'        Wichtig: Es dÅrfen keine LÅcken in der Indexfolge entstehen!
'IconFile$ = Der Dateiname des Symbols
'Caption$ = Die Beschreibung des Symbols
'value = Der Wert (-1 oder 0)
SUB AddIcon (sName$, Index, IconFile$, caption$, Value)
DIM sFile AS STRING * 12
IF 12 < LEN(IconFile$) THEN
    Dialog "Fehler in AddIcon: Falsche Angabe des File (" + sFile + ")"
    EXIT SUB
  END IF
  sFile = IconFile$
  zRessourceSet sName$, Index, sFile + caption$, Value
END SUB

'Erstellt ein Button-Steuerelement
'sGroup$ = Die Steuerelementgruppe
'Caption$ = Die Caption
'x, y = Die Koordinaten
SUB Button (sGroup$, caption$, x, y)
  zCreateControl zButton, sGroup$, "", caption$, 0, x, y, x + TextLen(caption$) + 8, y + 21, 0, 0, "", 0
  CurrentX = x + TextLen(caption$) + 15: CurrentY = y
END SUB

'Erstellt ein Checkbox-Steuerelement
'sGroup$ = Die Steuerelementegruppe
'Caption$ = Die Caption
'x, y = Die Koordinaten
'value = Der Wert (-1 oder 0)
SUB CheckBox (sGroup$, caption$, x, y, Value)
  zCreateControl zCheckBox, sGroup$, "", caption$, Value, x, y, x + TextLen(caption$) + 32, y + 18, 0, 0, "", 0
END SUB

'Zeigt ein Dialog an, worin der Benutzer ein Symbol auswÑhlen kann.
'
'MarkedIcon$ = Das Symbol, das zu Beginn markiert sein soll.
'
'Gibt einen Leerstring ("") zurÅck, wenn Abgebrochen wurde, die Symboldatei,
'falls das Symbol ausgewÑhlt wurde. (Enter oder Doppelklick auf ein Symbol)
FUNCTION ChooseIcon$ (MarkedIcon$)

  Frame "zchgicon", "Symbol auswÑhlen", 150, 120, 490, 280
  ActiveCtrl = NextCtrl
  IconList "zchgicon", "zicons", 0, 0, 0, 0, True

  '*.lsb
  TimeLock 1
  SHELL "dir " + laosdir + "icons\*.icn /b >" + tempdir + "temp.lst"
  f = FREEFILE
  OPEN tempdir + "temp.lst" FOR INPUT AS f
  i% = 0
  WHILE NOT EOF(f)
    LINE INPUT #f, a$
    i% = i% + 1
    AddIcon "zicons", i%, a$, LEFT$(a$, INSTR(a$, ".") - 1), 1
    IF LCASE$(RTRIM$(a$)) = LCASE$(RTRIM$(MarkedIcon$)) THEN PutValue "zicons", i%
  WEND
  CLOSE f

  '*.ico
  TimeLock 0
  SHELL "dir " + laosdir + "icons\*.lsb /b >" + tempdir + "temp.lst"
  f = FREEFILE
  OPEN tempdir + "temp.lst" FOR INPUT AS f
  errorlevel = 0
  DO WHILE NOT EOF(f)
    LINE INPUT #f, a$
    i% = i% + 1
    AddIcon "zicons", i%, a$, LEFT$(a$, INSTR(a$, ".") - 1), 2
    IF LCASE$(RTRIM$(a$)) = LCASE$(RTRIM$(MarkedIcon$)) THEN PutValue "zicons", i%
    IF errorlevel THEN EXIT DO
  LOOP
  CLOSE f
  DrawCtrls "zchgicon"

  ShortKeys = ""
  GetInp "zchgicon"

  IF keyb = "enter" THEN
    GetString "zicons", a$
    ChooseIcon$ = LEFT$(a$, 12)
  END IF

  CloseFrame "zchgicon"

END FUNCTION

'Entfernt Steuerelemente aus dem Speicher
'sGroup$ = Die Steuerelementegruppe ("" = alle)
SUB ClearCtrls (sGroup$)
SHARED zoldsgroup AS STRING * 8
  Mouse.Clicked = 0
  CLOSE RscFile
  IF sGroup$ = "" THEN
    ERASE Controls
    ERASE zStrings
    KILL tempdir + "*.rs~"
    NextCtrl = 1
    DefaultCtrl = 0
    zoldsgroup = ""
  ELSE
    IF RTRIM$(Controls(DefaultCtrl).Group) = sGroup$ THEN DefaultCtrl = 0
    DIM rsc AS zRessourceType
    ideleted = 0
    FOR i = 1 TO UBOUND(Controls)
      IF Controls(i).CtrlType = 0 THEN EXIT FOR
      IF RTRIM$(Controls(i).Group) = sGroup$ THEN
        Controls(i).CtrlType = 0
        Controls(i).SelfHandler = 0
        Controls(i).Group = ""
        ideleted = ideleted + 1
        'Strings des Objektes lîschen:
        FOR s% = 1 TO UBOUND(zStrings)
          IF RTRIM$(LEFT$(zStrings(s%), 8)) = RTRIM$(RTRIM$(Controls(i).caption)) THEN
            zStrings(s%) = ""
          END IF
        NEXT
        'Ressourcen des Objektes lîschen: (falls vorhanden...)
        KILL tempdir + RTRIM$(Controls(i).caption) + ".rs~"
        IF RTRIM$(zoldsgroup) = RTRIM$(Controls(i).caption) THEN zoldsgroup = ""
      END IF
    NEXT
    NextCtrl = NextCtrl - ideleted
    'von hinten beginnend alle leeren EintrÑge rausnehmen:
    FOR i = 1 TO UBOUND(Controls)
      IF Controls(i).CtrlType = 0 THEN
        iChanges = 0
        FOR z% = i + 1 TO UBOUND(Controls)
          IF Controls(z%).CtrlType > 0 THEN iChanges = -1
          SWAP Controls(z%), Controls(z% - 1)
        NEXT
        IF NOT iChanges THEN EXIT SUB
        i = i - 1
      END IF
    NEXT
    EXIT SUB
  END IF
END SUB

'Lîscht die EintrÑge einer beliebigen Liste (Symbolliste, Listbox, Ressource)
'sName$ = Der Name der Liste
SUB ClearList (sName$)
  CLOSE RscFile
  KILL tempdir + sName$ + ".rs~"
END SUB

'Schliesst einen Frame.
'sGroup$  Die Steuerelementgruppe des Frames
'Hinweis: Es kînnen nur Frames geschlossen werden, die mit angegebener sGroup$
'         erstelt wurden.
SUB CloseFrame (sGroup$)
  FullScreen
  ClearCtrls sGroup$
  ShowPicture -1, 0, tempdir + sGroup$ + ".fr~"
END SUB

'Lîscht den Bildschirm und zeichnet auf Wunsch die Exitbox und Uhr.
'Setzen Sie die Variable zShowTimeExitBox auf 0 (0), um die Zeit und
'ExitBox nicht anzuzeigen.
'sGroup$ = Die Steuerelementgruppe, zu dem die ExitBox gehîrt.
SUB CLW (sGroup$)
    SilentScreen
    CLS
    LINE (0, 0)-(639, 349), wincol, B
    LINE (1, 1)-(638, 348), frmcol, B
    COLOR tcol, bcol
    zSetOrigPalette
    left = 2
    Top = 2
    Right = 637
    Down = 347
    CurrentX = 2
    CurrentY = 2
    IF zShowTimeExitBox THEN
        found = 0
        FOR i = 1 TO UBOUND(Controls)
          IF RTRIM$(Controls(i).Group) = sGroup$ AND RTRIM$(Controls(i).caption) = "" THEN found = i: EXIT FOR
        NEXT
        IF found = 0 THEN ExitBox sGroup$, 619, 4 ELSE zDrawCtrl found, 0, 0
        Farbe = tcol
        EchoXY 575, 4, LEFT$(TIME$, 5)
        LINE (573, 4)-STEP(43, 13), btrcol, B
    END IF
    RestoreScreen
END SUB

FUNCTION ColorChoice (col)
SavePicture 80, 84, 240, 168, tempdir + "bg.tmp"
Frame "colors", "Farbe wÑhlen:", 80, 84, 240, 168
MouseClicker "colors", "click", 88, 110, 231, 153, "click", 0
DrawCtrls "colors"

Top = 9
left = 12
act = col
DO
IF act < 0 THEN act = 15
IF act > 15 THEN act = 0
LOCATE Top, left

FOR i = 0 TO 15
IF i = 5 THEN LOCATE Top + 1, left
IF i = 10 THEN LOCATE Top + 2, left
COLOR i
IF act <> i THEN
  PRINT "€€€";
ELSE
  IF act = 0 OR (act = 15 AND bcol = 63) THEN
    COLOR tcol
    PRINT " " + CHR$(4) + " ";
  ELSE
    PRINT "€" + CHR$(8) + "€";
  END IF
END IF
NEXT

a$ = zGK("colors")

IF a$ = "esc" THEN act = col: GOTO zColorChoiceTerminate
SELECT CASE a$
CASE "click"
  LOCATE Top, left
  FOR i = 0 TO 15
    IF i = 5 THEN LOCATE Top + 1, left
    IF i = 10 THEN LOCATE Top + 2, left
    COLOR i
    PRINT "€€€";
  NEXT
  act = POINT(Mouse.ClickX, Mouse.ClickY)
  a$ = "enter"
CASE "up", "left", "u", "8": act = act - 1
CASE "down", "right", "o", "k": act = act + 1
END SELECT
LOOP UNTIL a$ = "enter"
zColorChoiceTerminate:
ColorChoice = act
COLOR Farbe
ClearCtrls "colors"
ShowPicture 80, 84, tempdir + "bg.tmp"
END FUNCTION

'Gibt eine Meldung auf dem Bildschirm aus, den der Benutzer mit OK bestÑtigen
'muss.
'text = Der auszugebende Text.
SUB Dialog (Text AS STRING)
  z = ActiveCtrl
  ta = TextAlignment
  TextAlignment = AlignCenter
  left = 7
  Right = 632
  h = TextHeight(Text) + 14 + 30
  w = TextWidth(Text) + 16
  l = 630 / 2 - w / 2
  t = 350 / 2 - h / 2
  IF w < 120 THEN
    l = l - ((120 - w) / 2)
    w = 120
  END IF
  x1 = 5 + l
  y1 = t - 2
  x2 = 5 + l + w
  y2 = t + h
  SavePicture x1, y1, x2, y2, tempdir + "scrdlg.tmp"
  ax = x1
  VIEW
  Frame "", "", x1, y1, x2 - 1, y2 - 1
  Echo Text
  x1 = x1 + ((x2 - x1) - TextLen("    OK     ") - 8) / 2
  Button "MsgBox", "    OK     ", x1, y2 - 30
  DrawCtrls "MsgBox"
  GetInp "MsgBox"
  x1 = l
  x2 = l + w
  ClearCtrls "MsgBox"
  ActiveCtrl = z
  TextAlignment = ta
  FullScreen
  ShowPicture ax, y1, tempdir + "scrdlg.tmp"
END SUB

SUB DisableCtrl (iCtrl)
  i = Controls(iCtrl).CtrlType
  zDrawCtrl iCtrl, 0, 0
  IF i > 0 THEN Controls(iCtrl).CtrlType = -i
END SUB

' Build 6
'Zeigt einen Datei-ôffnen Dialog an und zwar fÅr MSDOS-Dateien!
'DateiSuffix$   Gibt an, welche Dateien angezeigt werden sollen.
'               *.* fÅr alle Dateien, *.bas fÅr Basic-Dateien etc...
'
'Tipp: Verwenden Sie ->OpenDialog, um ein LaOS-Dokument zu îffnen.
FUNCTION DosOpenDialog$ (DateiSuffix$)

  bFirst = True

  IF DateiSuffix$ = "" THEN DateiSuffix$ = "*.*"

  f = FREEFILE
  OPEN laosdir + "nodrives.txt" FOR INPUT AS f
  IF NOT EOF(f) THEN LINE INPUT #f, NoDrives$
  CLOSE f

  '*** Controls laden ***
  ListBox "dosfile", "Drive", 17 * 8, 9 * 14, 35 * 8, 10 * 14, False
  ActiveCtrl = NextCtrl
  ListBox "dosfile", "Folder", 17 * 8, 12 * 14, 35 * 8, 19 * 14, False
  ListBox "dosfile", "File", 37 * 8, 9 * 14, 63 * 8, 19 * 14, False

  Frame "dosfile", "Datei îffnen", 15 * 8, 5 * 14, 65 * 8, 20 * 14

zOpenDosFile55:
  '*** Listen laden ***
  TimeLock 2
  ClearList "Folder"  'ula, 30.07.2003 Listen erst lîschen...
  ClearList "File"
  f = FREEFILE
  SHELL "CD >" + tempdir + "CD.tmp"
  OPEN tempdir + "CD.tmp" FOR INPUT AS #f
  INPUT #f, cd$
  CLOSE #f
  IF bFirst THEN
    AltesDir$ = cd$
    a = 0
    FOR i = 65 TO 90
      IF INSTR(LCASE$(NoDrives$), LCASE$(CHR$(i))) = 0 THEN
        TimeLock 1
        errorlevel = 0
        CHDIR CHR$(i) + ":\"
        IF errorlevel = 0 THEN a = a + 1: ListAdd "Drive", a, CHR$(i) + ":", False
        IF UCASE$(LEFT$(AltesDir$, 1)) = CHR$(i) THEN markdrive = a
        TimeLock 2
      END IF
    NEXT
    CHDIR AltesDir$
    PutValue "Drive", markdrive
  END IF
  IF LEN(cd$) > 46 THEN cd$ = LEFT$(cd$, 43) + "..."
  CLOSE #f
  SHELL "DIR /B /ON /AD *.* >" + tempdir + "FOLDER.tmp"
  OPEN tempdir + "FOLDER.tmp" FOR INPUT AS #f
  a = 1
  IF LEN(cd$) > 3 THEN
    a = 2
    ListAdd "Folder", 1, "..", False
  END IF
  DO UNTIL EOF(f)
    INPUT #f, a$
    ListAdd "Folder", a, a$, False
    a = a + 1
  LOOP
  CLOSE #f
  SHELL "DIR /B /ON /A-D " + DateiSuffix$ + " >" + tempdir + "FILE.tmp"
  OPEN tempdir + "FILE.tmp" FOR INPUT AS #f
  a = 0
  DO UNTIL EOF(f)
    INPUT #f, a$
    a = a + 1
    ListAdd "File", a, a$, False
  LOOP
  CLOSE #f

  '*** Controls zeichnen ***
  IF bFirst THEN
    bFirst = False
  ELSE
    Frame "", "Datei îffnen", 15 * 8, 5 * 14, 65 * 8, 20 * 14
  END IF
  LOCATE 8, 18
  PRINT cd$ + SPACE$(46 - LEN(cd$))
  DrawCtrls "dosfile"

  '*** Eingabe ***
zOpenDosFile99:
  ShortKeys = ""
  GetInp "dosfile"

  '*** Eingabe-Auswertung ***
  IF keyb = "esc" THEN GOTO zOpenDosFileEnd
  IF Focus = "Folder" THEN
    GetString "Folder", wahl$
    CHDIR wahl$
    GOTO zOpenDosFile55
  END IF
  IF Focus = "File" THEN
    GetString "File", file$
    IF RIGHT$(cd$, 1) = "\" THEN DosOpenDialog = cd$ + file$
    IF RIGHT$(cd$, 1) <> "\" THEN DosOpenDialog = cd$ + "\" + file$
    GOTO zOpenDosFileEnd
   END IF
   IF Focus = "Drive" THEN
    GetString "Drive", drv$
    SHELL drv$
    GOTO zOpenDosFile55
   END IF

  GOTO zOpenDosFile99

  '*** Ende ***
zOpenDosFileEnd:
  CloseFrame "dosfile"  'ula, 30.07.2003 leert alle Listen, entfernt alle Controls
  SHELL LEFT$(AltesDir$, 1) + ":"
  CHDIR AltesDir$

END FUNCTION

'DateiSuffix$   Die Dateiendung die standardmÑssig verwendet werden soll.
'               (Falls der Benutzer keine andere angibt)
FUNCTION DosSaveDialog$ (DateiSuffix$)

IF LEFT$(DateiSuffix$, 1) = "*" THEN DateiSuffix$ = MID$(DateiSuffix$, 2)
IF LEFT$(DateiSuffix$, 1) = "." THEN DateiSuffix$ = MID$(DateiSuffix$, 2)

Frame "dossave", "Datei speichern", 140, 90, 500, 270
ActiveCtrl = NextCtrl
ListBox "dossave", "Folder", 150, 170, 317, 260, False
zCreateControl 6, "dossave", "", "File", 3, 330, 170, 479, 186, 18, 1, MKI$(12), -1
DefaultCtrl = NextCtrl
zCreateControl 2, "dossave", "", "   OK", 0, 330, 240, 402, 261, 0, 0, MKD$(2.437226570133212D-152) + MKL$(538976288), 0
zCreateControl 2, "dossave", "", "Abbrechen", 0, 410, 240, 490, 261, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
zCreateControl 5, "dossave", "", "Drive", 2, 150, 140, 267, 157, 14, 1, MKD$(0) + MKL$(538976288), -1

f = FREEFILE
OPEN laosdir + "nodrives.txt" FOR INPUT AS f
IF NOT EOF(f) THEN LINE INPUT #f, NoDrives$
CLOSE f

DIM dir AS STRING * 43

  ClearList "Drive"
  bFirst = True

dossavedlgload:
  '*** Listen laden ***
  TimeLock 2
  ClearList "Folder"  'ula, 30.07.2003 Listen erst lîschen...
  f = FREEFILE
  SHELL "CD >" + tempdir + "CD.tmp"
  OPEN tempdir + "CD.tmp" FOR INPUT AS #f
  INPUT #f, cd$
  CLOSE #f
  IF bFirst THEN
    AltesDir$ = cd$
    a = 0
    FOR i = 65 TO 90
      IF INSTR(LCASE$(NoDrives$), LCASE$(CHR$(i))) = 0 THEN
        TimeLock 1
        errorlevel = 0
        CHDIR CHR$(i) + ":\"
        IF errorlevel = 0 THEN a = a + 1: ListAdd "Drive", a, CHR$(i) + ":", False
        IF LCASE$(LEFT$(AltesDir$, 1)) = LCASE$(CHR$(i)) THEN markdrive = a
        TimeLock 2
      END IF
    NEXT
    PutValue "Drive", markdrive
    bFirst = False
  END IF
  CLOSE #f
  CHDIR cd$
  SHELL "DIR /B /ON /AD *.* >" + tempdir + "FOLDER.tmp"
  OPEN tempdir + "FOLDER.tmp" FOR INPUT AS #f
  a = 1
  IF LEN(cd$) > 3 THEN
    a = 2
    ListAdd "Folder", 1, "..", False
  END IF
  DO UNTIL EOF(f)
    INPUT #f, a$
    ListAdd "Folder", a, a$, False
    a = a + 1
  LOOP
  CLOSE #f

SilentScreen
TimeLock 0
Frame "", "Datei speichern", 140, 90, 500, 270
EchoXY 330, 150, "Dateiname:"
PutValue "Folder", 1
DrawCtrls "dossave"
RestoreScreen
DO
  dir = cd$
  EchoXY 150, 120, dir
  ShortKeys = ""
  GetInp "dossave"

  IF Focus = "Folder" THEN
    GetString "Folder", wahl$
    CHDIR wahl$
    GOTO dossavedlgload
  END IF
  IF Focus = "OK" THEN
    GetString "File", file$
    IF INSTR(file$, ".") = 0 THEN
      IF LEN(file$) > 8 THEN file$ = LEFT$(file$, 8)
      file$ = file$ + "." + DateiSuffix$
    END IF
    IF RIGHT$(cd$, 1) = "\" THEN DosSaveDialog = cd$ + file$
    IF RIGHT$(cd$, 1) <> "\" THEN DosSaveDialog = cd$ + "\" + file$
    EXIT DO
   END IF
   IF Focus = "Drive" THEN
    GetString "Drive", drv$
    SHELL drv$
    GOTO dossavedlgload
   END IF

  SELECT CASE Focus
  CASE "Abbrechen"
      'Write here Code for a Click on this Button
      EXIT DO
  CASE "esc"
      'Write here Code for Cancel this Dialog:
      EXIT DO
  END SELECT
LOOP UNTIL keyb = "esc"

  CloseFrame "dossave"
  SHELL LEFT$(AltesDir$, 1) + ":"
  CHDIR AltesDir$

END FUNCTION

'Zeichnet die Controls
'sGroup$ = Die Steuerelementgruppe ("" = alle)
SUB DrawCtrls (sGroup$)
  FOR i = 1 TO UBOUND(Controls)
    IF RTRIM$(Controls(i).Group) = sGroup$ OR sGroup$ = "" THEN
      zDrawCtrl i, 0, -1
    END IF
    IF Controls(i).CtrlType = 0 THEN EXIT SUB
  NEXT
END SUB

'Gibt ein Text auf dem Bildschirm aus. BerÅcksichtigt dabei die Bereichs-
'begrenzungen Left, Top, Right und Down, die zuvor durch einen Rahmen (Frame)
'gesetzt wurden.
SUB Echo (Text AS STRING)
Text = RTRIM$(Text)
DIM linie AS STRING, txt AS STRING
x = CurrentX
y = CurrentY
txt = Text
panewidth = Right - x
leftpane = x
DO
  'holt die nÑchste Zeile:
  aw = 0
  DO
    w = INSTR(aw + 1, txt, " ")
    cr = INSTR(aw + 1, txt, crlf)
    IF w = 0 THEN w = LEN(txt)
    IF cr > 0 THEN
      IF NOT (TextLen(LEFT$(txt, cr - 1)) > panewidth) THEN
        linie = LEFT$(txt, cr - 1)
        txt = MID$(txt, cr + 2)
        EXIT DO
      END IF
    END IF
    IF TextLen(LEFT$(txt, w)) > panewidth OR w = LEN(txt) THEN
      IF aw = 0 THEN aw = LEN(txt)
      IF w = LEN(txt) AND TextLen(LEFT$(txt, w)) < panewidth THEN aw = LEN(txt)
      IF aw = LEN(txt) AND TextLen(LEFT$(txt, w)) > panewidth THEN aw = FIX(panewidth / 8)
      linie = LEFT$(txt, aw)
      txt = MID$(txt, aw + 1)
      EXIT DO
    ELSE
      aw = w
    END IF
  LOOP UNTIL LEN(txt) = 0
  SELECT CASE TextAlignment
  CASE AlignRight
    x = Right - TextLen(linie)
  CASE AlignCenter
    x = left + ((panewidth - TextLen(linie)) / 2)
  END SELECT
  IF y + 14 <= Down THEN
    EchoXY x, y, linie
    x = leftpane
    y = y + 14
  ELSE
    'Platz reicht nicht!
    EXIT DO
  END IF
LOOP UNTIL LEN(txt) = 0
CurrentX = x
CurrentY = y
END SUB

'Schreibt einen Text ohne BerÅcksichtigung der Bereichsbegrenzungen und ohne
'Zeilenumbruch! Siehe auch ->Echo
'text = Text
'x, y = Die Position
SUB EchoXY (x, y, Text AS STRING)
IF LEN(Text) = 0 THEN EXIT SUB
DIM g AS LONG
  IF x + TextLen(Text) > 639 THEN x2 = 639 ELSE x2 = x + TextLen(Text) - 1
  g = 4 + INT(((PMAP(x2, 0) - PMAP(x, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y + 14, 1) - PMAP(y, 1) + 1)
  DIM fontchar(g)
  SCREEN 9, , 1, 0
  IF SilentScreenMode THEN
      DIM oldchar(g)
      GET (0, 0)-(x2 - x, 14), oldchar
  END IF
  LOCATE 1, 1
  IF InversFarbe THEN
      COLOR Farbe XOR InversFarbe
  ELSE
      COLOR Farbe
  END IF
  PRINT Text
  GET (0, 0)-(x2 - x, 13), fontchar
  IF SilentScreenMode THEN
    PUT (0, 0), oldchar, PSET
    IF SaveScreenMode THEN SCREEN 9, , 0, 0
  ELSE
    SCREEN 9, , 0, 0
  END IF
  IF InversFarbe THEN
    LINE (x, y)-(x2, y + 13), InversFarbe, BF
    PUT (x, y), fontchar, XOR
  ELSE
    PUT (x, y), fontchar, PSET
  END IF
  COLOR tcol
END SUB

'Erstellt ein Editbox-Steuerelement
'sGroup$ = die Steuerelementgruppe
'sName$ = Der Name der Editbox
'x,y = Die Koordinaten
'iAnzahlSpalten = Anzahl Spalten
'iAnzahlZeilen = Anzahl Zeilen
'iMaxZeichen = Maximale Anzahl einzugebender Zeichen (0=hîchstmîgliches)
SUB EditBox (sGroup$, sName$, x, y, iAnzahlSpalten, iAnzahlZeilen, iMaxZeichen)
    zCreateControl zEditBox, sGroup$, sName$, "", 1, x, y, x + (iAnzahlSpalten * 8) + 5, y + (iAnzahlZeilen * 14) + 2, iAnzahlSpalten, iAnzahlZeilen, MKI$(iMaxZeichen), -1
END SUB

SUB Editor (sGroup$, sName$, file$, x, y, x2, y2, w, h)
  iCtrl = NextCtrl
  KILL tempdir + sName$ + ".tmp"
  IF file$ <> "" THEN
    f = FREEFILE
    OPEN file$ FOR INPUT AS f
    f2 = FREEFILE
    OPEN tempdir + sName$ + ".tmp" FOR RANDOM AS f2 LEN = (w + 5)
    max = 0
    errorlevel = 0
    DO WHILE NOT EOF(f)
      LINE INPUT #f, a$
      max = max + 1
      a$ = LEFT$(a$, w)
      PUT f2, max, a$
      IF errorlevel THEN EXIT DO
    LOOP
    CLOSE f, f2
  END IF
  zCreateControl zEditor, sGroup$, sName$, "", 1, x, y, x2, y2, w, h, STRING$(12, MKI$(0)), True
  IF max = 0 THEN max = 1
  ctl = NextCtrl
  ScrollBar sGroup$, "", x2 + 2, y - 1, 0, y2 - y + 2, 1, max, 1, iCtrl
  Controls(ctl).Value = 1
END SUB

SUB EnableCtrl (iCtrl)
  i = Controls(iCtrl).CtrlType
  IF i < 0 THEN Controls(iCtrl).CtrlType = -i
  zDrawCtrl iCtrl, 0, 0
END SUB

'PrÅft, ob eine MSDOS-Datei existiert.
'Gibt -1 zurÅck, falls die Datei existiert, andernfalls 0.
FUNCTION exists% (FilePath$)
  f = FREEFILE
  e = errorlevel
  errorlevel = 0
  exists% = -1
  OPEN FilePath$ FOR INPUT AS f
  CLOSE f
  IF errorlevel THEN exists% = 0
  errorlevel = e
END FUNCTION

'Erstellt eine Exit-Box
'sGroup$ = Die Steuerelement-Gruppe
'x,y = Die Position
SUB ExitBox (sGroup$, x, y)
    IF x + y = 0 THEN x = Right - 17: y = Top + 2
    i = NextCtrl
    MouseClicker sGroup$, "", x, y, x + 15, y + 13, "esc", 0
    zDrawCtrl i, 0, -1
END SUB

'Erstellt ein Exit-Fokus Objekt.
'Sobald dieses Objekt den Fokus erhÑlt oder auf dieses Objekt geklickt wurde,
'wird GetInp beendet und die Steuerung dem Programmablauf Åbergeben...
'Das Objekt ist unsichtbar!
SUB Exitfocus (sGroup$, sName$, x, y, x2, y2)
  IF x + y + x2 + y2 = 0 THEN x2 = 639: y2 = 349
  zCreateControl zExitFocus, sGroup$, sName$, "", 0, x, y, x2, y2, 0, 0, "", 0
END SUB

'Kopiert eine Datei
'Title$   Die Beschreibung des Vorgangs (diese wird dem Benutzer angezeigt)
'source$  Die Quelldatei, die kopiert werden soll.
'dest$    Die Zieldatei.
'
'Wichtig: Bereits bestehende Dateien werden ohne RÅckfrage Åberschrieben.
SUB FileCopy (Title$, source$, dest$)

IF source$ = dest$ THEN EXIT SUB

errorlevel = 0
ProgressBar Title$, 0

f% = FREEFILE
OPEN source$ FOR BINARY AS f%
 whole = LOF(f%) \ 500
 part = LOF(f%) MOD 500
 buffer$ = STRING$(500, 0)
 start& = 1

f2% = FREEFILE
OPEN dest$ FOR BINARY AS f2%: CLOSE f2%: KILL dest$ 'stellt sicher, dass Dest$ leer ist.
OPEN dest$ FOR BINARY AS f2%

FOR x = 1 TO whole
  GET f%, start&, buffer$
  PUT f2%, start&, buffer$
  start& = start& + 500
  ProgressBar "", LOF(f2%) / LOF(f%)
  IF errorlevel THEN
    Dialog "Beim Kopieren der Datei ist ein Fehler aufgetreten. Vielleicht ist zuwenig Speicherplatz verfÅgbar, oder der DatentrÑger ist schreibgeschÅtzt."
    EXIT FOR
  END IF
NEXT x

buffer$ = STRING$(part, 0)
GET f%, start&, buffer$
PUT f2%, start&, buffer$

ProgressBar "", 1

CLOSE f%, f2%

END SUB

'Erstellt einen Bereichsrahmen (Register). Der Textbereich wird auf diesen
'Rahmen eingeschrÑnkt.
'sGroup$ - Die Steuerelementgruppe, zu welcher der Rahmen gehîren wird...
'Caption$ - Die Bezeichnung des Rahmens
'x, y, x2, y2 - Die Koordinaten des Fensters
'Tipp: Sie kînnen die ersten vier Parameter auf 0 setzen, um ein Register
'      fÅr den ganzen Bildschirm zu machen.
SUB Frame (sGroup$, caption$, x, y, x2, y2)
  IF x + y + x2 + y2 = 0 THEN
    CLW sGroup$
  ELSE
    left = x + 2
    Top = y + 2
    Right = x2 - 2
    Down = y2 - 2
    IF sGroup$ > "" THEN
      SavePicture x, y, x2, y2, tempdir + sGroup$ + ".fr~"
      FOR i = 1 TO UBOUND(Controls)
        IF RTRIM$(Controls(i).Group) = sGroup$ AND RTRIM$(Controls(i).caption) = "" THEN i = -1: EXIT FOR
      NEXT
      IF i > 0 THEN ExitBox sGroup$, 0, 0
    END IF
    LINE (x, y)-(x2, y2), wincol, B
    LINE (x + 1, y + 1)-(x2 - 1, y2 - 1), frmcol, B
    LINE (x + 2, y + 2)-(x2 - 2, y2 - 2), 0, BF
  END IF
  CurrentX = left + 2
  CurrentY = Top + 2
  IF caption$ > "" THEN
    LINE (left, Top + 18)-(Right, Top + 18), frmlcol
    Echo LEFT$(caption$, FIX((Right - left - 25)) / 8)
    Top = Top + 19
    CurrentY = Top + 2
  END IF
END SUB

'Ermîglicht die Ausgabe von Text mit Echo im Vollbildschirm-Modus.
'FÅhren Sie FullScreen aus, wenn Sie Text ausserhalb des zuletzt erstellten
'Rahmenfensters (Frame) ausgeben wollen.
SUB FullScreen
  VIEW
  left = 2
  Top = 2
  Right = 637
  Down = 347
END SUB

'Gibt die Nummer eines Controls zurÅck.
'name$ = Der Name/Caption des Controls
FUNCTION GetCtrl (name$)
  FOR i = UBOUND(Controls) TO 1 STEP -1
    IF RTRIM$(Controls(i).caption) = name$ AND Controls(i).CtrlType THEN
      GetCtrl = i
      EXIT FUNCTION
    END IF
  NEXT
END FUNCTION

'Wartet auf Benutzereingaben unter berÅcksichtigung der LaOS-Steuerelemente
'Group = Die Steuerelement-Gruppe, welche explizit gehandelt werden soll
'        (Nur Steuerelemente die zu dieser Gruppe gehîren, sind dann
'         aktivierbar)
'        "" = alle Steuerelemente
SUB GetInp (sGroup$)
WHILE INKEY$ <> "": WEND
Mouse.ClickX = 0
IF Controls(ActiveCtrl).CtrlType = 0 THEN ActiveCtrl = DefaultCtrl
zGetInpInit:
'Ist ein Control der Gruppe markiert? - Wenn nein, das erste gefundene aktivieren!
IF Controls(ActiveCtrl).CtrlType = 0 OR (RTRIM$(Controls(ActiveCtrl).Group) <> sGroup$ AND sGroup$ <> "") THEN
  'das erste gefundene wird aktiviert!
  FOR i = 1 TO UBOUND(Controls)
    IF (sGroup$ = "" OR RTRIM$(Controls(i).Group) = sGroup$) AND Controls(i).CtrlType <= zControlTypeCount THEN
      ActiveCtrl = i
      EXIT FOR
    END IF
  NEXT
END IF
Mouse.Down = 0
Mouse.Clicked = 0
iAlt = ActiveCtrl
IF NOT Controls(iAlt).SelfHandler THEN zDrawCtrl iAlt, 0, 0
DO
zGetInpStartLoop:
  zSwapFocus iAlt
  'Ist Fokus auf Exitfocus? - wenn ja, GetInp verlassen...
  IF Controls(ActiveCtrl).CtrlType = zExitFocus THEN EXIT DO
  'aufwÑndige Controls: werden hier seperat gehandelt:
  IF NOT zSelfHandleControl(sGroup$, Controls(ActiveCtrl).CtrlType) THEN
      keyb = zGK(sGroup$)
  END IF
  SELECT CASE keyb
  CASE "passfocus"
    keyb = ""
  CASE "left", "up"
    zLastControl sGroup$
  CASE "right", "down"
    zNextCtrl sGroup$
  CASE "listopen"
    i = ActiveCtrl: x = Controls(i).x: y = Controls(i).y
    x2 = Controls(i).x2: y2 = Controls(i).y2
    SavePicture x - 5, y - 14, x2 + 6, y2 + 14, tempdir + "scr~1.tmp"
    a = NextCtrl
    MouseClicker "z$", "esc", 0, 0, 0, 0, "esc", False
    ListBox "z$", RTRIM$(Controls(i).caption), x - 5, y - 14, x2 + 5, y2 + 14, 0
    Controls(a + 1).Value = Controls(i).Value
    DrawCtrls "z$"
    GetInp "z$"
    Controls(i).Value = Controls(a + 1).Value
    ShowPicture x - 5, y - 14, tempdir + "scr~1.tmp"
    ActiveCtrl = i
    zDrawCtrl i, 0, -1
    FOR i = a TO a + 2
      Controls(i).CtrlType = 0
      Controls(i).SelfHandler = 0
      Controls(i).Group = ""
    NEXT
    NextCtrl = NextCtrl - 3
    IF keyb = "esc" THEN keyb = ""
  CASE "popup"
  CASE "tab"
    zSwapFocus iAlt
    GOTO zGetInpInit
  CASE "esc"
      IF Controls(ActiveCtrl).CtrlType = zMenu THEN
        iold = ActiveCtrl
        ActiveCtrl = DefaultCtrl
        zDrawCtrl iold, 0, 0
        keyb = ""
      END IF
  CASE "enter"
      k = 0
      IF DefaultCtrl > 0 THEN
          IF Controls(ActiveCtrl).CtrlType = zEditBox AND (RTRIM$(Controls(DefaultCtrl).Group) = sGroup$ OR sGroup$ = "") THEN k = 1: ActiveCtrl = DefaultCtrl
      END IF
      IF Controls(ActiveCtrl).CtrlType = zButton OR k THEN
          zDrawCtrl ActiveCtrl, -1, 0
          DO: k = INP(&H60): a$ = INKEY$: LOOP UNTIL k <> 28 AND k <> 224
          zDrawCtrl ActiveCtrl, 0, 0
          WHILE INKEY$ <> "": WEND
          IF k <> 156 AND Mouse.Clicked = 0 AND k <> 57 THEN GOTO zGetInpStartLoop
      END IF
      zHandleControls ActiveCtrl, sGroup$, iChanges
      IF NOT iChanges THEN EXIT DO
      keyb = ""
  CASE ELSE
    FOR i = 1 TO UBOUND(Controls)
      IF LCASE$(LEFT$(LTRIM$(Controls(i).caption), LEN(keyb))) = LCASE$(keyb) AND (RTRIM$(Controls(i).Group) = sGroup$ OR sGroup$ = "") THEN
        iold = ActiveCtrl
        ActiveCtrl = i
        IF iold <> i THEN EXIT FOR
      END IF
    NEXT
  END SELECT
LOOP UNTIL zQuitKey
zSwapFocus iAlt
i = ActiveCtrl: ActiveCtrl = 0: zSwapFocus iAlt: ActiveCtrl = i
Focus = LTRIM$(RTRIM$(Controls(ActiveCtrl).caption))
IF keyb = "esc" THEN Focus = "esc"
Farbe = tcol
InversFarbe = 0
COLOR tcol
END SUB

'Liest einen String eines beliebigen Objektes aus (siehe auch ->PutString)
'objekt = Der Name des Objektes, dessen String herausgelesen werden soll.
'Text = RÅckgabewert: Der String des Objektes
SUB GetString (objekt$, Text$)
  FOR i = 1 TO UBOUND(zStrings)
    IF RTRIM$(LEFT$(zStrings(i), 8)) = RTRIM$(objekt$) THEN
      'Objekt gefunden -> diesen String zurÅckgeben!
      Text$ = MID$(zStrings(i), 9)
      EXIT SUB
    END IF
  NEXT
  errorlevel = 0
  zRessourceGet objekt$, Controls(GetCtrl(objekt$)).Value, Text$, 0
  IF errorlevel THEN Dialog "Control " + objekt$ + " existiert nicht!"
END SUB

'Liest den Wert eines beliebigen Steuerelementes aus (siehe auch
'->PutValue)
'objekt$ = Der Name des Objektes, dessen Wert herausgelesen werden soll
'Wert = RÅckgabewert: Der Wert des Steuerelementes
SUB GetValue (objekt$, wert)
  i = GetCtrl(objekt$)
  zRessourceGet objekt$, Controls(i).Value, "", wert
  IF wert = 0 THEN wert = Controls(i).Value
END SUB

'Erstellt ein Symbollisten-Steuerelement
'Die Symbole werden mit AddIcon hinzugefÅgt.
'sGroup$ = Die Steuerelemente-Gruppe
'sName$ = Der Name der Symbolliste
'x,y,x2,y2 = Die Position des Rahmens, innerhalb dessen sich die Symbole befinden
'            werden.
'iScrollBars = -1: Die Symbolliste wird mit Scrollbars angezeigt.
SUB IconList (sGroup$, sName$, x, y, x2, y2, iScrollBars)
  tag$ = STRING$(8, 0)
  IF x + y + x2 + y2 = 0 THEN
    x = left
    y = Top
    x2 = Right
    IF iScrollBars THEN x2 = x2 - 20
    y2 = Down
  END IF
  IF iScrollBars THEN MID$(tag$, 3, 2) = MKI$(NextCtrl + 1)
  zCreateControl zIconList, sGroup$, sName$, "", 1, x, y, x2, y2, 0, 0, tag$, -1
  IF iScrollBars THEN
    ScrollBar sGroup$, "", x2 + 7, Top, 0, Down - Top, 1, 0, 1, NextCtrl - 1
  END IF
END SUB

'Gibt -1 (-1) zurÅck, wenn der Text nur aus ascii(0)-Zeichen besteht.
FUNCTION IsNull (Text$)
  IF STRING$(LEN(Text$), 0) = Text$ THEN IsNull = -1
END FUNCTION

'Erstellt ein Text-Label
'x,y,x2,y2 = Der Bereich, innerhalb dessen der Text ausgegeben wird.
'Text = Der auszugebende Text
'Alignment = Ausrichtung des Textes (0 = links, 1 = Mitte, 2 = rechts)
'bgColor = Die bcol
SUB Label (x, y, x2, y2, Text AS STRING, Alignment, bgColor)
ta = TextAlignment
TextAlignment = Alignment
InversFarbe = bgColor
l = left
t = Top
r = Right
d = Down
left = x
Top = y
Right = x2
Down = y2
CurrentY = y
CurrentX = x + 2
LINE (x, y)-(x2, y2), bgColor, BF
Echo Text
TextAlignment = ta
InversFarbe = 0
left = l
Top = t
Right = r
Down = d
END SUB

'Gibt die Datei zurÅck, die durch LaOS gestartet wurde.
FUNCTION LinkedFile$
  f = FREEFILE
  OPEN tempdir + "linkway.tmp" FOR INPUT AS f
  LINE INPUT #f, LinkedFile$
  CLOSE f
END FUNCTION

'FÅllt eine zuvor erstellte ListBox mit dem Text und dem Standardwert
'sName$ = Der Name der Listbox, der beim Erstellen der Listbox angegeben wurde
'Index = Der Index des Eintrages (erforderlich: muss grîsser als 0 sein!)
'        Wichtig: Es dÅrfen keine LÅcken in der Indexfolge entstehen!
'text = Der Text
'value = Der Wert (-1 oder 0)
SUB ListAdd (sName$, Index, Text AS STRING, Value)
  IF RTRIM$(Text) = "" THEN Text = CHR$(0)
  zRessourceSet sName$, Index, Text, Value
END SUB

'Erstellt ein Listbox-Steuerelement
'Die Liste muss anschliessend mit ListAdd gefÅllt werden.
'Mit ListDeleteItem kînnen ListeneintrÑge entfernt werden.
'Group = Die Steuerelementegruppe
'sName$ = Der Name der Listbox (wird fÅr ListAdd und ListDeleteItem verwendet)
'x, y, x2, y2 = Die Koordinaten der Listbox
'WithCheckBoxes = -1 = Die Listbox wird mit Checkboxen angezeigt
SUB ListBox (sGroup$, sName$, x, y, x2, y2, WithCheckBoxes)
  'Es wird gerundet auf die Texthîhe:
  IF (y2 - (y + 4)) MOD 14 > 7 THEN
    y2 = y2 + (14 - ((y2 - y - 4) MOD 14))
  ELSEIF (y2 - (y + 4)) MOD 14 <= 7 THEN
    y2 = y2 - ((y2 - y - 4) MOD 14)
  END IF
  x2 = x2 - 13
  tag$ = STRING$(8, 0)
  IF WithCheckBoxes THEN MID$(tag$, 2, 1) = "C"
  IF sName$ <> "MenuInt" AND FIX((y2 - y - 4) / 14) > 2 THEN MID$(tag$, 3, 2) = MKI$(NextCtrl + 1)
  zCreateControl zListBox, sGroup$, sName$, "", 1, x, y, x2, y2 - 1, FIX((x2 - x - 4) / 8), FIX((y2 - y - 4) / 14), tag$, -1
  IF sName$ <> "MenuInt" AND FIX((y2 - y - 4) / 14) > 2 THEN
    ScrollBar sGroup$, "", x2 + 1, y, 0, y2 - y - 1, 1, 0, FIX((y2 - y - 4) / 14), NextCtrl - 1
  END IF
END SUB

SUB Main
  CLW "main"
  Menu "main", "Datei"
  Editor "edit", "edit1", "c:\laos\run.bas", 10, 22, 620, 345, 255, 22
  DO
    DrawCtrls ""
    GetInp ""
    SELECT CASE Focus
    CASE "esc"
      EXIT SUB
    CASE "Datei"
      a$ = MenuInt("Neu|Ctrl+N,-,Datei îffnen|Ctrl+O,Datei speichern|Ctrl+S,-,Beenden")
      SELECT CASE a$
      CASE "Neu"
        file$ = ""
        ClearCtrls "edit"
        Editor "edit", "edit1", "", 10, 22, 620, 345, 255, 22
      CASE "Datei îffnen"
        file$ = OpenDialog("Textdokument")
        IF file$ <> "" THEN
          ClearCtrls "edit"
          Editor "edit", "edit1", file$, 10, 22, 620, 345, 255, 22
        END IF
      CASE "Datei speichern"
        file$ = SaveDialog("Textdokument", "text")
        IF file$ <> "" THEN
          ClearCtrls "edit"
          Editor "edit", "edit1", file$, 10, 22, 620, 345, 255, 22
        END IF
      END SELECT
    END SELECT
  LOOP


END SUB

'Markiert einen Bereich
'x,y,x2,y2  Die Koordinaten des Bereichs
'           Wenn alle Werte auf 0 gesetzt sind, wird die letzte Markierung
'           gelîscht.
SUB Markieren (x, y, x2, y2)
STATIC ox, oy, ox2, oy2, m

IF x + y + x2 + y2 = 0 THEN
  IF m = 1 THEN EXIT SUB
  x = ox: y = oy: x2 = ox2: y2 = oy2
  m = 1
ELSE
  m = 0
END IF
IF x + y + x2 + y2 = 0 THEN EXIT SUB
ox = x
oy = y
ox2 = x2
oy2 = y2

IF ((x - x2) < 0) THEN stp = -1 ELSE stp = 1
FOR nr = 1 TO 2
FOR i = x2 TO x STEP 5 * stp
  PSET (i, y), POINT(i, y) XOR focol
NEXT
IF y2 - y = 0 THEN EXIT FOR
SWAP y, y2
NEXT nr

IF ((y - y2) < 0) THEN stp = -1 ELSE stp = 1
FOR nr = 1 TO 2
FOR i = y2 TO y STEP 5 * stp
  PSET (x, i), POINT(x, i) XOR focol
NEXT
IF x2 - x = 0 THEN EXIT FOR
SWAP x, x2
NEXT nr
END SUB

'Setzt ein neues Menu hin!
'group = Die Steuerelement-Gruppe
'Caption$ = Die Caption des MenÅs
SUB Menu (sGroup$, caption$)
  IF NextCtrl = 1 THEN
    Oldx = left
    Oldy = Top
  ELSE
    FOR i = UBOUND(Controls) TO 1 STEP -1
      IF Controls(i).CtrlType = zMenu AND RTRIM$(Controls(i).Group) = sGroup$ THEN EXIT FOR
    NEXT
    IF i = 0 THEN
      Oldx = left
      Oldy = Top
    ELSE
      Oldx = Controls(i).x + TextLen(RTRIM$(Controls(NextCtrl - 1).caption)) + 16
      Oldy = Controls(i).y
    END IF
    IF Oldx + TextLen(caption$) > 639 THEN Oldx = 2: Oldy = Oldy + 14
  END IF
  zCreateControl zMenu, sGroup$, "", caption$, 0, Oldx, Oldy, Oldx + TextLen(caption$), Oldy + 13, 0, 0, "", 0
END SUB

'Zeigt ein DropDown-MenÅ an fÅr das aktivierte Steuerelement
'Kann auch als PopUp-MenÅ verwendet werden!
FUNCTION MenuInt$ (items AS STRING)
  iOldCtrl = ActiveCtrl
  zDrawCtrl iOldCtrl, 0, 0
  IF Controls(iOldCtrl).CtrlType = zMenu THEN
    x = Controls(iOldCtrl).x
    y = Controls(iOldCtrl).y + 15
  ELSE
    x = Mouse.x
    y = Mouse.y
  END IF
  l = 0
  maxwidth = 0
  ai = 1
  FOR i = 1 TO LEN(items) + 1
    IF MID$(items, i, 1) = "," OR i > LEN(items) THEN
      a$ = MID$(items, ai, i - ai)
      t = INSTR(a$, "|")
      IF t > 0 THEN
        a$ = LEFT$(a$, t - 1) + SPACE$(3) + MID$(a$, t + 1)
      END IF
      IF LEN(a$) > maxwidth THEN maxwidth = LEN(a$)
      ai = i + 1
    END IF
  NEXT
  l = 0
  ai = 1
  FOR i = 1 TO LEN(items) + 1
    IF MID$(items, i, 1) = "," OR i > LEN(items) THEN
      IF i = LEN(items) THEN i = i + 1
      l = l + 1
      a$ = MID$(items, ai, i - ai)
      t = INSTR(a$, "|")
      IF t > 0 THEN
        b$ = MID$(a$, t + 1)
        a$ = LEFT$(a$, t - 1) + CHR$(0) + SPACE$(3)
        a$ = a$ + SPACE$(maxwidth - LEN(a$) - LEN(b$) + 1) + b$
      END IF
      ListAdd "MenuInt", l, a$, 0
      ai = i + 1
    END IF
  NEXT
  MouseClicker "MenuInt", "Cancel", 0, 0, 0, 0, "esc", 0
  c = NextCtrl
  x2 = x + maxwidth * 8 + 5 + 13
  y2 = y + l * 14 + 4
  IF x2 > 639 THEN x = x - (x2 - 639): x2 = 639
  IF y2 > 349 THEN y = y - (y2 - 349): y2 = 349
  ListBox "MenuInt", "MenuInt", x, y, x2 - 1, y2, 0
  MID$(Controls(c).tag, 1, 1) = "M"
  g = 4 + INT(((PMAP(x2, 0) - PMAP(x, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y2, 1) - PMAP(y, 1) + 1)
  IF g > MaxIntArraySize THEN
    SavePicture x, y, x2, y2, tempdir + "scr~1.tmp"
  ELSE
    DIM bg(g)
    GET (x, y)-(x2, y2), bg
  END IF
  zDrawCtrl c, 0, -1
  ShortKeys = ""
  GetInp "MenuInt"
  IF g > MaxIntArraySize THEN
    ShowPicture x, y, tempdir + "scr~1.tmp"
  ELSE
    PUT (x, y), bg, PSET
    REDIM bg(0)
  END IF
  choice = Controls(c).Value
  IF keyb = "esc" THEN
    a$ = "": choice = 0
  ELSE
    zRessourceGet "MenuInt", choice, a$, 0
  END IF
  ClearCtrls "MenuInt"
  IF INSTR(a$, CHR$(0)) > 0 THEN
    MenuInt$ = LTRIM$(LEFT$(a$, INSTR(a$, CHR$(0)) - 1))
  ELSE
    MenuInt$ = LTRIM$(a$)
  END IF
  IF keyb = "enter" THEN ActiveCtrl = DefaultCtrl ELSE ActiveCtrl = iOldCtrl
  zDrawCtrl iOldCtrl, 0, 0
END FUNCTION

'Erstellt ein MouseClicker-Steuerelement
'Der Benutzer kann auf den Bereich klicken, der Programmierer bekommt
'dies per GetInp (Focus) bestÑtigt.
'sGroup$ = Die Steuerelementgruppe
'sName$ = Der Name des Clickers
'x, y, x2, y2 = Die Koordinaten
'sSimulateKey = Die Taste, die zurÅckgegeben werden soll (keyb),
'               wenn der Benutzer auf den Bereich klickt.
'iWithShadow = -1 = Der Bereich wird schattiert, wenn der Benutzer auf
'                     diesen klickt.
SUB MouseClicker (sGroup$, sName$, x, y, x2, y2, sSimulateKey AS STRING, iWithShadow)
  IF x + y + x2 + y2 = 0 THEN x2 = 639: y2 = 349
  IF sName$ = "" THEN h = 1
  zCreateControl zMouseClicker, sGroup$, sName$, "", 0, x, y, x2, y2, iWithShadow, h, sSimulateKey, 0
END SUB

'Zeigt ein Datei-Laden Dialog an. Hat der Benutzer Abgebrochen, so kommt
'"" zurÅck.
FUNCTION OpenDialog$ (PrgStarter$)
  DIM zfile AS zFileType, zCurrentfolder AS LONG, zftt AS zFileType

  f = FREEFILE
  OPEN laosdir + "files.bnk" FOR BINARY AS f LEN = LEN(zFileType)
  zCurrentfolder = 1

  Frame "zload", "Datei îffnen", 5, 80, 634, 279
  zfolders = NextCtrl
  ActiveCtrl = zfolders
  ListBox "zload", "zfolders", left + 2, Top + 18, 260, 273, 0
  ListBox "zload", "zfiles", 270, Top + 2, Right - 5, 253, 0
  Button "zload", " Abbrechen ", 403, 252
  zbtnLoad = NextCtrl
  Button "zload", " Datei îffnen ", 507, 252

DO
  DefaultCtrl = zbtnLoad
  'zfile steht nun auf dem aktuellen Ordner:
  GET f, zCurrentfolder, zfile
  infldr = zfile.id
  CurrentX = left + 2
  CurrentY = Top + 2
  Farbe = tcol2
  Label left + 2, Top + 2, 261, Top + 16, RTRIM$(zfile.caption), 2, tcol3
  fid = 0
  IF zCurrentfolder > 1 THEN
    ListAdd "zfolders", 1, "..", zfile.relpos
    fid = 1
  END IF
  zid = 0
  SEEK f, 1 + LEN(zftt)
  DO
    zid = zid + 1
    GET f, , zftt
    IF zftt.id = 0 THEN EXIT DO
    IF zftt.relpos = zfile.id AND zftt.SysType = "D" AND zftt.id > 0 THEN
      fid = fid + 1
      ListAdd "zfolders", fid, zftt.caption, zftt.id
    END IF
  LOOP
  SEEK f, 1 + LEN(zftt)
  fid = 0
  DO
    GET f, , zftt
    IF zftt.id = 0 THEN EXIT DO
    IF zftt.relpos = infldr AND RTRIM$(zftt.PrgStarter) = RTRIM$(PrgStarter$) AND zftt.id > 0 THEN
      fid = fid + 1
      ListAdd "zfiles", fid, zftt.caption, zftt.docint
    END IF
  LOOP
  DrawCtrls "zload"
  ShortKeys = ""
  GetInp "zload"
  zRessourceGet "zfolders", Controls(zfolders).Value, zfolder$, fid
  IF keyb = "esc" THEN EXIT DO
  SELECT CASE Focus
  CASE "Abbrechen"
    EXIT DO
  CASE "zfolders"
    'in folder wechseln...
    SEEK f, 1
    DO
      zCurrentfolder = SEEK(f)
      GET f, , zftt
    LOOP UNTIL zftt.id = fid OR zftt.id = 0
    ClearList "zfiles"
  CASE "Datei îffnen", "zfiles"
    GetValue "zfiles", fid
    OpenDialog$ = laosdir + "doc\" + LTRIM$(STR$(fid))
    EXIT DO
  END SELECT
  ClearList "zfolders"
LOOP
  CloseFrame "zload"
  CLOSE f
END FUNCTION

SUB ProgressBar (Text$, Stat AS SINGLE)
  IF Text$ <> "" THEN
    Frame "", "", 160, 120, 480, 215
    Echo Text$
  END IF
  IF Stat > 1 THEN Stat = 1
  LINE (179, 174)-(459, 194), Farbe, B
  x = 278 * Stat
  LINE (180, 175)-(180 + x, 193), tcol2, BF
END SUB

'Speichert einen String zu einer beliebigen Editbox (siehe auch ->GetString)
'objekt$ = Der Name der Editbox
'Text = Der Text
SUB PutString (objekt$, Text$)
DIM o   AS STRING * 8
  FOR i = 1 TO UBOUND(Controls)
    IF RTRIM$(Controls(i).caption) = RTRIM$(objekt$) AND Controls(i).h = 1 THEN Controls(i).Value = LEN(Text$) + 1
  NEXT
  o = objekt$
  'existiert objekt bereits?
  FOR i = 1 TO UBOUND(zStrings)
    IF LEFT$(zStrings(i), 8) = o THEN
      'diesen String aktualisieren!
      zStrings(i) = o + Text$
      EXIT SUB
    END IF
  NEXT
  'Objekt existiert noch nicht: suche freien Platz
  FOR i = 1 TO UBOUND(zStrings)
    IF zStrings(i) = "" THEN
      zStrings(i) = o + Text$
      EXIT SUB
    END IF
  NEXT
  'kein freier Platz: option radicale
  ERASE zStrings
  zStrings(1) = o + Text$
END SUB

'Speichert einen Wert zu einem existierenden Steuerelement
'(siehe auch ->GetValue)
'objekt$ = Der Name des Objektes
'Wert = Der Wert, der dem Objekt zugewiesen werden soll.
SUB PutValue (objekt$, wert)
  i = GetCtrl(objekt$)
  Controls(i).Value = wert
END SUB

'Erstellt ein RadioButton-Steuerelement
'sGroup$ = Die Steuerelementgruppe
'Caption$ = Die Caption
'x, y = Die Koordinaten
'value = Der Wert (-1 oder 0)
SUB RadioButton (sGroup$, caption$, x, y, Value)
  zCreateControl zRadioButton, sGroup$, "", caption$, Value, x, y, x + TextLen(caption$) + 32, y + 18, 0, 0, "", 0
  IF Value = -1 THEN
    zHandleControls NextCtrl - 1, sGroup$, 0
  END IF
END SUB

'Wird ausgefÅhrt, wenn ein Control den Fokus bekommt.
'Falls Sie Dialog oder andere Benutzereingaben verwenden, mÅssen Sie
'anschliessend keyb auf "" zurÅcksetzen.
'iOldCtrl Das Control, das zuvor aktiviert war
'iCtrl    Das Control, das aktiviert wurde
'name$    Der Name des Controls, das aktiviert wurde
SUB RaiseGotFocus (iOldCtrl, iCtrl, name$)

END SUB

'Wird ausgefÅhrt, wenn der Benutzer eine Taste drÅckt.
'iCtrl = Das derzeit aktive Control
'sKey  = Die gedrÅckte Taste
'
'Wichtig: Verwenden Sie mit Vorliebe die Stringvariable
'         ShortKeys, um bestimmte TastendrÅcke auszuwerten.
SUB RaiseKeyPress (iCtrl, sKey AS STRING)

END SUB

'Wird ausgefÅhrt, wenn die Maus Åber ein Control bewegt wird.
'iCtrl   Die Ctrl-Nummer
'name$   Der Name des Controls
'iButton Der Button (1=links, 2=rechts)
SUB RaiseMouseMove (iCtrl, name$, iButton)

END SUB

'Wird ausgefÅhrt, wenn die Maus geklickt wurde.
'iCtrl   Die Ctrl-Nummer
'name$   Der Name des Controls
'iButton Der Button (1=links, 2=rechts)
SUB RaiseMouseUp (iCtrl, name$, iButton)

END SUB

'Wird ausgefÅhrt, wenn auf einem unterstÅtzten Steuerelement die rechte
'Maustaste gedrÅckt wurde.
'iCtrl = Die Nummer des Controls
'iCtrl = Der Name (Caption) des Controls
'
'Tipp: Mit ->MenuInt kann nun ein PopUpMenÅ angezeigt werden!
'
SUB RaisePopupMenu (iCtrl, name$, ExitInput)

END SUB

'Event: Wird ausgefÅhrt, wenn der Benutzer mit der ScrollBar gescrollt hat.
'       FÅr die ScrollBar-Controls
'iCtrl = Die ControlNr der Scrollbar
'name$ = Der Name der Scrollbar
SUB RaiseScroll (iCtrl, name$)

END SUB

'Wird ausgefÅhrt, wenn die Maus auf einem Control ruht.
'Hier kînnen Sie Tooltiptexte fÅr ihre Controls erfassen!
'iCtrl  Die ControlNr, des Controls, auf dessen die Maus
'       ruht.
SUB RaiseToolTipText (iCtrl, name$)

  SELECT CASE name$
  END SELECT

  IF Text$ <> "" THEN zToolTipText Text$

END SUB

'Holt den im ruhigen Bildaufbaumodus aufgebaute Bildschirm in den Vordergrund.
'Das Bild wird dadurch angezeigt.
'Verwenden Sie SilentScreen, um den ruhigen Bildaufbaumodus zu aktivieren.
'->SilentScreen
SUB RestoreScreen
  IF zSSMStop THEN zSSMStop = zSSMStop - 1: EXIT SUB
  SCREEN 9, , 0, 0
  PCOPY 1, 0
  SilentScreenMode = 0
  SaveScreenMode = 0
  COLOR tcol
END SUB

'Zeigt einen Speichern-Dialog an und gibt den neuen Dateinamen zurÅck.
'Kommt eine leere Zeichenfolge zurÅck, so hat der Benutzer Abbrechen gedrÅckt.
'
'PrgStarter$    Der DateitypenregistrierungsschlÅssel
'icon$          Das zu verwendende Symbol
FUNCTION SaveDialog$ (PrgStarter$, icon$)
  DIM zfile AS zFileType, zCurrentfolder AS LONG, zftt AS zFileType

  f = FREEFILE
  OPEN "folder.ini" FOR BINARY AS f LEN = 2
  GET f, 1, fpos
  CLOSE f
  OPEN laosdir + "files.bnk" FOR BINARY AS f LEN = LEN(zFileType)
  zCurrentfolder = 1

zSvdlg:
  Frame "zsave", "Speichern", 5, 80, 634, 279
  zfolders = NextCtrl
  ActiveCtrl = zfolders
  ListBox "zsave", "zfolders", left + 2, Top + 18, 260, 273, 0
  EchoXY 270, Top + 4, "Dateiname:"
  EchoXY 270, Top + 32, "Dateiinfo:"
  EditBox "zsave", "zfile", 367, Top + 2, 32, 1, 40
  EditBox "zsave", "zinfo", 367, Top + 30, 32, 5, 256
  Button "zsave", "Neuer Ordner", 270, 252
  Button "zsave", " Abbrechen ", 429, 252
  zbtnSave = NextCtrl
  Button "zsave", " Speichern ", 531, 252

DO
  DefaultCtrl = zbtnSave
  'zfile steht nun auf dem aktuellen Ordner:
  GET f, zCurrentfolder, zfile
  infldr = zfile.id
  CurrentX = left + 2
  CurrentY = Top + 2
  Farbe = tcol2
  Label left + 2, Top + 2, 261, Top + 16, RTRIM$(zfile.caption), 2, tcol3
  fid = 0
  IF zCurrentfolder > 1 THEN
    ListAdd "zfolders", 1, "..", zfile.relpos
    fid = 1
  END IF
  zid = 0
  SEEK f, 1 + LEN(zftt)
  DO
    zid = zid + 1
    GET f, , zftt
    IF zftt.id = 0 THEN EXIT DO
    IF zftt.relpos = zfile.id AND zftt.SysType = "D" AND zftt.id > 0 THEN
      fid = fid + 1
      IF zftt.id = zmrkid THEN Controls(zfolders).Value = fid
      ListAdd "zfolders", fid, zftt.caption, zftt.id
    END IF
  LOOP
  DrawCtrls "zsave"
  ShortKeys = "backspace"
  GetInp "zsave"
  IF keyb = "backspace" THEN
    IF Focus = "zfolders" THEN
      Controls(zfolders).Value = 1
      GetString "zfolders", a$
      IF a$ = ".." THEN Focus = "zfolders" ELSE Focus = ""
    ELSE
      Focus = ""
    END IF
  END IF
  zRessourceGet "zfolders", Controls(zfolders).Value, zfolder$, fid
  IF keyb = "esc" THEN EXIT DO
  SELECT CASE Focus
  CASE "Abbrechen"
    EXIT DO
  CASE "zfolders"
    'in folder wechseln...
    zmrkid = zfile.id
    Controls(zfolders).Value = 1
    SEEK f, 1
    DO
      zCurrentfolder = SEEK(f)
      GET f, , zftt
    LOOP UNTIL zftt.id = fid OR zftt.id = 0

  CASE "Neuer Ordner"
    Frame "zord", "Neuer Ordner in " + zfile.caption, 80, 120, 470, 220
    Echo "Ordnername:"
    PutString "zord", file$
    EditBox "zord", "zord", 90, 160, 40, 1, 40
    PutString "zord", file$
    DefaultCtrl = NextCtrl
    Button "zord", ">> Weiter", 350, 180
    DrawCtrls "zord"
    GetInp "zord"
    IF keyb = "enter" THEN
      'Der neue Ordner wird erstellt:
      SEEK f, 1
      zid = 0
      DO
       IF zftt.id >= zid THEN zid = zftt.id + 1
       p& = SEEK(f)
       GET f, , zftt
      LOOP UNTIL zftt.id = 0
      zftt.id = zid
      zftt.relpos = zfile.id
      GetString "zord", a$
      zftt.caption = a$
      zftt.SysType = "D"
      zftt.Created = DATE$
      zftt.time = TIME$
      zftt.icon = "ordner.lsb"
      PUT f, p&, zftt
    END IF
    ClearCtrls "zsave"
    ClearCtrls "zord"
    GOTO zSvdlg
  CASE "Speichern"
      'Die neue Datei wird erstellt:
      zid = 0
      SEEK f, 1
      DO
       IF zfile.id >= zid THEN zid = zfile.id + 1
       p& = SEEK(f)
       GET f, , zfile
      LOOP UNTIL zfile.id = 0
      DO WHILE exists(laosdir + "doc\" + LTRIM$(STR$(zid)))
        zid = zid + 1
      LOOP
      zfile.id = zid
      zfile.relpos = infldr
      zfile.docint = zid
      GetString "zfile", a$
      zfile.caption = a$
      GetString "zinfo", a$
      zfile.info = a$
      zfile.icon = icon$
      zfile.SysType = "F"
      zfile.PrgStarter = PrgStarter$
      zfile.Created = DATE$
      zfile.time = TIME$
      PUT f, p&, zfile
      SaveDialog$ = laosdir + "doc\" + LTRIM$(STR$(zid))
      EXIT DO
  END SELECT
  ClearList "zfolders"
LOOP
  CloseFrame "zsave"
  ClearCtrls "zord"
  CLOSE f
END FUNCTION

'Liest ein Bild vom Bildschirm ein und speichert es in eine Datei.
'x,y,x2,y2  Die Koordinaten des Bildes
'file$      Die Datei (existierende Dateien werden Åberschrieben)
SUB SavePicture (x, y, x2, y2, file$)
DIM pic(123) AS DOUBLE
  f% = FREEFILE
  OPEN file$ FOR BINARY AS f%
  t& = CVL("ABMS")
  PUT f%, 1, t&
  w% = x2 - x + 1
  h% = y2 - y + 1
  PUT f%, , w%
  PUT f%, , h%
  PUT f%, , x
  PUT f%, , y
  FOR xx = x TO x2 STEP 41
    rx% = x2 - xx
    FOR yy = y TO y2 STEP 41
      ry% = y2 - yy
      IF rx% < 41 OR ry% < 41 THEN
        IF rx% >= 41 THEN rx% = 40
        IF ry% >= 41 THEN ry% = 40
        g% = ((4 + INT(((PMAP(rx%, 0) - PMAP(0!, 0) + 1) * (1) + 7) / 8) * 4 * (PMAP(ry%, 1) - PMAP(0!, 1) + 1))) / 8
        REDIM pic(g%) AS DOUBLE
        GET (xx, yy)-(xx + rx%, yy + ry%), pic
      ELSE
        REDIM pic(123) AS DOUBLE
        GET (xx, yy)-(xx + 40, yy + 40), pic
      END IF
      'Speichern:
      FOR i% = LBOUND(pic) TO UBOUND(pic)
          PUT f%, , pic(i%)
      NEXT
    NEXT
  NEXT
  CLOSE f%
END SUB

'Speichert den aktuellen Bildschirminhalt ab, um ihn danach mit
'->RestoreScreen wieder anzeigen zu kînnen. Verwenden Sie diesen Befehl,
'wenn Sie Aufgaben erledigen wollen und spÑter wieder zum ursprÅnglichen
'Bildschirm zurÅckwechseln wollen.
SUB SaveScreen
  IF SilentScreenMode = -1 THEN zSSMStop = zSSMStop + 1: EXIT SUB
  SaveScreenMode = -1
  SilentScreenMode = -1
  PCOPY 0, 1
END SUB

'Erstellt ein Scrollbar-Steuerelement
'sGroup$ = die Steuerelementgruppe
'sName$ = Der Name der Scrollbar
'x,y = Die Koordinaten
'w = Die Breite der Scrollbar (Width)
' - oder - h = Die Hîhe der Scrollbar (Height)
'min = Das Minimum des Scrollbar-Wertes
'max = Das Maximum des Scrollbar-Wertes
'stp = Der Schritt: Die Grîsse einer Bewegung der Scrollbar...
'iCtrl = Die ControlNr des Controls, auf dieses sich die Scrollbar bezieht.
'Wichtig: Nur entweder w oder h auf einen Wert hîher 0 setzen!
'         Je nach dem, welcher Wert angegeben ist, so wird die Scrollbar
'         vertikal oder horizontal angezeigt.
SUB ScrollBar (sGroup$, sName$, x, y, w, h, Min, max, stp, iCtrl)
  IF w > 0 THEN x2 = x + w: y2 = y + 13 ELSE y2 = y + h: x2 = x + 13
  DIM t AS STRING * 12
  MID$(t, 2, 2) = MKI$(Min)
  MID$(t, 4, 2) = MKI$(max)
  MID$(t, 6, 2) = MKI$(iCtrl)
  MID$(t, 10, 2) = MKI$(stp)
  'Tagpos. 9 ist reserviert (Scrollflag)
  zCreateControl zScrollBar, sGroup$, sName$, "", max, x, y, x2, y2, w, h, t, -1
END SUB

'Zeigt ein Bild oder ein Symbol an.
'UnterstÅtzt werden die folgenden Formate:
'- *.LDS (LaOS Draw Script)
'- *.LSB (LaOS LDS Symbole)
'- *.ABM (AGI Bitmap)
'xx,yy = Die Koordinaten geben an, wo das Bild gezeichnet wird.
'sFileName = Der Dateiname der Grafik
SUB ShowPicture (xx, yy, sFileName AS STRING)

  f% = FREEFILE
  sFileName = RTRIM$(sFileName)
 
  IF LCASE$(RIGHT$(sFileName, 4)) <> ".icn" THEN
    OPEN sFileName FOR BINARY AS f%: GET f%, 1, tst!
    IF tst! = 0 THEN CLOSE f%: OPEN laosdir + "sys.lsb" FOR BINARY AS f%
    SEEK f%, 1
  END IF

  SELECT CASE LCASE$(RIGHT$(sFileName, 4))
  CASE ".icn"
      DIM ico(0 TO 505)
      DEF SEG = VARSEG(ico(0))
      errorlevel = 0
      BLOAD sFileName, VARPTR(ico(0))
      IF errorlevel THEN GOTO zsplds
      PUT (xx, yy), ico, PSET

  CASE ".lds", ".lsb"
zsplds:
    DIM Befehle(5) AS SINGLE
    DO
    FOR nr = 0 TO 5
    GET f%, , Befehle(nr)
    NEXT
    SELECT CASE CHR$(Befehle(0))
    CASE "p": PSET (Befehle(1) + xx, Befehle(2) + yy), Befehle(3)
    CASE "m": PAINT (Befehle(1) + xx, Befehle(2) + yy), Befehle(3), Befehle(4)
    CASE "l": LINE (Befehle(1) + xx, Befehle(2) + yy)-(Befehle(3) + xx, Befehle(4) + yy), Befehle(5)
    CASE "r": LINE (Befehle(1) + xx, Befehle(2) + yy)-(Befehle(3) + xx, Befehle(4) + yy), Befehle(5), B
    CASE "b": LINE (Befehle(1) + xx, Befehle(2) + yy)-(Befehle(3) + xx, Befehle(4) + yy), Befehle(5), BF
    CASE "k": CIRCLE (Befehle(1) + xx, Befehle(2) + yy), Befehle(3), Befehle(4), , , Befehle(5)
    END SELECT
    LOOP WHILE Befehle(0) <> 0
 
  CASE ELSE 'AGI Bitmap oder unbekannte Typen
    'AGI Bitmap (ABM2,ABMS)
    DIM pic(123) AS DOUBLE
    GET f%, 1, t&
    GET f%, , w%
    GET f%, , h%
    IF MKL$(t&) = "ABM2" THEN
      'AGI Bitmap mit Farbpalette
      'Farb-Palette laden:
      p$ = SPACE$(64)
      GET f%, , p$
      FOR x = 1 TO LEN(p$) STEP 4
         r% = INT((ASC(MID$(p$, x, 1))))
         g% = INT((ASC(MID$(p$, x + 1, 1))))
         b% = INT((ASC(MID$(p$, x + 2, 1))))
         zRGBPalette ((x - 1) / 4), r%, g%, b%
      NEXT
    ELSEIF MKL$(t&) = "ABMS" THEN
      IF xx = -1 THEN
        GET f%, , xx
        GET f%, , yy
      ELSE
        GET f%, , c
        GET f%, , c
      END IF
    ELSE
      sFileName = laosdir + "sys.lsb"
      GOTO zsplds
    END IF
    c = xx + w% - 1
    d = yy + h% - 1
    FOR x = xx TO c STEP 41
      rx% = c - x
      FOR y = yy TO d STEP 41
        ry% = d - y
        IF rx% < 41 OR ry% < 41 THEN
          IF rx% >= 41 THEN rx% = 40
          IF ry% >= 41 THEN ry% = 40
          g% = ((4 + INT(((PMAP(rx%, 0) - PMAP(0!, 0) + 1) * (1) + 7) / 8) * 4 * (PMAP(ry%, 1) - PMAP(0!, 1) + 1))) / 8
          REDIM pic(g%) AS DOUBLE
        ELSE
          REDIM pic(123) AS DOUBLE
        END IF
        FOR i% = LBOUND(pic) TO UBOUND(pic)
          GET f%, , pic(i%)
        NEXT
        IF y + 40 > 349 OR x + 40 > 639 THEN
          'Blockbreite und -Hîhe bestimmen:
          IF ry% >= 40 THEN ybb = 40 ELSE ybb = ry%
          IF rx% >= 40 THEN xbb = 40 ELSE xbb = rx%
          'Nicht zeichenbarer Rest bestimmen:
          IF y + 40 > 349 THEN resty = (y + ybb) - 349 ELSE resty = 0
          IF x + 40 > 639 THEN restx = (x + xbb) - 639 ELSE restx = 0
        ELSEIF resty + restx > 0 THEN
          resty = 0
          restx = 0
        END IF
        IF resty + restx > 0 THEN
          SCREEN 9, , 1, 0
          PUT (0, 0), pic, PSET
          GET (0, 0)-(xbb - restx, ybb - resty), pic
          SCREEN 9, , 0, 0
        END IF
        PUT (x, y), pic, PSET
      NEXT
    NEXT
  END SELECT
  CLOSE f%
  VIEW
END SUB

'Schaltet in den ruhigen Bildaufbaumodus.
'Kann dazu verwendet werden, den Grafikaufbau im Hintergrund
'durchzufÅhren. Der Benutzer kann dann dem Aufbau nicht mehr zusehen.
'Um die aufgebaute Grafik schlussendlich anzuzeigen, verwenden Sie bitte
'->RestoreScreen
SUB SilentScreen
  IF SilentScreenMode = -1 THEN zSSMStop = zSSMStop + 1: EXIT SUB
  SilentScreenMode = -1
  PCOPY 0, 1
  SCREEN 9, , 1, 0
  COLOR tcol
END SUB

'Zeigt ein Statustext in der Statusbar an.
SUB StatusBar (Text$)
  LINE (2, 332)-(637, 347), sfcol, BF
  LINE (2, 332)-(637, 332), stcol
  Farbe = stcol
  InversFarbe = sfcol
  EchoXY 3, 333, Text$
  InversFarbe = 0
  Farbe = tcol
END SUB

'Gibt das Datum im Datumsformat des Benutzers formatiert zurÅck.
'Wichtig: Nur fÅr Anzeige verwenden, da jeder Benutzer ein anderes
'         Datumsformat haben kînnte: Niemals als Datum Speichern...
'
'd$ = Das Datum, das zurÅckgegeben werden soll
'
FUNCTION SysDate$ (d$)
  SELECT CASE Dateformat
  CASE 0'TT.MM.JJJJ
  SysDate$ = MID$(d$, 4, 2) + "." + LEFT$(d$, 2) + "." + RIGHT$(d$, 4)
  CASE 1'MM-TT-JJJJ
  SysDate$ = d$
  CASE 2'JJJJ/MM/TT
  SysDate$ = RIGHT$(d$, 4) + "/" + LEFT$(d$, 2) + "/" + MID$(d$, 4, 2)
  END SELECT
END FUNCTION

'Gibt die Hîhe eines Textes zurÅck.
'BerÅcksichtigt dabei die durch einen Rahmen gesetzte Bereichsbegrenzung!
'text = Der Text dessen Hîhe zÅrÅckgegeben wird.
FUNCTION TextHeight% (Text AS STRING)
DIM linie AS STRING, txt AS STRING
x = 0
y = 0
txt = Text
panewidth = Right - left
leftpane = 0
maxy = 0
DO
  'holt die nÑchste Zeile:
  aw = 0
  DO
    w = INSTR(aw + 1, txt, " ")
    cr = INSTR(aw + 1, txt, crlf) 'Zeilenvorschub & WagenrÅcklauf?
    IF w = 0 THEN w = LEN(txt)
    IF cr > 0 THEN   'Zeilenvorschub & WagenrÅcklauf!
      IF NOT (TextLen(LEFT$(txt, cr - 1)) > panewidth) THEN
        'muss nicht vorher bereits umgebrochen werden?
        linie = LEFT$(txt, cr - 1)
        txt = MID$(txt, cr + 2)
        EXIT DO
      END IF
    END IF
    IF TextLen(LEFT$(txt, w)) > panewidth OR w = LEN(txt) THEN
      IF aw = 0 THEN aw = LEN(txt)
      IF w = LEN(txt) AND TextLen(LEFT$(txt, w)) < panewidth THEN aw = LEN(txt)
      IF aw = LEN(txt) AND TextLen(LEFT$(txt, w)) > panewidth THEN aw = FIX(panewidth / 8)
      linie = LEFT$(txt, aw)
      txt = MID$(txt, aw + 1)
      EXIT DO
    ELSE
      aw = w
    END IF
  LOOP UNTIL LEN(txt) = 0
  x = TextLen(linie)
  x = 0
  y = y + 14
  IF y > maxy THEN maxy = y
LOOP UNTIL LEN(txt) = 0
TextHeight% = maxy
END FUNCTION

'Gibt die LÑnge eines Textes zurÅck, berÅcksichtigt dabei nicht die
'BereichsbeschrÑnkungen eines Begrenzungs-Rahmens
'text = Der zu prÅfende Text
'-> Verwenden Sie TextLength%(text), um die effektive Breite eines Textes
'   zurÅckzugeben, welcher die BereichsbeschrÑnkungen der Begrenzungsrahmen
'   mitberÅcksichtigt.
FUNCTION TextLen% (Text AS STRING)
  TextLen% = LEN(Text) * 8
END FUNCTION

'Gibt die Breite eines Textblockes zurÅck, unter BerÅcksichtigung der
'BereichsbeschrÑnkungen des aktiven Begrenzungs-Rahmens.
'text = Der zu prÅfende Text
'-> Verwenden Sie TextLen%(text), um die LÑnge ohne BerÅcksichtigung
'   der BereichsbeschrÑnkungen zurÅckzugeben.
FUNCTION TextWidth% (Text AS STRING)
DIM linie AS STRING, txt AS STRING
  x = 0
  txt = Text
  panewidth = Right - left
  leftpane = 0
  max = 0
  DO
    aw = 0
    DO
      w = INSTR(aw + 1, txt, " ")
      cr = INSTR(aw + 1, txt, crlf)
      IF w = 0 THEN w = LEN(txt)
      IF cr > 0 THEN
        IF NOT (LEN(LEFT$(txt, cr - 1)) * 8 > panewidth) THEN
          linie = LEFT$(txt, cr - 1)
          txt = MID$(txt, cr + 2)
          EXIT DO
        END IF
      END IF
      IF TextLen(LEFT$(txt, w)) > panewidth OR w = LEN(txt) THEN
        IF aw = 0 THEN aw = LEN(txt)
        IF w = LEN(txt) AND TextLen(LEFT$(txt, w)) < panewidth THEN aw = LEN(txt)
        IF aw = LEN(txt) AND TextLen(LEFT$(txt, w)) > panewidth THEN aw = FIX(panewidth / 8)
        linie = LEFT$(txt, aw)
        txt = MID$(txt, aw + 1)
        EXIT DO
      ELSE
        aw = w
      END IF
    LOOP UNTIL LEN(txt) = 0
    x = LEN(linie) * 8
    IF x > max THEN max = x
    x = 0
  LOOP UNTIL LEN(txt) = 0
  TextWidth% = max
END FUNCTION

'Zeigt an, dass der Computer beschÑftigt ist.
SUB TimeLock (modus)
LINE (300, 164)-(338, 164), 8
LINE (339, 165)-(339, 183), 8
LINE (338, 184)-(300, 184), 8
LINE (299, 183)-(299, 165), 8
LINE (300, 165)-(338, 183), 7, BF
LINE (302, 167)-(336, 167), 6
LINE (336, 169)-(302, 169), 6
LINE (336, 182)-(302, 182), 3
IF modus = 1 THEN
  LINE (325, 176)-(336, 181), 14, BF
ELSE
  LINE (325, 176)-(336, 181), 11, BF
END IF
END SUB

SUB zCreateControl (iCtrlType, sGroup$, sName$, caption$, Value, x, y, x2, y2, w, h, tag$, iSelfHandler)
  IF LEN(sGroup$) > 8 THEN
    Dialog "Gruppenname zu lang! (" + sGroup$ + ") Max. 8 Zeichen"
    EXIT SUB
  ELSEIF LEN(sName$) > 8 THEN
    Dialog "Controlname zu lang! (" + sName$ + ") Max. 8 Zeichen"
    EXIT SUB
  ELSEIF NextCtrl > UBOUND(Controls) THEN
    PRINT "Maximale Anzahl Controls Åberschritten!"
  END IF
  IF LEN(sName$) > 0 THEN
    Controls(NextCtrl).caption = sName$
  ELSE
    Controls(NextCtrl).caption = caption$
  END IF
  IF NOT (x2 = 639 OR y2 = 349) THEN
    IF x + y = 0 AND x2 + y2 <> 0 THEN x = CurrentX: y = CurrentY: x2 = x2 + x: y2 = y2 + y
  END IF
  Controls(NextCtrl).x = x
  Controls(NextCtrl).y = y
  Controls(NextCtrl).x2 = x2
  Controls(NextCtrl).y2 = y2
  CurrentX = x
  CurrentY = y2 + 5
  Controls(NextCtrl).w = w
  Controls(NextCtrl).h = h
  Controls(NextCtrl).tag = tag$
  Controls(NextCtrl).SelfHandler = iSelfHandler
  Controls(NextCtrl).CtrlType = iCtrlType
  Controls(NextCtrl).Group = sGroup$
  Controls(NextCtrl).Value = Value
  NextCtrl = NextCtrl + 1
END SUB

SUB zDoEvents
  FOR i = 0 TO UBOUND(zTimeEvent)
    IF TIMER >= zTimeEvent(i).nextime AND zTimeEvent(i).inc > 0 THEN
      zTimeEventHandler i
      zTimeEvent(i).nextime = TIMER + zTimeEvent(i).inc
    END IF
  NEXT
END SUB

SUB zDoMouse
  IF MouseOn THEN
    DEF SEG = VARSEG(iMS(0))
    POKE VARPTR(iMS(0)) + 26, &H33
    CALL ABSOLUTE(3, b, x, y, VARPTR(iMS(0)))
    'PrÅfe auf Doppelklick:
    IF Mouse.time > 0 AND b = 1 THEN
      IF TIMER - Mouse.time < .8 THEN
        IF x > Mouse.ClickX - 2 AND x < Mouse.ClickX + 2 THEN
          IF y > Mouse.ClickY - 2 AND y < Mouse.ClickY + 2 THEN
            MouseOn = 2
            WHILE b > 0
              CALL ABSOLUTE(3, b, x, y, VARPTR(iMS(0)))
            WEND
          END IF
        END IF
      END IF
    END IF
  END IF
END SUB

SUB zDrawBox (x1, y1, x2, y2)
  LINE (x1, y1)-(x2, y2), wincol, B
  LINE (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), frmcol, B
  LINE (x1 + 2, y1 + 2)-(x2 - 2, y2 - 2), 0, BF
END SUB

SUB zDrawCtrl (iCtrl, MouseDown, Refresh)
DIM Text AS STRING
  i = iCtrl
  IF i = 0 THEN EXIT SUB
  x = Controls(i).x
  y = Controls(i).y
  x2 = Controls(i).x2
  y2 = Controls(i).y2
  w = Controls(i).w
  h = Controls(i).h
  cap$ = RTRIM$(Controls(i).caption)
  Activated = (ActiveCtrl = i)
  Value = Controls(i).Value
  am = InversFarbe
  af = Farbe
  InversFarbe = 0
  tag$ = Controls(i).tag
  Farbe = tcol

  SELECT CASE Controls(i).CtrlType
  CASE IS < 0
    Controls(i).CtrlType = -Controls(i).CtrlType
    zDrawCtrl i, MouseDown, Refresh
    Controls(i).CtrlType = -Controls(i).CtrlType
    FOR i = y + 2 TO y2 - 2 STEP 2
        LINE (x + 1, i)-(x2 - 2, i), 7, , 21845
        IF i < y2 - 2 THEN LINE (x + 2, i + 1)-(x2 - 2, i + 1), 7, , 21845
    NEXT

  CASE zEditor
    IF Refresh THEN
      LINE (x - 2, y)-(x2, y2), 15, BF
      LINE (x - 3, y - 1)-(x2 + 1, y2 + 1), 1, B
      zEditorHandler Controls(iCtrl).Group, iCtrl, Refresh, 0
    END IF
  CASE zIconList
    IF NOT MouseDown THEN
      IF Refresh THEN
        MID$(Controls(i).tag, 1, 2) = MKI$(0)
        MID$(Controls(iCtrl).tag, 5, 2) = MKI$(MaxItems)
      END IF
      zIconListHandler sGroup$, i, -1
    END IF

  CASE zScrollBar
    Min = CVI(MID$(tag$, 2, 2))
    max = CVI(MID$(tag$, 4, 2))
    IF max - Min = 0 THEN max = max + 1
    IF Value > max THEN Controls(iCtrl).Value = max: Value = max
    IF Value < Min THEN Controls(iCtrl).Value = Min: Value = Min
    IF Refresh THEN
      'Knîpfe oben und unten:
      LINE (x + 1, y + 1)-(x2 - 1, y + 10), sbbcol, BF
      LINE (x + 1, y2 - 1)-(x2 - 1, y2 - 10), sbbcol, BF
    END IF
    IF w > 0 THEN
      'horizontal:
      'FÅllung innen:
      LINE (x + 12, y + 1)-(x2 - 12, y2 - 1), sbfcol, BF
      IF Refresh THEN
        'Rahmen:
        LINE (x + 11, y)-(x2 - 11, y2), sbrcol, B
        LINE (x, y)-(x2, y2), sbrcol, B
        COLOR bttcol
        LINE (x + 8, y + 2)-(x + 8, y + 10)
        LINE (x + 8, y + 10)-(x + 4, y + 6)
        LINE (x + 4, y + 6)-(x + 8, y + 2)
        PAINT (x + 7, y + 6), bttcol

        LINE (x2 - 8, y + 2)-(x2 - 8, y + 10)
        LINE (x2 - 8, y + 10)-(x2 - 4, y + 6)
        LINE (x2 - 4, y + 6)-(x2 - 8, y + 2)
        PAINT (x2 - 6, y + 6), bttcol
      END IF
      x = x + 12
      x2 = x2 - 12
      x = (x + ((Value - Min) * CLNG((x2 - x - 10)) / (max - Min)))
      x2 = x + 10
      y = y + 1
      y2 = y2 - 1
    ELSE
      'vertikal:
      'FÅllung innen:
      LINE (x + 1, y + 12)-(x2 - 1, y2 - 12), sbfcol, BF
      IF Refresh THEN
        'Rahmen:
        LINE (x, y + 11)-(x2, y2 - 11), sbrcol, B
        LINE (x, y)-(x2, y2), sbrcol, B
        'Pfeil rauf
        COLOR tcol
        LINE (x + 7, y + 4)-(x + 10, y + 7)
        LINE (x + 7, y + 4)-(x + 3, y + 7)
        LINE (x + 3, y + 7)-(x + 10, y + 7)
        PAINT (x + 6, y + 5)
        'Pfeil runter
        LINE (x + 7, y2 - 4)-(x + 10, y2 - 7)
        LINE (x + 7, y2 - 4)-(x + 3, y2 - 7)
        LINE (x + 3, y2 - 7)-(x + 10, y2 - 7)
        PAINT (x + 6, y2 - 5)
      END IF
      y = y + 12
      y2 = y2 - 12
      y = (y + ((Value - Min) * CLNG((y2 - y - 10)) / (max - Min)))
      y2 = y + 10
      x = x + 1
      x2 = x2 - 1
    END IF
    'Button:
    IF sbscol < 0 THEN
      LINE (x, y)-(x2, y2), btfcol, BF
      LINE (x, y)-(x2, y2), btlcol, B
      LINE (x, y2)-(x2, y2), btrcol
      LINE (x2, y2)-(x2, y), btrcol
      LINE (x + 1, y2 - 1)-(x2 - 1, y2 - 1), btscol
      LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), btscol
    ELSE
      LINE (x + 1, y + 1)-(x2 - 1, y2 - 1), sbscol, BF
    END IF

  CASE zMenu
    IF Activated THEN
      InversFarbe = mfcol
      Farbe = mtcol
    END IF
    EchoXY x, y, cap$

  CASE zButton
    IF Refresh OR MouseDown THEN LINE (x, y)-(x2, y2), btfcol, BF
    IF MouseDown THEN
      LINE (x, y)-(x2, y2), btrcol, B
      LINE (x, y2)-(x2, y2), btlcol
      LINE (x2, y2)-(x2, y), btlcol
    ELSE
      LINE (x, y)-(x2, y2), btlcol, B
      LINE (x, y2)-(x2, y2), btrcol
      LINE (x2, y2)-(x2, y), btrcol
    END IF
    LINE (x + 1, y2 - 1)-(x2 - 1, y2 - 1), btscol
    LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), btscol
    Farbe = bttcol
    InversFarbe = btfcol
    IF MouseDown THEN
      EchoXY x + 6, y + 5, cap$
      LINE (x + 3, y + 3)-(x2 - 3, y2 - 3), focol, B, PunkteLinie
    ELSE
      IF Activated THEN
        LINE (x + 3, y + 3)-(x2 - 3, y2 - 3), focol, B, PunkteLinie
      ELSE
        LINE (x + 3, y + 3)-(x2 - 3, y2 - 3), btfcol, B
      END IF
      EchoXY x + 5, y + 4, cap$
    END IF
    Farbe = tcol
    IF DefaultCtrl = iCtrl THEN
      LINE (x, y2 + 1)-(x2 + 1, y2 + 1), btrcol
      LINE (x2 + 1, y2 + 1)-(x2 + 1, y), btrcol
    END IF
           
  CASE zRadioButton
    CIRCLE (x + 7, y + 9), 8, tcol
    EchoXY x + 24, y + 3, cap$
    IF Value = -1 THEN
      CIRCLE (x + 7, y + 9), 5, tcol
      PAINT (x + 7, y + 9), tcol
    ELSE
      PAINT (x + 7, y + 9), 0, 0
    END IF
    IF Activated = -1 THEN
      LINE (x + 22, y + 2)-(x2 - 3, y2 - 1), focol, B, PunkteLinie
    ELSE
      LINE (x + 22, y + 2)-(x2 - 3, y2 - 1), 0, B
    END IF

  CASE zCheckBox
    LINE (x, y + 3)-(x + 15, y + 15), 0, BF
    LINE (x, y + 3)-(x + 15, y + 15), tcol, B
    EchoXY x + 24, y + 3, cap$
    IF Value = -1 THEN
      LINE (x, y + 3)-(x + 15, y + 15), tcol
      LINE (x + 15, y + 3)-(x, y + 15), tcol
    END IF
    IF Activated = -1 THEN
      LINE (x + 22, y + 2)-(x2 - 3, y2 - 1), focol, B, PunkteLinie
    ELSE
      LINE (x + 22, y + 2)-(x2 - 3, y2 - 1), 0, B
    END IF

  CASE zEditBox
    IF NOT MouseDown THEN 'keine VerÑnderung bei MouseDown!!!
      LINE (x, y)-(x2, y2), tbfcol, BF
      LINE (x, y)-(x2, y2), tbbcol, B
      zEditBoxHandler "", cap$, iCtrl, x + 3, y + 1, x2, y2, w, h, Text, -1
    END IF

  CASE zListBox
    IF NOT MouseDown THEN
      IF Refresh THEN
        'MaxItems auf 0 setzen damit wieder erneut gezÑhlt wird...
        MID$(Controls(CVI(MID$(Controls(iCtrl).tag, 3, 2))).tag, 4, 2) = MKI$(0)
        IF LEFT$(tag$, 1) = "M" THEN
          'Menustyle:
          LINE (x, y)-(x2, y2), 0, BF
          LINE (x, y)-(x2, y2), tcol, B
          LINE (x2 + 1, y + 1)-(x2 + 1, y2 + 1), shdcol
          LINE (x2, y2 + 1)-(x + 1, y2 + 1), shdcol
        ELSE
          'normal:
          zDrawBox x, y, x2, y2
        END IF
      END IF
      zListboxHandler "", iCtrl, x, y, x2, y2, w, h, -1
    END IF

  CASE zMouseClicker
    'w = WithShadow
    'h = ExitBox
    IF h THEN
      LINE (x, y)-(x + 15, y + 13), btscol, BF
      LINE (x, y)-(x + 15, y + 13), btrcol, B
      LINE (x + 7, y + 6)-STEP(3, 3), 1, BF
      LINE (x + 5, y + 5)-STEP(3, 3), btrcol, BF
    ELSEIF w THEN
      IF MouseDown THEN
        LINE (x, y)-(x2, y2), btrcol, B
        LINE (x, y2)-(x2, y2), btlcol
        LINE (x2, y2)-(x2, y), btlcol
      ELSE
        LINE (x, y)-(x2, y2), btlcol, B
        LINE (x, y2)-(x2, y2), btrcol
        LINE (x2, y2)-(x2, y), btrcol
      END IF
      LINE (x + 1, y2 - 1)-(x2 - 1, y2 - 1), btscol
      LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), btscol
    END IF
  END SELECT
  Farbe = af
  COLOR Farbe
  InversFarbe = am
END SUB

'Zeichnet den Mauszeiger.
'Maximale Grîsse: 16 x 19 Pixel
SUB zDrawMouse (x, y)
  x2 = x + 15: IF x2 > 639 THEN x2 = 639
  y2 = y + 18: IF y2 > 349 THEN y2 = 349
  GET (x, y)-(x2, y2), zMHG
  SELECT CASE Mouse.Cursor
  CASE 1 'Geladener Standard-Mauszeiger
    DRAW "BM" + STR$(x) + "," + STR$(y) + zMCS
  CASE 2  'Text-Mauszeiger
    DRAW "BM" + STR$(x) + "," + STR$(y) + "c" + STR$(tbtcol) + "r2l1d13l1r2"
  CASE 3  'Standard + Verschieben
    DRAW "BM" + STR$(x) + "," + STR$(y) + zMCS
    LINE (x + 7, y + 12)-(x + 15, y + 12)
    LINE (x + 11, y + 8)-(x + 11, y + 16)
    LINE (x + 8, y + 3 + 8)-(x + 8, y + 13)
    LINE (x + 10, y + 1 + 8)-(x + 12, y + 9)
    LINE (x + 10, y + 7 + 8)-(x + 12, y + 15)
    LINE (x + 14, y + 3 + 8)-(x + 14, y + 13)
  END SELECT
END SUB

SUB zEditBoxHandler (sGroup$, sName$, iCtrl, x1, y1, x2, y2, iAnzSpalten, iAnzZeilen, Text AS STRING, Refresh)

DIM oldText         AS STRING 'behÑlt den alten Text (vor der Eingabe)
DIM linie           AS STRING
DIM txt             AS STRING
DIM txt2            AS STRING
DIM linie2          AS STRING
DIM echoLineNr       'Die Liniennummer der Ausgabe!
DIM topperLine       'Die Linie, die die erste sichtbare ist (im Fenster)
DIM visibleLines
DIM markerStart      'Das Startzeichen der Markierung innerhalb des Textes
DIM outerVisible     '-1 = es ist der Bereich Åberschritten!
DIM topperVPos       'Die Position im String, bei der das Sichtfenster beginnt!
DIM bottomVPos       'Die Position im String, bei der das Sichtgfenster aufhîrt!
DIM iAnzZeichen
DIM actualizeLines   'Anzahl Linien, nach CurrentLine, die aktualisiert werden mÅssen!
DIM InsertMode
DIM CurrentLineLen   'LÑnge der aktuellen Linie
DIM CurrentLineStart  'Startposition der aktuellen Linie im String
DIM iMaxZeichen

'0. Initialisierungen
zEditboxInitialize:
  GetString sName$, Text
  InsertMode = 0
  oldText = ""
  topperLine = 1
  visibleLines = iAnzZeilen
  markerStart = Controls(iCtrl).Value
  iMaxZeichen = CVI(MID$(Controls(iCtrl).tag, 1, 2))
  wishedTopper = CVI(MID$(Controls(iCtrl).tag, 3, 2))
  Colored = MID$(Controls(iCtrl).tag, 5, 1) = "C"

IF Mouse.Clicked = 2 AND Refresh = 0 THEN
  'KontextmenÅ:
  a$ = MenuInt(" Text einfÅgen , Text kopieren,-, Text lîschen")
  a$ = ""
  Mouse.Clicked = 0
  f = FREEFILE
  SELECT CASE choice
  CASE 1: OPEN tempdir + "zwa.txt" FOR INPUT AS f
          LINE INPUT #f, a$: Text = LEFT$(Text, markerStart - 1) + a$ + MID$(Text, markerStart)
  CASE 2: OPEN tempdir + "zwa.txt" FOR OUTPUT AS f
          PRINT #f, Text
  CASE 4: Text = ""
          LINE (x1, y1)-(x2 - 1, y2 - 1), tbfcol, BF
  END SELECT
  CLOSE f
  ActiveCtrl = iCtrl
  markerStart = LEN(Text) + 1
ELSEIF Mouse.Clicked = 1 AND NOT Refresh THEN
  markerStart = LEN(Text) + 1
END IF

zEditBoxCalculateCursor:
'1. Es muss geprÅft werden, welchen Bereich angezeigt werden muss:
'   (Der Bereich, wo markerStart drin ist!)
'Fragestellung: Welches ist die Zeile des Cursors (markerStart)?

    IF iMaxZeichen > 0 AND LEN(Text) > iMaxZeichen THEN Text = LEFT$(Text, iMaxZeichen)
    txt = Text
    panewidth = TextLen(SPACE$(iAnzSpalten))
    echoLineNr = 0
    iAnzZeichen = 0
    CurrentLine = 0
    IF markerStart < 0 THEN markerStart = 1
    DO
        'holt die nÑchste Zeile:
        aw = 0
        echoLineNr = echoLineNr + 1
        DO
            w = INSTR(aw + 1, txt, " ")
            cr = INSTR(aw + 1, txt, crlf) 'Zeilenvorschub & WagenrÅcklauf?

            IF w = 0 THEN w = LEN(txt)

            IF cr > 0 THEN   'Zeilenvorschub & WagenrÅcklauf!
                IF NOT (TextLen(LEFT$(txt, cr - 1)) > panewidth) THEN
                    'muss nicht vorher bereits umgebrochen werden?
                    linie = LEFT$(txt, cr - 1)
                    txt = MID$(txt, cr + LEN(crlf))
                    EXIT DO
                END IF
            END IF

            IF TextLen(LEFT$(txt, w)) > panewidth OR w = LEN(txt) THEN
                IF w = LEN(txt) AND w < iAnzSpalten THEN aw = LEN(txt)
                IF aw = 0 THEN aw = w
                IF aw > iAnzSpalten THEN aw = iAnzSpalten
                linie = LEFT$(txt, aw)
                txt = MID$(txt, aw + 1)
                EXIT DO
            ELSE
                aw = w
            END IF
        LOOP UNTIL LEN(txt) = 0

        'Linie auslesen:
        FOR i = 1 TO LEN(linie)
            iAnzZeichen = iAnzZeichen + 1
            IF iAnzZeichen = markerStart THEN
                CurrentLine = echoLineNr
            END IF
        NEXT

    'Exit Loop bei:
    ' - Der Textbuffer 'txt' ist leer!
    LOOP UNTIL LEN(txt) = 0

    'Cursor ist nach dem Text:
    IF CurrentLine = 0 THEN
        CurrentLine = echoLineNr
    END IF

    IF wishedTopper > 0 THEN
        topperLine = wishedTopper: wishedTopper = 0
    ELSE
        IF topperLine > CurrentLine THEN
            topperLine = CurrentLine
            actualizeLines = iAnzZeilen
            IF iAnzZeilen > 1 THEN
                zMoveScreen x1, y1, x1 + (iAnzSpalten * 8) - 1, y1 + (iAnzZeilen * 14) - 15, x1, y1 + 14
            END IF
        ELSEIF CurrentLine + 1 > topperLine + iAnzZeilen THEN
            topperLine = CurrentLine - iAnzZeilen + 1
            actualizeLines = iAnzZeilen
            IF iAnzZeilen > 1 THEN
                zMoveScreen x1, y1 + 14, x1 + (iAnzSpalten * 8) - 1, y1 + (iAnzZeilen * 14) - 1, x1, y1
            END IF
        END IF
    END IF

zEditboxWriteText:
'2. Schreibe alles, was in den sichtbaren Bereich passt:

    'SilentScreen 'ruhiger Bildschirm
    x = x1
    y = y1
    txt = Text
    panewidth = TextLen(SPACE$(iAnzSpalten))
    leftpane = x
    txt2 = oldText
    echoLineNr = 0
    topperVPos = 1
    bottomVPos = 0
    CurrentX = 0
    iCurOverEnd = -1
    iAnzZeichen = 0
    IF CurrentLine < topperLine THEN
        'Alle Zeilen in der aktuellen Ansicht werden aktualisiert
        topperLine = CurrentLine
        actualizeLines = iAnzZeilen
    ELSE
        actualizeLines = 0
    END IF
    IF Colored THEN
      InversFarbe = 1
      Farbe = 15
    ELSE
      InversFarbe = tbfcol
      Farbe = tbtcol
    END IF
    DO
        'holt die nÑchste Zeile:
        aw = 0
        echoLineNr = echoLineNr + 1
        DO
            w = INSTR(aw + 1, txt, " ")
            cr = INSTR(aw + 1, txt, crlf) 'Zeilenvorschub & WagenrÅcklauf?

            IF w = 0 THEN w = LEN(txt)

            IF cr > 0 THEN   'Zeilenvorschub & WagenrÅcklauf!
                IF NOT (TextLen(LEFT$(txt, cr - 1)) > panewidth) THEN
                    'muss nicht vorher bereits umgebrochen werden?
                    linie = LEFT$(txt, cr - 1)
                    txt = MID$(txt, cr + LEN(crlf))
                    EXIT DO
                END IF
            END IF

            IF TextLen(LEFT$(txt, w)) > panewidth OR w = LEN(txt) THEN
                IF w = LEN(txt) AND w < iAnzSpalten THEN aw = LEN(txt)
                IF aw = 0 THEN aw = w
                IF aw > iAnzSpalten THEN aw = iAnzSpalten
                linie = LEFT$(txt, aw)
                txt = MID$(txt, aw + 1)
                EXIT DO
            ELSE
                aw = w
            END IF
        LOOP UNTIL LEN(txt) = 0

        'holt die nÑchste Zeile von oldtext:
        aw = 0
        DO
            w = INSTR(aw + 1, txt2, " ")
            cr = INSTR(aw + 1, txt2, crlf) 'Zeilenvorschub & WagenrÅcklauf?

            IF w = 0 THEN w = LEN(txt2)

            IF cr > 0 THEN   'Zeilenvorschub & WagenrÅcklauf!
                IF NOT (TextLen(LEFT$(txt2, cr - 1)) > panewidth) THEN
                    'muss nicht vorher bereits umgebrochen werden?
                    linie2 = LEFT$(txt2, cr - 1)
                    txt2 = MID$(txt2, cr + LEN(crlf))
                    EXIT DO
                END IF
            END IF

            IF TextLen(LEFT$(txt2, w)) > panewidth OR w = LEN(txt2) THEN
                IF w = LEN(txt2) AND w < iAnzSpalten THEN aw = LEN(txt2)
                IF aw = 0 THEN aw = w
                IF aw > iAnzSpalten THEN aw = iAnzSpalten
                linie2 = LEFT$(txt2, aw)
                txt2 = MID$(txt2, aw + 1)
                EXIT DO
            ELSE
                aw = w
            END IF
        LOOP UNTIL LEN(txt2) = 0

        'Nur, Linien, die sich im Sichtfenster befinden!
        IF echoLineNr >= topperLine AND echoLineNr <= topperLine + visibleLines - 1 THEN

            'Hat Linie gewechselt?, oder ist explizit gewÅnscht, diese Linie
            'neu zu zeichnen!
            IF linie2 <> linie OR echoLineNr = CurrentLine OR echoLineNr >= CurrentLine AND echoLineNr <= CurrentLine + actualizeLines THEN
                IF echoLineNr = CurrentLine THEN
                    CurrentLineLen = LEN(linie)
                    CurrentLineStart = iAnzZeichen + 1
                END IF
                'leeren Text entfernen auf dieser Zeile!
                LINE (x + TextLen(linie), y)-(x + TextLen(linie) + TextLen(SPACE$(iAnzSpalten - LEN(linie))), y + 13), tbfcol, BF
                'Text schreiben:
                EchoXY x, y, linie
                FOR i = 1 TO LEN(linie)
                    iAnzZeichen = iAnzZeichen + 1
                    IF Mouse.Clicked THEN
                        IF Mouse.x + 4 <= x + 8 THEN
                            IF Mouse.x + 4 >= x THEN
                                IF (Mouse.y + 7) <= y + 14 THEN
                                    IF (Mouse.y + 7) + 2 >= y THEN
                                        'Hier ist der Cursor!
                                        markerStart = iAnzZeichen
                                    END IF
                                END IF
                            END IF
                        END IF
                    END IF

                    IF iAnzZeichen = markerStart THEN
                        CurrentX = x
                        CurrentY = y
                        CurrentLine = echoLineNr
                        iCurOverEnd = 0
                    END IF
                    x = x + 8
                NEXT

                'Cursor ist nach dem Text:
                IF iCurOverEnd THEN
                    CurrentX = leftpane + TextLen(linie)
                    CurrentY = y
                    CurrentLine = echoLineNr
                    IF echoLineNr = CurrentLine AND iAnzZeichen + 1 < markerStart THEN
                        CurrentLineLen = LEN(linie)
                        CurrentLineStart = iAnzZeichen + 1
                    END IF
                END IF

            ELSE
                'linie nicht erneut zeichnen! - Zeichen doch zÑhlen...
                iAnzZeichen = iAnzZeichen + LEN(linie)
            END IF
         
            x = leftpane
            y = y + 14

        ELSE
            'Linie ist nicht im aktuellen Sichtfenster!
            iAnzZeichen = iAnzZeichen + LEN(linie)
            'schlÑgt oben an:
            IF echoLineNr < topperLine THEN
                topperVPos = topperVPos + LEN(linie)
            ELSE
                'schlÑgt unten an!
                IF bottomVPos = 0 THEN bottomVPos = iAnzZeichen
            END IF
        END IF
  
    'Exit Loop bei:
    ' - Linie lÑuft Åber das Sichtfenster hinaus!
    ' - Der Textbuffer 'txt' ist leer!
    LOOP UNTIL (LEN(txt) = 0 AND LEN(txt2) = 0)
  
    IF bottomVPos = 0 THEN bottomVPos = LEN(Text)
    'text war leer - Cursor jetzt positionieren:
    IF CurrentX = 0 THEN
        CurrentX = leftpane
        CurrentY = y - 14
    END IF

    IF RIGHT$(Text, LEN(crlf)) = crlf AND markerStart > LEN(Text) THEN
        CurrentY = CurrentY + 14
        CurrentX = leftpane
    END IF

    'wenn nur Anzeige aktualisieren gew. -> hier beenden!
    IF Refresh THEN GOTO zEditBoxTerminate
    IF Colored = 0 OR LEN(Text) = 0 THEN
      'Cursor zeichnen:
      FOR y = CurrentY TO CurrentY + 13
        IF POINT(CurrentX, y) = tbfcol THEN
          PSET (CurrentX, y), tbtcol
        ELSE
          PSET (CurrentX, y), tbfcol
        END IF
      NEXT
    END IF
    Farbe = tcol
    a$ = zGK(sGroup$)
    IF Colored THEN
      'Colored: Text lîschen!
      IF LEN(keyb) = 1 OR keyb = "backspace" OR keyb = "delete" THEN Text = ""
      Colored = 0
      MID$(Controls(iCtrl).tag, 5, 1) = " "
    END IF
    'Cursor lîschen:
    FOR y = CurrentY TO CurrentY + 13
      IF POINT(CurrentX, y) = tbfcol THEN
        PSET (CurrentX, y), tbtcol
      ELSE
        PSET (CurrentX, y), tbfcol
      END IF
    NEXT
    IF markerStart > LEN(Text) THEN markerStart = LEN(Text) + 1
    IF Mouse.Clicked THEN GOTO zEditBoxTerminate
    oldText = Text
    IF keyb = "home" AND iAnzZeilen = 1 THEN keyb = "ctrl-home"
    IF keyb = "end" AND iAnzZeilen = 1 THEN keyb = "ctrl-end"
    SELECT CASE keyb
    CASE "event"
    CASE "pagedown"
        markerStart = bottomVPos + 1
        IF markerStart > LEN(Text) + 1 THEN markerStart = LEN(Text) + 1
        oldText = ""
       
    CASE "pageup"
        markerStart = topperVPos - 1
        IF markerStart < 1 THEN markerStart = 1
        oldText = ""

    CASE "ctrl-right"
        'Wort nach rechts:
        FOR i = markerStart + 1 TO LEN(Text) + 1
            IF MID$(Text, i, 1) = " " THEN EXIT FOR
        NEXT
        IF i > LEN(Text) THEN i = LEN(Text) + 1
        markerStart = i
  
    CASE "ctrl-end"
        markerStart = LEN(Text) + 1
        oldText = ""

    CASE "ctrl-home"
        markerStart = 1
        oldText = ""
  
    CASE "ctrl-left"
        'Wort nach links:
        FOR i = markerStart - 1 TO 1 STEP -1
            IF i <= 1 OR MID$(Text, i, 1) = " " THEN EXIT FOR
        NEXT
        IF i < 1 THEN i = 1
        markerStart = i

    CASE "home"
        markerStart = CurrentLineStart

    CASE "right"
        markerStart = markerStart + 1
        IF markerStart > LEN(Text) + 1 THEN
            markerStart = LEN(Text) + 1
        END IF

    CASE "end"
        'Line-End:
        markerStart = CurrentLineStart + CurrentLineLen - 1
        IF markerStart = LEN(Text) THEN
            markerStart = markerStart + 1
        END IF

    CASE "up"
        IF markerStart - iAnzSpalten > 1 THEN
            markerStart = CurrentLineStart - FIX(iAnzSpalten / 2)
        ELSE
            markerStart = 1
        END IF
    CASE "down"
        IF markerStart + iAnzSpalten <= LEN(Text) THEN
          markerStart = CurrentLineStart + FIX(iAnzSpalten / 2) + CurrentLineLen
        ELSE
          markerStart = LEN(Text) + 1
        END IF
    CASE "left"
        markerStart = markerStart - 1
        IF markerStart < 1 THEN markerStart = 1
    CASE "insert"
        InsertMode = -1 - InsertMode
    CASE "delete"
        Text = LEFT$(Text, markerStart - 1) + MID$(Text, markerStart + 1)
    CASE "backspace"
      IF markerStart > 1 THEN
        Text = LEFT$(Text, markerStart - 2) + MID$(Text, markerStart)
        markerStart = markerStart - 1
      END IF
    CASE ELSE
      IF zQuitKey THEN GOTO zEditBoxTerminate
      'Anderer Tastendruck:
      ' -> Buchstaben hinzu! (Die Markierung wird durch Buchstaben ersetzt)
      IF NOT (iMaxZeichen > 0 AND LEN(Text) + LEN(a$) > iMaxZeichen) THEN
        IF InsertMode THEN
          Text = LEFT$(Text, markerStart - 1) + a$ + MID$(Text, markerStart + LEN(a$))
        ELSE
          Text = LEFT$(Text, markerStart - 1) + a$ + MID$(Text, markerStart)
        END IF
        markerStart = markerStart + LEN(a$)
      END IF
    END SELECT
    GOTO zEditBoxCalculateCursor
zEditBoxTerminate:
  InversFarbe = 0
  PutString sName$, Text
  Controls(iCtrl).Value = markerStart
  MID$(Controls(iCtrl).tag, 3, 2) = MKI$(topperLine)
END SUB

SUB zEditorHandler (sGroup$, iCtrl, iRefresh, dblClicked)

  'Abmessungen und Daten des Editors einlesen:
  CurrentLine = Controls(iCtrl).Value 'Linie von Scrollbar
  sb = iCtrl + 1
  zDrawCtrl sb, 0, 0
  MaxLines = CVI(MID$(Controls(sb).tag, 4, 2))

  pageBreakPos = Controls(iCtrl).w 'Maximale Anzahl Zeichen in der Horizontale (Breite)
  name$ = RTRIM$(Controls(iCtrl).caption)
  WindH = Controls(iCtrl).h 'Fensterhîhe
  WindW = INT((Controls(iCtrl).x2 - Controls(iCtrl).x) / 8) 'Fensterbreite
  WindLeft = Controls(iCtrl).x
  WindTop = Controls(iCtrl).y
  IF WindW > pageBreakPos THEN
    'Die Fensterbreite darf nicht hîher sein als die Zeilenumbruch-Position:
    WindW = pageBreakPos
  END IF

  'Fixe:
  f = FREEFILE
  OPEN tempdir + name$ + ".tmp" FOR RANDOM AS f LEN = (pageBreakPos + 5) 'Reserve fÅr Wîrter einberechnen!

  IF CurrentLine > 0 THEN
    'Die Linie wurde von Scrollbar geholt!
    CurrentPos = 1
  END IF

  'Variable - werden durch den Algol geÑndert.
  IF CurrentLine = 0 THEN
    'Falls noch kein Text verfÅgbar - alles auf 1 setzen:
    CurrentLine = 1
    CurrentPos = 1
  END IF

  FirstVisPos = CVI(MID$(Controls(iCtrl).tag, 9, 2))
  FirstVisLine = CVI(MID$(Controls(iCtrl).tag, 11, 2))
  IF FirstVisLine = 0 OR FirstVisPos = 0 THEN
    FirstVisLine = 1
    FirstVisPos = 1
  END IF
 
  IF CurrentLine < FirstVisLine OR CurrentLine > FirstVisLine + WindH THEN CurrentLine = FirstVisLine
  IF CurrentPos < FirstVisPos OR CurrentPos > FirstVisPos + WindW THEN CurrentPos = FirstVisPos

zEditorInput:
  MarkSpalte1 = CVI(MID$(Controls(iCtrl).tag, 1, 2))
  MarkSpalte2 = CVI(MID$(Controls(iCtrl).tag, 3, 2))
  MarkZeile1 = CVI(MID$(Controls(iCtrl).tag, 5, 2))
  MarkZeile2 = CVI(MID$(Controls(iCtrl).tag, 7, 2))
  Farbe = 8
  InversFarbe = 15
  'Allgemeine Variablen-Korrekturen vornehmen:
  IF CurrentLine > MaxLines THEN
    'Scrollbar nachfÅhren mit der neuen Anzahl Zeilen:
    MID$(Controls(sb).tag, 4, 2) = MKI$(MaxLines)
    Controls(sb).Value = CurrentLine
    zDrawCtrl sb, 0, 0
    MaxLines = CurrentLine
  END IF
  IF Controls(sb).Value <> CurrentLine THEN
    'Zeile hat geÑndert: Scrollbar aktualisieren!
    Controls(sb).Value = CurrentLine
    zDrawCtrl sb, 0, 0
  END IF
  IF CurrentLine < 1 THEN
    'BOF! Keine niedrige Zeile mehr...
    CurrentLine = 1
  END IF
  IF CurrentPos < 1 THEN
    'Position muss neu berechnet werden, und zwar auf die LÑnge der
    'aktuellen Zeile gesetzt werden!
    GET f, CurrentLine, a$
    CurrentPos = LEN(RTRIM$(a$)) + 1
  END IF
  IF CurrentPos > pageBreakPos + 1 THEN
    'Cursor an die nÑchste Zeile setzen:
    CurrentPos = 1
    CurrentLine = CurrentLine + 1
  END IF
  IF CurrentPos > WindW THEN
    'Wenn die aktuelle Position nicht mehr ins Bild passt,
    'wird die FirstVisPos angepasst:
    FirstVisPos = CurrentPos - WindW
  END IF
  IF CurrentPos < FirstVisPos THEN
    'Cursorposition nicht mehr im Sichtbereich:
    'Sichtbereich nach links korrigieren!
    FirstVisPos = CurrentPos
  END IF
  IF FirstVisPos < 1 THEN FirstVisPos = 1
  IF CurrentLine < FirstVisLine THEN
    'Sichtfenster nach oben korrigieren!
    FirstVisLine = CurrentLine
  ELSEIF CurrentLine > FirstVisLine + WindH THEN
    'Sichtfenster so viel wie nîtig nach unten korrigieren!
    DO
      FirstVisLine = FirstVisLine + 1
    LOOP WHILE CurrentLine > FirstVisLine + WindH
  END IF

  'Ansicht wird aktualisiert:
  'Texte mittels absoluten Zahlen auslesen und anzeigen:
  x = WindLeft
  y = WindTop
  anz = 0
  FOR i = FirstVisLine TO FirstVisLine + WindH
    GET f, i, a$
    a$ = MID$(a$, FirstVisPos, WindW) 'Nur den Bildschirmausschnitt holen
    EchoXY x, y, a$
    IF LEN(a$) < WindW THEN
      'Text mit Leerzeichen erweitern, falls er nicht die ganze
      'Bildschirmzeile fÅllt:
      LINE (x + LEN(a$) * 8, y)-(x + WindW * 8, y + 14), 15, BF
    END IF
    IF Mouse.Clicked THEN
      'Maus wurde gedrÅckt!
      zDoMouse 'SpÑteren Doppelklick abfangen
      IF Mouse.y > y AND Mouse.y < y + 14 THEN
        'Setze den Cursor hierhin!
        CurrentLine = i
        'berechne die Spalte, auf die geklickt wurde:
        anz = FIX((Mouse.x - x) / 8) + 1
        IF dblClicked THEN
          'Doppelklick: Wort markieren...
          'Anfang und Ende des markierten Wortes suchen:
          MarkZeile1 = i
          MarkZeile2 = i
          FOR s = anz TO 1 STEP -1
            IF MID$(a$, s, 1) = " " THEN
              MarkSpalte1 = s + 1
              EXIT FOR
            ELSEIF s = 1 THEN
              MarkSpalte1 = 1
              EXIT FOR
            END IF
          NEXT
          FOR s = anz TO LEN(a$)
            IF MID$(a$, s, 1) = " " THEN
              MarkSpalte2 = s
              EXIT FOR
            ELSEIF s = LEN(a$) THEN
              MarkSpalte2 = s + 1
              EXIT FOR
            END IF
          NEXT
        END IF
        IF anz > (LEN(RTRIM$(a$)) - FirstVisPos + 1) THEN
          'nach dem Text geklickt:
          CurrentPos = LEN(RTRIM$(a$)) + 1
        ELSE
          CurrentPos = FirstVisPos + anz
        END IF
        anz = -1
      END IF
    END IF
    IF i >= MarkZeile1 AND i <= MarkZeile2 THEN
      Farbe = 15
      InversFarbe = 1
      IF MarkSpalte1 = 0 THEN
        'ganze Zeile!
        EchoXY x, y, a$
      ELSE
        'einzelne Zeichen
        EchoXY x + MarkSpalte1 * 8 - 8, y, MID$(a$, MarkSpalte1, MarkSpalte2 - MarkSpalte1)
      END IF
      Farbe = 8
      InversFarbe = 15
    END IF
    IF ActiveCtrl = iCtrl THEN
      IF i = CurrentLine AND NOT (anz = 0 AND Mouse.Clicked = 1) THEN
        'Dies ist die aktuelle Zeile der Cursorposition:
        '-> Cursor zeichnen
        b = y
        IF CurrentPos = FirstVisPos THEN
          'An erster Zeile den Cursor etwas weiter rechts zeichnen!
          a = x + CurrentPos * 8 - 8
        ELSEIF CurrentPos > WindW THEN
          'Ganz rechts zeichnen!
          a = x + WindW * 8 - 1
        ELSE
          'sonst etwas weiter links!
          a = x + (CurrentPos - (FirstVisPos - 1)) * 8 - 9
        END IF
        LINE (a, b)-(a, b + 13), 9
      END IF
    END IF
    y = y + 14
    'nÑchste Zeile...
  NEXT

  'Sichern des aktuellen Sichtbereiches:
  MID$(Controls(iCtrl).tag, 9, 2) = MKI$(FirstVisPos)
  MID$(Controls(iCtrl).tag, 11, 2) = MKI$(FirstVisLine)
 
  IF iRefresh THEN GOTO zEditorQuit

  a$ = zGK$(sGroup$)

  'NachtrÑgliche Cursor-Korrekturen:
  IF CurrentPos > pageBreakPos THEN
    'auf nÑchste Zeile setzen!
    CurrentPos = 1
    CurrentLine = CurrentLine + 1
  END IF

  SELECT CASE keyb
  CASE "left", "right", "up", "down"
    'Cursorbewegende Tasten: Markierung entfernen!
    MID$(Controls(iCtrl).tag, 1, 2) = MKI$(0)
    MID$(Controls(iCtrl).tag, 3, 2) = MKI$(0)
    MID$(Controls(iCtrl).tag, 5, 2) = MKI$(0)
    MID$(Controls(iCtrl).tag, 7, 2) = MKI$(0)
  END SELECT

  SELECT CASE keyb
  CASE "tab"
    LINE (a, b)-(a, b + 13), 15
  CASE "pageup"
    CurrentLine = CurrentLine - WindH - 1
  CASE "pagedown"
    CurrentLine = CurrentLine + WindH + 1
  CASE "passfocus"
  CASE "ctrl-right"
    GET f, CurrentLine, a$
    anz = 0
    FOR i = CurrentPos + 1 TO LEN(a$)
      IF MID$(a$, i, 1) = " " THEN
        CurrentPos = i + 1
        anz = True
        EXIT FOR
      END IF
    NEXT
    IF anz = 0 THEN
      CurrentPos = 1
      CurrentLine = CurrentLine + 1
    END IF
  CASE "ctrl-left"
    GET f, CurrentLine, a$
    IF CurrentPos = 1 THEN
      IF CurrentLine > 1 THEN
        CurrentLine = CurrentLine - 1
        GET f, CurrentLine, a$
        CurrentPos = LEN(a$) + 1
      END IF
    ELSE
      FOR i = CurrentPos - 2 TO 1 STEP -1
        IF MID$(a$, i, 1) = " " THEN
          CurrentPos = i + 1
          EXIT FOR
        ELSEIF i = 1 THEN
          CurrentPos = 1
        END IF
      NEXT
    END IF
  CASE "ctrl-end"
    errorlevel = 0
    i = CurrentLine
    anz = 0
    DO
      GET f, i, a$
      i = i + 1
      IF a$ = "" THEN
        anz = anz + 1
      ELSE
        anz = 0
      END IF
    LOOP UNTIL LEFT$(a$, pageBreakPos) = STRING$(pageBreakPos, CHR$(0)) OR errorlevel > 0 OR anz > 20
    IF anz > 0 THEN
      CurrentLine = i - anz - 1
      CurrentPos = -1'neu berechnen lassen
    END IF

  CASE "home"
    CurrentPos = 1
  CASE "end"
    CurrentPos = -1 'neu berechnen lassen auf Ende der aktuellen Zeile!
  CASE "ctrl-home"
    CurrentPos = 1
    CurrentLine = 1
  CASE "delete"
    IF MarkSpalte1 = 0 AND MarkZeile1 = 0 THEN
      GET f, CurrentLine, a$
      'Zeichen wegnehmen an der Cursor-Position:
      IF a$ = "" THEN
        'Diese Zeile entfernen!
        i = CurrentLine
        errorlevel = 0
        anz = 0
        DO
          GET f, i + 1, a$
          PUT f, i, a$
          i = i + 1
          IF a$ = "" THEN
            anz = anz + 1
          ELSE
            anz = 0
          END IF
        LOOP UNTIL LEFT$(a$, pageBreakPos) = STRING$(pageBreakPos, CHR$(0)) OR errorlevel > 0 OR anz > 20
      ELSE
        'Sonst: ganz normal ein Zeichen entfernen!
        a$ = LEFT$(a$, CurrentPos - 1) + MID$(a$, CurrentPos + 1)
        PUT f, CurrentLine, a$
      END IF
    ELSE
      'Markierten Text lîschen!
      keyb = ""
      GOTO deletingmark
    END IF

  CASE "backspace"
    IF MarkSpalte1 = 0 AND MarkZeile1 = 0 THEN
      GET f, CurrentLine, a$
      IF LEN(a$) > 1 AND CurrentPos > 2 THEN
        'Mittlere Zeichen wegnehmen an der Cursor-Position:
        a$ = LEFT$(a$, CurrentPos - 2) + MID$(a$, CurrentPos)
        PUT f, CurrentLine, a$
      ELSEIF CurrentPos = 2 THEN
        'Ausnahmebehandlung fÅr erstes Zeichen:
        a$ = MID$(a$, 2)
        PUT f, CurrentLine, a$
      ELSE
        CurrentLine = CurrentLine - 1
        GET f, CurrentLine, a$
        a$ = LEFT$(a$, LEN(a$) - 1)
        PUT f, CurrentLine, a$
        CurrentPos = -1 'soll neu berechnet werden!
      END IF
      CurrentPos = CurrentPos - 1
    ELSE
      'ganzen markierten Text lîschen!
      keyb = ""
      GOTO deletingmark
    END IF
  CASE "up"
    CurrentLine = CurrentLine - 1
  CASE "down"
    CurrentLine = CurrentLine + 1
  CASE "right"
    CurrentPos = CurrentPos + 1
  CASE "left"
    CurrentPos = CurrentPos - 1
    IF CurrentPos < 1 THEN
      'eine Zeile nach oben!
      CurrentLine = CurrentLine - 1
      'Merke: Die Position wird dann neu berechnet!
    END IF
  CASE "event"
  CASE "enter"
    'Nun alles runter schieben...
    anz = 0
    i = CurrentLine
    GET f, i, a$
    errorlevel = 0
    n$ = MID$(a$, CurrentPos)
    DO
      GET f, i, a$ 'das nÑchste festhalten
      PUT f, i, n$ 'das vorher nÑchstes war jetzt setzen
      n$ = a$ 'das nÑchste wird nachher das jetztige
      i = i + 1
      IF a$ = "" THEN
        anz = anz + 1
      ELSE
        anz = 0
      END IF
    LOOP UNTIL LEFT$(a$, pageBreakPos) = STRING$(pageBreakPos, CHR$(0)) OR errorlevel > 0 OR anz > 20
    i = CurrentLine
    GET f, i, a$
    GET f, i + 1, n$
    n$ = LEFT$(n$, CurrentPos - 1)
    PUT f, i, n$
    PUT f, i + 1, a$
    CurrentLine = CurrentLine + 1
    CurrentPos = 1
    keyb = "" 'Die Enter-Taste darf hier kein QuitKey sein!
    'todo ula
  CASE "@1"
    'Ctrl + A:
    'Alles markieren!
    MID$(Controls(iCtrl).tag, 5, 2) = MKI$(1)
    MID$(Controls(iCtrl).tag, 7, 2) = MKI$(MaxLines)
    MID$(Controls(iCtrl).tag, 1, 2) = MKI$(0)

  CASE ELSE
    IF zQuitKey THEN GOTO zEditorQuit
deletingmark:
    IF NOT (MarkSpalte1 = 0 AND MarkZeile1 = 0) THEN
        'Zeilen entfernen:
        IF MarkZeile1 = 1 AND MarkZeile2 = MaxLines AND MarkSpalte1 = 0 THEN
          'ganzen Text lîschen!
          CLOSE f
          KILL tempdir + name$ + ".tmp"
          OPEN tempdir + name$ + ".tmp" FOR RANDOM AS f LEN = (pageBreakPos + 5) 'Reserve fÅr Wîrter einberechnen!
          CurrentPos = 1
        ELSE
          IF MarkZeile1 <> MarkZeile2 THEN
            FOR z = MarkZeile1 TO MarkZeile2
              errorlevel = 0
              anz = 0
              i = z
              'Diese Zeile entfernen!
              DO
                GET f, i + 1, a$
                PUT f, i, a$
                i = i + 1
                IF a$ = "" THEN
                  anz = anz + 1
                ELSE
                  anz = 0
                END IF
              LOOP UNTIL LEFT$(a$, pageBreakPos) = STRING$(pageBreakPos, CHR$(0)) OR errorlevel > 0 OR anz > 20
            NEXT
            CurrentPos = 1
          ELSE
            'nur Zeichen entfernen in einer Zeile!
            GET f, CurrentLine, a$
            a$ = LEFT$(a$, MarkSpalte1 - 1) + MID$(a$, MarkSpalte2)
            PUT f, CurrentLine, a$
            CurrentPos = MarkSpalte1
          END IF
        END IF
        CurrentLine = MarkZeile1
        'Markierung entfernen:
        MID$(Controls(iCtrl).tag, 1, 2) = MKI$(0)
        MID$(Controls(iCtrl).tag, 3, 2) = MKI$(0)
        MID$(Controls(iCtrl).tag, 5, 2) = MKI$(0)
        MID$(Controls(iCtrl).tag, 7, 2) = MKI$(0)
    END IF
    GET f, CurrentLine, a$
    IF LEN(a$) < CurrentPos - 1 AND CurrentPos < pageBreakPos AND CurrentPos > 1 THEN
      'Der Text muss mit Leerzeichen erweitert werden!
      a$ = a$ + SPACE$(CurrentPos - LEN(a$) - 1)
    END IF
    a$ = LEFT$(a$, CurrentPos - 1) + keyb + MID$(a$, CurrentPos)
    PUT f, CurrentLine, a$
    CurrentPos = CurrentPos + LEN(keyb)
  END SELECT
  IF zQuitKey THEN GOTO zEditorQuit

  IF LEN(a$) > pageBreakPos THEN
    'Nun alles korrigieren!
    '->Abgeschnittene Zeichen der nÑchsten Zeile hinzutun...
    i = CurrentLine
    DO
      GET f, i, a$
      IF LEN(a$) > pageBreakPos THEN
        'Rest, der kein Platz mehr auf der Zeile hat, behalten...
        b$ = MID$(a$, pageBreakPos + 1)
        'Zeile kÅrzen:
        a$ = LEFT$(a$, pageBreakPos)
        PUT f, i, a$
        '...und auf die nÑchste Zeile schieben:
        GET f, i + 1, a$
        a$ = b$ + a$
        PUT f, i + 1, a$
      END IF
      i = i + 1
    LOOP UNTIL LEN(a$) <= pageBreakPos
  END IF

  GOTO zEditorInput

zEditorQuit:
  MID$(Controls(iCtrl).tag, 1, 2) = MKI$(MarkSpalte1)
  MID$(Controls(iCtrl).tag, 3, 2) = MKI$(MarkSpalte2)
  MID$(Controls(iCtrl).tag, 5, 2) = MKI$(MarkZeile1)
  MID$(Controls(iCtrl).tag, 7, 2) = MKI$(MarkZeile2)
  CLOSE f
END SUB

SUB zEventClick (iCtrl, iButton)
tag$ = RTRIM$(Controls(iCtrl).tag)
SELECT CASE Controls(iCtrl).CtrlType
CASE zListBox
  IF Controls(iCtrl).h = 1 THEN
    keyb = "listopen"
  END IF
CASE zScrollBar
  ActiveCtrl = CVI(MID$(tag$, 6, 2))
CASE zButton
  zDrawCtrl iCtrl, 0, 0
CASE zExitFocus
  keyb = "passfocus"
CASE zMouseClicker
  keyb = tag$
  zDrawCtrl iCtrl, 0, 0
  IF iButton = 2 THEN zRspm iCtrl
END SELECT
RaiseMouseUp iCtrl, RTRIM$(Controls(iCtrl).caption), iButton
END SUB

SUB zEventDblClick (iCtrl)

    SELECT CASE Controls(iCtrl).CtrlType
    CASE zEditor
      zEditorHandler Controls(iCtrl).Group, iCtrl, True, True
    CASE zEditBox
      MID$(Controls(iCtrl).tag, 5, 1) = "C"
    CASE zListBox, zIconList
      keyb = "enter"
    END SELECT
END SUB

SUB zEventMouseDown (iCtrl, iButton)
ActiveCtrl = iCtrl
  x = Controls(iCtrl).x
  y = Controls(iCtrl).y
  x2 = Controls(iCtrl).x2
  y2 = Controls(iCtrl).y2
  w = Controls(iCtrl).w
  h = Controls(iCtrl).h
  Value = Controls(iCtrl).Value
  tag$ = RTRIM$(Controls(iCtrl).tag)

  SELECT CASE Controls(iCtrl).CtrlType
  CASE zListBox
    IF MID$(tag$, 1, 1) = "M" THEN
      IF iButton = 2 THEN
        keyb = "esc"
      ELSE
        Mouse.Clicked = iButton
        zDrawCtrl iCtrl, 0, 0
        Mouse.Clicked = 0
      END IF
    END IF
  CASE zScrollBar
    MID$(Controls(iCtrl).tag, 9, 1) = "1"
    IF (w > 0 AND Mouse.x < x + 12) OR (w = 0 AND Mouse.y < y + 12) THEN
      Controls(iCtrl).Value = Value - CVI(MID$(tag$, 10, 2))
      zDrawCtrl iCtrl, 0, 0
      iCtrlParent = CVI(MID$(tag$, 6, 2))
      RaiseScroll iCtrl, RTRIM$(Controls(iCtrl).caption)
    ELSEIF (w > 0 AND Mouse.x > x2 - 12) OR (w = 0 AND Mouse.y > y2 - 12) THEN
      Controls(iCtrl).Value = Value + CVI(MID$(tag$, 10, 2))
      zDrawCtrl iCtrl, 0, 0
      iCtrlParent = CVI(MID$(tag$, 6, 2))
      RaiseScroll iCtrl, RTRIM$(Controls(iCtrl).caption)
    ELSE
      MID$(Controls(iCtrl).tag, 9, 1) = "0"
      zEventMouseMove iCtrl, iButton
      EXIT SUB
    END IF
    IF iCtrlParent > 0 THEN
      ActiveCtrl = iCtrlParent
      keyb = "passfocus"
      Controls(iCtrlParent).Value = Controls(iCtrl).Value
      zDrawCtrl iCtrlParent, 1, 0
    END IF
  END SELECT
END SUB

SUB zEventMouseMove (iCtrl, iButton)

    Mouse.Cursor = 1 'Standard-Cursor!

    x = Controls(iCtrl).x
    y = Controls(iCtrl).y
    x2 = Controls(iCtrl).x2
    y2 = Controls(iCtrl).y2
    w = Controls(iCtrl).w
    h = Controls(iCtrl).h
    Value = Controls(iCtrl).Value
    tag$ = Controls(iCtrl).tag
    Typ = Controls(iCtrl).CtrlType

    SELECT CASE Typ
    CASE zListBox
        IF MID$(tag$, 1, 1) = "M" AND iButton THEN
            Mouse.Clicked = -1
            zDrawCtrl iCtrl, 0, 0
            Mouse.Clicked = 0
        END IF

    CASE zScrollBar
        IF zMouseDown = iCtrl AND iButton = 1 THEN
            Min = CVI(MID$(tag$, 2, 2))
            stp = CVI(MID$(tag$, 10, 2))
            max = INT(((CVI(MID$(tag$, 4, 2)) - Min + 1) / stp) * -1) * -1 + 1
            'Keine Bewegungen akzeptieren, wenn zuvor auf einen der Pfeile
            'gedrÅckt wurde:
            IF MID$(Controls(iCtrl).tag, 9, 1) = "1" THEN EXIT SUB
            IF w > 0 THEN
                'horizontal:
                x = x + 12: x2 = x2 - 12
                nv = (Mouse.x - x)
                IF nv > x2 - x THEN nv = x2 - x
                IF nv > 0 THEN
                    nv = Min + (nv * CLNG((max - Min)) / (x2 - x))
                    nv = (nv * stp) - stp + (Value MOD stp)
                ELSE
                    nv = Min
                END IF
            ELSE
                'vertikal:
                y = y + 12: y2 = y2 - 12
                nv = (Mouse.y - y)
                IF nv > y2 - y THEN nv = y2 - y
                IF nv > 0 THEN
                    nv = Min + (nv * CLNG((max - Min)) / (y2 - y))
                    nv = (nv * stp) - stp + (Value MOD stp)
                ELSE
                    nv = Min
                END IF
            END IF
            max = CVI(MID$(tag$, 4, 2))
            IF nv > max THEN nv = max
            Controls(iCtrl).Value = nv
            iCtrlParent = CVI(MID$(tag$, 6, 2))
            IF Value <> nv THEN
                RaiseScroll iCtrl, RTRIM$(Controls(iCtrl).caption)
                IF iCtrlParent > 0 THEN
                    ActiveCtrl = iCtrlParent
                    keyb = "passfocus"
                    Controls(iCtrlParent).Value = nv
                    RaiseScroll iCtrlParent, RTRIM$(Controls(iCtrlParent).caption)
                END IF
            END IF
        END IF

    CASE zEditBox, zEditor
        Mouse.Cursor = 2

    END SELECT

    RaiseMouseMove iCtrl, RTRIM$(Controls(iCtrl).caption), iButton

END SUB

SUB zGetMouse (b, x, y)
IF MouseOn THEN
  DEF SEG = VARSEG(iMS(0))
  POKE VARPTR(iMS(0)) + 26, &H33
  CALL ABSOLUTE(3, b, x, y, VARPTR(iMS(0)))
ELSE
  x = Mouse.x
  y = Mouse.y
END IF
END SUB

FUNCTION zGK$ (sGroup$)
FOR i = 0 TO UBOUND(zTimeEvent)
    zTimeEvent(i).nextime = TIMER + zTimeEvent(i).inc
NEXT
IF Mouse.x + 14 > 639 THEN x2 = Mouse.x + (639 - Mouse.x) ELSE x2 = Mouse.x + 14
IF Mouse.y + 24 > 349 THEN y2 = Mouse.y + (349 - Mouse.y) ELSE y2 = Mouse.y + 24
GET (Mouse.x, Mouse.y)-(x2, y2), zMHG
keyb = ""
Mouse.Down = 0
Mouse.Clicked = 0
Mouse.Cursor = -Mouse.Cursor
Mouse.time = TIMER
DO
  DO
    DO
      a$ = INKEY$
      IF LEN(a$) = 0 THEN k = INP(&H60)
      x = Mouse.x
      y = Mouse.y
      IF zLauscheEvents(sGroup$) THEN
        IF keyb = "" THEN
            IF Controls(ActiveCtrl).SelfHandler THEN
                'Selfhandler-Control:
                a$ = "event"
                EXIT DO
            ELSE
                'Nur-Klick-Control:
                a$ = CHR$(13)
            END IF
        ELSEIF keyb = "nofocus" THEN
            keyb = ""
        ELSE
            a$ = keyb
        END IF
        EXIT DO
      END IF
      IF y <> Mouse.y THEN
        'Screensaver-Zeit wegen Bewegung zurÅcksetzen:
        zTimeEvent(0).nextime = TIMER + zTimeEvent(0).inc
      END IF
      zDoEvents
      SELECT CASE k
      CASE 56: a$ = "alt"
        WHILE k < 128
          k = INP(&H60)
        WEND
        IF k <> 184 THEN a$ = ""
      END SELECT
    LOOP WHILE a$ = ""
    IF LEN(a$) > 0 THEN
      SELECT CASE ASC(a$)
      CASE 32: IF NOT Controls(ActiveCtrl).SelfHandler THEN a$ = "enter"
      CASE 27: a$ = "esc"
      CASE 13: a$ = "enter"
      CASE 9: a$ = "tab"
      CASE 8: a$ = "backspace"
      CASE 10: Mouse.Clicked = 2
        IF MouseOn = 0 THEN
          Mouse.x = CurrentX + 10
          Mouse.y = CurrentY + 10
        END IF
      END SELECT
    END IF
  LOOP WHILE LEN(a$) = 0

  IF LEN(a$) = 2 THEN
      SELECT CASE ASC(MID$(a$, 2, 1))
          CASE 134 'F12: Bildschirminhalt speichern
            SavePicture 0, 0, 639, 349, tempdir + "zwa.abm"
            a$ = ""
          CASE 15: a$ = "shift-tab"
          CASE 77: a$ = "right"
          CASE 75: a$ = "left"
          CASE 72: a$ = "up"
          CASE 80: a$ = "down"
          CASE 82: a$ = "insert"
          CASE 83: a$ = "delete"
          CASE 71: a$ = "home"
          CASE 79: a$ = "end"
          CASE 73: a$ = "pageup"
          CASE 81: a$ = "pagedown"
          CASE 115: a$ = "ctrl-left"
          CASE 116: a$ = "ctrl-right"
          CASE 119: a$ = "ctrl-home"
          CASE 117: a$ = "ctrl-end"
          CASE ELSE
              IF ASC(LEFT$(a$, 1)) = 0 THEN a$ = "#" + LTRIM$(STR$(ASC(MID$(a$, 2, 1))))
      END SELECT
  END IF

  IF ASC(a$) < 32 THEN a$ = "@" + LTRIM$(STR$(ASC(a$)))

  k = INP(&H60)

  IF MouseOn THEN PUT (Mouse.Oldx, Mouse.Oldy), zMHG, PSET
  ERASE zMHG
 
  RaiseKeyPress ActiveCtrl, a$
LOOP WHILE a$ = ""

SELECT CASE a$
CASE "#138" 'Ctrl+f12: Controls auflisten
  Frame "zinfo", "", 0, 0, 0, 0
  FOR i% = 1 TO UBOUND(Controls)
    ListAdd "info", i%, STR$(i%) + "  " + Controls(i%).Group + "  " + Controls(i%).caption + STR$(Controls(i%).CtrlType), 0
  NEXT
  MouseClicker "zinfo", "esc", 0, 0, 639, 349, "esc", 0
  ListBox "zinfo", "info", 7, 23, 613, 320, 0
  DrawCtrls "zinfo"
  GetInp "zinfo"
  CloseFrame "zinfo"
CASE "alt"
  IF Controls(ActiveCtrl).CtrlType <> zMenu THEN
    a$ = ""
    'markiert das erste gefundene Menu-Control:
    FOR i = 1 TO UBOUND(Controls)
        IF Controls(i).CtrlType = zMenu THEN
            IF sGroup$ = "" OR sGroup$ = RTRIM$(Controls(i).Group) THEN
                ActiveCtrl = i
                a$ = "tab"
                EXIT FOR
            END IF
        END IF
    NEXT
  END IF
CASE "shift-tab"
    'nÑchster Typ aktivieren:
    a$ = "tab"
    FOR i = ActiveCtrl - 1 TO 1 STEP -1
        IF (sGroup$ = "" OR RTRIM$(Controls(i).Group) = sGroup$) AND Controls(i).CtrlType <= zControlTypeCount THEN
          IF Controls(i).CtrlType > 0 THEN
            ActiveCtrl = i
            EXIT FOR
          END IF
        END IF
    NEXT
CASE "tab"
    'nÑchstes Ctrl aktivieren:
    FOR i = ActiveCtrl + 1 TO UBOUND(Controls)
        IF (sGroup$ = "" OR RTRIM$(Controls(i).Group) = sGroup$) AND Controls(i).CtrlType <= zControlTypeCount THEN
          IF Controls(i).CtrlType > 0 THEN
            ActiveCtrl = i
            GOTO zGKExit
          END IF
        END IF
    NEXT
    ActiveCtrl = 0
END SELECT

zGKExit:
zGK = a$
keyb = a$

END FUNCTION

SUB zHandleControls (iCtrl, sGroup$, iChanges)
iChanges = 0
  SELECT CASE Controls(iCtrl).CtrlType
  CASE zCheckBox
    'Wert wechselt:
    Controls(iCtrl).Value = -1 - Controls(iCtrl).Value
    zDrawCtrl iCtrl, 0, 0
    iChanges = -1
  CASE zRadioButton
    'alle deaktivieren:
    FOR i = 1 TO UBOUND(Controls)
      IF sGroup$ = "" OR RTRIM$(Controls(i).Group) = sGroup$ THEN
        IF Controls(i).CtrlType = zRadioButton THEN
          IF Controls(i).Value = -1 AND iCtrl <> i THEN
            Controls(i).Value = 0
            zDrawCtrl i, 0, 0
          ELSEIF iCtrl = i THEN
            Controls(i).Value = -1
            zDrawCtrl i, 0, 0
            iChanges = -1
          END IF
        END IF
      END IF
    NEXT
  END SELECT
END SUB

SUB zIconListHandler (sGroup$, iCtrl, iRefresh)
DIM Hintergrund(505)

l = Controls(iCtrl).x
t = Controls(iCtrl).y
r = Controls(iCtrl).x2
d = Controls(iCtrl).y2

sName$ = RTRIM$(Controls(iCtrl).caption)

'aktiviertes Symbol:
sv = Controls(iCtrl).Value

'altes Sichtfenster:
otopper = CVI(MID$(Controls(iCtrl).tag, 1, 2))
MaxItems = CVI(MID$(Controls(iCtrl).tag, 5, 2))

topperIcon = 1
bottomIcon = 0
nr = 0
col = POINT(l, t)

IF Mouse.Clicked = 2 THEN
  ppm = 1
  IF keyb = "10" THEN GOTO zIcListPopUp
END IF

DO
IF Value > MaxItems AND MaxItems > 0 THEN Value = MaxItems
IF Value < 1 THEN Value = 1
cntMode = False
IF Value < topperIcon THEN
    zRessourceGet "ic" + LTRIM$(STR$(iCtrl)), topperIcon, "", wert
    IF wert = 0 THEN NoRefresh = True
    topperIcon = wert
    bottomIcon = wert - 1
END IF

IF Value > bottomIcon THEN
    nr = bottomIcon + 1
    topperIcon = nr
    IF sv = 0 THEN LINE (l, t)-(r, d), col, BF
ELSE
    nr = topperIcon
END IF
Refresh = False
mc = False
zIcListAct:
x = l
y = t + 45

DO
IF NoRefresh THEN NoRefresh = False: EXIT DO
IF nr > MaxItems AND MaxItems > 0 THEN bottomIcon = MaxItems: EXIT DO
zRessourceGet sName$, nr, a$, 0
zDoMouse
IF a$ = "" THEN
    IF cntMode OR MaxItems = 0 THEN MaxItems = nr - 1: EXIT DO
END IF
name$ = RTRIM$(LEFT$(a$, 12))
Kennung$ = MID$(a$, 13)
IF x + 41 > r OR x + LEN(Kennung$) * 8 > r THEN x = l: y = y + 65
IF LEN(Kennung$) < 6 THEN x = x + (6 - LEN(Kennung$)) * 4
sx = x + INT((LEN(Kennung$) * 8 - 41) / 2)
IF sv = 0 THEN
    VIEW
    IF Value = nr AND iCtrl = ActiveCtrl THEN
        LINE (sx - 2, y - 44)-(sx + 42, y + 1), focol, B, PunkteLinie
        lx = sx - 2
        ly = y - 44
    ELSEIF y < d THEN
        LINE (sx - 2, y - 44)-(sx + 42, y + 1), col, B
    END IF
END IF
IF y + 14 > d THEN
    IF NOT cntMode THEN
        zRessourceSet "ic" + LTRIM$(STR$(iCtrl)), nr, "", topperIcon
        bottomIcon = nr - 1
        cntMode = True
    END IF
    IF MaxItems > 0 THEN EXIT DO
ELSE
IF mc THEN
  IF Mouse.Clicked AND iCtrl = ActiveCtrl THEN
    px = x
    w = LEN(Kennung$) * 8
    IF w < 41 THEN px = sx - 3: w = 44
    IF Mouse.x > px AND Mouse.x <= px + w THEN
      IF Mouse.y > y - 43 AND Mouse.y <= y + 16 THEN
        sv = nr
        Mouse.Clicked = 0
        mc = 0
        Value = nr
        GOTO zIcListAct
      END IF
    END IF
  END IF
END IF
  IF (Value < topperIcon OR Value > bottomIcon AND sv = 0) OR Refresh THEN
    Farbe = tcol
    InversFarbe = col
    EchoXY x, y + 2, Kennung$
    InversFarbe = 0
    IF col > 0 THEN c = col ELSE c = icfcol
    VIEW (sx, y - 42)-(sx + 40, y - 1), c, c
    IF oldicon$ <> name$ THEN
        IF LCASE$(RIGHT$(name$, 3)) = "icn" THEN
          VIEW
          ShowPicture sx, y - 42, laosdir + "icons\" + name$
        ELSE 'lsb
          VIEW (sx, y - 42)-(sx + 40, y - 1), c, c
          ShowPicture 0, 0, laosdir + "icons\" + name$
        END IF
        oldicon$ = name$
        i = 0
        GET (sx, y - 42)-(sx + 40, y - 1), Hintergrund
    ELSE
        PUT (0, 0), Hintergrund, PSET
    END IF
    VIEW
  END IF
END IF
x = x + LEN(Kennung$) * 8 + 16
nr = nr + 1
LOOP
IF sv <> 0 THEN
    IF sv <> Value AND Value < MaxItems THEN
        Value = Value + 1
    ELSE
        sv = 0
        nr = topperIcon
        IF otopper <> topperIcon THEN
            Refresh = True
            LINE (l, t)-(r, d), col, BF
        END IF
        mc = True
        GOTO zIcListAct
    END IF
END IF
IF sv = 0 THEN

'Scrollbar zeichnen:
sb = CVI(MID$(Controls(iCtrl).tag, 3, 2))
MID$(Controls(sb).tag, 4, 2) = MKI$(MaxItems)
Controls(sb).Value = Value
zDrawCtrl sb, 0, 0
'Die Scrollbar hat die Schrittweite der aktuellen Seite:
MID$(Controls(sb).tag, 10, 2) = MKI$(bottomIcon - topperIcon + 1)

Controls(iCtrl).Value = Value
MID$(Controls(iCtrl).tag, 1, 2) = MKI$(topperIcon)

zIcListPopUp:
IF ppm AND ActiveCtrl = iCtrl THEN zRspm iCtrl: ppm = 0: Mouse.ClickX = 0: EXIT SUB

MID$(Controls(iCtrl).tag, 5, 2) = MKI$(MaxItems)

IF iRefresh THEN EXIT SUB

a$ = zGK(sGroup$)

IF Value <> Controls(iCtrl).Value THEN
    Value = Controls(iCtrl).Value
    topperIcon = CVI(MID$(Controls(iCtrl).tag, 1, 2))
    bottomIcon = MaxItems
END IF

IF Mouse.Clicked THEN EXIT SUB

IF keyb = "tab" THEN LINE (lx, ly)-(lx + 44, ly + 45), col, B
IF zQuitKey THEN EXIT SUB

SELECT CASE a$
CASE "end"
    sv = MaxItems
    kb$ = ""
CASE "home"
    sv = 1
    kb$ = ""
CASE "pageup"
    sv = topperIcon - 1
    IF sv = 0 THEN sv = 1
CASE "pagedown"
    sv = bottomIcon + 1
CASE "left", "up"
    Value = Value - 1
    kb$ = ""
CASE "right", "down"
    sv = Value + 1
    kb$ = ""
CASE "backspace"
    kb$ = ""
CASE "event"
CASE ELSE
    kb$ = kb$ + LCASE$(a$)
    found = 0
    FOR i% = Value TO MaxItems
        zRessourceGet sName$, i%, a$, 0
        IF LCASE$(MID$(a$, 13, LEN(kb$))) = kb$ THEN found = i%: EXIT FOR
    NEXT
    IF found = 0 THEN
        FOR i% = 1 TO Value - 1
            zRessourceGet sName$, i%, a$, 0
            IF LCASE$(MID$(a$, 13, LEN(kb$))) = kb$ THEN found = i%: EXIT FOR
        NEXT
    END IF
    sv = found
END SELECT
IF sv > 0 THEN otopper = topperIcon
IF sv > 0 AND sv < Value THEN
    topperIcon = 1
    bottomIcon = 0
    Value = 1
    nr = 0
END IF
END IF
LOOP
END SUB

SUB zLastControl (sGroup$)
    iold = ActiveCtrl
    'Suche nÑchstes Control:
    FOR z% = ActiveCtrl - 1 TO 1 STEP -1
        IF RTRIM$(Controls(z%).Group) = sGroup$ OR sGroup$ = "" THEN
            IF Controls(z%).CtrlType = Controls(iold).CtrlType THEN
                ActiveCtrl = z%
                zDrawCtrl iold, 0, 0
                zDrawCtrl z%, 0, 0
                EXIT FOR
            END IF
        END IF
    NEXT
END SUB

FUNCTION zLauscheEvents% (sGroup$)
  IF MouseOn = 0 THEN EXIT FUNCTION
  zLauscheEvents% = 0
  iOverControl = 0
  DEF SEG = VARSEG(iMS(0))
  POKE VARPTR(iMS(0)) + 26, &H33
  CALL ABSOLUTE(3, b, x, y, VARPTR(iMS(0)))
  IF (Mouse.Oldx <> x OR Mouse.Oldy <> y AND zMHG(0) > 0) OR Mouse.Cursor < 0 THEN
    IF Mouse.Cursor < 0 THEN Mouse.Cursor = Mouse.Cursor * -1
    PUT (Mouse.Oldx, Mouse.Oldy), zMHG, PSET
    zDrawMouse x, y
  END IF
  Mouse.x = x
  Mouse.y = y
  IF x <> Mouse.Oldx AND y <> Mouse.Oldy THEN Mouse.time = TIMER
  IF MouseOn = 2 THEN PUT (x, y), zMHG, PSET: MouseOn = -1: i = ActiveCtrl: GOTO zLEClick
  'Gehe alle Controls durch, und schaue auf Events:
  FOR i = UBOUND(Controls) TO 1 STEP -1
    IF Controls(i).CtrlType > 0 THEN
      IF RTRIM$(Controls(i).Group) = sGroup$ OR sGroup$ = "" THEN
        IF x >= Controls(i).x AND x <= Controls(i).x2 THEN
          IF y >= Controls(i).y AND y <= Controls(i).y2 THEN
            'Maus ist Åber einem Control positioniert!
            iOverControl = -1
            IF zMouseDown = i AND b = 0 THEN
              'Maustaste wurde auf dem Objekt losgelassen!
              ' - Click-Ereignis!
              PUT (x, y), zMHG, PSET
              zMouseDown = 0
              ActiveCtrl = i
              Focus = RTRIM$(Controls(i).caption)
              IF Controls(i).CtrlType > zControlTypeCount THEN
                  keyb = "nofocus"
              END IF
              Mouse.Clicked = Mouse.Down
              Mouse.Down = 0
              zEventClick i, Mouse.Clicked
              'PrÅfe auf Doppelklick:
              IF Mouse.time > 0 AND Mouse.Clicked = 1 THEN
                IF TIMER - Mouse.time < .8 THEN
                  IF x > Mouse.ClickX - 2 AND x < Mouse.ClickX + 2 THEN
                    IF y > Mouse.ClickY - 2 AND y < Mouse.ClickY + 2 THEN
zLEClick:
                      IF x >= Controls(ActiveCtrl).x AND x <= Controls(ActiveCtrl).x2 THEN
                        IF y >= Controls(ActiveCtrl).y AND y <= Controls(ActiveCtrl).y2 THEN
                          Mouse.Clicked = -1
                          zEventDblClick i
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
              END IF
              Mouse.time = TIMER
              x = Mouse.x
              y = Mouse.y
              zDrawMouse x, y
              IF Mouse.Clicked THEN zLauscheEvents% = -1
              Mouse.ClickX = x
              Mouse.ClickY = y
              GOTO zLEventsTerminate
            ELSEIF b > 0 THEN
                'Maustaste zum ersten mal auf diesem Objekt
                'gedrÅckt!
                IF zMouseDown = 0 THEN
zLEMd:
                  'Altes Control deaktivieren:
                  ' -> neues aktivieren!
                  iold = ActiveCtrl
                  IF Controls(i).CtrlType <= zControlTypeCount THEN
                    ActiveCtrl = i
                  END IF
                  PUT (x, y), zMHG, PSET
                  zMouseDown = i
                  Mouse.Down = b
                  IF NOT Controls(i).SelfHandler THEN zDrawCtrl i, -1, 0
                  IF NOT Controls(iold).SelfHandler AND iold <> i THEN zDrawCtrl iold, 0, 0
                  zEventMouseDown i, b
                  x = Mouse.x
                  y = Mouse.y
                  zDrawMouse x, y
                  IF keyb <> "" THEN zLauscheEvents% = -1
                  Mouse.dx = x
                  Mouse.dy = y
                'Mousedown wiederholen nach .5 Sekunden...
                ELSEIF TIMER - Mouse.time > .5 AND Controls(i).CtrlType = zScrollBar THEN
                  Mouse.time = TIMER
                  GOTO zLEMd
                END IF
                GOTO zLEventsTerminate
            ELSEIF b = 0 THEN
                Mouse.Clicked = 0
                'Taste ist nicht gedrÅckt!
                IF Mouse.x <> Mouse.Oldx OR Mouse.y <> Mouse.Oldy THEN
                  PUT (x, y), zMHG, PSET
                  zEventMouseMove i, b
                  x = Mouse.x
                  y = Mouse.y
                  zDrawMouse x, y
                ELSE
                  IF TIMER - Mouse.time > 1 THEN
                    RaiseToolTipText i, RTRIM$(Controls(i).caption)
                  END IF
                END IF
            END IF
            'Nur das oberste Control wird berÅcksichtigt!
            EXIT FOR
          END IF
        END IF
      END IF
    END IF
  NEXT
zLEventsTerminate:
  IF NOT iOverControl THEN Mouse.Cursor = 1 'Standard
  IF b = 0 THEN
    Mouse.Down = 0
    IF zMouseDown > 0 THEN
      'Control war angeklickt! -> wieder un-angeklickt anzeigen:
      PUT (x, y), zMHG, PSET
      zDrawCtrl zMouseDown, 0, 0
      zDrawMouse x, y
      zMouseDown = 0
    END IF
  ELSE
    'Control ziehen und ablegen... (Mousemove mit gedrÅckter Taste)
    IF (x <> Mouse.Oldx OR y <> Mouse.Oldy) AND zMouseDown > 0 THEN
      PUT (x, y), zMHG, PSET
      zEventMouseMove zMouseDown, b
      zDrawMouse x, y
      IF keyb <> "" THEN zLauscheEvents% = -1
    END IF
  END IF
  Mouse.Oldx = x
  Mouse.Oldy = y
END FUNCTION

SUB zListboxHandler (sGroup$, iCtrl, x, y, x2, y2, iAnzSpalten, iAnzZeilen, RefreshOnly)

DIM sName$, Value

'Der markierte Eintrag:
Value = Controls(iCtrl).Value

'Der Controlname:
sName$ = Controls(iCtrl).caption

'Checked:
IF MID$(Controls(iCtrl).tag, 2, 1) = "C" THEN iCheckBoxes = -1

'MenuStyle:
MenuStyle = MID$(Controls(iCtrl).tag, 1, 1) = "M"

sb = CVI(MID$(Controls(iCtrl).tag, 3, 2))
MaxItems = CVI(MID$(Controls(sb).tag, 4, 2))

zListBoxReDraw:

IF Value <= 0 THEN Value = 1
IF Value > MaxItems AND MaxItems > 0 THEN Value = MaxItems

iTopvisible = (FIX((Value - 1) / iAnzZeilen) * iAnzZeilen) + 1
iDownVisible = iTopvisible + iAnzZeilen - 1

IF iCtrl = ActiveCtrl AND Mouse.Clicked THEN vstop = -1 ELSE vstop = 0

'SilentScreen

ytop = 2
z% = 0
a$ = CHR$(0)
DO
    z% = z% + 1
    zDoMouse
    IF z% >= iTopvisible AND z% <= iDownVisible THEN
        zRessourceGet sName$, z%, a$, checked
        IF a$ <> "" THEN
        IF a$ <> "-" THEN a$ = a$ + SPACE$(iAnzSpalten - LEN(a$))
            IF INSTR(a$, crlf) > 0 THEN zReplace a$, crlf, "  "
            'Ist Eintrag markiert?
zListBoxHandlerZeigeDiesen:
            IF iCtrl = ActiveCtrl AND Mouse.Clicked THEN
                IF Mouse.y >= y + ytop AND Mouse.y <= y + ytop + 13 THEN
                    IF Mouse.x > x AND Mouse.x <= x2 THEN
                        Value = z%
                        vstop = 0
                        IF NOT RefreshOnly AND iCheckBoxes AND Mouse.x < x + 17 THEN
                            'CheckBox wurde angeklickt
                            zRessourceSet sName$, z%, "", AutoVal
                            checked = -1 - checked
                        END IF
                    END IF
                END IF
            END IF
            IF z% = Value AND a$ <> "-" AND NOT vstop THEN
                Farbe = lbtcol
                IF (NOT RefreshOnly AND NOT MenuStyle) OR iCtrl = ActiveCtrl THEN
                    InversFarbe = lbacol
                    LINE (x + iAnzSpalten * 8, y + ytop)-(x2 - 2, y + ytop + 13), InversFarbe, BF
                ELSE
                    InversFarbe = lbicol
                    LINE (x + iAnzSpalten * 8, y + ytop)-(x2 - 2, y + ytop + 13), InversFarbe, BF
                END IF
            ELSE
                InversFarbe = 0
                Farbe = tcol
                LINE (x + iAnzSpalten * 8, y + ytop)-(x2 - 2, y + ytop + 13), 0, BF
            END IF
            IF INSTR(a$, CHR$(0)) > 0 THEN
                a$ = LEFT$(a$, INSTR(a$, CHR$(0)) - 1) + MID$(a$, INSTR(a$, CHR$(0)) + 1)
            END IF
            IF iCheckBoxes THEN a$ = "  " + a$
            IF a$ <> "-" THEN
              EchoXY x + 2, y + ytop, LEFT$(a$, iAnzSpalten)
            ELSE
              LINE (x + 2, y + ytop + 7)-(x2 - 2, y + ytop + 7), Farbe
            END IF
            COLOR Farbe
            IF iCheckBoxes THEN
                IF checked = -1 THEN
                    LINE (x + 3, y + ytop + 1)-STEP(11, 11), , B
                    LINE (x + 3, y + ytop + 1)-STEP(11, 11)
                    LINE (x + 3, y + ytop + 12)-STEP(11, -11)
                ELSEIF checked = 0 THEN
                    LINE (x + 3, y + ytop + 1)-STEP(11, 11), , B
                END IF
            END IF
            ytop = ytop + 14
            InversFarbe = 0
        END IF
    ELSEIF MaxItems = 0 THEN
        zRessourceGet sName$, z%, a$, checked
    END IF
LOOP UNTIL (z% > iDownVisible AND MaxItems > 0) OR (MaxItems = 0 AND a$ = "")
IF MaxItems = 0 THEN MaxItems = z% - 1

IF y2 - (y + ytop) > 1 THEN
  LINE (x + 2, y + ytop)-(x2 - 2, y2 - 2), lbfcol, BF
END IF

'Scrollbar zeichnen:
MID$(Controls(sb).tag, 4, 2) = MKI$(MaxItems)
Controls(sb).Value = Value
zDrawCtrl sb, 0, 0

'RestoreScreen
IF RefreshOnly THEN
    'Value zurÅckspeichern:
    Controls(iCtrl).Value = Value
    EXIT SUB
END IF

Controls(iCtrl).Value = Value

a$ = zGK(sGroup$)
'Doppelclick bei Checkboxen nicht zulassen:
IF Mouse.Clicked = -1 AND iCheckBoxes THEN a$ = "insert"

Value = Controls(iCtrl).Value

IF Mouse.Clicked = 2 AND ActiveCtrl = iCtrl THEN zRspm iCtrl

IF Mouse.Clicked > 0 THEN
    IF ActiveCtrl = iCtrl AND MenuStyle THEN
        IF keyb <> "esc" THEN keyb = "enter"
        zRessourceGet sName$, Value, a$, 0
        IF a$ <> "-" THEN GOTO zListBoxTerminate
    ELSE
        GOTO zListBoxTerminate
    END IF
END IF

IF a$ = "up" AND MenuStyle AND Value = 1 THEN keyb = "end"

IF zQuitKey THEN
  IF ActiveCtrl = iCtrl THEN EXIT SUB
  GOTO zListBoxTerminate
END IF

dir = 1
SELECT CASE keyb
CASE "pageup"
    keybuffer$ = ""
    Value = Value - iAnzZeilen
    dir = 0
CASE "pagedown"
    keybuffer$ = ""
    Value = Value + iAnzZeilen
CASE "home"
    keybuffer$ = ""
    Value = 0
CASE "end"
    keybuffer$ = ""
    Value = MaxItems
CASE "up"
    keybuffer$ = ""
    Value = Value - 1
    dir = 0
CASE "down"
    keybuffer$ = ""
    Value = Value + 1
CASE "backspace"
    keybuffer$ = ""
CASE "insert"
    zRessourceSet sName$, Value, "", AutoVal
    Value = Value + 1
CASE "event"
CASE ELSE
    IF keybuffer$ = "" AND keyb = " " AND iCheckBoxes THEN
        zRessourceSet sName$, Value, "", AutoVal
    ELSE
        keybuffer$ = keybuffer$ + LCASE$(a$)
        found = 0
        FOR i = Value TO MaxItems
            zRessourceGet sName$, i, a$, 0
            IF LCASE$(LEFT$(a$, LEN(keybuffer$))) = keybuffer$ THEN Value = i: found = -1: EXIT FOR
        NEXT
        IF NOT found THEN
            FOR i = 1 TO Value - 1
                zRessourceGet sName$, i, a$, 0
                IF LCASE$(LEFT$(a$, LEN(keybuffer$))) = keybuffer$ THEN Value = i: found = -1: EXIT FOR
            NEXT
        END IF
        IF keyb = " " AND NOT found AND iCheckBoxes THEN keybuffer$ = "": zRessourceSet sName$, Value, "", AutoVal
    END IF
END SELECT

zRessourceGet sName$, Value, a$, checked
IF a$ = "-" THEN
    IF dir = 1 THEN Value = Value + 1 ELSE Value = Value - 1
END IF

GOTO zListBoxReDraw

zListBoxTerminate:
RefreshOnly = -1
iCtrl = 0
GOTO zListBoxReDraw

END SUB

SUB zMoveScreen (x1, y1, x2, y2, ToX, ToY)

DIM Groesse AS LONG
    IF y1 > y2 THEN SWAP y1, y2
    IF x1 > x2 THEN SWAP x1, y2
    Groesse = 4 + INT(((PMAP(x2, 0) - PMAP(x1, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y2, 1) - PMAP(y1, 1) + 1)
    DIM background(Groesse)
    GET (x1, y1)-(x2, y2), background
    PUT (ToX, ToY), background, PSET
    REDIM background(0)

END SUB

SUB zNextCtrl (sGroup$)
    iold = ActiveCtrl
    'Suche nÑchstes Control:
    FOR z% = ActiveCtrl + 1 TO UBOUND(Controls)
        IF RTRIM$(Controls(z%).Group) = sGroup$ OR sGroup$ = "" THEN
            IF Controls(z%).CtrlType = Controls(iold).CtrlType THEN
                ActiveCtrl = z%
                zDrawCtrl iold, 0, 0
                zDrawCtrl z%, 0, 0
                EXIT FOR
            END IF
        END IF
    NEXT
END SUB

FUNCTION zQuitKey
SELECT CASE keyb
CASE "esc", "enter", "tab", "passfocus", "alt", "popup", "listopen"
  zQuitKey = -1
CASE IS <> ""
  zQuitKey = (INSTR("," + ShortKeys + ",", "," + keyb + ",") > 0)
  IF LEN(keyb) = 1 AND Controls(ActiveCtrl).CtrlType = zEditBox THEN zQuitKey = 0
END SELECT
END FUNCTION

DEFSNG A-Z
'Ersetzt im text$ den Ausdruck find$ durch den Ausdruck replce$
SUB zReplace (Text$, find$, replce$)
    DO
        i = i + 1
        IF MID$(Text$, i, LEN(find$)) = find$ THEN
            Text$ = LEFT$(Text$, i - 1) + replce$ + MID$(Text$, i + LEN(find$))
            i = i + LEN(find$)
        END IF
    LOOP UNTIL i > LEN(Text$)
END SUB

DEFINT A-Z
SUB zRessourceGet (sGroup$, Index, Text$, Value)
SHARED zoldsgroup AS STRING * 8
    Text$ = ""
    Value = 0
    DIM rsc AS zRessourceType
    IF RTRIM$(zoldsgroup) <> RTRIM$(sGroup$) OR LOC(RscFile) < 0 THEN
        CLOSE RscFile
        OPEN tempdir + RTRIM$(sGroup$) + ".rs~" FOR BINARY AS RscFile LEN = LEN(rsc)
        zoldsgroup = sGroup$
        errorlevel = 0
    END IF
    GET RscFile, (Index - 1) * LEN(rsc) + 1, rsc
    IF rsc.Text <> STRING$(80, 0) THEN Text$ = RTRIM$(rsc.Text)
    Value = rsc.Value
END SUB

SUB zRessourceSet (sGroup$, Index, Text$, Value)
SHARED zoldsgroup AS STRING * 8
    IF Index < 1 THEN Dialog "UngÅltiger Index: " + STR$(Index)
    'Ressourcen in File speichern:
    DIM rsc AS zRessourceType
    IF RTRIM$(zoldsgroup) <> RTRIM$(sGroup$) OR LOC(RscFile) < 0 THEN
        CLOSE RscFile
        OPEN tempdir + RTRIM$(sGroup$) + ".rs~" FOR BINARY AS RscFile LEN = LEN(rsc)
        zoldsgroup = sGroup$
    END IF
    IF LEN(Text$) = 0 OR Value = AutoVal THEN
      GET RscFile, (Index - 1) * LEN(rsc) + 1, rsc
    END IF
    IF Value = AutoVal THEN Value = -1 - rsc.Value
    rsc.Value = Value
    IF LEN(Text$) > 0 THEN rsc.Text = Text$
    PUT RscFile, (Index - 1) * LEN(rsc) + 1, rsc
END SUB

SUB zRGBPalette (slot%, r%, g%, b%)
SELECT CASE slot%
CASE IS <= 5: s% = slot%
CASE 6: s% = 20
CASE 7: s% = 7
CASE IS >= 8: s% = 48 + slot%
END SELECT
OUT &H3C8, s%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, b%
END SUB

SUB zRspm (iCtrl)
  RaisePopupMenu iCtrl, RTRIM$(Controls(iCtrl).caption), q
  IF q THEN
    ActiveCtrl = iCtrl
    keyb = "popup"
  ELSE
    keyb = ""
  END IF
END SUB

SUB zScreenSaver
'Sternenfeld Bildschirmschoner
'Geschrieben von Urs Langmeier
IF SaveScreenMode THEN
  bytes = 28000
  DEF SEG = &HA000
  FOR i = 0 TO 3
    OUT &H3CE, 4
    OUT &H3CF, i
    BSAVE tempdir + "temp" + ".sc" + CHR$(49 + i), 0, bytes
  NEXT
ELSE
  PCOPY 0, 1
END IF
zGetMouse b, x, y
IF b <> 0 THEN EXIT SUB
DIM stars(100) AS startype
'Sterne setzen:
FOR i = 1 TO UBOUND(stars)
  x% = INT(RND * 639)
  y% = INT(RND * 349)
  col% = 15
  stars(i).x = x%
  stars(i).y = y%
  stars(i).col = col%
NEXT
CLS
COLOR , 0
DO
  nz! = TIMER + .1
  'Sterne zeichnen:
  FOR i = 1 TO UBOUND(stars)
    IF stars(i).col <> 0 THEN
      WHILE stars(i).x > 315 AND stars(i).x < 325 AND stars(i).y > 170 AND stars(i).y < 180
        stars(i).x = INT(RND * 639)
        stars(i).y = INT(RND * 349)
      WEND
      LINE ((stars(i).x - 320) / 1.1 + 320 - stars(i).float - 1, (stars(i).y - 175) / 1.1 + 175 - stars(i).float - 1)-((stars(i).x - 320) / 1.1 + 320 + stars(i).float + 1, (stars(i).y - 175) / 1.1 + 175 + stars(i).float + 1), 0, BF
      LINE (stars(i).x, stars(i).y - stars(i).float)-(stars(i).x, stars(i).y + stars(i).float), stars(i).col
      LINE (stars(i).x - stars(i).float, stars(i).y)-(stars(i).x + stars(i).float, stars(i).y), stars(i).col
      LINE (stars(i).x + (stars(i).float / 1.5), stars(i).y + stars(i).float / 1.5)-(stars(i).x - stars(i).float / 1.5, stars(i).y - stars(i).float / 1.5), stars(i).col
      LINE (stars(i).x - stars(i).float / 1.5, stars(i).y + stars(i).float / 1.5)-(stars(i).x + stars(i).float / 1.5, stars(i).y - stars(i).float / 1.5), stars(i).col
    END IF
  NEXT
  FOR i = 1 TO UBOUND(stars)
    IF stars(i).col <> 0 THEN
      stars(i).x = (stars(i).x - 320) * 1.1 + 320
      stars(i).y = (stars(i).y - 175) * 1.1 + 175
      stars(i).float = stars(i).float + .1
      SELECT CASE stars(i).x
      CASE IS > 639, IS < 0
        LINE ((stars(i).x - 320) / 1.1 + 320 - stars(i).float - 1, (stars(i).y - 175) / 1.1 + 175 - stars(i).float - 1)-((stars(i).x - 320) / 1.1 + 320 + stars(i).float + 1, (stars(i).y - 175) / 1.1 + 175 + stars(i).float + 1), 0, BF
        x% = INT(RND * 639)
        y% = INT(RND * 349)
        col% = 15
        stars(i).x = x%
        stars(i).y = y%
        stars(i).col = col%
        stars(i).float = 0
      END SELECT
      SELECT CASE stars(i).y
      CASE IS > 349, IS < 0
        LINE ((stars(i).x - 320) / 1.1 + 320 - stars(i).float - 1, (stars(i).y - 175) / 1.1 + 175 - stars(i).float - 1)-((stars(i).x - 320) / 1.1 + 320 + stars(i).float + 1, (stars(i).y - 175) / 1.1 + 175 + stars(i).float + 1), 0, BF
        x% = INT(RND * 639)
        y% = INT(RND * 349)
        col% = 15
        stars(i).x = x%
        stars(i).y = y%
        stars(i).col = col%
        stars(i).float = 0
      END SELECT
    END IF
  NEXT
  DO WHILE TIMER < nz!: LOOP
  zGetMouse b, nx, ny
  LOOP UNTIL INKEY$ <> "" OR (nx <> Mouse.x) OR b <> 0 OR (ny <> Mouse.y)
IF SaveScreenMode THEN
  DEF SEG = &HA000
  FOR i = 0 TO 3
    OUT &H3C4, 2
    OUT &H3C5, 2 ^ i
    BLOAD tempdir + "temp" + ".sc" + CHR$(49 + i), 0
  NEXT
ELSE
  PCOPY 1, 0
END IF
COLOR Farbe, bcol
END SUB

FUNCTION zSelfHandleControl (sGroup$, iCtrlType)

DIM Text AS STRING

    i = ActiveCtrl
    x = Controls(i).x
    y = Controls(i).y
    x2 = Controls(i).x2
    y2 = Controls(i).y2
    w = Controls(i).w
    h = Controls(i).h
    cap$ = Controls(i).caption
    Value = Controls(i).Value
    zSelfHandleControl = 0

    SELECT CASE iCtrlType
    CASE zIconList
        zIconListHandler sGroup$, i, 0
        zSelfHandleControl = -1

    CASE zListBox
        zListboxHandler sGroup$, i, x, y, x2, y2, w, h, 0
        zSelfHandleControl = -1

    CASE zEditBox
        zEditBoxHandler sGroup$, cap$, i, x + 3, y + 1, x2, y2, w, h, Text, 0
        zSelfHandleControl = -1

    CASE zEditor
        zEditorHandler sGroup$, i, 0, 0
        zSelfHandleControl = -1

    END SELECT

END FUNCTION

'Legt die Datei fÅr den zukÅnftigen Mousecursor fest.
'sCursor$ = Der Cursor der Maus (*.cur-Datei im LaOS-Hauptverzeichnis)
'Wichtig:
'Ein Mauscursor hat folgende Abmessung: Maximal 15*24 Pixels
SUB zSetMouseCursor (sCursor$)
DIM l

    'Weg hier, falls Maus deaktiviert ist!
    IF MouseOn = 0 THEN EXIT SUB

    Mouse.Oldx = Mouse.x
    Mouse.Oldy = Mouse.y
    Mouse.Cursor = 1

    IF LEN(sCursor$) > 0 THEN
        'lade Mousecursor:
        f% = FREEFILE
        OPEN sCursor$ + ".cur" FOR BINARY AS f%
        GET f%, 1, l
        zMCS = SPACE$(l)
        GET f%, , zMCS
        CLOSE f%
    END IF

    'Muss Maschinenprogramm erst geladen werden?
    IF iMS(0) = 0 THEN
        RESTORE zMouseData
        DEF SEG = VARSEG(iMS(0))

        FOR i = 0 TO 99
         READ Byte$
         IF Byte$ = "-" THEN EXIT FOR
         POKE VARPTR(iMS(0)) + i, VAL("&H" + Byte$)
        NEXT i
    END IF

    DEF SEG = VARSEG(iMS(0))
    POKE VARPTR(iMS(0)) + 26, &H33

    'Mauszeiger unsichtbar:
    CALL ABSOLUTE(2, 0, 0, 0, VARPTR(iMS(0)))

    'Maustempo setzen:
    CALL ABSOLUTE(4, 0, Mouse.Speed, Mouse.Speed * 2, VARPTR(iMS(0)))

    'Mauscursor setzen:
    CALL ABSOLUTE(4, 0, Mouse.x, Mouse.y, VARPTR(iMS(0)))

END SUB

SUB zSetOrigPalette
  RESTORE zColors
  FOR i = 0 TO 15
    READ r, g, b
    zRGBPalette i, r, g, b
  NEXT
END SUB

SUB zSwapFocus (iAlt)
  iNeu = ActiveCtrl
  IF iAlt <> iNeu THEN
    RaiseGotFocus iAlt, iNeu, RTRIM$(Controls(iNeu).caption)
    IF Controls(iNeu).CtrlType = zEditBox THEN MID$(Controls(iNeu).tag, 5, 1) = "C"
    IF Controls(iAlt).CtrlType = zEditBox THEN MID$(Controls(iAlt).tag, 5, 1) = " ": zDrawCtrl iAlt, 0, 0
    IF NOT Controls(iAlt).SelfHandler THEN zDrawCtrl iAlt, 0, 0
    IF NOT Controls(iNeu).SelfHandler THEN zDrawCtrl iNeu, 0, 0
  END IF
  iAlt = iNeu
END SUB

SUB zTimeEventHandler (nr)
of = Farbe
om = InversFarbe
SELECT CASE nr
CASE 0  'Bildschirmschoner
  zScreenSaver
CASE 1  'Zeit aktualisieren
  IF SilentScreenMode AND NOT SaveScreenMode THEN EXIT SUB
  IF zShowTimeExitBox THEN
    InversFarbe = 0
    y = 2 + 2
    x = 573
    IF Mouse.x >= x AND Mouse.x <= x + 43 THEN
      IF Mouse.y >= y AND Mouse.y < y + 14 THEN
        zTimeEvent(1).inc = 0
        zToolTipText SysDate(DATE$)
        zTimeEvent(1).inc = 1
        EXIT SUB
      END IF
    END IF
    Farbe = tcol
    LINE (x + 1, y + 1)-STEP(41, y + 8), 0, B
    EchoXY x + 2, y, LEFT$(TIME$, 5)
    LINE (x, y)-STEP(43, y + 9), btrcol, B
  END IF
END SELECT
Farbe = of
InversFarbe = om
END SUB

SUB zToolTipText (Text$)
  x = Mouse.x + 5
  y = Mouse.y + 5
  x2 = x + TextWidth(Text$) + 4
  y2 = y + TextHeight(Text$) + 2
  IF x2 > 637 THEN x = x + (637 - x2): x2 = x2 + (637 - x2)
  IF y2 > 347 THEN y = y + (347 - y2): y2 = y2 + (347 - y2)
  g = 4 + INT(((PMAP(x2, 0) - PMAP(x, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y2, 1) - PMAP(y, 1) + 1)
  DIM bg(g)
  GET (x, y)-(x2, y2), bg
  Farbe = tttcol
  Label x + 1, y + 1, x2, y2, Text$, 0, ttfcol
  LINE (x, y)-(x2, y2), ttrcol, B
  ax = Mouse.x
  ay = Mouse.y
  DO
  LOOP UNTIL INKEY$ = ""
  DEF SEG = VARSEG(iMS(0))
  DO
    zGetMouse b, nx, ny
    k = INP(&H60)
    zDoEvents
  LOOP UNTIL nx <> ax OR b <> 0 OR ny <> ay OR (k < 128 AND k <> 0)
  PUT (x, y), bg, PSET
  Mouse.time = TIMER
  Farbe = tcol
END SUB

