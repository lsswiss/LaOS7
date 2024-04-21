DECLARE SUB Eigenschaften ()
DECLARE SUB RaiseMouseUp (iCtrl%, name$, iButton%)
DECLARE SUB MouseCurPaintOut (x%, y%)
DECLARE FUNCTION IsAbout% (IsX%, IsY%, AboutX%, AboutY%)
DECLARE SUB zDrawMouse (x%, y%)
DECLARE SUB SetActive (oFile AS ANY)
DECLARE SUB GetAbsoluteCoords (oFile AS ANY, x%, y%, x2%, y2%)
DECLARE FUNCTION OpenDosFile$ (DateiSuffix$)
DECLARE SUB Options ()
DECLARE SUB zSwapFocus (iAlt%)
DECLARE SUB SetCoordsToGrid (x%, y%, x2%, y2%)
DECLARE SUB LoadSettings ()
DECLARE SUB RefreshMark (deletefirst%)
DECLARE SUB SavePicture (x%, y%, x2%, y2%, file$)
DECLARE SUB Ablegen (redraw%, remark%)
DECLARE SUB MenuExtras ()
DECLARE SUB MenuZeichnen ()
DECLARE SUB MenuObjekte ()
DECLARE SUB RedrawScreen ()
DECLARE SUB CreateGUI ()
DECLARE FUNCTION LinkedFile$ ()
DECLARE FUNCTION NewName$ (name$)
DECLARE SUB DisableCtrl (iCtrl%)
DECLARE SUB EnableCtrl (iCtrl%)
DECLARE FUNCTION GetCtrl% (name$)
DECLARE FUNCTION AskAndSave% (ask%)
DECLARE SUB Laden (file$, oFilePos AS LONG)
DECLARE FUNCTION OpenDialog$ (PrgStarter$)
DECLARE SUB Speichern (file$)
DECLARE SUB ValidateCoord (x%, y%, x2%, y2%)
DECLARE FUNCTION GetCtrlType% ()
DECLARE FUNCTION ColorChoice% (col%)
DECLARE SUB CreateObject (oType%, filepos AS LONG, x%, y%, x2%, y2%, txt$, iOption1%)
DECLARE SUB LineXor (x%, y%, x2%, y2%)
DECLARE SUB Markieren (x%, y%, px%, py%)
DECLARE FUNCTION ChooseIcon$ (MarkedIcon$)
DECLARE SUB formatc ()
DECLARE FUNCTION IsNull% (Text$)
DECLARE SUB subDateiEigenschaften (id%)
DECLARE SUB subNeuerOrdner ()
DECLARE SUB subDateiLoeschen (id%)
DECLARE SUB RunFile (PrgStarter$)
'Lege hier die maximale Anzahl Controls und Strings (Editboxen) fest:
CONST MaxControls = 50, MaxStrings = 6

'Snowflake GUI v.0.2.3
'04.11.2003
'Entwickelt fÅr LaOS 7
'Dies ist OpenSource
'Programmierer, die an diesem Code mitgearbeitet haben, schreiben sich bitte
'hier ein, wenn mîglich mit kurzer Beschreibung der Arbeit:
'------------------------------------------------------------
'Urs Langmeier (Snowflake Standard-GUI)

CONST MaxIntArraySize = 50    'Legt die maximale Integer-Datenfeldgrîsse fest
                              'Standard = 4456

DECLARE SUB zHandleControls (iCtrl%, sGroup$, iChanges%)
DECLARE FUNCTION MenuInt$ (items AS STRING)
DECLARE SUB RaiseMouseMove (iCtrl%, name$, iButton%)
DECLARE SUB RaiseGotFocus (iOldCtrl%, iCtrl%, name$)
DECLARE SUB AddIcon (sName$, Index%, IconFile$, Caption$, Value%)
DECLARE SUB RaiseToolTipText (iCtrl%, name$)
DECLARE FUNCTION zQuitKey% ()
DECLARE SUB RaisePopupMenu (iCtrl%, name$, ExitInput%)
DECLARE SUB zRspm (iCtrl%)
DECLARE SUB Frame (sGroup$, Caption$, x1%, y1%, x2%, y2%)
DECLARE SUB zDoEvents ()
DECLARE SUB CheckBox (sGroup$, Caption$, x%, y%, Value%)
DECLARE SUB FullScreen ()
DECLARE SUB ClearList (sName$)
DECLARE FUNCTION SaveDialog$ (PrgStarter$)
DECLARE SUB ExitBox (sGroup$, x%, y%)
DECLARE SUB EditBox (sGroup$, sName$, x%, y%, iAnzahlSpalten%, iAnzahlZeilen%, iMaxZeichen%)
DECLARE SUB IconList (sGroup$, sName$, Caption$, x%, y%, x2%, y2%, iScrollBars%, iExitbox%)
DECLARE SUB SaveScreen ()
DECLARE SUB CLW (sGroup$)
DECLARE SUB StatusBar (Text$)
DECLARE SUB ProgressBar (Text$, Stat AS SINGLE)
DECLARE FUNCTION exists% (FilePath$)
DECLARE SUB TimeLock (modus%)
DECLARE SUB RadioButton (sGroup$, Caption$, x%, y%, Value%)
DECLARE SUB zDoMouse ()
DECLARE SUB ScrollBar (sGroup$, sName$, x%, y%, w%, h%, Min%, Max%, stp%, iCtrl%)
DECLARE SUB zEventDblClick (iCtrl%)
DECLARE SUB ExitFocus (sGroup$, sName$, x%, y%, x2%, y2%)
DECLARE SUB ListAdd (sName$, Index%, Text AS STRING, Value%)
DECLARE SUB zGetMouse (B%, x%, y%)
DECLARE FUNCTION SysDate$ (d$)
DECLARE SUB Label (x%, y%, x2%, y2%, Text AS STRING, Alignment%, bgColor%)
DECLARE SUB zToolTipText (Text$)
DECLARE SUB zEventMouseRest (iCtrl%)
DECLARE SUB zScreenSaver ()
DECLARE SUB zDrawCtrl (iCtrl%, MouseDown%, refresh%)
DECLARE SUB RaiseScroll (iCtrl%, name$)
DECLARE SUB zReplace (Text$, find$, replce$)
DECLARE SUB zSetMouseCursor (sCursor$)
DECLARE SUB PutValue (objekt AS STRING, wert%)
DECLARE SUB zEditBoxHandler (sGroup$, sName$, iCtrl%, x1%, y1%, x2%, y2%, iAnzSpalten%, iAnzZeilen%, Text AS STRING, refresh%)
DECLARE SUB zIconListHandler (sGroup$, iCtrl%, refresh%)
DECLARE SUB zRessourceSet (sGroup$, Index%, Text$, Value%)
DECLARE SUB ListBox (sGroup$, sName$, x%, y%, x2%, y2%, WithCheckBoxes%)
DECLARE SUB zRessourceGet (sGroup$, Index%, Text$, Value%)
DECLARE SUB zCreateControl (iCtrlType%, sGroup$, sName$, Caption$, Value%, x%, y%, x2%, y2%, w%, h%, Tag$, iSelfHandler%)
DECLARE SUB zSetOrigPalette ()
DECLARE FUNCTION TextLen% (Text AS STRING)
DECLARE SUB GetValue (objekt AS STRING, wert%)
DECLARE SUB MouseClicker (sGroup$, sName$, x%, y%, x2%, y2%, sSimulateKey AS STRING, iWithShadow%)
DECLARE FUNCTION TextHeight% (Text AS STRING)
DECLARE SUB zListboxHandler (sGroup$, iCtrl%, x%, y%, x2%, y2%, iAnzSpalten%, iAnzZeilen%, RefreshOnly%)
DECLARE FUNCTION TextWidth% (Text AS STRING)
DECLARE SUB zRGBPalette (slot%, r%, g%, B%)
DECLARE SUB ShowPicture (xx%, yy%, sFileName AS STRING)
DECLARE SUB RestoreScreen ()
DECLARE SUB zEventMouseMove (iCtrl%, iButton%)
DECLARE SUB SilentScreen ()
DECLARE SUB Main ()
DECLARE SUB zEventClick (iCtrl%, iButton%)
DECLARE SUB zEventMouseDown (iCtrl%, iButton%)
DECLARE FUNCTION zLauscheEvents% (sGroup$)
DECLARE FUNCTION zSelfHandleControl% (sGroup$, iCtrlType%)
DECLARE FUNCTION zGK$ (sGroup$)
DECLARE SUB PutString (objekt AS STRING, Text AS STRING)
DECLARE SUB GetString (objekt AS STRING, Text AS STRING)
DECLARE SUB zTimeEventHandler (Nr%)
DECLARE SUB zMoveScreen (x1%, y1%, x2%, y2%, ToX%, ToY%)
DECLARE SUB ClearControls (sGroup$)
DECLARE SUB zDrawBox (x1%, y1%, x2%, y2%)
DECLARE SUB DrawControls (sGroup$)
DECLARE SUB Button (sGroup$, Caption$, x%, y%)
DECLARE SUB EchoXY (x%, y%, Text AS STRING)
DECLARE SUB zNextCtrl (sGroup$)
DECLARE SUB zLastControl (sGroup$)
DECLARE SUB Echo (Text AS STRING)
DECLARE SUB Menu (sGroup$, Caption$)
DECLARE SUB GetInp (sGroup$)
DECLARE SUB Dialog (Text AS STRING)
DECLARE SUB zGUIConfiguration ()

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
    Caption     AS STRING * 40  'Die Beschreibung des Controls
    Group       AS STRING * 8   'Die Steuerelement-Gruppe, zu der das Control gehîrt
    x           AS INTEGER      'X-Position
    y           AS INTEGER      'Y-Position
    x2          AS INTEGER      'Position
    y2          AS INTEGER      'Position
    w           AS INTEGER      'Kann fÅr die Anz Zeichen (horizontal) verwendet werden
    h           AS INTEGER      'Kann fÅr die Anz Zeichen (vertikal) verwendet werden
    Tag         AS STRING * 12  'Unterschiedliche Verwendung
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
    clicked     AS INTEGER '1 = linke Taste geclickt, 2 = rechte Taste
    Down        AS INTEGER '1 = linke Taste unten, 2 = rechte Taste
    oldx        AS INTEGER
    oldy        AS INTEGER
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

'Controltypen ohne Tabstopp sind hîher als zControlTypeCount!
CONST zControlTypeCount = 8

'Controls ohne Tabstop:
CONST zScrollBar = 97
CONST zMouseClicker = 98

'Variablen und Datenfelder fÅr die LaOS-Technlologien:
DIM SHARED laosdir            AS STRING   'Das LaOS-Heimatverzeichnis
DIM SHARED tempdir            AS STRING   'Das Verzeichnis fÅr temporÑre Dateien

'Variablen und Datenfelder fÅr die LaOS-GUI:
'Maximale Mauszeigerdimensionen: 15x18 Pixels!
DIM SHARED Controls(1 TO MaxControls) AS zControlType, zStrings(1 TO MaxStrings) AS STRING, zTimeEvent(1) AS zTiEvType, zMCS AS STRING, zMHG(80), iMS(45), Mouse AS zMouseType, SilentScreenMode, SaveScreenMode, zSSMStop, Left, Top, Right, Down,  _
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
DIM SHARED DefaultCtrl, errorlevel, zDebugMode
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
    Caption AS STRING * 40
    SysType AS STRING * 1
    Created AS STRING * 10
    time AS STRING * 5
    Attrib AS STRING * 1
    info AS STRING * 256
    PrgStarter AS STRING * 40
    icon AS STRING * 12
END TYPE

'GUI Configuration
'********************
'Hier kannst du Einstellungen vornehmen, welche das Verhalten der  LaOS-GUI
'beeinflussen:

zDebugMode = 0
'-1 = Es wird der Speicherplatz in der Statusbar angezeigt

zShowTimeExitBox = -1
'-1 = Zeigt die Uhr und Exitbox im Hauptfenster an.

Programm:
'********************
'Schreibe hier dein Programm!
'Am besten schreibst du dein Programm in der Sub ->Main.
'Wenn du globale Variablen brauchst kannst du sie hier deklarieren.

DIM SHARED LastActivated AS LONG
CONST ucIconList = 70

CONST oEcho = 71
CONST oEchoXY = 72
CONST oFrame = 73
CONST oLine = 74
CONST oRect = 75
CONST oBox = 76
CONST oPicture = 77

TYPE ObjectType
  ObjType AS INTEGER
  txt AS STRING * 160
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
  Option1 AS INTEGER
  Option2 AS INTEGER
END TYPE
DIM SHARED ofilenr, ToInsert, Changed, Savefile$, oFilePos AS LONG
DIM SHARED oFarbe, iGridLen, iGridCol
DIM SHARED ctlX, ctlY, ctlX2, ctlY2, ctlCaption, ctlSet, NextTabCtrl

MouseClicker "pane", "pane", 0, 0, 0, 0, "##", 0

oFilePos = 1
ofilenr = FREEFILE
KILL tempdir + "objects.tmp"
OPEN tempdir + "objects.tmp" FOR BINARY AS ofilenr LEN = LEN(ObjectType)

DO
  Main
LOOP UNTIL AskAndSave(-1)

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

SUB Ablegen (redraw, remark)
DIM oFile AS ObjectType

IF ToInsert THEN
  Changed = -1
  redraw = 0
  ctl = NextCtrl
  ctl2 = NextCtrl + 1
  LastActivated = ctl
  ActiveCtrl = ctl
  x = Mouse.x
  y = Mouse.y
  dx = Mouse.dx
  dy = Mouse.dy
  SetCoordsToGrid x, y, dx, dy
  SetCoordsToGrid Mouse.x, Mouse.y, Mouse.dx, Mouse.dy
  zSetMouseCursor laosdir + "mouse"
  SELECT CASE ToInsert
  '*2
  CASE oPicture
    a$ = OpenDosFile("*.lds")
    IF a$ <> "" THEN
      LastActivated = -oFilePos
      IF x = dx AND y = dy THEN x = 639: y = 349
      CreateObject oPicture, oFilePos, dx, dy, x, y, a$, 0
      redraw = -1
    END IF
  CASE oEcho, oEchoXY
    LastActivated = -oFilePos
    SaveScreen
    w = 20
    h = 1
    IF ToInsert = oEcho THEN
      x = Mouse.dx: y = Mouse.dy
      w = FIX((Mouse.x - x) / 8) - 1
      h = 2
    END IF
    IF w < 2 THEN w = 10
    IF x + w * 8 > 637 THEN w = FIX((637 - x) / 8) - 1
    IF w < 2 THEN w = 2: x = 620
    MouseClicker "txt", "enter", 0, 0, 0, 0, "enter", 0
    ActiveCtrl = NextCtrl
    EditBox "txt", "Text", x, y, w, h, 80
    ERASE zStrings
    PutString "Text", ""
    DrawControls "txt"
    ShortKeys = ""
    IF ToInsert = oEcho THEN
      Left = Mouse.dx
      Top = Mouse.dy
      IF Mouse.x - Mouse.dx > 20 THEN Right = Mouse.x
      IF Mouse.y - Mouse.dy > 32 THEN Down = Mouse.y
    END IF
    GetInp "txt"
    RestoreScreen
    Farbe = oFarbe
    IF keyb = "enter" AND Mouse.clicked = 1 OR Mouse.clicked = 0 THEN
      GetString "Text", a$
      IF ToInsert = oEcho THEN
        CurrentX = Left
        CurrentY = Top
        Echo a$
        CreateObject oEcho, oFilePos, Left, Top, Right, Down, a$, oFarbe
      ELSE
        EchoXY x, y, a$
        CreateObject oEchoXY, oFilePos, x, y, 0, 0, a$, oFarbe
      END IF
      Farbe = tcol
    END IF
    ClearControls "txt"
  CASE oFrame
    LastActivated = -oFilePos
    ValidateCoord Mouse.dx, Mouse.dy, x, y
    IF x - Mouse.dx < 20 THEN x = Mouse.dx + 80
    IF y - Mouse.dy < 20 THEN y = Mouse.dy + 20
    CreateObject oFrame, oFilePos, Mouse.dx, Mouse.dy, x, y, "Frame", 0
    redraw = -1
  CASE oBox
    LastActivated = -oFilePos
    LINE (dx, dy)-(x, y), oFarbe, BF
    CreateObject oBox, oFilePos, dx, dy, x, y, "", oFarbe
  CASE oRect
    LastActivated = -oFilePos
    LINE (dx, dy)-(x, y), oFarbe, B
    CreateObject oRect, oFilePos, dx, dy, x, y, "", oFarbe
  CASE oLine
    LastActivated = -oFilePos
    LineXor 0, 0, 0, 0
    IF INP(&H60) = 42 THEN
      'shift-key pressed
      h = Mouse.x - Mouse.dx: v = Mouse.y - Mouse.dy
      IF h < 0 THEN h = h * -1
      IF v < 0 THEN v = v * -1
      IF h > v THEN
        y = dy
      ELSE
        x = dx
      END IF
    END IF
    LINE (dx, dy)-(x, y), oFarbe
    CreateObject oLine, oFilePos, dx, dy, x, y, "", oFarbe
  CASE zIconList
    ValidateCoord Mouse.dx, Mouse.dy, x, y
    IF x - Mouse.dx < 80 THEN x = Mouse.dx + 70
    IF y - Mouse.dy < 80 THEN y = Mouse.dy + 70
    a$ = NewName$("Icon")
    IconList "user", a$, "IconList", Mouse.dx, Mouse.dy, x, y, 0, 0
    AddIcon a$, 1, "icons.lsb", "Listitem", 0
    AddIcon a$, 2, "icons.lsb", "Listitem", 0
  CASE zListBox
    ValidateCoord Mouse.dx, Mouse.dy, x, y
    IF x - Mouse.dx < 20 THEN x = Mouse.dx + 80
    IF y - Mouse.dy < 20 THEN y = Mouse.dy + 20
    a$ = NewName$("List")
    ListBox "user", a$, Mouse.dx, Mouse.dy, x, y, 0
    ListAdd a$, 1, "Listitem", 0
    ListAdd a$, 2, "Listitem", 0
    ListAdd a$, 3, "Listitem", 0
  CASE zEditBox
    EditBox "user", NewName$("Edit"), Mouse.x, Mouse.y, 8, 1, 0
  CASE zRadioButton
    RadioButton "user", NewName$("Option"), Mouse.x, Mouse.y, -1
    Controls(ctl).Tag = MKI$(0)
  CASE zCheckBox
    CheckBox "user", NewName$("Check"), Mouse.x, Mouse.y, -1
    Controls(ctl).Tag = MKI$(0)
  CASE zButton
    Button "user", NewName$("Button"), Mouse.x, Mouse.y
    Controls(ctl).Tag = MKI$(0)
  END SELECT
  zDrawCtrl ctl, 0, -1
  zDrawCtrl ctl2, 0, -1
  ToInsert = 0
  IF LastActivated < 0 THEN
    GET ofilenr, -LastActivated, oFile
    SetActive oFile
  ELSE
    RaiseGotFocus 0, INT(LastActivated), ""
  END IF
ELSE
  'prÅfe, ob user auf ein erweitertes Objekt geklickt hat:
  found = 0
  iActive = -LastActivated
  IF iActive < 0 THEN iActive = 1
  IF Changed <= -5 THEN
    'Der Benutzer hat mit der Maus ein Objekt verschoben
    '->nun bitte noch die Koordinaten dem Grid ausrichten!
    ValidateCoord oFile.x, oFile.y, oFile.x2, oFile.y2
    Changed = -1: redraw = -1
    GET ofilenr, iActive, oFile
    SELECT CASE oFile.ObjType
    CASE oEcho, oBox, oRect, oLine, oFrame, oPicture
      SetCoordsToGrid oFile.x, oFile.y, oFile.x2, oFile.y2
    CASE oEchoXY
      SetCoordsToGrid oFile.x, oFile.y, 0, 0
    END SELECT
    PUT ofilenr, iActive, oFile
    SetActive oFile
    EXIT SUB
  END IF
  SEEK ofilenr, iActive
vqbRefind:
  DO
    fpos& = SEEK(ofilenr)
    GET ofilenr, , oFile
    GetAbsoluteCoords oFile, x, y, x2, y2
    'prÅfe: hat Benutzer innerhalb dieser Koordinaten geklickt?
    IF Mouse.x >= x AND Mouse.x <= x2 AND oFile.ObjType > 0 THEN
        IF Mouse.y >= y AND Mouse.y <= y2 AND fpos& > iActive THEN
          'ja: geklickt
          found = -1
          SetActive oFile
          LastActivated = -fpos&
          EXIT DO
        END IF
    END IF
  LOOP UNTIL oFile.ObjType = 0
  IF NOT found THEN found = -1: iActive = 0: SEEK ofilenr, 1: GOTO vqbRefind
  remark = -1
END IF
END SUB

'FÅgt ein Symbol zu einer Symbolliste hinzu. (Mit ->IconList erstellt man die
'Symbolliste)
'sName$ = Der Name der Symbolliste
'Index = Der Index des Eintrages (erforderlich: muss grîsser als 0 sein!)
'        Wichtig: Es dÅrfen keine LÅcken in der Indexfolge entstehen!
'IconFile$ = Der Dateiname des Symbols
'Caption$ = Die Beschreibung des Symbols
'value = Der Wert (-1 oder 0)
SUB AddIcon (sName$, Index, IconFile$, Caption$, Value)
DIM sFile AS STRING * 12
IF 12 < LEN(IconFile$) THEN
    Dialog "Fehler in AddIcon: Falsche Angabe des File (" + sFile + ")"
    EXIT SUB
  END IF
  sFile = IconFile$
  zRessourceSet sName$, Index, sFile + Caption$, Value
END SUB

FUNCTION AskAndSave% (ask)
  AskAndSave% = 0
  IF Changed THEN
    'user sollte speichern...
    IF ask THEN
      SaveScreen
      Frame "", "Frame", 111, 110, 526, 227
      zCreateControl 2, "zask", "", "    Ja", 0, 184, 190, 272, 211, 0, 0, MKD$(6.013470016988178D-154) + MKL$(538976288), 0
      zCreateControl 2, "zask", "", "   Nein", 0, 280, 190, 368, 211, 0, 0, MKD$(6.013470016988177D-154) + MKL$(538976288), 0
      zCreateControl 2, "zask", "", "Abbrechen", 0, 377, 190, 457, 211, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
      CurrentX = 121: CurrentY = 143
      Echo "Mîchten Sie die énderungen speichern?"
      ShortKeys = ""
      DrawControls "zask"
      GetInp "zask"
      ClearControls "zask"
      RestoreScreen
      SELECT CASE Focus
      CASE "Nein"
        AskAndSave% = -1
        EXIT FUNCTION
      CASE "Abbrechen", "esc"
        EXIT FUNCTION
      END SELECT
    END IF
    IF Savefile$ = "" THEN
      SaveScreen
      a$ = SaveDialog("LaoSoft Visual Snowflake")
      RestoreScreen
    ELSE
      a$ = Savefile$
    END IF
    IF a$ <> "" THEN
      Speichern a$
      Savefile$ = a$
      IF NOT Changed THEN AskAndSave% = -1
    END IF
  ELSE
    'nichts geÑndert
    AskAndSave% = -1
  END IF
END FUNCTION

'Erstellt ein Button-Steuerelement
'sGroup$ = Die Steuerelementgruppe
'Caption$ = Die Caption
'x, y = Die Koordinaten
SUB Button (sGroup$, Caption$, x, y)
  zCreateControl zButton, sGroup$, "", Caption$, 0, x, y, x + TextLen(Caption$) + 8, y + 21, 0, 0, "", 0
  CurrentX = x + TextLen(Caption$) + 15: CurrentY = y
END SUB

'Erstellt ein Checkbox-Steuerelement
'sGroup$ = Die Steuerelementegruppe
'Caption$ = Die Caption
'x, y = Die Koordinaten
'value = Der Wert (-1 oder 0)
SUB CheckBox (sGroup$, Caption$, x, y, Value)
  zCreateControl zCheckBox, sGroup$, "", Caption$, Value, x, y, x + TextLen(Caption$) + 32, y + 18, 0, 0, "", 0
END SUB

'Zeigt ein Dialog an, worin der Benutzer ein Symbol auswÑhlen kann.
'
'MarkedIcon$ = Das Symbol, das zu Beginn markiert sein soll.
'
'Gibt einen Leerstring ("") zurÅck, wenn Abgebrochen wurde, die Symboldatei,
'falls das Symbol ausgewÑhlt wurde. (Enter oder Doppelklick auf ein Symbol)
FUNCTION ChooseIcon$ (MarkedIcon$)

  IconList "zchgicon", "zicons", "Symbol auswÑhlen", 150, 120, 490, 280, -1, -1

  '*.lsb
  TimeLock 1
  SHELL "dir " + laosdir + "*.lsb /b >" + tempdir + "temp.lst"
  f = FREEFILE
  OPEN tempdir + "temp.lst" FOR INPUT AS f
  i = 0
  WHILE NOT EOF(f)
    LINE INPUT #f, a$
    i = i + 1
    AddIcon "zicons", i, a$, LEFT$(a$, INSTR(a$, ".") - 1), 1
    IF LCASE$(RTRIM$(a$)) = LCASE$(RTRIM$(MarkedIcon$)) THEN PutValue "zicons", i
  WEND
  CLOSE f

  '*.ico
  TimeLock 0
  SHELL "dir " + laosdir + "*.ico /b >" + tempdir + "temp.lst"
  f = FREEFILE
  OPEN tempdir + "temp.lst" FOR INPUT AS f
  errorlevel = 0
  DO WHILE NOT EOF(f)
    LINE INPUT #f, a$
    i = i + 1
    AddIcon "zicons", i, a$, LEFT$(a$, INSTR(a$, ".") - 1), 2
    IF LCASE$(RTRIM$(a$)) = LCASE$(RTRIM$(MarkedIcon$)) THEN PutValue "zicons", i
    IF errorlevel THEN EXIT DO
  LOOP
  CLOSE f
  DrawControls "zchgicon"

  ShortKeys = ""
  GetInp "zchgicon"

  IF keyb = "enter" THEN
    GetString "zicons", a$
    ChooseIcon$ = LEFT$(a$, 12)
  END IF
  ClearControls "zchgicon"

END FUNCTION

'Entfernt Steuerelemente aus dem Speicher
'sGroup$ = Die Steuerelementegruppe ("" = alle)
SUB ClearControls (sGroup$)
SHARED zoldsgroup AS STRING * 8
  Mouse.clicked = 0
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
          IF RTRIM$(LEFT$(zStrings(s%), 8)) = RTRIM$(RTRIM$(Controls(i).Caption)) THEN
            zStrings(s%) = ""
          END IF
        NEXT
        'Ressourcen des Objektes lîschen: (falls vorhanden...)
        KILL tempdir + RTRIM$(Controls(i).Caption) + ".rs~"
        IF RTRIM$(zoldsgroup) = RTRIM$(Controls(i).Caption) THEN zoldsgroup = ""
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
    Left = 2
    Top = 2
    Right = 637
    Down = 347
    CurrentX = 2
    CurrentY = 2
    IF zShowTimeExitBox THEN
        FOR i = 1 TO UBOUND(Controls)
          IF RTRIM$(Controls(i).Group) = sGroup$ AND RTRIM$(Controls(i).Caption) = "" THEN i = -1: EXIT FOR
        NEXT
        IF i > 0 THEN ExitBox sGroup$, 619, 4
        Farbe = tcol
        EchoXY 575, 4, LEFT$(TIME$, 5)
        LINE (573, 4)-STEP(43, 13), btrcol, B
    END IF
    DrawControls sGroup$
    RestoreScreen
END SUB

FUNCTION ColorChoice (col)
SavePicture 80, 84, 240, 168, tempdir + "bg.tmp"
Frame "colors", "Farbe wÑhlen:", 80, 84, 240, 168
MouseClicker "colors", "click", 88, 110, 231, 153, "click", 0
DrawControls "colors"

Top = 9
Left = 12
act = col
DO
IF act < 0 THEN act = 15
IF act > 15 THEN act = 0
LOCATE Top, Left

FOR i = 0 TO 15
IF i = 5 THEN LOCATE Top + 1, Left
IF i = 10 THEN LOCATE Top + 2, Left
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
  LOCATE Top, Left
  FOR i = 0 TO 15
    IF i = 5 THEN LOCATE Top + 1, Left
    IF i = 10 THEN LOCATE Top + 2, Left
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
ClearControls "colors"
ShowPicture 80, 84, tempdir + "bg.tmp"
END FUNCTION

SUB CreateGUI
  FullScreen
  ClearControls "prop"
  ClearControls "main"
  Menu "main", "Datei"
  Menu "main", "Bearbeiten"
  Menu "main", "Objekte"
  Menu "main", "Zeichnen"
  Menu "main", "Extras"
  ctlX = NextCtrl
  EditBox "prop", "x", 30, 330, 3, 1, 3
  ctlY = NextCtrl
  EditBox "prop", "y", 90, 330, 3, 1, 3
  ctlX2 = NextCtrl
  EditBox "prop", "x2", 160, 330, 3, 1, 3
  ctlY2 = NextCtrl
  EditBox "prop", "y2", 230, 330, 3, 1, 3
  ctlCaption = NextCtrl
  EditBox "prop", "caption", 340, 330, 20, 1, 40
  ctlSet = NextCtrl
  Button "prop", "Setzen", 581, 326
END SUB

'Visual Snowflake
SUB CreateObject (oType, filepos AS LONG, x, y, x2, y2, txt$, iOption1)
DIM oFile AS ObjectType
  IF oType <> oLine THEN ValidateCoord x, y, x2, y2
  oFile.ObjType = oType
  oFile.x = x
  oFile.y = y
  oFile.x2 = x2
  oFile.y2 = y2
  oFile.txt = txt$
  oFile.Option1 = iOption1
  PUT ofilenr, filepos, oFile
  filepos = SEEK(ofilenr)
END SUB

'Gibt eine Meldung auf dem Bildschirm aus, den der Benutzer mit OK bestÑtigen
'muss.
'text = Der auszugebende Text.
SUB Dialog (Text AS STRING)
  z = ActiveCtrl
  ta = TextAlignment
  TextAlignment = AlignCenter
  Left = 7
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
  SavePicture x1, y1, x2 - 1, y2, tempdir + "scrdlg.tmp"
  ax = x1
  VIEW
  Frame "", "", x1, y1, x2 - 1, y2 - 1
  Echo Text
  x1 = x1 + ((x2 - x1) - TextLen("    OK     ") - 8) / 2
  Button "MsgBox", "    OK     ", x1, y2 - 30
  DrawControls "MsgBox"
  GetInp "MsgBox"
  x1 = l
  x2 = l + w
  ClearControls "MsgBox"
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

'Zeichnet die Controls
'sGroup$ = Die Steuerelementgruppe ("" = alle)
SUB DrawControls (sGroup$)
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
    x = Left + ((panewidth - TextLen(linie)) / 2)
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

SUB Eigenschaften
DIM oFile AS ObjectType
  IF LastActivated <= 1 THEN
    GET ofilenr, -LastActivated, oFile
    Typ = oFile.ObjType
  ELSE
    Typ = Controls(LastActivated).CtrlType
  END IF
  Frame "", "Eigenschaften", 160, 60, 460, 270
  IF Typ = zListBox OR Typ = zIconList THEN
    zCreateControl 4, "property", "", "Scrollbar verwenden", -1, 170, 90, 354, 108, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
  ELSEIF Typ = oFrame THEN
    zCreateControl 4, "property", "", "Exitbox verwenden", -1, 170, 90, 354, 108, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
  END IF
  IF Typ = zEditBox THEN
    zCreateControl 6, "property", "", "Edit1", 2, 330, 120, 359, 136, 3, 1, MKD$(6.013469303013751D-154) + MKL$(538976288), -1
    PutString "Edit1", LTRIM$(STR$(CVI(Controls(LastActivated).Tag)))
  END IF
  DefaultCtrl = NextCtrl
  zCreateControl 2, "property", "", "    OK", 0, 260, 240, 348, 261, 0, 0, MKD$(6.013470016988178D-154) + MKL$(538976288), 0
  zCreateControl 2, "property", "", " Abbrechen", 0, 355, 240, 451, 261, 0, 0, MKD$(6.013470016988174D-154) + MKL$(538976288), 0
  IF Typ = zButton THEN
    zCreateControl 4, "property", "", "Standard-Knopf (DefaultCtrl)", (RTRIM$(Controls(LastActivated).Tag) = "default"), 172, 177, 428, 195, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
  END IF
  IF Typ = oEcho THEN
    l = 0: z = 0: r = 0
    IF oFile.Option2 <= 1 THEN l = -1
    IF oFile.Option2 = 2 THEN z = -1
    IF oFile.Option2 = 3 THEN r = -1
    zCreateControl 3, "property", "", "Links", l, 170, 205, 242, 223, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
    zCreateControl 3, "property", "", "Zentriert", z, 245, 205, 349, 223, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
    zCreateControl 3, "property", "", "Rechts", r, 355, 205, 435, 223, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
  END IF
  IF Typ = zEditBox THEN
    EchoXY 170, 120, "Maximale TextlÑnge:"
    EchoXY 370, 120, "Zeichen"
  END IF
  SELECT CASE Typ
  CASE oEcho, oEchoXY, oLine, oRect, oBox
    oCol = oFile.Option1
    MouseClicker "property", "farbe", 231, 151, 259, 165, "", True
    EchoXY 170, 150, "Farbe:"
    LINE (230, 150)-(260, 166), tcol, B
    LINE (231, 151)-(259, 165), oCol, BF
  END SELECT

  DrawControls "property"
  DO
    ShortKeys = ""
    GetInp "property"

    SELECT CASE Focus
    CASE "farbe"
      oCol = ColorChoice(oCol)
      LINE (230, 150)-(260, 166), tcol, B
      LINE (231, 151)-(259, 165), oCol, BF
      DrawControls "property"
    CASE "OK"
        'Write here Code for a Click on this Button
        SELECT CASE Typ
        CASE oEcho, oEchoXY, oLine, oRect, oBox
          IF Typ = oEcho THEN
            GetValue "Links", wert
            IF wert THEN
              wert = AlignLeft
            ELSE
              GetValue "Zentriert", wert
              IF wert THEN
                wert = AlignCenter
              ELSE
                GetValue "Rechts", wert
                IF wert THEN wert = AlignRight
              END IF
            END IF
            IF wert = 0 THEN wert = AlignLeft
            oFile.Option2 = wert
          END IF
          oFile.Option1 = oCol
          PUT ofilenr, -LastActivated, oFile
        CASE zEditBox
          'maximale Anzahl Zeichen setzen:
          GetString "Edit1", a$
          Controls(LastActivated).Tag = MKI$(VAL(a$))
        CASE zButton
          'DefaultCtrl exklusiv setzen:
          FOR i% = 1 TO UBOUND(Controls)
            IF Controls(i%).Group = "user" THEN
              IF Controls(i%).CtrlType = zButton THEN
                Controls(i%).Tag = ""
              END IF
            END IF
          NEXT
          IF Controls(LastActivated).CtrlType = zButton THEN
            Controls(LastActivated).Tag = "default"
          END IF
        CASE oFrame
          GetValue "Exitbox verwenden", oFile.Option1
          PUT ofilenr, -LastActivated, oFile
        CASE zListBox, zIconList
          STOP
        END SELECT
        ClearControls "property"
        RedrawScreen
        RefreshMark 0
        EXIT SUB
    CASE "Abbrechen", "esc"
        'Write here Code for a Click on this Button
        ClearControls "property"
        RedrawScreen
        RefreshMark 0
        EXIT SUB
    END SELECT
  LOOP
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
SUB ExitFocus (sGroup$, sName$, x, y, x2, y2)
  IF x + y + x2 + y2 = 0 THEN x2 = 639: y2 = 349
  zCreateControl zExitFocus, sGroup$, sName$, "", 0, x, y, x2, y2, 0, 0, "", 0
END SUB

'Erstellt einen Bereichsrahmen (Register). Der Textbereich wird auf diesen
'Rahmen eingeschrÑnkt.
'sGroup$ - Die Steuerelementgruppe, zu welcher der Rahmen gehîren wird...
'Caption$ - Die Bezeichnung des Rahmens
'x, y, x2, y2 - Die Koordinaten des Fensters
'Tipp: Sie kînnen die ersten vier Parameter auf 0 setzen, um ein Register
'      fÅr den ganzen Bildschirm zu machen.
SUB Frame (sGroup$, Caption$, x, y, x2, y2)
  IF x + y + x2 + y2 = 0 THEN
    CLW sGroup$
  ELSE
    LINE (x, y)-(x2, y2), wincol, B
    LINE (x + 1, y + 1)-(x2 - 1, y2 - 1), frmcol, B
    LINE (x + 2, y + 2)-(x2 - 2, y2 - 2), 0, BF
    Left = x + 2
    Top = y + 2
    Right = x2 - 2
    Down = y2 - 2
    IF sGroup$ > "" THEN
      FOR i = 1 TO UBOUND(Controls)
        IF RTRIM$(Controls(i).Group) = sGroup$ AND RTRIM$(Controls(i).Caption) = "" THEN i = -1: EXIT FOR
      NEXT
      IF i > 0 THEN ExitBox sGroup$, 0, 0
    END IF
  END IF
  CurrentX = Left + 2
  CurrentY = Top + 2
  IF LEN(Caption$) > 0 THEN
    LINE (Left, Top + 18)-(Right, Top + 18), frmlcol
    Echo LEFT$(Caption$, FIX((Right - Left - 25)) / 8)
    Top = Top + 19
    CurrentY = Top + 2
  END IF
END SUB

'Ermîglicht die Ausgabe von Text mit Echo im Vollbildschirm-Modus.
'FÅhren Sie FullScreen aus, wenn Sie Text ausserhalb des zuletzt erstellten
'Rahmenfensters (Frame) ausgeben wollen.
SUB FullScreen
  VIEW
  Left = 2
  Top = 2
  Right = 637
  Down = 347
END SUB

SUB GetAbsoluteCoords (oFile AS ObjectType, x, y, x2, y2)
  'Gibt die Absoluten Koordinaten zurÅck in x,y,x2 und y2 eines
  'erweiterten Objektes
  SELECT CASE oFile.ObjType
  CASE oEcho, oBox, oRect, oLine, oFrame, oPicture
    x = oFile.x
    y = oFile.y
    x2 = oFile.x2
    y2 = oFile.y2
    ValidateCoord x, y, x2, y2
  CASE oEchoXY
    x = oFile.x
    y = oFile.y
    x2 = oFile.x + LEN(RTRIM$(oFile.txt)) * 8
    y2 = y + 14
  CASE -1
    x = -1
    y = -1
    x2 = -1
    y2 = -1
  END SELECT
END SUB

'Gibt die Nummer eines Controls zurÅck.
'name$ = Der Name/Caption des Controls
FUNCTION GetCtrl (name$)
  FOR i = UBOUND(Controls) TO 1 STEP -1
    IF RTRIM$(Controls(i).Caption) = name$ THEN
      GetCtrl = i
      EXIT FUNCTION
    END IF
  NEXT
END FUNCTION

'Visual Snowflake: gibt den angewÑhlten Objekt- oder Control-Typ zurÅck.
FUNCTION GetCtrlType
DIM oFile AS ObjectType
  IF LastActivated < 0 THEN
    GET ofilenr, -LastActivated, oFile
    GetCtrlType = oFile.ObjType
  ELSE
    GetCtrlType = Controls(LastActivated).CtrlType
  END IF
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
Mouse.clicked = 0
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
    ListBox "z$", RTRIM$(Controls(i).Caption), x - 5, y - 14, x2 + 5, y2 + 14, 0
    Controls(a + 1).Value = Controls(i).Value
    DrawControls "z$"
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
          IF k <> 156 AND Mouse.clicked = 0 AND k <> 57 THEN GOTO zGetInpStartLoop
      END IF
      zHandleControls ActiveCtrl, sGroup$, iChanges
      IF NOT iChanges THEN EXIT DO
      keyb = ""
  CASE ELSE
    FOR i = 1 TO UBOUND(Controls)
      IF LCASE$(LEFT$(LTRIM$(Controls(i).Caption), LEN(keyb))) = LCASE$(keyb) AND (RTRIM$(Controls(i).Group) = sGroup$ OR sGroup$ = "") THEN
        iold = ActiveCtrl
        ActiveCtrl = i
        IF iold <> i THEN EXIT FOR
      END IF
    NEXT
  END SELECT
LOOP UNTIL zQuitKey
zSwapFocus iAlt
i = ActiveCtrl: ActiveCtrl = 0: zSwapFocus iAlt: ActiveCtrl = i
Focus = LTRIM$(RTRIM$(Controls(ActiveCtrl).Caption))
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
  zRessourceGet objekt$, Controls(GetCtrl(objekt$)).Value, Text$, 0
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
'Caption$ = Die öberschrift der Symbolliste
'x,y,x2,y2 = Die Position des Rahmens, innerhalb dessen sich die Symbole befinden
'            werden.
'iScrollBars = -1: Die Symbolliste wird mit Scrollbars und Rahmen angezeigt.
'iExitBox = -1: Exitbox wird angezeigt
SUB IconList (sGroup$, sName$, Caption$, x, y, x2, y2, iScrollBars, iExitbox)
  Tag$ = STRING$(8, 0)
  IF iScrollBars OR iExitbox THEN
    MID$(Tag$, 3, 2) = MKI$(NextCtrl + 1)
    Frame "", Caption$, x, y, x2, y2
    l = x2 - 19
    t = y + 4
    x = Left + 5
    y = Top + 5
    x2 = Right - 18
    y2 = Down
  END IF
  zCreateControl zIconList, sGroup$, sName$, "", 1, x, y, x2, y2, 0, 0, Tag$, -1
  IF iScrollBars OR iExitbox THEN
    ScrollBar sGroup$, "", x2 + 5, Top, 0, Down - Top, 1, 0, 1, NextCtrl - 1
    IF iExitbox THEN ExitBox sGroup$, l, t
  END IF
END SUB

'Gibt True zurÅck, wenn die beiden X/Y-Koordinaten ungefÑhr gleich sind.
'Genauigkeit: 3 Pixel
FUNCTION IsAbout (IsX, IsY, AboutX, AboutY)
  IsAbout = (IsX > AboutX - 5 AND IsX < AboutX + 5 AND IsY > AboutY - 5 AND IsY < AboutY + 5)
END FUNCTION

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
l = Left
t = Top
r = Right
d = Down
Left = x
Top = y
Right = x2
Down = y2
CurrentY = y
CurrentX = x + 2
LINE (x, y)-(x2, y2), bgColor, BF
Echo Text
TextAlignment = ta
InversFarbe = 0
Left = l
Top = t
Right = r
Down = d
END SUB

SUB Laden (file$, oFilePos AS LONG)
DIM oFile AS ObjectType
ClearControls "user"
CLOSE ofilenr
KILL tempdir + "objects.tmp"
OPEN tempdir + "objects.tmp" FOR BINARY AS ofilenr LEN = LEN(ObjectType)
ERASE zStrings

  f = FREEFILE
  OPEN file$ FOR BINARY AS f
  i = NextCtrl - 1
  DO
    i = i + 1
    GET f, , Controls(i)
  LOOP UNTIL Controls(i).CtrlType = 0
  NextCtrl = i
  'Load extended Objects:
  CLOSE f
  OPEN file$ + ".xoj" FOR BINARY AS f
  DO
    oFilePos = SEEK(f)
    GET f, , oFile
    PUT ofilenr, , oFile
  LOOP UNTIL oFile.ObjType = 0
  CLOSE f
END SUB

SUB LineXor (x, y, x2, y2)
STATIC ox, oy, ox2, oy2, m
DIM ds AS SINGLE, a AS SINGLE, B AS SINGLE

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

IF x2 < x THEN xmin = -1 ELSE xmin = 1
IF y2 < y THEN ymin = -1 ELSE ymin = 1

hseite = x - x2
IF hseite < 0 THEN hseite = -hseite
vseite = y - y2
IF vseite < 0 THEN vseite = -vseite

IF vseite >= hseite THEN
IF vseite = 0 THEN EXIT SUB
ds = hseite / vseite
DO
a = a + ds
B = B + 1
PSET (x + a * xmin, y + B * ymin), POINT(x + a * xmin, y + B * ymin) XOR tcol
LOOP UNTIL B >= vseite
ELSEIF hseite > vseite THEN
IF hseite = 0 THEN EXIT SUB
ds = vseite / hseite
DO
B = B + ds
a = a + 1
PSET (x + a * xmin, y + B * ymin), POINT(x + a * xmin, y + B * ymin) XOR tcol
LOOP UNTIL a >= hseite
END IF

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
  Tag$ = STRING$(8, 0)
  IF WithCheckBoxes THEN MID$(Tag$, 2, 1) = "C"
  IF sName$ <> "MenuInt" AND FIX((y2 - y - 4) / 14) > 2 THEN MID$(Tag$, 3, 2) = MKI$(NextCtrl + 1)
  zCreateControl zListBox, sGroup$, sName$, "", 1, x, y, x2, y2 - 1, FIX((x2 - x - 4) / 8), FIX((y2 - y - 4) / 14), Tag$, -1
  IF sName$ <> "MenuInt" AND FIX((y2 - y - 4) / 14) > 2 THEN
    ScrollBar sGroup$, "", x2 + 1, y, 0, y2 - y - 1, 1, 0, FIX((y2 - y - 4) / 14), NextCtrl - 1
  END IF
END SUB

SUB LoadSettings
  f = FREEFILE
  OPEN "vqb.ini" FOR INPUT AS f
  INPUT #f, iGridLen, iGridCol
  CLOSE f
END SUB

SUB Main
DIM oFile AS ObjectType

  LoadSettings
 
  oFarbe = tcol
  CreateGUI
 
  Savefile$ = LinkedFile
  IF Savefile$ <> "" THEN Laden Savefile$, oFilePos

redraw:
  RedrawScreen
  bDelFirst = False

remark:
  RefreshMark bDelFirst
  bDelFirst = True

  '* Eingabe *
  DO
vqb99:
  IF LastActivated < 0 THEN
    'sichert die alten Koordinaten:
    GET ofilenr, -LastActivated, oFile
    x = oFile.x
    y = oFile.y
    x2 = oFile.x2
    y2 = oFile.y2
  END IF
  DefaultCtrl = ctlSet
  ShortKeys = "#46,#48,#38,#33,#23,#19,#50,#18,f5,#63,##,#64,#65,#66,#67,#68,insert,t,l,r,b,delete,@15,@19,@14,@12,@17"
  GetInp ""
  Markieren 0, 0, 0, 0
  SELECT CASE keyb
  CASE "#33"
    ToInsert = oFrame
    zSetMouseCursor "vqbrect"
  CASE "#38"
    ToInsert = zListBox
    zSetMouseCursor "vqbrect"
  CASE "#46"
    ToInsert = zCheckBox
    zSetMouseCursor "vqbins"
  CASE "#19"
    ToInsert = zRadioButton
    zSetMouseCursor "vqbins"
  CASE "#48"
    ToInsert = zButton
    zSetMouseCursor "vqbins"
  CASE "#18"
    ToInsert = zEditBox
    zSetMouseCursor "vqbins"
  CASE "#23"
    ToInsert = zIconList
    zSetMouseCursor "vqbrect"
  CASE "f5"
    RedrawScreen
  CASE "@17"
    EXIT SUB
  CASE "@19"
    GOTO vqbsave
  CASE "@14"
    GOTO vqbneu
  CASE "@15", "@12"
    GOTO vqbopen
  CASE "delete"
    GOTO vqbdelete
  CASE "t"
    ToInsert = oEchoXY
    zSetMouseCursor "vqbtext"
    GOTO vqb99
  CASE "l"
    ToInsert = oLine
    zSetMouseCursor "vqbline"
    GOTO vqb99
  CASE "b"
    ToInsert = oBox
    zSetMouseCursor "vqbrect"
    GOTO vqb99
  CASE "r"
    ToInsert = oRect
    zSetMouseCursor "vqbrect"
    GOTO vqb99
  CASE "popup"
    MenuObjekte
    GOTO vqb99
  CASE "insert"
    Ablegen redraw, refresh
    IF redraw THEN GOTO redraw
    IF remark THEN GOTO remark
  CASE "#64"
    ActiveCtrl = ctlX
    GOTO vqb99
  CASE "#65"
    ActiveCtrl = ctlY
    GOTO vqb99
  CASE "#66"
    ActiveCtrl = ctlX2
    GOTO vqb99
  CASE "#67"
    ActiveCtrl = ctlY2
    GOTO vqb99
  CASE "#68"
    ActiveCtrl = ctlCaption
    GOTO vqb99
  CASE "#63"
    GOTO redraw
  END SELECT
  SELECT CASE Focus
  CASE "Setzen"
    'Werte auslesen:
    GetString "x", a$: x = VAL(a$)
    GetString "y", a$: y = VAL(a$)
    GetString "x2", a$: x2 = VAL(a$)
    GetString "y2", a$: y2 = VAL(a$)
    GetString "caption", Caption$
    i = GetCtrl(RTRIM$(Caption$))
    IF i <> LastActivated AND i > 0 THEN
      IF Controls(i).Group = "user" THEN
        Dialog "Der Steuerelemente-Name (" + Caption$ + ") ist nicht eindeutig!" + crlf + "Bitte wÑhlen Sie einen anderen Namen!"
        ActiveCtrl = ctlCaption
        GOTO vqb99
      END IF
    END IF
    Changed = -1
    'Validierung je nach Controltyp:
    SELECT CASE GetCtrlType
    CASE zCheckBox, zRadioButton
      Controls(LastActivated).Tag = MKI$(LEN(Caption$) - LEN(RTRIM$(Caption$)))
      x2 = x + TextLen(Caption$) + 32
      y2 = y + 18
      ValidateCoord x, y, x2, y2
    CASE zButton
      Controls(LastActivated).Tag = MKI$(LEN(Caption$) - LEN(RTRIM$(Caption$)))
      y2 = y + 21
      x2 = x + TextLen(Caption$) + 8
      ValidateCoord x, y, x2, y2
    CASE zEditBox
      w = x2
      h = y2
      x2 = x + (w * 8) + 5
      y2 = y + (h * 14) + 2
    CASE zListBox
      IF x2 < x THEN x2 = x + 80
      IF y2 < y THEN y2 = y + 80
      ValidateCoord x, y, x2, y2
    CASE ELSE
      ValidateCoord x, y, x2, y2
    END SELECT
    Changed = Controls(LastActivated).x <> x OR Controls(LastActivated).y <> y OR Controls(LastActivated).x2 > x2 OR Controls(LastActivated).y2 > y2 OR RTRIM$(Controls(LastActivated).Caption) > Caption$

    'Speichern der validierten Werte:
    'je nach Controltyp zusÑtzlich:
    SELECT CASE Controls(LastActivated).CtrlType
    CASE zIconList
      sb = CVI(MID$(Controls(LastActivated).Tag, 3, 2))
      IF sb > 0 THEN
        'Scrollbar aktualisieren:
        Controls(sb).x = x2 + 1
        Controls(sb).y = y
        Controls(sb).x2 = x2 + 14
        Controls(sb).y2 = y2
      END IF
    CASE zListBox
      Controls(LastActivated).w = FIX((x2 - x - 4) / 8)
      Controls(LastActivated).h = FIX((y2 - y - 4) / 14)
      sb = CVI(MID$(Controls(LastActivated).Tag, 3, 2))
      IF sb > 0 THEN
        'Scrollbar aktualisieren:
        Controls(sb).x = x2 + 1
        Controls(sb).y = y
        Controls(sb).x2 = x2 + 14
        Controls(sb).y2 = y2
      END IF
    CASE zEditBox
      Controls(LastActivated).w = w
      Controls(LastActivated).h = h
    END SELECT
    'Werte speichern:
    IF LastActivated < 0 THEN
      GET ofilenr, -LastActivated, oFile
      oFile.x = x
      oFile.y = y
      oFile.x2 = x2
      oFile.y2 = y2
      oFile.txt = Caption$
      oFile.ObjType = GetCtrlType
      PUT ofilenr, -LastActivated, oFile
      Changed = -1
    ELSE
      Controls(LastActivated).y = y
      Controls(LastActivated).x = x
      Controls(LastActivated).y2 = y2
      Controls(LastActivated).x2 = x2
      Controls(LastActivated).Caption = Caption$
      ActiveCtrl = LastActivated
    END IF
    IF Changed THEN
      GOTO redraw
    ELSE
      zDrawCtrl LastActivated + 0, 0, -1
      zDrawCtrl LastActivated + 1, 0, -1
    END IF
  CASE "pane"
    'GewÑhltes Control einfÅgen:
    Ablegen redraw, remark
    IF redraw THEN GOTO redraw
    IF remark THEN GOTO remark
  CASE "esc"
    IF NextTabCtrl > 0 THEN
      'Ende der Steuerelemente-Reihenfolge Bestimmung!
      NextTabCtrl = 0
      zSetMouseCursor laosdir + "mouse"
    END IF
    IF Changed <= -5 THEN
      keyb = ""
      'sichert die alten Koordinaten zurÅck:
      GET ofilenr, -LastActivated, oFile
      oFile.x = x
      oFile.y = y
      oFile.x2 = x2
      oFile.y2 = y2
      PUT ofilenr, -LastActivated, oFile
      Mouse.Cursor = 1
      Ablegen 0, 0
      'RedrawScreen
      'Changed = -1
    ELSE
      EXIT SUB
    END IF
  CASE "Datei"
    a$ = MenuInt("Neu|Ctrl+N,-,ôffnen...|Ctrl+O,Speichern|Ctrl+S,Speichern unter...,-,Auf QBasic-Programm speichern,-,Beenden")
    SELECT CASE a$
    CASE "Auf QBasic-Programm speichern"
      IF AskAndSave(0) THEN
        f = FREEFILE
        OPEN tempdir + "linkway.tmp" FOR OUTPUT AS f
        PRINT #f, Savefile$
        CLOSE f
        CHAIN "vqbcoder.mod"
        Dialog "Error Initializing vqbcoder.mod"
      END IF
    CASE "Beenden"
      EXIT SUB
    CASE "Neu"
vqbneu:
      IF AskAndSave(-1) THEN
        Changed = 0
        Savefile$ = ""
        ClearControls "user"
        CLOSE ofilenr
        KILL tempdir + "objects.tmp"
        OPEN tempdir + "objects.tmp" FOR BINARY AS ofilenr LEN = LEN(ObjectType)
        oFilePos = 1
        ERASE zStrings
        GOTO redraw
      END IF
    CASE "ôffnen..."
vqbopen:
      SaveScreen
      a$ = OpenDialog("LaoSoft Visual Snowflake")
      RestoreScreen
      IF a$ <> "" THEN
        Laden a$, oFilePos
        Savefile$ = a$
        Changed = 0
        GOTO redraw
      END IF
    CASE "Speichern"
vqbsave:
      IF Savefile$ = "" THEN GOTO vqbsaveas
      Speichern Savefile$
    CASE "Speichern unter..."
vqbsaveas:
      SaveScreen
      a$ = SaveDialog("LaoSoft Visual Snowflake")
      RestoreScreen
      IF a$ <> "" THEN Speichern a$: Savefile$ = a$
    END SELECT
  CASE "Zeichnen"
    MenuZeichnen
  CASE "Objekte"
    MenuObjekte
  CASE "Bearbeiten"
    a$ = MenuInt$("Lîschen|Delete,-,Eigenschaften")
    SELECT CASE a$
    CASE "Eigenschaften"
      Eigenschaften
    CASE "Lîschen"
vqbdelete:
      IF LastActivated < 0 THEN
        GET ofilenr, -LastActivated, oFile
        SELECT CASE oFile.ObjType
        CASE oFrame
          IF oFile.Option1 THEN
            'ZusÑtzlich ExitBox entfernen:
            Controls(oFile.Option1).Group = "#delete@"
            ClearControls "#delete@"
          END IF
        END SELECT
        oFile.ObjType = -1
        PUT ofilenr, -LastActivated, oFile
      ELSE
        SELECT CASE Controls(LastActivated).CtrlType
        CASE zListBox
          Controls(LastActivated + 1).Group = "#delete@"
        END SELECT
        Controls(LastActivated).Group = "#delete@"
        ClearControls "#delete@"
      END IF
      LastActivated = 0
      ERASE zStrings
      DrawControls "prop"
      GOTO redraw
    END SELECT
  CASE "Extras"
    MenuExtras
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
FOR Nr = 1 TO 2
FOR i = x2 TO x STEP 5 * stp
  PSET (i, y), POINT(i, y) XOR focol
NEXT
IF y2 - y = 0 THEN EXIT FOR
SWAP y, y2
NEXT Nr

IF ((y - y2) < 0) THEN stp = -1 ELSE stp = 1
FOR Nr = 1 TO 2
FOR i = y2 TO y STEP 5 * stp
  PSET (x, i), POINT(x, i) XOR focol
NEXT
IF x2 - x = 0 THEN EXIT FOR
SWAP x, x2
NEXT Nr
END SUB

'Setzt ein neues Menu hin!
'group = Die Steuerelement-Gruppe
'Caption$ = Die Caption des MenÅs
SUB Menu (sGroup$, Caption$)
  IF NextCtrl = 1 THEN
    oldx = Left
    oldy = Top
  ELSE
    FOR i = UBOUND(Controls) TO 1 STEP -1
      IF Controls(i).CtrlType = zMenu AND RTRIM$(Controls(i).Group) = sGroup$ THEN EXIT FOR
    NEXT
    IF i = 0 THEN
      oldx = Left
      oldy = Top
    ELSE
      oldx = Controls(i).x + TextLen(RTRIM$(Controls(NextCtrl - 1).Caption)) + 16
      oldy = Controls(i).y
    END IF
    IF oldx + TextLen(Caption$) > 639 THEN oldx = 2: oldy = oldy + 14
  END IF
  zCreateControl zMenu, sGroup$, "", Caption$, 0, oldx, oldy, oldx + TextLen(Caption$), oldy + 13, 0, 0, "", 0
END SUB

SUB MenuExtras
  a$ = MenuInt$("Reihenfolge der Controls bestimmen,Als Screenshot speichern...,Optionen...")
  SELECT CASE a$
  CASE "Reihenfolge der Controls bestimmen"
    FOR i% = 1 TO UBOUND(Controls)
      IF RTRIM$(Controls(i%).Group) = "user" THEN
        NextTabCtrl = i%
        EXIT FOR
      END IF
    NEXT
    IF NextTabCtrl > 0 THEN
      Dialog "Bitte klicken Sie der Reihe nach auf das jenige Control, das Sie als nÑchstes an die Reihe setzen mîchten!"
    ELSE
      Dialog "Sie kînnen die Steuerelemente-Reihenfolge erst bestimmen, wenn Sie mindestens ein Control eingefÅgt haben!"
    END IF

  CASE "Als Screenshot speichern..."
    file$ = SaveDialog("AGI Bitmap")
    IF file$ <> "" THEN
      'Datei speichern!
      CLW ""
      keyb = "npz"
      zShowTimeExitBox = False
      RedrawScreen
      zShowTimeExitBox = True
      keyb = ""
      SavePicture 0, 0, 639, 349, file$
    END IF
    RedrawScreen
  CASE "Optionen..."
    Options
    RedrawScreen
  END SELECT
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
        B$ = MID$(a$, t + 1)
        a$ = LEFT$(a$, t - 1) + CHR$(0) + SPACE$(3)
        a$ = a$ + SPACE$(maxwidth - LEN(a$) - LEN(B$) + 1) + B$
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
  MID$(Controls(c).Tag, 1, 1) = "M"
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
  ClearControls "MenuInt"
  IF INSTR(a$, CHR$(0)) > 0 THEN
    MenuInt$ = LTRIM$(LEFT$(a$, INSTR(a$, CHR$(0)) - 1))
  ELSE
    MenuInt$ = LTRIM$(a$)
  END IF
  IF keyb = "enter" THEN ActiveCtrl = DefaultCtrl ELSE ActiveCtrl = iOldCtrl
  zDrawCtrl iOldCtrl, 0, 0
END FUNCTION

SUB MenuObjekte
  a$ = MenuInt("Text|t,Fliesstext,-,Frame|Alt+F,-,Button|Alt+B,Editbox|Alt+E,ListBox|Alt+L,CheckBox|Alt+C,RadioButton|Alt+R,IconList|Alt+I")
  ToInsert = 0
  SELECT CASE a$
  '*1
  CASE "Text"
    ToInsert = oEchoXY
    zSetMouseCursor "vqbtext"
  CASE "Fliesstext"
    ToInsert = oEcho
    zSetMouseCursor "vqbtext"
  CASE "Frame"
    ToInsert = oFrame
    zSetMouseCursor "vqbrect"
  CASE "ListBox"
    ToInsert = zListBox
    zSetMouseCursor "vqbrect"
  CASE "CheckBox"
    ToInsert = zCheckBox
    zSetMouseCursor "vqbins"
  CASE "RadioButton"
    ToInsert = zRadioButton
    zSetMouseCursor "vqbins"
  CASE "Button"
    ToInsert = zButton
    zSetMouseCursor "vqbins"
  CASE "Editbox"
    ToInsert = zEditBox
    zSetMouseCursor "vqbins"
  CASE "IconList"
    ToInsert = zIconList
    zSetMouseCursor "vqbrect"
  END SELECT
END SUB

SUB MenuZeichnen
  a$ = MenuInt("Rechteck|r,GefÅlltes Rechteck|b,Linie|l,Bild,-,Zeichenfarbe")
  SELECT CASE a$
  CASE "GefÅlltes Rechteck"
    ToInsert = oBox
    zSetMouseCursor "vqbrect"
  CASE "Rechteck"
    ToInsert = oRect
    zSetMouseCursor "vqbrect"
  CASE "Linie"
    ToInsert = oLine
    zSetMouseCursor "vqbline"
  CASE "Bild"
    ToInsert = oPicture
    zSetMouseCursor "vqbins"
  CASE "Zeichenfarbe"
    oFarbe = ColorChoice(oFarbe)
  END SELECT
END SUB

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

FUNCTION NewName$ (name$)
  i = 0
  nn$ = name$
  DO
    i = i + 1
    nn$ = name$ + LTRIM$(STR$(i))
  LOOP UNTIL GetCtrl(nn$) = 0
  NewName$ = nn$
END FUNCTION

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
  ListBox "zload", "zfolders", Left + 2, Top + 18, 260, 273, 0
  ListBox "zload", "zfiles", 270, Top + 2, Right - 5, 253, 0
  Button "zload", " Abbrechen ", 403, 252
  zbtnLoad = NextCtrl
  Button "zload", " Datei îffnen ", 507, 252

DO
  DefaultCtrl = zbtnLoad
  'zfile steht nun auf dem aktuellen Ordner:
  GET f, zCurrentfolder, zfile
  infldr = zfile.id
  CurrentX = Left + 2
  CurrentY = Top + 2
  Farbe = tcol2
  Label Left + 2, Top + 2, 261, Top + 16, RTRIM$(zfile.Caption), 2, tcol3
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
      ListAdd "zfolders", fid, zftt.Caption, zftt.id
    END IF
  LOOP
  SEEK f, 1 + LEN(zftt)
  fid = 0
  DO
    GET f, , zftt
    IF zftt.id = 0 THEN EXIT DO
    IF zftt.relpos = infldr AND RTRIM$(zftt.PrgStarter) = RTRIM$(PrgStarter$) AND zftt.id > 0 THEN
      fid = fid + 1
      ListAdd "zfiles", fid, zftt.Caption, zftt.docint
    END IF
  LOOP
  DrawControls "zload"
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
  ClearControls "zload"
  ClearControls "zord"
  CLOSE f
END FUNCTION

' Build 6
'Zeigt einen Datei-ôffnen Dialog an und zwar fÅr MSDOS-Dateien!
'DateiSuffix$   Gibt an, welche Dateien angezeigt werden sollen.
'               *.* fÅr alle Dateien, *.bas fÅr Basic-Dateien etc...
'
'Tipp: Verwenden Sie ->OpenDialog, um ein LaOS-Dokument zu îffnen.
FUNCTION OpenDosFile$ (DateiSuffix$)
SaveScreen  'ula, 30.07.2003: sichert den Bildschirm

  bFirst = True

  IF DateiSuffix$ = "" THEN DateiSuffix$ = "*.*"
  '*** Controls laden ***
  ListBox "dosfile", "Drive", 17 * 8, 9 * 14, 35 * 8, 10 * 14, False
  ListBox "dosfile", "Folder", 17 * 8, 12 * 14, 35 * 8, 19 * 14, False
  ListBox "dosfile", "File", 37 * 8, 9 * 14, 63 * 8, 19 * 14, False

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
      TimeLock 1
      errorlevel = 0
      CHDIR CHR$(i) + ":\"
      IF errorlevel = 0 THEN a = a + 1: ListAdd "Drive", a, CHR$(i) + ":", False
      TimeLock 2
    NEXT
    bFirst = False
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
 
  '*** Controls zeichnen ***
  Frame "dosfile", "Datei îffnen", 15 * 8, 5 * 14, 65 * 8, 20 * 14
  LOCATE 8, 18
  PRINT cd$ + SPACE$(46 - LEN(cd$))
  DrawControls "dosfile"

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
    IF RIGHT$(cd$, 1) = "\" THEN OpenDosFile = cd$ + file$
    IF RIGHT$(cd$, 1) <> "\" THEN OpenDosFile = cd$ + "\" + file$
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
  ClearControls "dosfile"  'ula, 30.07.2003 leert alle Listen, entfernt alle Controls
  SHELL LEFT$(AltesDir$, 1) + ":"
  CHDIR AltesDir$
RestoreScreen    'ula, 30.07.2003: setzt den ursprÅnglichen Bildschirm zurÅck
END FUNCTION

SUB Options
SaveScreen
  Frame "options", "Einstellungen", 160, 80, 440, 250
  zCreateControl 4, "options", "", "Gitternetz verwenden", -1, 170, 110, 362, 128, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
  ctlEdit1 = NextCtrl
  zCreateControl 6, "options", "", "Edit1", 1, 340, 130, 409, 146, 8, 1, MKD$(6.013469303013751D-154) + MKL$(538976288), -1
  zCreateControl 2, "options", "", "Abbrechen", 0, 270, 220, 350, 241, 0, 0, MKD$(6.013470016988173D-154) + MKL$(538976288), 0
  DefaultCtrl = NextCtrl
  zCreateControl 2, "options", "", "   OK", 0, 360, 220, 432, 241, 0, 0, MKD$(6.013470016988177D-154) + MKL$(538976288), 0
  EchoXY 210, 130, "Grîsse (Pixel):"
  IF iGridLen > 0 THEN
    PutValue "Gitternetz verwenden", True
    PutString "Edit1", LTRIM$(STR$(iGridLen))
  ELSE
    PutValue "Gitternetz verwenden", False
  END IF
  igCol = iGridCol
  LINE (170, 210)-(430, 210), 8
  LINE (340, 150)-(370, 165), iGridCol, BF
  LINE (340, 150)-(370, 165), tcol, B
  MouseClicker "options", "color", 341, 151, 370, 165, "enter", -1
  EchoXY 210, 150, "Punktfarbe:"
  DrawControls "options"
  DO
    ShortKeys = ""
    GetInp "options"
    SELECT CASE Focus
    CASE "color"
        igCol = ColorChoice(igCol)
        LINE (340, 150)-(370, 165), igCol, BF
        LINE (340, 150)-(370, 165), tcol, B
        DrawControls "options"
    CASE "OK"
        'Speichern der Einstellungen:
        GetValue "Gitternetz verwenden", i
        ok = True
        IF i THEN
          GetString "Edit1", a$
          IF VAL(a$) <= 1 THEN
            Dialog "Die Gitternetz-Grîsse muss mindestens 2 Pixel betragen!"
            ActiveCtrl = ctlEdit1
            ok = False
          ELSE
            iGridLen = VAL(a$)
          END IF
        ELSE
          iGridLen = 0
        END IF
        IF ok THEN
          iGridCol = igCol
          f = FREEFILE
          OPEN "vqb.ini" FOR OUTPUT AS f
          WRITE #f, iGridLen, iGridCol
          CLOSE f
          EXIT DO
        END IF
    CASE "Abbrechen", "esc"
        EXIT DO
    END SELECT
  LOOP
  ClearControls "options"
RestoreScreen
END SUB

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
    IF RTRIM$(Controls(i).Caption) = RTRIM$(objekt$) AND Controls(i).h = 1 THEN Controls(i).Value = LEN(Text$) + 1
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
  'Kein Platz fÅr neuen String
  'Option radicale
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
SUB RadioButton (sGroup$, Caption$, x, y, Value)
  zCreateControl zRadioButton, sGroup$, "", Caption$, Value, x, y, x + TextLen(Caption$) + 32, y + 18, 0, 0, "", 0
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
  IF RTRIM$(Controls(ActiveCtrl).Group) = "user" THEN
    IF NextTabCtrl > 0 THEN
      SWAP Controls(NextTabCtrl), Controls(iCtrl)
      LOCATE 5, 5: PRINT NextTabCtrl
      iCtrl = NextTabCtrl
      ActiveCtrl = iCtrl
      NextTabCtrl = NextTabCtrl + 1
      IF RTRIM$(Controls(NextTabCtrl).Group) <> "user" THEN
        Dialog "Die Reihenfolge-Bestimmung ist zu Ende."
        NextTabCtrl = 0
      END IF
      EXIT SUB
    END IF
    Markieren 0, 0, 0, 0
    ERASE zStrings
    'Auf ein user-Control geklickt: Texte laden...
    'je nach Controltyp
    SELECT CASE Controls(iCtrl).CtrlType
    CASE zListBox, zIconList
      PutString "x", LTRIM$(STR$(Controls(iCtrl).x))
      PutString "y", LTRIM$(STR$(Controls(iCtrl).y))
      PutString "x2", LTRIM$(STR$(Controls(iCtrl).x2))
      PutString "y2", LTRIM$(STR$(Controls(iCtrl).y2))
      PutString "caption", RTRIM$(Controls(iCtrl).Caption)
      EnableCtrl GetCtrl("x2")
      EnableCtrl GetCtrl("y2")
      EnableCtrl GetCtrl("caption")
      cap$ = "Name:   "
      maxlen = 8
    CASE zButton, zCheckBox, zRadioButton
      PutString "x", LTRIM$(STR$(Controls(iCtrl).x))
      PutString "y", LTRIM$(STR$(Controls(iCtrl).y))
      PutString "x2", "---"
      PutString "y2", "---"
      DisableCtrl GetCtrl("x2")
      DisableCtrl GetCtrl("y2")
      EnableCtrl GetCtrl("caption")
      PutString "caption", RTRIM$(Controls(iCtrl).Caption) + SPACE$(CVI(LEFT$(Controls(iCtrl).Tag, 2)))
      maxlen = 40
    CASE zEditBox
      x2$ = "s: "
      y2$ = "z: "
      PutString "x", LTRIM$(STR$(Controls(iCtrl).x))
      PutString "y", LTRIM$(STR$(Controls(iCtrl).y))
      PutString "x2", LTRIM$(STR$(Controls(iCtrl).w))
      PutString "y2", LTRIM$(STR$(Controls(iCtrl).h))
      EnableCtrl GetCtrl("caption")
      EnableCtrl GetCtrl("x2")
      EnableCtrl GetCtrl("y2")
      cap$ = "Name:   "
      PutString "caption", RTRIM$(Controls(iCtrl).Caption)
      maxlen = 8
    END SELECT
    IF x$ = "" THEN EchoXY 10, 330, "x:" ELSE EchoXY 10, 330, x$
    IF y$ = "" THEN EchoXY 70, 330, "y:" ELSE EchoXY 70, 330, y$
    IF x2$ = "" THEN EchoXY 130, 330, "x2:" ELSE EchoXY 130, 330, x2$
    IF y2$ = "" THEN EchoXY 200, 330, "y2:" ELSE EchoXY 200, 330, y2$
    IF cap$ = "" THEN EchoXY 270, 330, "Caption:" ELSE EchoXY 270, 330, cap$
    Controls(ctlCaption).Tag = MKI$(maxlen)
    DrawControls "prop"
    LastActivated = iCtrl
  END IF
END SUB

'Wird ausgefÅhrt, wenn die Maus Åber ein Control bewegt wird.
'iCtrl   Die Ctrl-Nummer
'name$   Der Name des Controls
'iButton Der Button (1=links, 2=rechts)
SUB RaiseMouseMove (iCtrl, name$, iButton)
DIM oFile AS ObjectType

IF RTRIM$(Controls(iCtrl).Group) = "user" AND iButton = 1 AND LastActivated = iCtrl THEN
'Standard-Control verschieben
  x = Controls(iCtrl).x
  y = Controls(iCtrl).y
  x2 = Controls(iCtrl).x2
  y2 = Controls(iCtrl).y2
  IF Mouse.oldx >= x AND Mouse.oldy >= y THEN
    IF Mouse.oldx <= x2 AND Mouse.oldy <= y2 THEN
      IF Controls(iCtrl).CtrlType = zScrollBar THEN
        'wird nicht unterstÅtzt!
        EXIT SUB
      END IF
      IF keyb = "f5" THEN
        origx = x
        origy = y
        SetCoordsToGrid x, y, 0, 0
      END IF
      Markieren 0, 0, 0, 0
      px = Mouse.x - Mouse.oldx
      py = Mouse.y - Mouse.oldy
      Controls(iCtrl).x = x + px
      Controls(iCtrl).y = y + py
      SELECT CASE Controls(iCtrl).CtrlType
      CASE zIconList, zListBox
        IF keyb = "f5" THEN
          SetCoordsToGrid x2, y2, 0, 0
        END IF
        Controls(iCtrl).x2 = x2 + px
        Controls(iCtrl).y2 = y2 + py
        sb = CVI(MID$(Controls(iCtrl).Tag, 3, 2))
        IF sb > 0 THEN
          'Scrollbar aktualisieren:
          Controls(sb).x = x2 + 1
          Controls(sb).y = y
          Controls(sb).x2 = x2 + 14
          Controls(sb).y2 = y2
        END IF
      CASE ELSE
        'normal verschieben!
        IF keyb = "f5" THEN
          Controls(iCtrl).x2 = Controls(iCtrl).x2 + (x - origx)
          Controls(iCtrl).y2 = Controls(iCtrl).y2 + (y - origy)
        ELSE
          Controls(iCtrl).x2 = x2 + px
          Controls(iCtrl).y2 = y2 + py
        END IF
      END SELECT
      Markieren x, y, x2, y2
      Changed = -4
    END IF
  END IF

ELSEIF name$ = "pane" THEN
  'Mauszeiger mit dem Verschieben-Logo Åberzeichnen, falls dieser am Rand
  'des aktiven Objektes ist:
  IF LastActivated < 0 THEN
    'erweitertes Objekt:
    GET ofilenr, -LastActivated, oFile
    GetAbsoluteCoords oFile, x, y, x2, y2
    IF Changed <> -5 AND (IsAbout(x2, y2, Mouse.x, Mouse.y) OR Changed = -7 OR Mouse.Cursor = 3) THEN
      'An der unteren Ecke:
      'Objekt kann vergrîssert werden!
      Mouse.Cursor = 3
      IF iButton = 1 THEN
        Markieren 0, 0, 0, 0
        px = Mouse.x - Mouse.oldx
        py = Mouse.y - Mouse.oldy
        SELECT CASE oFile.ObjType
        CASE oEcho, oBox, oRect, oLine, oFrame, oPicture
          oFile.x2 = oFile.x2 + px
          oFile.y2 = oFile.y2 + py
        CASE oEchoXY
          'nicht zu berÅcksichtigen!
        END SELECT
        GetAbsoluteCoords oFile, x, y, x2, y2
        Markieren x, y, x2, y2
        PUT ofilenr, -LastActivated, oFile
        Changed = -7
        EXIT SUB
      END IF
    ELSE
      Mouse.Cursor = 1
    END IF
  ELSEIF LastActivated > 0 THEN
    'normales Control, das Resizing zulÑsst:
    SELECT CASE Controls(LastActivated).CtrlType
    CASE zListBox, zIconList
      x = Controls(LastActivated).x
      y = Controls(LastActivated).y
      x2 = Controls(LastActivated).x2
      y2 = Controls(LastActivated).y2
      sb = CVI(MID$(Controls(LastActivated).Tag, 3, 2))
      IF sb > 0 THEN
        'Listbox oder IconList mit Scrollbar:
        x2 = x2 + 13
      END IF
      IF Changed <> -5 AND (IsAbout(x2, y2, Mouse.x, Mouse.y) OR Changed = -7 OR Mouse.Cursor = 3) THEN
        'An der unteren Ecke:
        'Objekt kann vergrîssert werden!
        Mouse.Cursor = 3
        IF iButton = 1 THEN
          Markieren 0, 0, 0, 0
          px = Mouse.x - Mouse.oldx
          py = Mouse.y - Mouse.oldy
          IF keyb = "f5" THEN
            SetCoordsToGrid px, py, x2, y2
          END IF
          IF sb > 0 THEN
            'Listbox oder IconList mit Scrollbar:
            x2 = x2 - 13
          END IF
          x2 = x2 + px
          y2 = y2 + py
          Controls(LastActivated).x2 = x2
          Controls(LastActivated).y2 = y2
          Controls(LastActivated).w = FIX((x2 - x - 4) / 8)
          Controls(LastActivated).h = FIX((y2 - y - 4) / 14)
          IF sb > 0 THEN
            'Scrollbar aktualisieren:
            Controls(sb).x = x2 + 1
            Controls(sb).y = y
            Controls(sb).x2 = x2 + 14
            Controls(sb).y2 = y2
          END IF
          Markieren x, y, x2, y2
          Changed = -7
          EXIT SUB
        END IF
      ELSE
        Mouse.Cursor = 1
      END IF
    END SELECT
  END IF
  IF iButton = 1 THEN
    SELECT CASE ToInsert
    CASE 0
      IF LastActivated < 0 THEN
        GET ofilenr, -LastActivated, oFile
        GetAbsoluteCoords oFile, x, y, x2, y2
        IF Mouse.oldx >= x AND Mouse.oldy >= y THEN
          IF Mouse.oldx <= x2 AND Mouse.oldy <= y2 THEN
            Markieren 0, 0, 0, 0
            px = Mouse.x - Mouse.oldx
            py = Mouse.y - Mouse.oldy
            oFile.x = oFile.x + px
            oFile.y = oFile.y + py
            SELECT CASE oFile.ObjType
            CASE oEcho, oBox, oRect, oLine, oFrame, oPicture
              oFile.x2 = oFile.x2 + px
              oFile.y2 = oFile.y2 + py
            CASE oEchoXY
              'nicht zu berÅcksichtigen!
            END SELECT
            GetAbsoluteCoords oFile, x, y, x2, y2
            Markieren x, y, x2, y2
            PUT ofilenr, -LastActivated, oFile
            Changed = -5
          END IF
        END IF
      END IF

    CASE oEcho, oFrame, oRect, oBox, zListBox, zIconList, oPicture
      'Letzte Markierung lîschen:
      Markieren 0, 0, 0, 0
      'Neue Markierung setzen:
      Markieren Mouse.x, Mouse.y, Mouse.dx, Mouse.dy
    CASE oLine
      LineXor 0, 0, 0, 0
      IF INP(&H60) = 42 THEN
        'shift-key pressed
        h = Mouse.x - Mouse.dx
        v = Mouse.y - Mouse.dy
        IF h < 0 THEN h = h * -1
        IF v < 0 THEN v = v * -1
        IF h > v THEN
          LineXor Mouse.x, Mouse.dy, Mouse.dx, Mouse.dy
        ELSE
          LineXor Mouse.dx, Mouse.dy, Mouse.dx, Mouse.y
        END IF
      ELSE
        LineXor Mouse.x, Mouse.y, Mouse.dx, Mouse.dy
      END IF
    END SELECT
  END IF
END IF
END SUB

'Wird ausgefÅhrt, wenn die Maus geklickt wurde.
'iCtrl   Die Ctrl-Nummer
'name$   Der Name des Controls
'iButton Der Button (1=links, 2=rechts)
SUB RaiseMouseUp (iCtrl, name$, iButton)
  IF Changed = -4 THEN
    'Der Benutzer hat mit der Maus ein Control verschoben
    '->nun bitte Screen aktualisieren!
    keyb = "f5"
    RaiseMouseMove iCtrl, name$, iButton
    Changed = -1
    RaiseGotFocus 0, iCtrl, ""
  ELSEIF Changed = -7 AND LastActivated > 0 THEN
    'Der Benutzer hat ein Steuerelement resized:
    '->nun bitte die Koordinaten noch am Grid anpassen!
    keyb = "f5"
    RaiseMouseMove iCtrl, name$, iButton
    'SetCoordsToGrid Controls(LastActivated).x, Controls(LastActivated).y, Controls(LastActivated).x2, Controls(LastActivated).y2
  END IF
END SUB

'Wird ausgefÅhrt, wenn auf einem unterstÅtzten Steuerelement die rechte
'Maustaste gedrÅckt wurde.
'iCtrl = Die Nummer des Controls
'iCtrl = Der Name (Caption) des Controls
'
'Tipp: Mit ->MenuInt kann nun ein PopUpMenÅ angezeigt werden!
'
SUB RaisePopupMenu (iCtrl, name$, ExitInput)
 
  SELECT CASE name$
  CASE "IconList"
    a$ = MenuInt("Eintrag hinzufÅgen,-,Eintrag entfernen")
  CASE "pane"
    ExitInput = -1
  END SELECT

END SUB

'Event: Wird ausgefÅhrt, wenn der Benutzer mit der ScrollBar gescrollt hat.
'       FÅr die ScrollBar-Controls
'iCtrl = Die ControlNr der Scrollbar
'name$ = Der Name der Scrollbar
SUB RaiseScroll (iCtrl, name$)
STATIC x, y

    SELECT CASE name$
    END SELECT

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

SUB RedrawScreen
DIM oFile AS ObjectType
 
  SilentScreen
  IF keyb <> "npz" THEN
    CLW "main"
    IF iGridLen > 0 THEN
      'Gitternetz zeichnen
      FOR x = iGridLen TO 639 STEP iGridLen
        FOR y = iGridLen TO 349 STEP iGridLen
          PSET (x, y), iGridCol
        NEXT
      NEXT
    END IF
  END IF

  '* Zeichnen *
  'Im Hintergrund stehende erweiterte Objekte zeichnen:
  '(Frames, etc..)
  SEEK ofilenr, 1
  DO
    GET ofilenr, , oFile
    SELECT CASE oFile.ObjType
    CASE oPicture
      IF oFile.x2 > 639 THEN oFile.x2 = 639
      IF oFile.y2 > 349 THEN oFile.y2 = 349
      VIEW (oFile.x, oFile.y)-(oFile.x2, oFile.y2)
      ShowPicture 0, 0, oFile.txt
      VIEW
    CASE oFrame
      Frame "", RTRIM$(oFile.txt), oFile.x, oFile.y, oFile.x2, oFile.y2
      IF oFile.Option1 THEN
        'Mit ExitBox zeichnen:
        x = oFile.x2 - 19
        y = oFile.y + 4
        LINE (x, y)-(x + 15, y + 13), btscol, BF
        LINE (x, y)-(x + 15, y + 13), btrcol, B
        LINE (x + 7, y + 6)-STEP(3, 3), 1, BF
        LINE (x + 5, y + 5)-STEP(3, 3), btrcol, BF
      END IF
      IF iGridLen > 0 AND keyb <> "npz" THEN
        'Gitternetz zeichnen
        SetCoordsToGrid oFile.x, oFile.y, oFile.x2, oFile.y2
        FOR x = oFile.x TO oFile.x2 STEP iGridLen
          FOR y = oFile.y TO oFile.y2 STEP iGridLen
            IF (x > oFile.x + 3 AND x < oFile.x2 - 2) AND (y > oFile.y + 22 AND y < oFile.y2 - 3) THEN
              PSET (x, y), iGridCol
            END IF
          NEXT
        NEXT
      END IF
    END SELECT
  LOOP UNTIL oFile.ObjType = 0

  'erweiterte Objekte auslesen und zeichnen: (im Vordergrund stehende)
  SEEK ofilenr, 1
  DO
    GET ofilenr, , oFile
    SELECT CASE oFile.ObjType
    CASE oEcho
      Farbe = oFile.Option1
      Left = oFile.x
      Top = oFile.y
      Right = oFile.x2
      Down = oFile.y2
      CurrentX = Left
      CurrentY = Top
      TextAlignment = oFile.Option2
      Echo RTRIM$(oFile.txt)
      TextAlignment = 1
    CASE oBox
      LINE (oFile.x, oFile.y)-(oFile.x2, oFile.y2), oFile.Option1, BF
    CASE oRect
      LINE (oFile.x, oFile.y)-(oFile.x2, oFile.y2), oFile.Option1, B
    CASE oLine
      LINE (oFile.x, oFile.y)-(oFile.x2, oFile.y2), oFile.Option1
    CASE oEchoXY
      Farbe = oFile.Option1
      EchoXY oFile.x, oFile.y, RTRIM$(oFile.txt)
    END SELECT
  LOOP UNTIL oFile.ObjType = 0

  'DrawControls:
  FOR i = 1 TO UBOUND(Controls)
    SELECT CASE Controls(i).CtrlType
    CASE zIconList
      'Rahmen zeichnen um IconList
      c = POINT(Controls(i).x, Controls(i).y)
      zDrawCtrl i, 0, -1
      LINE (Controls(i).x, Controls(i).y)-(Controls(i).x2, Controls(i).y2), tcol2, B, PunkteLinie
      PSET (Controls(i).x, Controls(i).y), c
    CASE ELSE
      IF NOT (keyb = "npz" AND (RTRIM$(Controls(i).Group) = "main" OR RTRIM$(Controls(i).Group) = "prop")) THEN
        zDrawCtrl i, 0, -1
      END IF
    END SELECT
  NEXT

  IF keyb <> "npz" THEN
    'Properties zeichnen: mit Standardcaptions
    Farbe = tcol
    LINE (2, 325)-(647, 325), frmlcol
    EchoXY 200, 330, "y2:"
    EchoXY 130, 330, "x2:"
    EchoXY 10, 330, "x:"
    EchoXY 70, 330, "y:"
    EchoXY 270, 330, "Caption:"
  END IF
  RestoreScreen
END SUB

SUB RefreshMark (deletefirst)
DIM oFile AS ObjectType
  'Markierung zeichnen:
  IF LastActivated < 0 THEN
    GET ofilenr, -LastActivated, oFile
    GetAbsoluteCoords oFile, x, y, x2, y2
    IF deletefirst THEN Markieren 0, 0, 0, 0
    Markieren x, y, x2, y2
  END IF
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
FUNCTION SaveDialog$ (PrgStarter$)
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
  ListBox "zsave", "zfolders", Left + 2, Top + 18, 260, 273, 0
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
  CurrentX = Left + 2
  CurrentY = Top + 2
  Farbe = tcol2
  Label Left + 2, Top + 2, 261, Top + 16, RTRIM$(zfile.Caption), 2, tcol3
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
      ListAdd "zfolders", fid, zftt.Caption, zftt.id
    END IF
  LOOP
  DrawControls "zsave"
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
    Frame "zord", "Neuer Ordner in " + zfile.Caption, 80, 120, 470, 220
    Echo "Ordnername:"
    PutString "zord", file$
    EditBox "zord", "zord", 90, 160, 40, 1, 40
    PutString "zord", file$
    DefaultCtrl = NextCtrl
    Button "zord", ">> Weiter", 350, 180
    DrawControls "zord"
    GetInp "zord"
    IF keyb = "enter" THEN
      'Der neue Ordner wird erstellt:
      SEEK f, 1
      zid = 0
      DO
       zid = zid + 1
       p& = SEEK(f)
       GET f, , zftt
      LOOP UNTIL zftt.id = 0
      zftt.id = zid
      zftt.relpos = zfile.id
      GetString "zord", a$
      zftt.Caption = a$
      zftt.SysType = "D"
      zftt.Created = DATE$
      zftt.time = TIME$
      zftt.icon = "ordner.lsb"
      IF RTRIM$(a$) <> "" THEN PUT f, p&, zftt
    END IF
    ClearControls "zsave"
    ClearControls "zord"
    GOTO zSvdlg
  CASE "Speichern"
      'Die neue Datei wird erstellt:
      zid = 0
      SEEK f, 1
      DO
       zid = zid + 1
       p& = SEEK(f)
       GET f, , zfile
      LOOP UNTIL zfile.id = 0
      zfile.id = zid
      zfile.relpos = infldr
      DO WHILE exists(laosdir + "doc\" + LTRIM$(STR$(zid)))
        zid = zid + 1
      LOOP
      zfile.docint = zid
      GetString "zfile", a$
      zfile.Caption = a$
      GetString "zinfo", a$
      zfile.info = a$
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
  ClearControls "zsave"
  ClearControls "zord"
  CLOSE f
END FUNCTION

'Liest ein Bild vom Bildschirm ein und speichert es in eine Datei.
'x,y,x2,y2  Die Koordinaten des Bildes
'file$      Die Datei (existierende Dateien werden Åberschrieben)
SUB SavePicture (x, y, x2, y2, file$)
DIM Pic(123) AS DOUBLE
  f% = FREEFILE
  OPEN file$ FOR BINARY AS f%
  t& = CVL("ABMS")
  PUT f%, 1, t&
  w% = x2 - x + 1
  h% = y2 - y + 1
  PUT f%, , w%
  PUT f%, , h%
  FOR xx = x TO x2 STEP 41
    rx% = x2 - xx
    FOR yy = y TO y2 STEP 41
      ry% = y2 - yy
      IF rx% < 41 OR ry% < 41 THEN
        IF rx% >= 41 THEN rx% = 40
        IF ry% >= 41 THEN ry% = 40
        g% = ((4 + INT(((PMAP(rx%, 0) - PMAP(0!, 0) + 1) * (1) + 7) / 8) * 4 * (PMAP(ry%, 1) - PMAP(0!, 1) + 1))) / 8
        REDIM Pic(g%) AS DOUBLE
        GET (xx, yy)-(xx + rx%, yy + ry%), Pic
      ELSE
        REDIM Pic(123) AS DOUBLE
        GET (xx, yy)-(xx + 40, yy + 40), Pic
      END IF
      'Speichern:
      FOR i% = LBOUND(Pic) TO UBOUND(Pic)
          PUT f%, , Pic(i%)
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
SUB ScrollBar (sGroup$, sName$, x, y, w, h, Min, Max, stp, iCtrl)
  IF w > 0 THEN x2 = x + w: y2 = y + 13 ELSE y2 = y + h: x2 = x + 13
  DIM t AS STRING * 12
  MID$(t, 2, 2) = MKI$(Min)
  MID$(t, 4, 2) = MKI$(Max)
  MID$(t, 6, 2) = MKI$(iCtrl)
  MID$(t, 10, 2) = MKI$(stp)
  'Tagpos. 9 ist reserviert (Scrollflag)
  zCreateControl zScrollBar, sGroup$, sName$, "", Max, x, y, x2, y2, w, h, t, -1
END SUB

SUB SetActive (oFile AS ObjectType)
  'holt ein erweitertes Objekt in die Properties
  ERASE zStrings
  GetAbsoluteCoords oFile, x, y, x2, y2
  SELECT CASE oFile.ObjType
  CASE oEcho, oBox, oRect, oLine, oFrame, oPicture
    PutString "x", LTRIM$(STR$(x))
    PutString "y", LTRIM$(STR$(y))
    PutString "x2", LTRIM$(STR$(x2))
    PutString "y2", LTRIM$(STR$(y2))
    IF oFile.ObjType = oEcho OR oFile.ObjType = oFrame OR oFile.ObjType = oPicture THEN
      PutString "caption", RTRIM$(oFile.txt)
      EnableCtrl ctlCaption
    ELSE
      PutString "caption", ""
      DisableCtrl ctlCaption
    END IF
    EnableCtrl ctlX2
    EnableCtrl ctlY2
    maxlen = 160
  CASE oEchoXY
    PutString "x", LTRIM$(STR$(x))
    PutString "y", LTRIM$(STR$(y))
    PutString "x2", "---"
    PutString "y2", "---"
    DisableCtrl ctlX2
    DisableCtrl ctlY2
    PutString "caption", RTRIM$(oFile.txt)
    EnableCtrl ctlCaption
    maxlen = 40
  END SELECT
  EchoXY 10, 330, "x:"
  EchoXY 70, 330, "y:"
  EchoXY 130, 330, "x2:"
  EchoXY 200, 330, "y2:"
  IF oFile.ObjType = oPicture THEN
    EchoXY 270, 330, "File:   "
  ELSE
    EchoXY 270, 330, "Caption:"
  END IF
  Controls(ctlCaption).Tag = MKI$(maxlen)
  GetString "caption", a$
  DrawControls "prop"
  Markieren x, y, x2, y2
END SUB

'Passt die Koordinaten dem eingestellten Gitternetz an...
SUB SetCoordsToGrid (x, y, x2, y2)
  x = CINT(x / iGridLen) * iGridLen
  y = CINT(y / iGridLen) * iGridLen
  x2 = CINT(x2 / iGridLen) * iGridLen
  y2 = CINT(y2 / iGridLen) * iGridLen
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
 
  OPEN sFileName FOR BINARY AS f%: GET f%, 1, tst!
  IF tst! = 0 THEN CLOSE f%: OPEN laosdir + "sys.lsb" FOR BINARY AS f%
  SEEK f%, 1
 
  SELECT CASE LCASE$(RIGHT$(sFileName, 4))
  CASE ".lds", ".lsb"
zsplds:
    DIM Befehle(5) AS SINGLE
    DO
    FOR Nr = 0 TO 5
    GET f%, , Befehle(Nr)
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
    DIM Pic(123) AS DOUBLE
    GET f%, 1, t&
    GET f%, , w%
    GET f%, , h%
    c = xx + w% - 1
    d = yy + h% - 1
    IF MKL$(t&) = "ABM2" THEN
      'AGI Bitmap mit Farbpalette
      'Farb-Palette laden:
      p$ = SPACE$(64)
      GET f%, , p$
      FOR x = 1 TO LEN(p$) STEP 4
         r% = INT((ASC(MID$(p$, x, 1))))
         g% = INT((ASC(MID$(p$, x + 1, 1))))
         B% = INT((ASC(MID$(p$, x + 2, 1))))
         zRGBPalette ((x - 1) / 4), r%, g%, B%
      NEXT
    ELSEIF MKL$(t&) <> "ABMS" THEN
      sFileName = laosdir + "sys.lsb"
      GOTO zsplds
    END IF
    FOR x = xx TO c STEP 41
      rx% = c - x
      FOR y = yy TO d STEP 41
        ry% = d - y
        IF rx% < 41 OR ry% < 41 THEN
          IF rx% >= 41 THEN rx% = 40
          IF ry% >= 41 THEN ry% = 40
          g% = ((4 + INT(((PMAP(rx%, 0) - PMAP(0!, 0) + 1) * (1) + 7) / 8) * 4 * (PMAP(ry%, 1) - PMAP(0!, 1) + 1))) / 8
          REDIM Pic(g%) AS DOUBLE
        ELSE
          REDIM Pic(123) AS DOUBLE
        END IF
        FOR i% = LBOUND(Pic) TO UBOUND(Pic)
          GET f%, , Pic(i%)
        NEXT
        PUT (x, y), Pic, PSET
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

SUB Speichern (file$)
DIM oFile AS ObjectType

  KILL file$ + "a"
  KILL file$ + "a.xoj"
  errorlevel = 0
  f = FREEFILE
  OPEN file$ + "a" FOR BINARY AS f
  FOR i = 1 TO UBOUND(Controls)
    IF RTRIM$(Controls(i).Group) = "user" THEN
      PUT f, , Controls(i)
    END IF
  NEXT

  'Save extended Objects:
  CLOSE f
  OPEN file$ + "a.xoj" FOR BINARY AS f
  SEEK ofilenr, 1
  DO
    GET ofilenr, , oFile
    IF oFile.ObjType > 0 THEN PUT f, , oFile
  LOOP UNTIL oFile.ObjType = 0
  CLOSE f
  IF errorlevel THEN
    Dialog "Fehler beim Speichern"
  ELSE
    KILL file$ + ".bak"
    KILL file$ + ".xbk"
    NAME file$ AS file$ + ".bak"
    NAME file$ + ".xoj" AS file$ + ".xbk"
    NAME file$ + "a.xoj" AS file$ + ".xoj"
    NAME file$ + "a" AS file$
    Changed = 0
  END IF

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
panewidth = Right - Left
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
  panewidth = Right - Left
  leftpane = 0
  Max = 0
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
    IF x > Max THEN Max = x
    x = 0
  LOOP UNTIL LEN(txt) = 0
  TextWidth% = Max
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

SUB ValidateCoord (x, y, x2, y2)
  IF x2 + y2 > 0 THEN
    IF x > x2 THEN SWAP x, x2
    IF y > y2 THEN SWAP y, y2
  END IF
  IF x > 637 THEN x = 637
  IF y > 347 THEN y = 347
  IF x < 0 THEN x = 0
  IF y < 0 THEN y = 0
END SUB

SUB zCreateControl (iCtrlType, sGroup$, sName$, Caption$, Value, x, y, x2, y2, w, h, Tag$, iSelfHandler)
'todo ula: die folgende zeilen sind nur fÅr snowflake visqb gemacht:
  IF sGroup$ = "user" THEN
    IF NextCtrl > 32 THEN Dialog "Die maximal mîgliche Anzahl Controls ist erreicht!": EXIT SUB
  END IF
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
    Controls(NextCtrl).Caption = sName$
  ELSE
    Controls(NextCtrl).Caption = Caption$
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
  Controls(NextCtrl).Tag = Tag$
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
    CALL ABSOLUTE(3, B, x, y, VARPTR(iMS(0)))
    'PrÅfe auf Doppelklick:
    IF Mouse.time > 0 AND B = 1 THEN
      IF TIMER - Mouse.time < .8 THEN
        IF x > Mouse.ClickX - 2 AND x < Mouse.ClickX + 2 THEN
          IF y > Mouse.ClickY - 2 AND y < Mouse.ClickY + 2 THEN
            MouseOn = 2
            WHILE B > 0
              CALL ABSOLUTE(3, B, x, y, VARPTR(iMS(0)))
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

SUB zDrawCtrl (iCtrl, MouseDown, refresh)
DIM Text AS STRING
  i = iCtrl
  IF i = 0 THEN EXIT SUB
  x = Controls(i).x
  y = Controls(i).y
  x2 = Controls(i).x2
  y2 = Controls(i).y2
  w = Controls(i).w
  h = Controls(i).h
  cap$ = RTRIM$(Controls(i).Caption)
  Activated = (ActiveCtrl = i)
  Value = Controls(i).Value
  am = InversFarbe
  af = Farbe
  InversFarbe = 0
  Tag$ = Controls(i).Tag
  Farbe = tcol

  SELECT CASE Controls(i).CtrlType
  CASE IS < 0
    Controls(i).CtrlType = -Controls(i).CtrlType
    zDrawCtrl i, MouseDown, refresh
    Controls(i).CtrlType = -Controls(i).CtrlType
    FOR i = y + 2 TO y2 - 2 STEP 2
        LINE (x + 1, i)-(x2 - 2, i), 7, , 21845
        IF i < y2 - 2 THEN LINE (x + 2, i + 1)-(x2 - 2, i + 1), 7, , 21845
    NEXT

  CASE zIconList
    IF NOT MouseDown THEN
      IF refresh THEN
        MID$(Controls(i).Tag, 1, 2) = MKI$(0)
        MID$(Controls(iCtrl).Tag, 5, 2) = MKI$(MaxItems)
      END IF
      zIconListHandler sGroup$, i, -1
    END IF

  CASE zScrollBar
    Min = CVI(MID$(Tag$, 2, 2))
    Max = CVI(MID$(Tag$, 4, 2))
    IF Max - Min = 0 THEN Max = Max + 1
    IF Value > Max THEN Controls(iCtrl).Value = Max: Value = Max
    IF Value < Min THEN Controls(iCtrl).Value = Min: Value = Min
    IF refresh THEN
      'Knîpfe oben und unten:
      LINE (x + 1, y + 1)-(x2 - 1, y + 10), sbbcol, BF
      LINE (x + 1, y2 - 1)-(x2 - 1, y2 - 10), sbbcol, BF
    END IF
    IF w > 0 THEN
      'horizontal:
      'FÅllung innen:
      LINE (x + 12, y + 1)-(x2 - 12, y2 - 1), sbfcol, BF
      IF refresh THEN
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
      x = (x + ((Value - Min) * CLNG((x2 - x - 10)) / (Max - Min)))
      x2 = x + 10
      y = y + 1
      y2 = y2 - 1
    ELSE
      'vertikal:
      'FÅllung innen:
      LINE (x + 1, y + 12)-(x2 - 1, y2 - 12), sbfcol, BF
      IF refresh THEN
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
      y = (y + ((Value - Min) * CLNG((y2 - y - 10)) / (Max - Min)))
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
    IF refresh OR MouseDown THEN LINE (x, y)-(x2, y2), btfcol, BF
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
      IF refresh THEN
        'MaxItems auf 0 setzen damit wieder erneut gezÑhlt wird...
        MID$(Controls(CVI(MID$(Controls(iCtrl).Tag, 3, 2))).Tag, 4, 2) = MKI$(0)
        IF LEFT$(Tag$, 1) = "M" THEN
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

SUB zEditBoxHandler (sGroup$, sName$, iCtrl, x1, y1, x2, y2, iAnzSpalten, iAnzZeilen, Text AS STRING, refresh)

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
DIM CurrentLine      'Linie, in der sich der Cursor befindet
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
  iMaxZeichen = CVI(MID$(Controls(iCtrl).Tag, 1, 2))
  wishedTopper = CVI(MID$(Controls(iCtrl).Tag, 3, 2))

IF Mouse.clicked = 2 AND refresh = 0 THEN
  'KontextmenÅ:
  a$ = MenuInt(" Text einfÅgen , Text kopieren,-, Text lîschen")
  Mouse.clicked = 0
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
ELSEIF Mouse.clicked = 1 AND NOT refresh THEN
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
    InversFarbe = tbfcol
    Farbe = tbtcol

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
                    IF Mouse.clicked THEN
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
    IF refresh THEN GOTO zEditBoxTerminate
    'Cursor zeichnen:
    FOR y = CurrentY TO CurrentY + 13
      IF POINT(CurrentX, y) = tbfcol THEN
        PSET (CurrentX, y), tbtcol
      ELSE
        PSET (CurrentX, y), tbfcol
      END IF
    NEXT
    a$ = zGK(sGroup$)
    'Cursor lîschen:
    FOR y = CurrentY TO CurrentY + 13
      IF POINT(CurrentX, y) = tbfcol THEN
        PSET (CurrentX, y), tbtcol
      ELSE
        PSET (CurrentX, y), tbfcol
      END IF
    NEXT
    IF Mouse.clicked THEN GOTO zEditBoxTerminate
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
        merkerEnd = markerStart
        

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
  'todo ula
  'der if..then block ist nur eine Anpassung
  'fÅr visqb!
  IF RTRIM$(Controls(iCtrl).Group) <> "user" THEN
    PutString sName$, Text
  END IF
  Controls(iCtrl).Value = markerStart
  MID$(Controls(iCtrl).Tag, 3, 2) = MKI$(topperLine)
END SUB

SUB zEventClick (iCtrl, iButton)
Tag$ = RTRIM$(Controls(iCtrl).Tag)
SELECT CASE Controls(iCtrl).CtrlType
CASE zListBox
  IF Controls(iCtrl).h = 1 THEN
    keyb = "listopen"
  END IF
CASE zScrollBar
  ActiveCtrl = CVI(MID$(Tag$, 6, 2))
CASE zButton
  zDrawCtrl iCtrl, 0, 0
CASE zExitFocus
  keyb = "passfocus"
CASE zMouseClicker
  keyb = Tag$
  zDrawCtrl iCtrl, 0, 0
  IF iButton = 2 THEN zRspm iCtrl
END SELECT
RaiseMouseUp iCtrl, RTRIM$(Controls(iCtrl).Caption), iButton
END SUB

SUB zEventDblClick (iCtrl)

    SELECT CASE Controls(iCtrl).CtrlType
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
  Tag$ = RTRIM$(Controls(iCtrl).Tag)

  SELECT CASE Controls(iCtrl).CtrlType
  CASE zListBox
    IF MID$(Tag$, 1, 1) = "M" THEN
      IF iButton = 2 THEN
        keyb = "esc"
      ELSE
        Mouse.clicked = iButton
        zDrawCtrl iCtrl, 0, 0
        Mouse.clicked = 0
      END IF
    END IF
  CASE zScrollBar
    MID$(Controls(iCtrl).Tag, 9, 1) = "1"
    IF (w > 0 AND Mouse.x < x + 12) OR (w = 0 AND Mouse.y < y + 12) THEN
      Controls(iCtrl).Value = Value - CVI(MID$(Tag$, 10, 2))
      zDrawCtrl iCtrl, 0, 0
      iCtrlParent = CVI(MID$(Tag$, 6, 2))
      RaiseScroll iCtrl, RTRIM$(Controls(iCtrl).Caption)
    ELSEIF (w > 0 AND Mouse.x > x2 - 12) OR (w = 0 AND Mouse.y > y2 - 12) THEN
      Controls(iCtrl).Value = Value + CVI(MID$(Tag$, 10, 2))
      zDrawCtrl iCtrl, 0, 0
      iCtrlParent = CVI(MID$(Tag$, 6, 2))
      RaiseScroll iCtrl, RTRIM$(Controls(iCtrl).Caption)
    ELSE
      MID$(Controls(iCtrl).Tag, 9, 1) = "0"
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
    Tag$ = Controls(iCtrl).Tag
    Typ = Controls(iCtrl).CtrlType

    SELECT CASE Typ
    CASE zListBox
        IF MID$(Tag$, 1, 1) = "M" AND iButton THEN
            Mouse.clicked = -1
            zDrawCtrl iCtrl, 0, 0
            Mouse.clicked = 0
        END IF

    CASE zScrollBar
        IF zMouseDown = iCtrl AND iButton = 1 THEN
            Min = CVI(MID$(Tag$, 2, 2))
            stp = CVI(MID$(Tag$, 10, 2))
            Max = INT(((CVI(MID$(Tag$, 4, 2)) - Min + 1) / stp) * -1) * -1 + 1
            'Keine Bewegungen akzeptieren, wenn zuvor auf einen der Pfeile
            'gedrÅckt wurde:
            IF MID$(Controls(iCtrl).Tag, 9, 1) = "1" THEN EXIT SUB
            IF w > 0 THEN
                'horizontal:
                x = x + 12: x2 = x2 - 12
                nv = (Mouse.x - x)
                IF nv > x2 - x THEN nv = x2 - x
                IF nv > 0 THEN
                    nv = Min + (nv * CLNG((Max - Min)) / (x2 - x))
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
                    nv = Min + (nv * CLNG((Max - Min)) / (y2 - y))
                    nv = (nv * stp) - stp + (Value MOD stp)
                ELSE
                    nv = Min
                END IF
            END IF
            Max = CVI(MID$(Tag$, 4, 2))
            IF nv > Max THEN nv = Max
            Controls(iCtrl).Value = nv
            iCtrlParent = CVI(MID$(Tag$, 6, 2))
            IF Value <> nv THEN
                RaiseScroll iCtrl, RTRIM$(Controls(iCtrl).Caption)
                IF iCtrlParent > 0 THEN
                    ActiveCtrl = iCtrlParent
                    keyb = "passfocus"
                    Controls(iCtrlParent).Value = nv
                    RaiseScroll iCtrlParent, RTRIM$(Controls(iCtrlParent).Caption)
                END IF
            END IF
        END IF

    CASE zEditBox
        Mouse.Cursor = 2

    END SELECT

    RaiseMouseMove iCtrl, RTRIM$(Controls(iCtrl).Caption), iButton

END SUB

SUB zGetMouse (B, x, y)
IF MouseOn THEN
  DEF SEG = VARSEG(iMS(0))
  POKE VARPTR(iMS(0)) + 26, &H33
  CALL ABSOLUTE(3, B, x, y, VARPTR(iMS(0)))
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
Mouse.clicked = 0
Mouse.Cursor = -Mouse.Cursor
Mouse.time = TIMER
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
    CASE 16 'Ctrl+P: Bildschirminhalt speichern
      SavePicture 0, 0, 639, 349, tempdir + "zwa.abm"
    CASE 32: IF NOT Controls(ActiveCtrl).SelfHandler THEN a$ = "enter"
    CASE 27: a$ = "esc"
    CASE 13: a$ = "enter"
    CASE 9: a$ = "tab"
    CASE 8: a$ = "backspace"
    CASE 10: Mouse.clicked = 2
      IF MouseOn = 0 THEN
        Mouse.x = CurrentX + 10
        Mouse.y = CurrentY + 10
      END IF
    END SELECT
  END IF
LOOP WHILE LEN(a$) = 0

IF LEN(a$) = 2 THEN
    SELECT CASE ASC(MID$(a$, 2, 1))
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

IF MouseOn THEN PUT (Mouse.oldx, Mouse.oldy), zMHG, PSET
ERASE zMHG

SELECT CASE a$
CASE "@3" 'Ctrl+C: Controls auflisten
  SaveScreen
  FOR i% = 1 TO UBOUND(Controls)
    ListAdd "info", i%, STR$(i%) + "  " + Controls(i%).Group + "  " + Controls(i%).Caption + STR$(Controls(i%).CtrlType), 0
  NEXT
  MouseClicker "zinfo", "esc", 0, 0, 639, 349, "esc", 0
  ListBox "zinfo", "info", 7, 23, 613, 320, 0
  DrawControls "zinfo"
  GetInp "zinfo"
  ClearControls "zinfo"
  RestoreScreen
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
          ActiveCtrl = i
          EXIT FOR
        END IF
    NEXT
CASE "tab"
    'nÑchstes Ctrl aktivieren:
    FOR i = ActiveCtrl + 1 TO UBOUND(Controls)
        IF (sGroup$ = "" OR RTRIM$(Controls(i).Group) = sGroup$) AND Controls(i).CtrlType <= zControlTypeCount THEN
          ActiveCtrl = i
          GOTO zGKExit
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

SUB zIconListHandler (sGroup$, iCtrl, refresh)
DIM Hintergrund(505)

l = Controls(iCtrl).x
t = Controls(iCtrl).y
r = Controls(iCtrl).x2
d = Controls(iCtrl).y2

sName$ = Controls(iCtrl).Caption

'aktiviertes Symbol:
sv = Controls(iCtrl).Value

'altes Sichtfenster:
otopper = CVI(MID$(Controls(iCtrl).Tag, 1, 2))
MaxItems = CVI(MID$(Controls(iCtrl).Tag, 5, 2))

topperIcon = 1
bottomIcon = 0
Nr = 0
col = POINT(l, t)

IF Mouse.clicked = 2 THEN
  ppm = 1
  IF keyb = "10" THEN GOTO zIcListPopUp
END IF

DO
IF Value > MaxItems AND MaxItems > 0 THEN Value = MaxItems
IF Value < 1 THEN Value = 1
cntMode = 0
IF Value < topperIcon THEN
    zRessourceGet sName$, topperIcon, "", wert
    IF wert = 0 THEN NoRefresh = -1
    topperIcon = wert
    bottomIcon = wert - 1
END IF

IF Value > bottomIcon THEN
    Nr = bottomIcon + 1
    topperIcon = Nr
    IF sv = 0 THEN LINE (l, t)-(r, d), col, BF
ELSE
    Nr = topperIcon
END IF
refresh = 0
mc = 0
zIcListAct:
x = l
y = t + 45

DO
IF NoRefresh THEN NoRefresh = 0: EXIT DO
IF Nr > MaxItems AND MaxItems > 0 THEN bottomIcon = MaxItems: EXIT DO
zRessourceGet sName$, Nr, a$, 0
zDoMouse
IF a$ = "" THEN
    IF cntMode OR MaxItems = 0 THEN MaxItems = Nr - 1: EXIT DO
END IF
name$ = RTRIM$(LEFT$(a$, 12))
Kennung$ = MID$(a$, 13)
IF x + 41 > r OR x + LEN(Kennung$) * 8 > r THEN x = l: y = y + 65
IF LEN(Kennung$) < 6 THEN x = x + (6 - LEN(Kennung$)) * 4
sx = x + INT((LEN(Kennung$) * 8 - 41) / 2)
IF sv = 0 THEN
    VIEW
    IF Value = Nr AND iCtrl = ActiveCtrl THEN
        LINE (sx - 2, y - 44)-(sx + 42, y + 1), focol, B, PunkteLinie
        lx = sx - 2
        ly = y - 44
    ELSEIF y < d THEN
        LINE (sx - 2, y - 44)-(sx + 42, y + 1), col, B
    END IF
END IF
IF y + 14 > d THEN
    IF NOT cntMode THEN
        zRessourceSet sName$, Nr, "", topperIcon
        bottomIcon = Nr - 1
        cntMode = -1
    END IF
    IF MaxItems > 0 THEN EXIT DO
ELSE
IF mc THEN
  IF Mouse.clicked AND iCtrl = ActiveCtrl THEN
    IF Mouse.x >= x AND Mouse.x <= x + LEN(Kennung$) * 8 THEN
        IF Mouse.y > y - 43 AND Mouse.y <= y + 16 THEN
            sv = Nr
            Mouse.clicked = 0
            mc = 0
            Value = Nr
            GOTO zIcListAct
        END IF
    END IF
  END IF
END IF
  IF (Value < topperIcon OR Value > bottomIcon AND sv = 0) OR refresh THEN
    Farbe = tcol
    InversFarbe = col
    EchoXY x, y + 2, Kennung$
    InversFarbe = 0
    IF col > 0 THEN c = col ELSE c = icfcol
    VIEW (sx, y - 42)-(sx + 40, y - 1), c, c
    IF oldicon$ <> name$ THEN
    IF RIGHT$(name$, 3) = "ico" THEN
        VIEW (sx + 3, y - 38)-(sx + 40, y - 1), c, c
    ELSE
        VIEW (sx, y - 42)-(sx + 40, y - 1), c, c
    END IF
    oldicon$ = name$
    ShowPicture 0, 0, laosdir + name$

    i = 0
        GET (sx, y - 42)-(sx + 40, y - 1), Hintergrund
    ELSE
        PUT (0, 0), Hintergrund, PSET
    END IF
    VIEW
  END IF
END IF
x = x + LEN(Kennung$) * 8 + 16
Nr = Nr + 1
LOOP
IF sv <> 0 THEN
    IF sv <> Value AND Value < MaxItems THEN
        Value = Value + 1
    ELSE
        sv = 0
        Nr = topperIcon
        IF otopper <> topperIcon THEN
            refresh = -1
            LINE (l, t)-(r, d), col, BF
        END IF
        mc = -1
        GOTO zIcListAct
    END IF
END IF
IF sv = 0 THEN

'Scrollbar zeichnen:
sb = CVI(MID$(Controls(iCtrl).Tag, 3, 2))
MID$(Controls(sb).Tag, 4, 2) = MKI$(MaxItems)
Controls(sb).Value = Value
zDrawCtrl sb, 0, 0
'Die Scrollbar hat die Schrittweite der aktuellen Seite:
MID$(Controls(sb).Tag, 10, 2) = MKI$(bottomIcon - topperIcon + 1)

Controls(iCtrl).Value = Value
MID$(Controls(iCtrl).Tag, 1, 2) = MKI$(topperIcon)

zIcListPopUp:
IF ppm AND ActiveCtrl = iCtrl THEN zRspm iCtrl: ppm = 0: Mouse.ClickX = 0: EXIT SUB

MID$(Controls(iCtrl).Tag, 5, 2) = MKI$(MaxItems)

IF refresh THEN EXIT SUB

a$ = zGK(sGroup$)

IF Value <> Controls(iCtrl).Value THEN
    Value = Controls(iCtrl).Value
    topperIcon = CVI(MID$(Controls(iCtrl).Tag, 1, 2))
    bottomIcon = MaxItems
END IF

IF Mouse.clicked THEN EXIT SUB

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
    FOR i = Value TO MaxItems
        zRessourceGet sName$, i, a$, 0
        IF LCASE$(MID$(a$, 13, LEN(kb$))) = kb$ THEN found = i: EXIT FOR
    NEXT
    IF found = 0 THEN
        FOR i = 1 TO Value - 1
            zRessourceGet sName$, i, a$, 0
            IF LCASE$(MID$(a$, 13, LEN(kb$))) = kb$ THEN found = i: EXIT FOR
        NEXT
    END IF
    sv = found
END SELECT
IF sv > 0 THEN otopper = topperIcon
IF sv > 0 AND sv < Value THEN
    topperIcon = 1
    bottomIcon = 0
    Value = 1
    Nr = 0
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
  CALL ABSOLUTE(3, B, x, y, VARPTR(iMS(0)))
  IF (Mouse.oldx <> x OR Mouse.oldy <> y AND zMHG(0) > 0) OR Mouse.Cursor < 0 THEN
    IF Mouse.Cursor < 0 THEN Mouse.Cursor = Mouse.Cursor * -1
    PUT (Mouse.oldx, Mouse.oldy), zMHG, PSET
    zDrawMouse x, y
  END IF
  Mouse.x = x
  Mouse.y = y
  IF x <> Mouse.oldx AND y <> Mouse.oldy THEN Mouse.time = TIMER
  IF MouseOn = 2 THEN PUT (x, y), zMHG, PSET: MouseOn = -1: i = ActiveCtrl: GOTO zLEClick
  'Gehe alle Controls durch, und schaue auf Events:
  FOR i = UBOUND(Controls) TO 1 STEP -1
    IF Controls(i).CtrlType > 0 THEN
      IF RTRIM$(Controls(i).Group) = sGroup$ OR sGroup$ = "" THEN
        IF x >= Controls(i).x AND x <= Controls(i).x2 THEN
          IF y >= Controls(i).y AND y <= Controls(i).y2 THEN
            'Maus ist Åber einem Control positioniert!
            iOverControl = -1
            IF zMouseDown = i AND B = 0 THEN
              'Maustaste wurde auf dem Objekt losgelassen!
              ' - Click-Ereignis!
              PUT (x, y), zMHG, PSET
              zMouseDown = 0
              ActiveCtrl = i
              Focus = RTRIM$(Controls(i).Caption)
              IF Controls(i).CtrlType > zControlTypeCount THEN
                  keyb = "nofocus"
              END IF
              Mouse.clicked = Mouse.Down
              Mouse.Down = 0
              zEventClick i, Mouse.clicked
              'PrÅfe auf Doppelklick:
              IF Mouse.time > 0 AND Mouse.clicked = 1 THEN
                IF TIMER - Mouse.time < .8 THEN
                  IF x > Mouse.ClickX - 2 AND x < Mouse.ClickX + 2 THEN
                    IF y > Mouse.ClickY - 2 AND y < Mouse.ClickY + 2 THEN
zLEClick:
                      IF x >= Controls(ActiveCtrl).x AND x <= Controls(ActiveCtrl).x2 THEN
                        IF y >= Controls(ActiveCtrl).y AND y <= Controls(ActiveCtrl).y2 THEN
                          Mouse.clicked = -1
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
              IF Mouse.clicked THEN zLauscheEvents% = -1
              Mouse.ClickX = x
              Mouse.ClickY = y
              GOTO zLEventsTerminate
            ELSEIF B > 0 THEN
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
                  Mouse.Down = B
                  IF NOT Controls(i).SelfHandler THEN zDrawCtrl i, -1, 0
                  IF NOT Controls(iold).SelfHandler AND iold <> i THEN zDrawCtrl iold, 0, 0
                  zEventMouseDown i, B
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
            ELSEIF B = 0 THEN
                Mouse.clicked = 0
                'Taste ist nicht gedrÅckt!
                IF Mouse.x <> Mouse.oldx OR Mouse.y <> Mouse.oldy THEN
                  PUT (x, y), zMHG, PSET
                  zEventMouseMove i, B
                  x = Mouse.x
                  y = Mouse.y
                  zDrawMouse x, y
                ELSE
                  IF TIMER - Mouse.time > 1 THEN
                    RaiseToolTipText i, RTRIM$(Controls(i).Caption)
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
  IF B = 0 THEN
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
    IF (x <> Mouse.oldx OR y <> Mouse.oldy) AND zMouseDown > 0 THEN
      PUT (x, y), zMHG, PSET
      zEventMouseMove zMouseDown, B
      zDrawMouse x, y
      IF keyb <> "" THEN zLauscheEvents% = -1
    END IF
  END IF
  Mouse.oldx = x
  Mouse.oldy = y
END FUNCTION

SUB zListboxHandler (sGroup$, iCtrl, x, y, x2, y2, iAnzSpalten, iAnzZeilen, RefreshOnly)

DIM sName$, Value

'Der markierte Eintrag:
Value = Controls(iCtrl).Value

'Der Controlname:
sName$ = Controls(iCtrl).Caption

'Checked:
IF MID$(Controls(iCtrl).Tag, 2, 1) = "C" THEN iCheckBoxes = -1

'MenuStyle:
MenuStyle = MID$(Controls(iCtrl).Tag, 1, 1) = "M"

sb = CVI(MID$(Controls(iCtrl).Tag, 3, 2))
MaxItems = CVI(MID$(Controls(sb).Tag, 4, 2))

zListBoxReDraw:

IF Value <= 0 THEN Value = 1
IF Value > MaxItems AND MaxItems > 0 THEN Value = MaxItems

iTopvisible = (FIX((Value - 1) / iAnzZeilen) * iAnzZeilen) + 1
iDownVisible = iTopvisible + iAnzZeilen - 1

IF iCtrl = ActiveCtrl AND Mouse.clicked THEN vstop = -1 ELSE vstop = 0

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
            IF iCtrl = ActiveCtrl AND Mouse.clicked THEN
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
MID$(Controls(sb).Tag, 4, 2) = MKI$(MaxItems)
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
IF Mouse.clicked = -1 AND iCheckBoxes THEN a$ = "insert"

Value = Controls(iCtrl).Value

IF Mouse.clicked = 2 AND ActiveCtrl = iCtrl THEN zRspm iCtrl

IF Mouse.clicked > 0 THEN
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

SUB zRGBPalette (slot%, r%, g%, B%)
SELECT CASE slot%
CASE IS <= 5: s% = slot%
CASE 6: s% = 20
CASE 7: s% = 7
CASE IS >= 8: s% = 48 + slot%
END SELECT
OUT &H3C8, s%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, B%
END SUB

SUB zRspm (iCtrl)
  RaisePopupMenu iCtrl, RTRIM$(Controls(iCtrl).Caption), q
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
zGetMouse B, x, y
IF B <> 0 THEN EXIT SUB
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
  zGetMouse B, nx, ny
  LOOP UNTIL INKEY$ <> "" OR (nx <> Mouse.x) OR B <> 0 OR (ny <> Mouse.y)
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
    cap$ = Controls(i).Caption
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

    Mouse.oldx = Mouse.x
    Mouse.oldy = Mouse.y
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
        READ r, g, B
        zRGBPalette i, r, g, B
    NEXT
END SUB

SUB zSwapFocus (iAlt)
  iNeu = ActiveCtrl
  IF iAlt <> iNeu THEN
    RaiseGotFocus iAlt, iNeu, RTRIM$(Controls(iNeu).Caption)
    IF NOT Controls(iAlt).SelfHandler THEN zDrawCtrl iAlt, 0, 0
    IF NOT Controls(iNeu).SelfHandler THEN zDrawCtrl iNeu, 0, 0
  END IF
  iAlt = iNeu
END SUB

SUB zTimeEventHandler (Nr)
of = Farbe
om = InversFarbe
SELECT CASE Nr
CASE 0  'Bildschirmschoner
  zScreenSaver
CASE 1  'Zeit aktualisieren
  IF SilentScreenMode AND NOT SaveScreenMode THEN EXIT SUB
  IF zDebugMode THEN
    LINE (2, 332)-(637, 347), btscol, BF
    LINE (2, 332)-(637, 332), Farbe
    Farbe = bttcol
    InversFarbe = btscol
    EchoXY 3, 333, "Maximale Datenfeldgrîsse:" + STR$(FRE(-1)) + " Stack:" + STR$(FRE(-2)) + " Zeichenfolgen:" + STR$(FRE(0))
  END IF
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
    zGetMouse B, nx, ny
    k = INP(&H60)
    zDoEvents
  LOOP UNTIL nx <> ax OR B <> 0 OR ny <> ay OR k < 128
  PUT (x, y), bg, PSET
  Mouse.time = TIMER
  Farbe = tcol
END SUB

