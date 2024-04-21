DECLARE SUB SavePicture (x%, y%, x2%, y2%, file$)
DECLARE SUB ScreenShot ()
DECLARE SUB RunFile (prgStarter$)
DECLARE FUNCTION ChooseIcon$ (MarkedIcon$)
DECLARE SUB formatc ()
DECLARE FUNCTION IsNull% (text$)
DECLARE SUB subDateiEigenschaften (id%)
DECLARE SUB subNeuerOrdner ()
DECLARE SUB subDateiLoeschen (id%)
'Lege hier die maximale Anzahl Controls und Strings (Editboxen) fest:
CONST MaxControls = 30, MaxStrings = 3

'Snowflake GUI v.0.1.9
'Entwickelt fÅr LaOS XT
'Dies ist OpenSource
'Programmierer, die an diesem Code mitgearbeitet haben, schreiben sich bitte
'hier ein, wenn mîglich mit kurzer Beschreibung der Arbeit:
'------------------------------------------------------------
'Urs Langmeier (Snowflake Standard-GUI)

DECLARE SUB AddIcon (sName$, Index%, IconFile$, Caption$, Value%)
DECLARE SUB RaiseToolTipText (iCtrl%, name$)
DECLARE FUNCTION zQuitKey% ()
DECLARE SUB RaisePopUpMenu (iCtrl%, name$, ExitInput%)
DECLARE SUB zRspm (iCtrl%)
DECLARE SUB Frame (sGroup$, Caption$, x1%, y1%, x2%, y2%)
DECLARE SUB zDoEvents ()
DECLARE SUB CheckBox (sGroup$, Caption$, x%, y%, Value%)
DECLARE SUB FullScreen ()
DECLARE SUB ClearList (sName$)
DECLARE FUNCTION SaveDialog$ (prgStarter$)
DECLARE SUB ExitBox (sGroup$, x%, y%)
DECLARE SUB EditBox (sGroup$, sName$, x%, y%, iAnzahlSpalten%, iAnzahlZeilen%, iMaxZeichen%)
DECLARE SUB IconList (sGroup$, sName$, Caption$, x%, y%, x2%, y2%, iScrollBars%, iExitbox%)
DECLARE SUB SaveScreen ()
DECLARE SUB CLW (sGroup$)
DECLARE SUB StatusBar (text$)
DECLARE SUB ProgressBar (text$, Stat AS SINGLE)
DECLARE FUNCTION exists% (FilePath$)
DECLARE SUB TimeLock (Modus%)
DECLARE SUB RadioButton (sGroup$, Caption$, x%, y%, Value%)
DECLARE SUB zDoMouse ()
DECLARE SUB ScrollBar (sGroup$, sName$, x%, y%, w%, h%, Min%, Max%, stp%, iCtrl%)
DECLARE SUB zEventDblClick (iCtrl%)
DECLARE SUB ExitFocus (sGroup$, sName$, x%, y%, x2%, y2%)
DECLARE SUB ListAdd (sName$, Index%, text AS STRING, Value%)
DECLARE SUB zGetMouse (b%, x%, y%)
DECLARE FUNCTION SysDate$ (d$)
DECLARE SUB Label (x%, y%, x2%, y2%, text AS STRING, Alignment%, bgColor%)
DECLARE SUB zToolTipText (text$)
DECLARE SUB zEventMouseRest (iCtrl%)
DECLARE SUB zScreenSaver ()
DECLARE SUB zDrawThisControl (iCtrl%, MouseDown%, iRefresh%)
DECLARE SUB RaiseScroll (iCtrl%, name$)
DECLARE SUB zReplace (text$, find$, replce$)
DECLARE SUB zSetMouseCursor (sCursor$)
DECLARE SUB PutValue (objekt AS STRING, wert%)
DECLARE SUB SliderBar (sGroup$, sName$, x%, y%, w%, h%, Min%, Max%)
DECLARE SUB zEditBoxHandler (sGroup$, sName$, iCtrl%, x1%, y1%, x2%, y2%, iAnzSpalten%, iAnzZeilen%, text AS STRING, iRefresh%)
DECLARE SUB zIconListHandler (sGroup$, iCtrl%, iRefresh%)
DECLARE SUB zRessourceSet (sGroup$, Index%, text$, Value%)
DECLARE SUB ListBox (sGroup$, sName$, x%, y%, x2%, y2%, WithCheckBoxes%)
DECLARE SUB zRessourceGet (sGroup$, Index%, text$, Value%)
DECLARE SUB zCreateControl (iCtrlType%, sGroup$, sName$, Caption$, Value%, x%, y%, x2%, y2%, w%, h%, tag$, iSelfHandler%)
DECLARE SUB zSetOrigPalette ()
DECLARE FUNCTION MenuInt$ (items AS STRING, Trennzeichen$)
DECLARE FUNCTION TextLen% (text AS STRING)
DECLARE SUB GetValue (objekt AS STRING, wert%)
DECLARE SUB MouseClicker (sGroup$, sName$, x%, y%, x2%, y2%, sSimulateKey AS STRING, iWithShadow%)
DECLARE FUNCTION TextHeight% (text AS STRING)
DECLARE SUB zListboxHandler (sGroup$, iCtrl%, x%, y%, x2%, y2%, iAnzSpalten%, iAnzZeilen%, iRefreshOnly%)
DECLARE FUNCTION TextWidth% (text AS STRING)
DECLARE SUB zRGBPalette (slot%, r%, g%, b%)
DECLARE SUB zShowPicture (xx%, yy%, sFileName AS STRING)
DECLARE SUB RestoreScreen ()
DECLARE SUB zEventMouseMove (iCtrl%, iButton%)
DECLARE SUB SilentScreen ()
DECLARE SUB Main ()
DECLARE SUB zEventClick (iCtrl%, iButton%)
DECLARE SUB zEventMouseDown (iCtrl%, iButton%)
DECLARE FUNCTION zLauscheEvents% (sGroup$)
DECLARE FUNCTION zSelfHandleControl% (sGroup$, iCtrlType%)
DECLARE FUNCTION GetKey$ (sGroup$)
DECLARE SUB PutString (objekt AS STRING, text AS STRING)
DECLARE SUB GetString (objekt AS STRING, text AS STRING)
DECLARE SUB zTimeEventHandler (nr%)
DECLARE SUB zMoveScreen (x1%, y1%, x2%, y2%, ToX%, ToY%)
DECLARE SUB ClearControls (sGroup$)
DECLARE SUB zGetOriginalValues (iRestore%)
DECLARE SUB zDrawBox (x1%, y1%, x2%, y2%)
DECLARE SUB DrawControls (sGroup$)
DECLARE SUB Button (sGroup$, Caption$, x%, y%)
DECLARE SUB zHandleControls (sGroup$, iChanges%)
DECLARE SUB zWriteTextXY (x%, y%, text AS STRING)
DECLARE SUB zNextCtrl (sGroup$)
DECLARE SUB zLastControl (sGroup$)
DECLARE SUB Echo (text AS STRING)
DECLARE SUB Menu (sGroup$, Caption$)
DECLARE SUB GetInp (sGroup$)
DECLARE SUB Dialog (text AS STRING)
DECLARE SUB zGUIConfiguration ()

'Initialisierungen:
'$STATIC
DEFINT A-Z
CLEAR , , 5000
CLOSE
ON ERROR GOTO errHandler
SCREEN 9, , 0, 0
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
    tag         AS STRING * 12  'Unterschiedliche Verwendung
    CtrlType    AS INTEGER      'Der Control-Typ (zwingend!)
    SelfHandler AS INTEGER      'True = Das Control handelt sich selbst! (aufwÑndige Controls)
END TYPE

TYPE zRessourceType
    Value       AS INTEGER
    text        AS STRING * 80  'Text
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
CONST zSliderBar = 99

'Variablen und Datenfelder fÅr die LaOS-Technlologien:
DIM SHARED laosdir            AS STRING   'Das LaOS-Heimatverzeichnis
DIM SHARED tempdir            AS STRING   'Das Verzeichnis fÅr temporÑre Dateien

'Variablen und Datenfelder fÅr die LaOS-GUI:
DIM SHARED Controls(1 TO MaxControls) AS zControlType, zStrings(1 TO MaxStrings) AS STRING, zTimeEvent(5) AS zTiEvType, zMCS AS STRING, zMHG(104), iMS(45), Mouse AS zMouseType, SilentScreenMode, SaveScreenMode, zSSMStop, Left, Top, Right, Down,  _
TextAlignment, zMouseDown

DIM SHARED NextCtrl
  'Das jeweils nÑchste Control
DIM SHARED keyb AS STRING
  'Speichert die zuletzt gedrÅckte Taste
DIM SHARED Focus AS STRING
  'Das Control mit dem Fokus
DIM SHARED crlf AS STRING
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
TextAlignment = AlignLeft
NextCtrl = 1
Mouse.x = 319
Mouse.y = 174
zSetMouseCursor "mouse"
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
    prgStarter AS STRING * 40
    icon AS STRING * 12
END TYPE

'GUI Configuration
'********************
'Hier kannst du Einstellungen vornehmen, welche das Verhalten der  LaOS-GUI
'beeinflussen:

zDebugMode = False
'True = Es wird der Speicherplatz in der Statusbar angezeigt

zShowTimeExitBox = True
'True = Zeigt die Uhr und Exitbox im Hauptfenster an.

Programm:
'********************
'Schreibe hier dein Programm!
'Am besten schreibst du dein Programm in der Sub ->Main.
'Wenn du globale Variablen brauchst kannst du sie hier deklarieren.

DIM SHARED ScrolledPage
DIM SHARED CurCol
DIM SHARED path AS STRING
DIM SHARED file$
DIM SHARED copy$
DIM SHARED copyfile$
DIM SHARED ctlfiles, ctlfolders
DIM SHARED markfile AS STRING
DIM SHARED filenr, bRefresh
DIM SHARED CurrentFile AS zFileType
DIM SHARED zFiles
DIM SHARED CurrentFolder AS LONG
DIM SHARED markfolder AS LONG

Main

'Beenden:
KILL laosdir + "folder.ini"
CLEAR
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
'value = Der Wert (True oder False)
SUB AddIcon (sName$, Index, IconFile$, Caption$, Value)
    DIM sFile AS STRING * 12

    IF 12 < LEN(IconFile$) THEN
        Dialog "Fehler in AddIcon: Falsche Angabe des File (" + sFile + ")"
        EXIT SUB
    END IF

    sFile = IconFile$

    zRessourceSet sName$, Index, sFile + Caption$, Value

END SUB

'Erstellt ein Button-Steuerelement
'sGroup$ = Die Steuerelementgruppe
'Caption$ = Die Caption
'x, y = Die Koordinaten
SUB Button (sGroup$, Caption$, x, y)
    IF x = 0 THEN
        x = Controls(NextCtrl - 1).x2 + 5
    END IF
    IF y = 0 THEN
        y = Controls(NextCtrl - 1).y
    END IF
    zCreateControl zButton, sGroup$, "", Caption$, 0, x, y, x + TextLen(Caption$) + 8, y + 21, 0, 0, "", False
END SUB

'Erstellt ein Checkbox-Steuerelement
'sGroup$ = Die Steuerelementegruppe
'Caption$ = Die Caption
'x, y = Die Koordinaten
'value = Der Wert (True oder False)
SUB CheckBox (sGroup$, Caption$, x, y, Value)
    zCreateControl zCheckBox, sGroup$, "", Caption$, Value, x, y, x + TextLen(Caption$) + 32, y + 18, 0, 0, "", False
END SUB

'Zeig ein Dialog an, worin der Benutzer ein Symbol auswÑhlen kann.
'
'MarkedIcon$ = Das Symbol, das zu Beginn markiert sein soll.
'
'Gibt einen Leerstring ("") zurÅck, wenn Abgebrochen wurde, die Symboldatei,
'falls das Symbol ausgewÑhlt wurde. (Enter oder Doppelklick auf ein Symbol)
FUNCTION ChooseIcon$ (MarkedIcon$)

  IconList "zchgicon", "zicons", "Symbol auswÑhlen", 150, 120, 490, 280, True, True

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
    Mouse.clicked = False
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
        FOR i% = 1 TO UBOUND(Controls)
            IF Controls(i%).CtrlType = 0 THEN EXIT FOR
            IF RTRIM$(Controls(i%).Group) = sGroup$ THEN
                Controls(i%).CtrlType = 0
                Controls(i%).SelfHandler = 0
                ideleted = ideleted + 1

                'Strings des Objektes lîschen:
                FOR s% = 1 TO UBOUND(zStrings)
                    IF RTRIM$(LEFT$(zStrings(s%), 8)) = RTRIM$(RTRIM$(Controls(i%).Caption)) THEN
                        zStrings(s%) = ""
                    END IF
                NEXT

                'Ressourcen des Objektes lîschen: (falls vorhanden...)
                KILL tempdir + RTRIM$(Controls(i%).Caption) + ".rs~"
                IF RTRIM$(zoldsgroup) = RTRIM$(Controls(i%).Caption) THEN zoldsgroup = ""
            END IF
        NEXT
        NextCtrl = NextCtrl - ideleted
        'von hinten beginnend alle leeren EintrÑge rausnehmen:
        FOR i% = 1 TO UBOUND(Controls)
          IF Controls(i%).CtrlType = 0 THEN
            iChanges = 0
            FOR z% = i% + 1 TO UBOUND(Controls)
              IF Controls(z%).CtrlType > 0 THEN iChanges = -1
              SWAP Controls(z%), Controls(z% - 1)
            NEXT
            IF NOT iChanges THEN EXIT SUB
            i% = i% - 1
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
'Setzen Sie die Variable zShowTimeExitBox auf False (0), um die Zeit und
'ExitBox nicht anzuzeigen.
'sGroup$ = Die Steuerelementgruppe, zu dem die ExitBox gehîrt.
SUB CLW (sGroup$)
    SilentScreen
    CLS
    LINE (0, 0)-(639, 349), wincol, B
    LINE (1, 1)-(638, 348), frmcol, B
    COLOR tcol, bcol
    Left = 2
    Top = 2
    Right = 637
    Down = 347
    CurrentX = 2
    CurrentY = 2
    IF zShowTimeExitBox THEN
        ExitBox sGroup$, 619, 4
        Farbe = tcol
        zWriteTextXY 575, 4, LEFT$(TIME$, 5)
        LINE (573, 4)-STEP(43, 13), btrcol, B
    END IF
    DrawControls sGroup$
    RestoreScreen
END SUB

'Gibt eine Meldung auf dem Bildschirm aus, den der Benutzer mit OK bestÑtigen
'muss.
'text = Der auszugebende Text.
SUB Dialog (text AS STRING)
  z = ActiveCtrl
  zGetOriginalValues False
  TextAlignment = AlignCenter
  Left = 7
  Right = 632
  h = TextHeight(text) + 14 + 30
  w = TextWidth(text) + 16
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
  asm = SilentScreenMode
  asa = SaveScreenMode
  ast = zSSMStop
  WHILE SilentScreenMode OR SaveScreenMode
    RestoreScreen
  WEND
  SaveScreen
  VIEW
  Frame "", "", x1, y1, x2 - 1, y2 - 1
  Echo text
  x1 = x1 + ((x2 - x1) - TextLen("    OK     ") - 8) / 2
  Button "MsgBox", "    OK     ", x1, y2 - 30
  DrawControls "MsgBox"
  GetInp "MsgBox"
  x1 = l
  y1 = t - 2
  x2 = l + w
  y2 = t + h
  zGetOriginalValues True
  ClearControls "MsgBox"
  RestoreScreen
  zSSMStop = ast
  SilentScreenMode = asm
  SaveScreenMode = asa
  ActiveCtrl = z
END SUB

'Zeichnet die Controls
'sGroup$ = Die Steuerelementgruppe ("" = alle)
SUB DrawControls (sGroup$)
    FOR i% = 1 TO UBOUND(Controls)
        IF RTRIM$(Controls(i%).Group) = sGroup$ OR sGroup$ = "" THEN
            zDrawThisControl i%, False, True
        END IF
    NEXT
END SUB

'Gibt ein Text auf dem Bildschirm aus. BerÅcksichtigt dabei die Bereichs-
'begrenzungen Left, Top, Right und Down, die zuvor durch einen Rahmen (Frame)
'gesetzt wurden.
SUB Echo (text AS STRING)
text = RTRIM$(text)

DIM linie AS STRING, txt AS STRING
x = CurrentX
y = CurrentY
txt = text
panewidth = Right - Left
leftpane = x

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
  
    SELECT CASE TextAlignment
        CASE AlignRight
            x = Right - TextLen(linie)
        CASE AlignCenter
            x = Left + ((panewidth - TextLen(linie)) / 2)
    END SELECT

    IF y + 14 <= Down THEN
        zWriteTextXY x, y, linie
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

'Erstellt ein Editbox-Steuerelement
'sGroup$ = die Steuerelementgruppe
'sName$ = Der Name der Editbox
'x,y = Die Koordinaten
'iAnzahlSpalten = Anzahl Spalten
'iAnzahlZeilen = Anzahl Zeilen
'iMaxZeichen = Maximale Anzahl einzugebender Zeichen (0=hîchstmîgliches)
SUB EditBox (sGroup$, sName$, x, y, iAnzahlSpalten, iAnzahlZeilen, iMaxZeichen)
    zCreateControl zEditBox, sGroup$, sName$, "", 1, x, y, x + (iAnzahlSpalten * 8) + 5, y + (iAnzahlZeilen * 14) + 2, iAnzahlSpalten, iAnzahlZeilen, MKI$(iMaxZeichen), True
END SUB

'PrÅft, ob eine MSDOS-Datei existiert.
'Gibt TRUE zurÅck, falls die Datei existiert, andernfalls FALSE.
FUNCTION exists% (FilePath$)
  f = FREEFILE
  e = errorlevel
  errorlevel = 0
  exists% = True
  OPEN FilePath$ FOR INPUT AS f
  CLOSE f
  IF errorlevel THEN exists% = False
  errorlevel = e
END FUNCTION

'Erstellt eine Exit-Box
'sGroup$ = Die Steuerelement-Gruppe
'x,y = Die Position
SUB ExitBox (sGroup$, x, y)
    IF x = 0 THEN x = Right - 17
    IF y = 0 THEN y = Top + 2
    i = NextCtrl
    MouseClicker sGroup$, "", x, y, x + 15, y + 13, "esc", False
    zDrawThisControl i, 0, -1
END SUB

'Erstellt ein Exit-Fokus Objekt.
'Sobald dieses Objekt den Fokus erhÑlt oder auf dieses Objekt geklickt wurde,
'wird GetInp beendet und die Steuerung dem Programmablauf Åbergeben...
'Das Objekt ist unsichtbar!
SUB ExitFocus (sGroup$, sName$, x, y, x2, y2)
    IF x = 0 AND y = 0 AND x2 = 0 AND y2 = 0 THEN
        x2 = 639
        y2 = 349
    END IF
    zCreateControl zExitFocus, sGroup$, sName$, "", 0, x, y, x2, y2, 0, 0, "", False
END SUB

SUB formatc
'LaOS-System Formatieren:
f = FREEFILE
DIM zfile AS zFileType
CLOSE
'KILL laosdir + "files.bnk"
'OPEN laosdir + "files.bnk" FOR BINARY AS f LEN = LEN(zFileType)
OPEN laosdir + "files.bnk" FOR BINARY AS f LEN = LEN(zfile)
'Der neue Ordner '\' wird erstellt:
zfile.id = 1
zfile.relpos = 1
zfile.Caption = "Hauptverzeichnis"
zfile.SysType = "D"
zfile.Created = DATE$
zfile.time = TIME$
PUT f, 1, zfile
CLOSE f
END SUB

'Erstellt einen Bereichsrahmen (Register). Der Textbereich wird auf diesen
'Rahmen eingeschrÑnkt.
'sGroup$ - Die Steuerelementgruppe, zu welcher der Rahmen gehîren wird...
'Caption$ - Die Bezeichnung des Rahmens
'x1, y1, x2, y2 - Die Koordinaten des Fensters
'Tipp: Sie kînnen die ersten vier Parameter auf 0 setzen, um ein Register
'      fÅr den ganzen Bildschirm zu machen.
SUB Frame (sGroup$, Caption$, x1, y1, x2, y2)
    IF x1 = 0 AND y1 = 0 AND x2 = 0 AND y2 = 0 THEN
      'ganzbildschirm-Rahmen:
      CLW sGroup$
    ELSE
      LINE (x1, y1)-(x2, y2), wincol, B
      LINE (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), frmcol, B
      LINE (x1 + 2, y1 + 2)-(x2 - 2, y2 - 2), 0, BF
      Left = x1 + 2
      Top = y1 + 2
      Right = x2 - 2
      Down = y2 - 2
      IF LEN(sGroup$) > 0 THEN ExitBox sGroup$, 0, 0
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
  Left = 2
  Top = 2
  Right = 637
  Down = 347
END SUB

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
    'kein Control ist aktiv - das erste gefundene wird aktiviert!
    FOR i% = 1 TO UBOUND(Controls)
        IF (sGroup$ = "" OR RTRIM$(Controls(i%).Group) = sGroup$) AND Controls(i%).CtrlType <= zControlTypeCount THEN
            ActiveCtrl = i%
            zDrawThisControl i%, False, 0
            EXIT FOR
        END IF
    NEXT
END IF

Mouse.Down = 0
Mouse.clicked = 0

DO
zGetInpStartLoop:

    'Ist Fokus auf Exitfocus? - wenn ja, GetInp verlassen...
    IF Controls(ActiveCtrl).CtrlType = zExitFocus THEN EXIT DO

    'aufwÑndige Controls: werden hier seperat gehandelt:
    IF NOT zSelfHandleControl(sGroup$, Controls(ActiveCtrl).CtrlType) THEN
        keyb = GetKey(sGroup$)
    END IF

    SELECT CASE keyb
    CASE "passfocus"
      keyb = ""
    CASE "left", "up"
      zLastControl sGroup$
    CASE "right", "down"
      zNextCtrl sGroup$
    CASE "popup"
      EXIT DO
    CASE "tab"
      GOTO zGetInpInit
    CASE "esc"
        IF Controls(ActiveCtrl).CtrlType = zMenu THEN
          iOld = ActiveCtrl
          ActiveCtrl = DefaultCtrl
          zDrawThisControl iOld, 0, 0
          keyb = ""
        ELSE
          EXIT DO
        END IF
    CASE "enter"
        k = 0
        IF DefaultCtrl > 0 THEN
            IF Controls(ActiveCtrl).CtrlType = zEditBox AND (RTRIM$(Controls(DefaultCtrl).Group) = sGroup$ OR sGroup$ = "") THEN k = 1: ActiveCtrl = DefaultCtrl
        END IF
        IF Controls(ActiveCtrl).CtrlType = zButton OR k THEN
            zDrawThisControl ActiveCtrl, True, 0
            DO: k = INP(&H60): a$ = INKEY$: LOOP UNTIL k <> 28 AND k <> 224
            zDrawThisControl ActiveCtrl, False, 0
            WHILE INKEY$ <> "": WEND
            IF k <> 156 AND Mouse.clicked = 0 AND k <> 57 THEN GOTO zGetInpStartLoop
        END IF

        zHandleControls sGroup$, iChanges

        IF NOT iChanges THEN EXIT DO

    CASE ELSE
      FOR i = 1 TO UBOUND(Controls)
        IF LCASE$(LEFT$(LTRIM$(Controls(i).Caption), LEN(keyb))) = LCASE$(keyb) AND RTRIM$(Controls(i).Group) = sGroup$ THEN
          iOld = ActiveCtrl
          ActiveCtrl = i
          zDrawThisControl i, 0, 0
          zDrawThisControl iOld, 0, 0
          IF iOld <> ActiveCtrl THEN EXIT FOR
        END IF
      NEXT
    END SELECT
LOOP UNTIL zQuitKey

    Focus = LTRIM$(RTRIM$(Controls(ActiveCtrl).Caption))
    IF keyb = "esc" THEN Focus = "esc"
    Farbe = tcol
    InversFarbe = 0
    COLOR tcol

END SUB

'Wartet auf eine Tastatureingabe, sowie lîst MouseEvents aus, falls
'auf ein Steuerelement geklickt wurde...
'sGroup$ = Die Steuerelement-Gruppe, fÅr welche Events ausgelesen werden sollen
'         ("" = alle)
FUNCTION GetKey$ (sGroup$)

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
        CASE 134 'Ctrl+P: Bildschirminhalt speichern
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

IF MouseOn THEN PUT (Mouse.oldx, Mouse.oldy), zMHG, PSET
ERASE zMHG

SELECT CASE a$
CASE "#138" 'Ctrl+f12: Controls auflisten
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
                zDrawThisControl i, 0, True
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
GetKey = a$
keyb = a$

END FUNCTION

'Liest einen String eines beliebigen Objektes aus (siehe auch ->PutString)
'
'objekt = Der Name des Objektes, dessen String herausgelesen werden soll.
'Text = RÅckgabewert: Der String des Objektes
SUB GetString (objekt$, text$)

    FOR i% = 1 TO UBOUND(zStrings)
        IF RTRIM$(LEFT$(zStrings(i%), 8)) = RTRIM$(objekt$) THEN
            'Objekt gefunden -> diesen String zurÅckgeben!
            text$ = MID$(zStrings(i%), 9)
            EXIT SUB
        END IF
    NEXT

    FOR i% = 1 TO UBOUND(Controls)
      IF RTRIM$(Controls(i%).Caption) = objekt$ THEN
        'Control gefunden -> diesen String aus Ressource
        'zurÅckgeben!
        zRessourceGet objekt$, Controls(i%).Value, text$, 0
        EXIT SUB
      END IF
    NEXT

END SUB

'Liest den Wert eines Listen-basierenden Steuerelementes aus (siehe auch
'->PutValue)
'objekt$ = Der Name des Objektes, dessen Wert herausgelesen werden soll
'Wert = RÅckgabewert: Der Wert des Steuerelementes
SUB GetValue (objekt$, wert)

    FOR i% = 1 TO UBOUND(Controls)
      IF RTRIM$(Controls(i%).Caption) = objekt$ THEN
        'Control gefunden: Wert aus Ressource zurÅckgeben
        zRessourceGet objekt$, Controls(i%).Value, "", wert
        EXIT SUB
      END IF
    NEXT

END SUB

'Erstellt ein Symbollisten-Steuerelement
'Die Symbole werden mit AddIcon hinzugefÅgt.
'sGroup$ = Die Steuerelemente-Gruppe
'sName$ = Der Name der Symbolliste
'Caption$ = Die öberschrift der Symbolleiste
'x,y,x2,y2 = Die Position des Rahmens, innerhalb dessen sich die Symbole befinden
'            werden.
'iScrollBars = True: Die Symbolliste wird mit Scrollbars und Rahmen angezeigt.
'iExitBox = True: Exitbox wird angezeigt
SUB IconList (sGroup$, sName$, Caption$, x, y, x2, y2, iScrollBars, iExitbox)
    tag$ = STRING$(8, 0)
    IF iScrollBars OR iExitbox THEN
      MID$(tag$, 3, 2) = MKI$(NextCtrl + 1)
      Frame "", Caption$, x, y, x2, y2
      l = x2 - 19
      t = y + 4
      x = Left + 5
      y = Top + 5
      x2 = Right - 18
      y2 = Down
    END IF
    zCreateControl zIconList, sGroup$, sName$, "", 0, x, y, x2, y2, 0, 0, tag$, True
    IF iScrollBars OR iExitbox THEN
      ScrollBar sGroup$, "", x2 + 5, Top, 0, Down - Top, 1, 0, 1, NextCtrl - 1
      IF iExitbox THEN ExitBox sGroup$, l, t
    END IF
END SUB

'Gibt True (-1) zurÅck, wenn der Text nur aus ascii(0)-Zeichen besteht.
FUNCTION IsNull (text$)
  IF STRING$(LEN(text$), 0) = text$ THEN IsNull = -1
END FUNCTION

'Erstellt ein Text-Label
'x,y,x2,y2 = Der Bereich, innerhalb dessen der Text ausgegeben wird.
'Text = Der auszugebende Text
'Alignment = Ausrichtung des Textes (0 = links, 1 = Mitte, 2 = rechts)
'bgColor = Die bcol
SUB Label (x, y, x2, y2, text AS STRING, Alignment, bgColor)
    zGetOriginalValues False
        TextAlignment = Alignment
        InversFarbe = bgColor
        Left = x
        Top = y
        Right = x2
        Down = y2
        CurrentY = y
        CurrentX = x + 2
        Echo text
    zGetOriginalValues True
END SUB

'FÅllt eine zuvor erstellte ListBox mit dem Text und dem Standardwert
'sName$ = Der Name der Listbox, der beim Erstellen der Listbox angegeben wurde
'Index = Der Index des Eintrages (erforderlich: muss grîsser als 0 sein!)
'        Wichtig: Es dÅrfen keine LÅcken in der Indexfolge entstehen!
'text = Der Text
'value = Der Wert (True oder False)
SUB ListAdd (sName$, Index, text AS STRING, Value)
    zRessourceSet sName$, Index, text, Value
END SUB

'Erstellt ein Listbox-Steuerelement
'Die Liste muss anschliessend mit ListAdd gefÅllt werden.
'Mit ListDeleteItem kînnen ListeneintrÑge entfernt werden.
'Group = Die Steuerelementegruppe
'sName$ = Der Name der Listbox (wird fÅr ListAdd und ListDeleteItem verwendet)
'x, y, x2, y2 = Die Koordinaten der Listbox
'WithCheckBoxes = True = Die Listbox wird mit Checkboxen angezeigt
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
    IF sName$ <> "MenuInt" THEN MID$(tag$, 3, 2) = MKI$(NextCtrl + 1)
    zCreateControl zListBox, sGroup$, sName$, "", 1, x, y, x2, y2 - 1, FIX((x2 - x - 4) / 8), FIX((y2 - y - 4) / 14), tag$, True
    IF sName$ <> "MenuInt" THEN
        ScrollBar sGroup$, "", x2 + 1, y, 0, y2 - y - 1, 1, 0, FIX((y2 - y - 4) / 14), NextCtrl - 1
    END IF
END SUB

SUB Main
Frame "IconMan", "Symbol-Manager", 0, 0, 0, 0
IconList "IconMan", "Icon1", "", 4, 23, 635, 345, True, False
CHDIR "C:\LAOS7\ICONS"
SHELL "DIR /B /ON *.ICN >ICONLIST"
OPEN "ICONLIST" FOR INPUT AS #1
DO UNTIL EOF(1)
INPUT #1, a$
a = a + 1
AddIcon "IconMan", 1, a$, LEFT$(a$, LEN(a$) - 4), 0
LOOP
CLOSE #1
KILL "ICONLIST"
DrawControls "IconMan"
GetInp "IconMan"
END SUB

'Setzt ein neues Menu hin!
'group = Die Steuerelement-Gruppe
'Caption$ = Die Caption des MenÅs
SUB Menu (sGroup$, Caption$)
    IF NextCtrl = 1 THEN
        oldx = Left
        oldy = Top
    ELSE
        FOR i% = UBOUND(Controls) TO 1 STEP -1
            IF Controls(i%).CtrlType = zMenu AND RTRIM$(Controls(i%).Group) = sGroup$ THEN EXIT FOR
        NEXT
        IF i% = 0 THEN
            oldx = Left
            oldy = Top
        ELSE
            oldx = Controls(i%).x + TextLen(RTRIM$(Controls(NextCtrl - 1).Caption)) + 16
            oldy = Controls(i%).y
        END IF
        IF oldx + TextLen(Caption$) > 639 THEN oldx = 2: oldy = oldy + 14
    END IF
    zCreateControl zMenu, sGroup$, "", Caption$, 0, oldx, oldy, oldx + TextLen(Caption$), oldy + 13, 0, 0, "", False
END SUB

'Zeigt ein DropDown-MenÅ an fÅr das aktivierte Steuerelement
'Kann auch als PopUp-MenÅ verwendet werden!
FUNCTION MenuInt$ (items AS STRING, Trennzeichen$)

    ioldCtrl = ActiveCtrl
    IF Controls(ioldCtrl).CtrlType = zMenu THEN
        x = Controls(ioldCtrl).x
        y = Controls(ioldCtrl).y + 15
    ELSE
        x = Mouse.x
        y = Mouse.y
    END IF

    l = 0
    maxwidth = 0
    ai% = 1
    FOR i% = 1 TO LEN(items) + 1
        IF MID$(items, i%, 1) = Trennzeichen$ OR i% > LEN(items) THEN
            a$ = MID$(items, ai%, i% - ai%)
            t = INSTR(a$, "|")
            IF t > 0 THEN
                a$ = LEFT$(a$, t - 1) + SPACE$(3) + MID$(a$, t + 1)
            END IF
            IF LEN(a$) > maxwidth THEN maxwidth = LEN(a$)
            ai% = i% + 1
        END IF
    NEXT

    l = 0
    ai% = 1
    FOR i% = 1 TO LEN(items) + 1
        IF MID$(items, i%, 1) = Trennzeichen$ OR i% > LEN(items) THEN
            IF i% = LEN(items) THEN i% = i% + 1
            l = l + 1
            a$ = MID$(items, ai%, i% - ai%)
            t = INSTR(a$, "|")
            IF t > 0 THEN
                b$ = MID$(a$, t + 1)
                a$ = LEFT$(a$, t - 1) + CHR$(0) + SPACE$(3)
                a$ = a$ + SPACE$(maxwidth - LEN(a$) - LEN(b$) + 1) + b$
            END IF
            ListAdd "MenuInt", l, a$, 0
            ai% = i% + 1
        END IF
    NEXT

    MouseClicker "MenuInt", "Cancel", 0, 0, 0, 0, "esc", False
    c = NextCtrl
    x2 = x + maxwidth * 8 + 5 + 13
    y2 = y + l * 14 + 4
    IF x2 > 639 THEN x = x - (x2 - 639): x2 = 639
    IF y2 > 349 THEN y = y - (y2 - 349): y2 = 349

    ListBox "MenuInt", "MenuInt", x, y, x2 - 1, y2, False
    MID$(Controls(c).tag, 1, 1) = "M"
    g = 4 + INT(((PMAP(x2, 0) - PMAP(x, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y2, 1) - PMAP(y, 1) + 1)
    DIM bg(g)
    GET (x, y)-(x2, y2), bg
    zDrawThisControl c, 0, True
    GetInp "MenuInt"
    PUT (x, y), bg, PSET
    REDIM bg(0)

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
    IF keyb = "enter" THEN zDrawThisControl ioldCtrl, False, 0: ActiveCtrl = DefaultCtrl ELSE ActiveCtrl = ioldCtrl

END FUNCTION

'Erstellt ein MouseClicker-Steuerelement
'Der Benutzer kann auf den Bereich klicken, der Programmierer bekommt
'dies per GetInp (Focus) bestÑtigt.
'sGroup$ = Die Steuerelementgruppe
'sName$ = Der Name des Clickers
'x, y, x2, y2 = Die Koordinaten
'sSimulateKey = Die Taste, die zurÅckgegeben werden soll (keyb),
'               wenn der Benutzer auf den Bereich klickt.
'iWithShadow = True = Der Bereich wird schattiert, wenn der Benutzer auf
'                     diesen klickt.
SUB MouseClicker (sGroup$, sName$, x, y, x2, y2, sSimulateKey AS STRING, iWithShadow)
    IF x = 0 THEN x = 0
    IF y = 0 THEN y = 0
    IF x2 = 0 THEN x2 = 639
    IF y2 = 0 THEN y2 = 349
    IF sName$ = "" THEN h = 1
    zCreateControl zMouseClicker, sGroup$, sName$, "", 0, x, y, x2, y2, iWithShadow, h, sSimulateKey, False
END SUB

SUB ProgressBar (text$, Stat AS SINGLE)
  IF text$ <> "" THEN
    Frame "", "", 160, 120, 480, 215
    Echo text$
  END IF
  IF Stat > 1 THEN Stat = 1
  LINE (179, 174)-(459, 194), Farbe, B
  x = 278 * Stat
  LINE (180, 175)-(180 + x, 193), tcol2, BF
END SUB

'Speichert einen String zu einer beliebigen Editbox (siehe auch ->GetString)
'objekt$ = Der Name der Editbox
'Text = Der Text
SUB PutString (objekt$, text$)
DIM o   AS STRING * 8

    FOR i% = 1 TO UBOUND(Controls)
      IF RTRIM$(Controls(i%).Caption) = RTRIM$(objekt$) AND Controls(i%).h = 1 THEN Controls(i%).Value = LEN(text$) + 1
    NEXT
    'existiert objekt bereits?
    FOR i% = 1 TO UBOUND(zStrings)
        IF RTRIM$(LEFT$(zStrings(i%), 8)) = RTRIM$(objekt$) THEN
            'ja, existiert bereits -> diesen String aktualisieren!
            o = objekt$
            zStrings(i%) = o + text$
            EXIT SUB
        END IF
    NEXT
    'Objekt existiert noch nicht:
    'suche freien Platz!
    FOR i% = 1 TO UBOUND(zStrings)
        IF zStrings(i%) = "" THEN
            'freien Platz gefunden -> String eintragen!
            o = objekt$
            zStrings(i%) = o + text$
            EXIT SUB
        END IF
    NEXT

END SUB

'Speichert einen Wert zu einem existierenden Steuerelement
'(siehe auch ->GetValue)
'objekt$ = Der Name des Objektes
'Wert = Der Wert, der dem Objekt zugewiesen werden soll.
SUB PutValue (objekt$, wert)

    'Suche das Objekt?
    FOR i% = 1 TO UBOUND(Controls)
        IF RTRIM$(Controls(i%).Caption) = RTRIM$(objekt$) THEN
            'ja, Objekt existiert -> diesen Wert aktualisieren!
            Controls(i%).Value = wert
            EXIT SUB
        END IF
    NEXT

    'Objekt existiert nicht!:
    Dialog "Fehler in PutValue: Das Objekt " + objekt$ + " existiert nicht!"

END SUB

'Erstellt ein RadioButton-Steuerelement
'sGroup$ = Die Steuerelementgruppe
'Caption$ = Die Caption
'x, y = Die Koordinaten
'value = Der Wert (True oder False)
SUB RadioButton (sGroup$, Caption$, x, y, Value)
    IF x = 0 THEN
        x = Controls(NextCtrl - 1).x
    END IF
    IF y = 0 THEN
        y = Controls(NextCtrl - 1).y + 21
    END IF
    'Initialisieren mit TRUE?
    IF Value = True THEN
        'Alle anderen auf False stellen:
        FOR i% = 1 TO UBOUND(Controls)
            IF sGroup$ = "" OR RTRIM$(Controls(i%).Group) = sGroup$ THEN
                IF Controls(i%).CtrlType = zRadioButton THEN
                    Controls(i%).Value = False
                END IF
            END IF
        NEXT
    END IF
    zCreateControl zRadioButton, sGroup$, "", Caption$, Value, x, y, x + TextLen(Caption$) + 32, y + 18, 0, 0, "", False
END SUB

'Wird ausgefÅhrt, wenn auf einem unterstÅtzten Steuerelement die rechte
'Maustaste gedrÅckt wurde.
'iCtrl = Die Nummer des Controls
'iCtrl = Der Name (Caption) des Controls
'
'Tipp: Mit ->MenuInt kann nun ein PopUpMenÅ angezeigt werden!
'
SUB RaisePopUpMenu (iCtrl, name$, ExitInput)

  SELECT CASE name$
  CASE "files"
    ExitInput = True
    ShortKeys = ""
    a$ = MenuInt("Neuer Ordner...,Verschieben|F7,Lîschen|Delete,-,Eigenschaften|Insert", ",")
    SELECT CASE choice
    CASE 0
      ExitInput = False
    CASE 1
      subNeuerOrdner
    CASE 2
    CASE 3
      GetString "files", a$
      IF a$ <> "up.lsb      .." THEN
        GetValue "files", id
        subDateiLoeschen id
      END IF
    CASE 5
      GetValue "files", id
      subDateiEigenschaften id
    END SELECT
  END SELECT

END SUB

'Event: Wird ausgefÅhrt, wenn der Benutzer mit der ScrollBar gescrollt hat.
'       FÅr die ScrollBar und SliderBar-Controls
'iCtrl = Die ControlNr der Scroll- oder Sliderbar
'name$ = Der Name der Scroll- oder Sliderbar
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
  CASE "zup"
    text$ = "öbergelegener Ordner"
  END SELECT

  IF text$ <> "" THEN zToolTipText text$

END SUB

'Holt den im ruhigen Bildaufbaumodus aufgebaute Bildschirm in den Vordergrund.
'Das Bild wird dadurch angezeigt.
'Verwenden Sie SilentScreen, um den ruhigen Bildaufbaumodus zu aktivieren.
'->SilentScreen
SUB RestoreScreen
    IF zSSMStop THEN zSSMStop = zSSMStop - 1: EXIT SUB
    SCREEN 9, , 0, 0
    PCOPY 1, 0
    SilentScreenMode = False
    SaveScreenMode = False
    COLOR tcol
END SUB

'Startet ein Programm
SUB RunFile (prgStarter$)
  f = FREEFILE
  OPEN laosdir + "prg.cfg" FOR INPUT AS f
  errorlevel = 0
  WHILE NOT EOF(f)
    INPUT #f, prg$, path$
    IF RTRIM$(prg$) = prgStarter$ THEN
      'Programm wurde gefunden:
      'Dieses jetzt starten!
      CLOSE
      'In das Laufwerk der LaOS-Installation wechseln:
      SHELL LEFT$(laosdir, 1) + ":"
      'In den Programmordner wechseln:
      CHDIR laosdir + "prg\" + LEFT$(path$, INSTR(path$, "\") - 1)
      CHAIN laosdir + "prg\" + path$
      RUN laosdir + "prg\" + path$
    END IF
    IF errorlevel THEN Dialog "Fehler beim Starten des Programmes: " + prgStarter$: EXIT SUB
  WEND
  CLOSE f
  Dialog "Das Programm " + prgStarter$ + " kann nicht erkannt werden! Bitte ÅberprÅfen Sie dessen Installation, oder setzen Sie sich mit dem Hersteller in Verbindung."
END SUB

'Zeigt einen Speichern-Dialog an und gibt den neuen Dateinamen zurÅck.
'Kommt eine leere Zeichenfolge zurÅck, so hat der Benutzer Abbrechen gedrÅckt.
FUNCTION SaveDialog$ (prgStarter$)
  DIM zfile AS zFileType, zCurrentfolder AS LONG, zftt AS zFileType

  f = FREEFILE
  ClearControls ""
  OPEN "folder.ini" FOR BINARY AS f LEN = 2
  GET f, 1, fpos
  CLOSE f
  OPEN laosdir + "files.bnk" FOR BINARY AS f LEN = LEN(zFileType)
  zCurrentfolder = 1

zSvdlg:
  Frame "zsave", "Speichern", 5, 80, 634, 279
  zfolders = NextCtrl
  ListBox "zsave", "zfolders", Left + 2, Top + 2, 260, 273, 0
  zWriteTextXY 280, Top + 4, "Dateiname:"
  zWriteTextXY 280, Top + 32, "Dateiinfo:"
  EditBox "zsave", "zfile", 367, Top + 2, 32, 1, 40
  EditBox "zsave", "zinfo", 367, Top + 30, 32, 5, 256
  Button "zsave", "Neuer Ordner", 280, 252
  zbtnSave = NextCtrl
  Button "zsave", "  Speichern  ", 515, 252

DO
  DefaultCtrl = zbtnSave
  'zfile steht nun auf dem aktuellen Ordner:
  GET f, zCurrentfolder, zfile
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
    IF zftt.relpos = zfile.id AND zftt.SysType = "D" THEN
      fid = fid + 1
      ListAdd "zfolders", fid, zftt.Caption, zftt.id
    END IF
  LOOP
  DrawControls "zsave"
  GetInp "zsave"
  zRessourceGet "zfolders", Controls(zfolders).Value, zfolder$, fid
  IF keyb = "esc" THEN EXIT FUNCTION
  SELECT CASE Focus
  CASE "zfolders"
    'in folder wechseln...
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
       IF zftt.id >= zid THEN zid = zftt.id + 1
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
      IF RTRIM$(a$) <> "" THEN
        PUT f, p&, zftt
      END IF
    END IF
    ClearControls "zsave"
    ClearControls "zord"
    GOTO zSvdlg
  CASE "Speichern"
      'Die neue Datei wird erstellt:
      zid = 0
      SEEK f, 1
      DO
       IF zftt.id >= zid THEN zid = zftt.id + 1
       p& = SEEK(f)
       GET f, , zfile
      LOOP UNTIL zfile.id = 0
      zfile.id = zid
      zfile.relpos = fid
      zftt.docint = zid
      GetString "zfile", a$
      zfile.Caption = a$
      GetString "zinfo", a$
      zfile.info = a$
      zfile.SysType = "F"
      zfile.prgStarter = prgStarter$
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
    IF SilentScreenMode = True THEN zSSMStop = zSSMStop + 1: EXIT SUB
    SaveScreenMode = True
    SilentScreenMode = True
    PCOPY 0, 1
END SUB

SUB ScreenShot
  'Screenshot-Programm von TT-Soft (www.East-Power-Soft.de
  'Email: TT-Soft@East-Power-Soft.de
  '
  ScrFile$ = laosdir + "zwa.bmp"
  'aktive Palette ermitteln:
  DIM ScrShotPAL(15, 2), ok AS LONG

  SilentScreenMode = True
  PCOPY 0, 1
  SCREEN 9, , 1, 1
  ProgressBar "Bildschirminhalt speichern...", 0
  SCREEN 9, , 0, 1

  'Fixe Snowflake-Palette:
  RESTORE zColors
  'FÅr die Hintergrundfarbe (Screen 9) spezielles vorgehen:
  FOR ScrI% = 0 TO 15
    READ r, g, b
    ScrShotPAL(ScrI%, 2) = CINT(r * 4.04)
    ScrShotPAL(ScrI%, 1) = CINT(g * 4.04)
    ScrShotPAL(ScrI%, 0) = CINT(b * 4.04)
  NEXT
  IF bcol = 63 THEN
    'Weisser Hintergrund!
    ScrShotPAL(0, 2) = CINT(63 * 4.04)
    ScrShotPAL(0, 1) = CINT(63 * 4.04)
    ScrShotPAL(0, 0) = CINT(63 * 4.04)
  END IF

  'Datei anlegen und Header speichern.
  ScrHead$ = ""
  ScrHead$ = ScrHead$ + "424D7658020000000000760000002800"
  ScrHead$ = ScrHead$ + "000080020000E0010000010004000000"
  ScrHead$ = ScrHead$ + "000000580200120B0000120B00001000"
  ScrHead$ = ScrHead$ + "000010000000"
  SCRN% = FREEFILE
  OPEN ScrFile$ FOR OUTPUT AS #SCRN%
  FOR ScrI% = 1 TO LEN(ScrHead$) STEP 2
    PRINT #SCRN%, CHR$(VAL("&H" + MID$(ScrHead$, ScrI%, 2)));
  NEXT
  FOR ScrI% = 0 TO 15
    PRINT #SCRN%, CHR$(ScrShotPAL(ScrI%, 0));
    PRINT #SCRN%, CHR$(ScrShotPAL(ScrI%, 1));
    PRINT #SCRN%, CHR$(ScrShotPAL(ScrI%, 2));
    PRINT #SCRN%, CHR$(0);
  NEXT
  'Daten schreiben
  PicWidth = 640
  PicHeight = 350
  ScrY% = PicHeight - 1
  DO WHILE ScrY% > -1
    ScrX% = 0: ScrLine$ = ""
    DO WHILE ScrX% < PicWidth
      ScrLeft% = POINT(ScrX%, ScrY%)
      ScrRight% = POINT(ScrX% + 1, ScrY%)
      ScrLine$ = ScrLine$ + CHR$(VAL("&H" + HEX$(ScrLeft%) + HEX$(ScrRight%)))
      ScrX% = ScrX% + 2
      ok = ok + 2
      apercent = percent
      percent = ok * CLNG(100) / (640 * CLNG(350))
      IF percent > apercent AND percent > 0 THEN
        IF FIX(percent / 20) * 20 = percent THEN
          'durch 20 teilbar = wieder den Fortschritt anzeigen!
          SCREEN 9, , 1, 1
          ProgressBar "", 1 / (100 / percent)
          SCREEN 9, , 0, 1
        END IF
      END IF
    LOOP
    PRINT #SCRN%, ScrLine$;
    ScrY% = ScrY% - 1
  LOOP
  CLOSE SCRN%
  OPEN ScrFile$ FOR BINARY AS #SCRN%
  SEEK #SCRN%, &H13: PUT #SCRN%, , PicWidth             '---> Bildbreite
  SEEK #SCRN%, &H17: PUT #SCRN%, , PicHeight             '---> Bildhîhe
  CLOSE SCRN%
  SilentScreenMode = False
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
    'Wichtig:
    'Position 9 des Tag ist reserviert fÅr ein Scrollflag
    '(fÅr zEventMouseDown/Move)!
    zCreateControl zScrollBar, sGroup$, sName$, "", Max, x, y, x2, y2, w, h, t, True
END SUB

'Schaltet in den ruhigen Bildaufbaumodus.
'Kann dazu verwendet werden, den Grafikaufbau im Hintergrund
'durchzufÅhren. Der Benutzer kann dann dem Aufbau nicht mehr zusehen.
'Um die aufgebaute Grafik schlussendlich anzuzeigen, verwenden Sie bitte
'->RestoreScreen
SUB SilentScreen
    IF SilentScreenMode = True THEN zSSMStop = zSSMStop + 1: EXIT SUB
    SilentScreenMode = True
    PCOPY 0, 1
    SCREEN 9, , 1, 0
    COLOR tcol
END SUB

'Erstellt ein Sliderbar-Steuerelement
'sGroup$ = die Steuerelementgruppe
'sName$ = Der Name der Sliderbar
'x,y = Die Koordinaten
'w = Die Breite der Sliderbar (Width)
' - oder - h = Die Hîhe der Sliderbar (Height)
'min = Das Minimum des Scrollbar-Wertes
'max = Das Maximum des Scrollbar-Wertes
'Wichtig: Nur entweder w oder h auf einen Wert hîher 0 setzen!
'         Je nach dem, welcher Wert angegeben ist, so wird die Sliderbar
'         vertikal oder horizontal angezeigt.
SUB SliderBar (sGroup$, sName$, x, y, w, h, Min, Max)
    IF w > 0 THEN x2 = x + w: y2 = y + 20 ELSE y2 = y + h: x2 = x + 20
    DIM t AS STRING * 12
    MID$(t, 2, 2) = MKI$(Min)
    MID$(t, 4, 2) = MKI$(Max)
    MID$(t, 10, 2) = MKI$(1) 'Step
    zCreateControl zSliderBar, sGroup$, sName$, "", Max, x, y, x2, y2, w, h, t, True
END SUB

'Zeigt ein Statustext in der Statusbar an.
SUB StatusBar (text$)
  zGetOriginalValues 0
  LINE (2, 332)-(637, 347), sfcol, BF
  LINE (2, 332)-(637, 332), stcol
  Farbe = stcol
  InversFarbe = sfcol
  zWriteTextXY 3, 333, text$
  zGetOriginalValues 1
END SUB

SUB subDateiEigenschaften (id)
DIM zftt AS zFileType
DIM zfolder AS zFileType

  SaveScreen
  SEEK filenr, 1
  DO
    lngpos& = SEEK(filenr)
    GET filenr, , zftt
  LOOP UNTIL zftt.id = id OR zftt.id = 0
  markfolder = lngpos&
  icon$ = zftt.icon

1010
  ClearControls "propdlg"
  GET filenr, CurrentFolder, zfolder
  Frame "propdlg", "Eigenschaften von " + zftt.Caption, 70, 30, 520, 330
  MouseClicker "propdlg", "chgicon", 90, 70, 130, 110, "insert", 0
  IF LCASE$(RIGHT$(RTRIM$(icon$), 3)) = "lsb" THEN
    VIEW (90, 70)-(130, 110), icfcol
    zShowPicture 0, 0, laosdir + "icons\" + icon$
  ELSE
    zShowPicture 90, 70, laosdir + "icons\" + icon$
  END IF
  VIEW
  zWriteTextXY 150, 70, "Dateiname:"
  EditBox "propdlg", "name", 250, 70, 30, 1, 40
  PutString "name", RTRIM$(zftt.Caption)
  zWriteTextXY 150, 90, "Ort:"
  Label 250, 90, 500, 118, RTRIM$(zfolder.Caption), AlignLeft, 0
  zWriteTextXY 150, 140, "Typ:"
  IF zftt.SysType = "D" THEN
    zWriteTextXY 250, 140, "Ordner"
  ELSE
    zWriteTextXY 250, 140, LEFT$(zftt.prgStarter, 32)
  END IF
  zWriteTextXY 150, 160, "Dateiinfo:"
  EditBox "propdlg", "info", 250, 160, 30, 4, 256
  IF NOT IsNull(zftt.info) THEN PutString "info", RTRIM$(zftt.info)
  zWriteTextXY 150, 230, "Erstellt:"
  zWriteTextXY 250, 230, SysDate(zftt.Created)

  Button "propdlg", "Abbrechen", 330, 300
  DefaultCtrl = NextCtrl
  Button "propdlg", "Speichern", 0, 0

  Farbe = tcol2
  zWriteTextXY 80, 310, "Insert - Symbol Ñndern"

  DrawControls "propdlg"
  ShortKeys = "insert"
  GetInp "propdlg"

  IF keyb = "insert" THEN
    a$ = ChooseIcon(icon$)
    IF a$ <> "" THEN icon$ = a$
    GOTO 1010
  END IF
  IF Focus = "Speichern" AND keyb = "enter" THEN
    GetString "name", a$
    IF a$ = "" THEN 1010
    zftt.Caption = a$
    GetString "info", a$
    zftt.info = a$
    zftt.icon = icon$
    PUT filenr, lngpos&, zftt
  END IF
  ClearControls "propdlg"
  RestoreScreen
END SUB

SUB subDateiLoeschen (id)
DIM zftt AS zFileType

  SEEK filenr, 1
  DO
    lngpos& = SEEK(filenr)
    GET filenr, , zftt
  LOOP UNTIL zftt.id = id OR zftt.id = 0
  zftt.id = zftt.id * -1
  markfolder = lngpos&

  Frame "delete", "Lîschen von " + zftt.Caption, 150, 120, 490, 230
  TextAlignment = AlignCenter
  CurrentY = 150
  IF zftt.SysType = "D" THEN
    Echo "Sind Sie sicher, dass Sie diesen Ordner lîschen mîchten?"
  ELSE
    Echo "Sind Sie sicher, dass Sie diese Datei lîschen mîchten?"
  END IF
  TextAlignment = AlignLeft
  Button "delete", "  Ja  ", 260, 195
  Button "delete", " Nein ", 0, 0
  DrawControls "delete"
  GetInp "delete"
  ClearControls "delete"
  IF Focus = "Ja" AND keyb = "enter" THEN
    SEEK filenr, lngpos&
    PUT filenr, , zftt
    IF zftt.SysType = "F" THEN
      KILL laosdir + "doc\" + LTRIM$(STR$(zftt.id * -1)) + ".*"
    END IF
  END IF

END SUB

SUB subNeuerOrdner
DIM zftt AS zFileType

    Frame "zord", "Neuer Ordner in " + CurrentFile.Caption, 80, 120, 470, 220
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
      SEEK filenr, 1
      zid = 0
      DO
       IF zftt.id >= zid THEN zid = zftt.id + 1
       p& = SEEK(filenr)
       GET filenr, , zftt
      LOOP UNTIL zftt.id = 0
      zftt.id = zid
      zftt.relpos = CurrentFile.id
      GetString "zord", a$
      zftt.icon = "ordner.lsb"
      zftt.Caption = a$
      zftt.SysType = "D"
      zftt.Created = DATE$
      'zftt.Changed = DATE$
      zftt.time = TIME$
      zftt.prgStarter = "Ordner"
      PUT filenr, p&, zftt
      markfolder = p&
    END IF
    ClearControls "zord"

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
FUNCTION TextHeight% (text AS STRING)

DIM linie AS STRING, txt AS STRING
x = 0
y = 0
txt = text
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
FUNCTION TextLen% (text AS STRING)
        TextLen% = LEN(text) * 8
END FUNCTION

'Gibt die Breite eines Textblockes zurÅck, unter BerÅcksichtigung der
'BereichsbeschrÑnkungen des aktiven Begrenzungs-Rahmens.
'text = Der zu prÅfende Text
'-> Verwenden Sie TextLen%(text), um die LÑnge ohne BerÅcksichtigung
'   der BereichsbeschrÑnkungen zurÅckzugeben.
FUNCTION TextWidth% (text AS STRING)
DIM linie AS STRING, txt AS STRING

    x = 0
    txt = text
    panewidth = Right - Left
    leftpane = 0
    Max = 0

    DO
        'holt die nÑchste Zeile:
        aw = 0
        DO
            w = INSTR(aw + 1, txt, " ")
            cr = INSTR(aw + 1, txt, crlf) 'Zeilenvorschub & WagenrÅcklauf?

            IF w = 0 THEN w = LEN(txt)

            IF cr > 0 THEN   'Zeilenvorschub & WagenrÅcklauf!
                IF NOT (LEN(LEFT$(txt, cr - 1)) * 8 > panewidth) THEN
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

        x = LEN(linie) * 8
        IF x > Max THEN Max = x
        x = 0
    LOOP UNTIL LEN(txt) = 0

    TextWidth% = Max

END FUNCTION

'Zeigt die Festplatte auf dem Bildschirm an...
'Modus: 0 = beschÑftigt
'       1 = unbeschÑftigt
SUB TimeLock (Modus)
LINE (300, 164)-(338, 164), 8
LINE (339, 165)-(339, 183), 8
LINE (338, 184)-(300, 184), 8
LINE (299, 183)-(299, 165), 8
LINE (300, 165)-(338, 183), 7, BF
LINE (302, 167)-(336, 167), 6
LINE (336, 169)-(302, 169), 6
LINE (336, 182)-(302, 182), 3
IF Modus = 0 THEN
  LINE (325, 176)-(336, 181), 3, BF
ELSE
  LINE (325, 176)-(336, 181), 11, BF
END IF
END SUB

'wird intern verwendet fÅr das Erstellen eines beliebigen Controls
SUB zCreateControl (iCtrlType, sGroup$, sName$, Caption$, Value, x, y, x2, y2, w, h, tag$, iSelfHandler)
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
    Controls(NextCtrl).x = x
    Controls(NextCtrl).y = y
    Controls(NextCtrl).x2 = x2
    Controls(NextCtrl).y2 = y2
    Controls(NextCtrl).w = w
    Controls(NextCtrl).h = h
    Controls(NextCtrl).tag = tag$
    Controls(NextCtrl).SelfHandler = iSelfHandler
    Controls(NextCtrl).CtrlType = iCtrlType
    Controls(NextCtrl).Group = sGroup$
    Controls(NextCtrl).Value = Value
    NextCtrl = NextCtrl + 1
END SUB

'Sorgt dafÅr, dass die Zeit-Ereignisse ausgefÅhrt werden. (Bildschirmschoner,
'Zeit anzeigen etc..)
'Vorteilhaft in Leerlaufschleifen zu verwenden.
SUB zDoEvents
  FOR i% = 0 TO UBOUND(zTimeEvent)
      IF TIMER >= zTimeEvent(i%).nextime AND zTimeEvent(i%).inc > 0 THEN
          zTimeEventHandler i%
          zTimeEvent(i%).nextime = TIMER + zTimeEvent(i%).inc
      END IF
  NEXT
END SUB

'Stellt sicher, dass Doppelklick-Ereignisse erfasst werden.
'Kann in Schleifen aufgerufen werden, die nach einzelnen Mausklicks
'stattfinden.
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

'Zeichnet ein Rahmen ohne dabei den Bereich darauf einzuschrÑnken.
SUB zDrawBox (x1, y1, x2, y2)
    LINE (x1, y1)-(x2, y2), wincol, B
    LINE (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), frmcol, B
    LINE (x1 + 2, y1 + 2)-(x2 - 2, y2 - 2), 0, BF
END SUB

'Zeichnet ein beliebiges Control
'iCtrl = Die Nr. des Controls
'MouseDown = True = Wird als angeklickt dargestellt
'iRefresh = True = Das Control soll vollstÑndig neu gezeichnet werden
SUB zDrawThisControl (iCtrl, MouseDown, iRefresh)

DIM text AS STRING

    i% = iCtrl
    IF i% = 0 THEN EXIT SUB
    x = Controls(i%).x
    y = Controls(i%).y
    x2 = Controls(i%).x2
    y2 = Controls(i%).y2
    w = Controls(i%).w
    h = Controls(i%).h
    cap$ = RTRIM$(Controls(i%).Caption)
    Activated = (ActiveCtrl = i%)
    Value = Controls(i%).Value
    am = InversFarbe
    af = Farbe
    InversFarbe = 0
    tag$ = Controls(i%).tag
    Farbe = tcol

        SELECT CASE Controls(i%).CtrlType
        CASE zIconList
            IF NOT MouseDown THEN
                IF iRefresh THEN
                  MID$(Controls(i%).tag, 1, 2) = MKI$(0)
                  MID$(Controls(iCtrl).tag, 5, 2) = MKI$(MaxItems)
                END IF
                zIconListHandler sGroup$, i%, True
            END IF

        CASE zScrollBar
            Min = CVI(MID$(tag$, 2, 2))
            Max = CVI(MID$(tag$, 4, 2))
            IF Max - Min = 0 THEN Max = Max + 1
            IF Value > Max THEN Controls(iCtrl).Value = Max: Value = Max
            IF Value < Min THEN Controls(iCtrl).Value = Min: Value = Min
            IF iRefresh THEN
              'Knîpfe oben und unten:
              LINE (x + 1, y + 1)-(x2 - 1, y + 10), sbbcol, BF
              LINE (x + 1, y2 - 1)-(x2 - 1, y2 - 10), sbbcol, BF
            END IF
            IF w > 0 THEN
              'horizontal:
              'FÅllung innen:
              LINE (x + 12, y + 1)-(x2 - 12, y2 - 1), sbfcol, BF
              IF iRefresh THEN
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
              IF iRefresh THEN
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
        CASE zSliderBar
            Min = CVI(MID$(tag$, 2, 2))
            Max = CVI(MID$(tag$, 4, 2))
            IF Value > Max THEN Controls(iCtrl).Value = Max: Value = Max
            IF Value < Min THEN Controls(iCtrl).Value = Min: Value = Min
            IF w > 0 THEN
                'horizontal:
                LINE (x, y + 1)-(x2, y2 - 1), 0, BF
                LINE (x, y + 5)-(x2, y + 14), 7, B
                bx = x + (Value * CLNG((x2 - x - 10)) / (Max - Min))
                x = bx
                y = y + 1
                x2 = bx + 10
                y2 = y + 18

                LINE (x, y)-(x2, y2), btfcol, BF
                IF MouseDown THEN
                    LINE (x, y)-(x2, y2), btrcol, B
                    LINE (x, y2)-(x2, y2), btlcol
                    LINE (x2, y2)-(x2, y), btlcol

                ELSE
                    LINE (x, y)-(x2, y2), btlcol, B
                    LINE (x, y2)-(x2, y2), btrcol
                    LINE (x2, y2)-(x2, y), btrcol
                    LINE (x + 1, y2 - 1)-(x2 - 1, y2 - 1), btscol
                    LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), btscol
                END IF
            ELSE
                'vertikal:
                LINE (x + 1, y)-(x2 - 1, y2), 0, BF
                LINE (x + 5, y)-(x + 14, y2), 7, B
                by = (y + ((Value - Min) * CLNG((y2 - y - 10)) / (Max - Min)))
                x = x + 1
                y = by
                x2 = x + 18
                y2 = by + 10

                LINE (x, y)-(x2, y2), btfcol, BF
                IF MouseDown THEN
                    LINE (x, y)-(x2, y2), btrcol, B
                    LINE (x, y2)-(x2, y2), btlcol
                    LINE (x2, y2)-(x2, y), btlcol

                ELSE
                    LINE (x, y)-(x2, y2), btlcol, B
                    LINE (x, y2)-(x2, y2), btrcol
                    LINE (x2, y2)-(x2, y), btrcol
                    LINE (x + 1, y2 - 1)-(x2 - 1, y2 - 1), btscol
                    LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), btscol
                END IF
            END IF

        CASE zMenu
            IF Activated = True THEN
                InversFarbe = mfcol
                Farbe = mtcol
            END IF
            zWriteTextXY x, y, cap$

        CASE zButton
            LINE (x, y)-(x2, y2), btfcol, BF
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
                zWriteTextXY x + 6, y + 5, cap$
                LINE (x + 3, y + 3)-(x2 - 3, y2 - 3), focol, B, PunkteLinie
            ELSE
                IF Activated = True THEN
                    LINE (x + 3, y + 3)-(x2 - 3, y2 - 3), focol, B, PunkteLinie
                END IF
                zWriteTextXY x + 5, y + 4, cap$
            END IF
            Farbe = tcol
            IF DefaultCtrl = iCtrl THEN
                LINE (x, y2 + 1)-(x2 + 1, y2 + 1), btrcol
                LINE (x2 + 1, y2 + 1)-(x2 + 1, y), btrcol
            END IF
       
        CASE zRadioButton
            CIRCLE (x + 7, y + 9), 8, tcol
            zWriteTextXY x + 24, y + 3, cap$
            IF Value = True THEN
                CIRCLE (x + 7, y + 9), 5, tcol
                PAINT (x + 7, y + 9), tcol
            ELSE
                PAINT (x + 7, y + 9), 0, 0
            END IF
            IF Activated = True THEN
                LINE (x + 22, y + 2)-(x2 - 3, y2 - 1), focol, B, PunkteLinie
            ELSE
                LINE (x + 22, y + 2)-(x2 - 3, y2 - 1), 0, B
            END IF

        CASE zCheckBox
            LINE (x, y + 3)-(x + 15, y + 15), 0, BF
            LINE (x, y + 3)-(x + 15, y + 15), tcol, B
            zWriteTextXY x + 24, y + 3, cap$
            IF Value = True THEN
                LINE (x, y + 3)-(x + 15, y + 15), tcol
                LINE (x + 15, y + 3)-(x, y + 15), tcol
            END IF
            IF Activated = True THEN
                LINE (x + 22, y + 2)-(x2 - 3, y2 - 1), focol, B, PunkteLinie
            ELSE
                LINE (x + 22, y + 2)-(x2 - 3, y2 - 1), 0, B
            END IF

        CASE zEditBox
            IF NOT MouseDown THEN 'keine VerÑnderung bei MouseDown!!!
                LINE (x, y)-(x2, y2), tbfcol, BF
                LINE (x, y)-(x2, y2), tbbcol, B
                zEditBoxHandler "", cap$, iCtrl, x + 3, y + 1, x2, y2, w, h, text, True
            END IF

        CASE zListBox
            IF NOT MouseDown THEN
                IF iRefresh THEN
                  'MaxItems auf 0 setzen damit wieder erneut gezÑhlt wird...
                  MID$(Controls(CVI(MID$(Controls(iCtrl).tag, 3, 2))).tag, 4, 2) = MKI$(0)
                    IF MID$(tag$, 1, 1) = "M" THEN
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
                zListboxHandler "", iCtrl, x, y, x2, y2, w, h, True
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

'Handler fÅr das Editbox-Steuerelement
SUB zEditBoxHandler (sGroup$, sName$, iCtrl, x1, y1, x2, y2, iAnzSpalten, iAnzZeilen, text AS STRING, iRefresh)

DIM oldText         AS STRING 'behÑlt den alten Text (vor der Eingabe)
DIM linie           AS STRING
DIM txt             AS STRING
DIM txt2            AS STRING
DIM linie2          AS STRING
DIM echoLineNr       'Die Liniennummer der Ausgabe!
DIM topperLine       'Die Linie, die die erste sichtbare ist (im Fenster)
DIM visibleLines
DIM markerStart      'Das Startzeichen der Markierung innerhalb des Textes
DIM outerVisible     'True = es ist der Bereich Åberschritten!
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
    GetString sName$, text
    InsertMode = False
    oldText = ""
    topperLine = 1
    visibleLines = iAnzZeilen
    markerStart = Controls(iCtrl).Value
    iMaxZeichen = CVI(MID$(Controls(iCtrl).tag, 1, 2))
    wishedTopper = CVI(MID$(Controls(iCtrl).tag, 3, 2))

IF Mouse.clicked = 2 THEN
    'KontextmenÅ:
    a$ = MenuInt(" Text einfÅgen , Text kopieren,-, Text lîschen", ",")
    Mouse.clicked = 0
    f = FREEFILE
    SELECT CASE choice
    CASE 1: OPEN tempdir + "zwa.txt" FOR INPUT AS f
            LINE INPUT #f, a$: text = LEFT$(text, markerStart - 1) + a$ + MID$(text, markerStart)
    CASE 2: OPEN tempdir + "zwa.txt" FOR OUTPUT AS f
            PRINT #f, text
    CASE 4: text = ""
            LINE (x1, y1)-(x2 - 1, y2 - 1), tbfcol, BF
    END SELECT
    CLOSE f
    ActiveCtrl = iCtrl
ELSEIF Mouse.clicked = 1 AND NOT iRefresh THEN
    markerStart = LEN(text) + 1
END IF

zEditBoxCalculateCursor:
'1. Es muss geprÅft werden, welchen Bereich angezeigt werden muss:
'   (Der Bereich, wo markerStart drin ist!)

'Fragestellung: Welches ist die Zeile des Cursors (markerStart)?

    IF iMaxZeichen > 0 AND LEN(text) > iMaxZeichen THEN text = LEFT$(text, iMaxZeichen)
    txt = text
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
        FOR i% = 1 TO LEN(linie)
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
    txt = text
    panewidth = TextLen(SPACE$(iAnzSpalten))
    leftpane = x
    txt2 = oldText
    echoLineNr = 0
    topperVPos = 1
    bottomVPos = 0
    CurrentX = 0
    iCurOverEnd = True
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
                zWriteTextXY x, y, linie
                FOR i% = 1 TO LEN(linie)
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
                        iCurOverEnd = False
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
   
    IF bottomVPos = 0 THEN bottomVPos = LEN(text)
    'text war leer - Cursor jetzt positionieren:
    IF CurrentX = 0 THEN
        CurrentX = leftpane
        CurrentY = y - 14
    END IF

    IF RIGHT$(text, LEN(crlf)) = crlf AND markerStart > LEN(text) THEN
        CurrentY = CurrentY + 14
        CurrentX = leftpane
    END IF

    'RestoreScreen

    'wenn nur Anzeige aktualisieren gew. -> hier beenden!
    IF iRefresh THEN GOTO zEditBoxTerminate

    'Cursor zeichnen:
    FOR y = CurrentY TO CurrentY + 13
        IF POINT(CurrentX, y) = tbfcol THEN
            PSET (CurrentX, y), tbtcol
        ELSE
            PSET (CurrentX, y), tbfcol
        END IF
    NEXT
    
    a$ = GetKey(sGroup$)

    'Cursor lîschen:
    FOR y = CurrentY TO CurrentY + 13
        IF POINT(CurrentX, y) = tbfcol THEN
            PSET (CurrentX, y), tbtcol
        ELSE
            PSET (CurrentX, y), tbfcol
        END IF
    NEXT
    IF Mouse.clicked THEN GOTO zEditBoxTerminate

    oldText = text

    IF zQuitKey THEN GOTO zEditBoxTerminate
    IF keyb = "home" AND iAnzZeilen = 1 THEN keyb = "ctrl-home"
    IF keyb = "end" AND iAnzZeilen = 1 THEN keyb = "ctrl-end"

    SELECT CASE keyb
    CASE "event"
    CASE "pagedown"
        markerStart = bottomVPos + 1
        IF markerStart > LEN(text) + 1 THEN markerStart = LEN(text) + 1
        oldText = ""
        
    CASE "pageup"
        markerStart = topperVPos - 1
        IF markerStart < 1 THEN markerStart = 1
        oldText = ""

    CASE "ctrl-right"
        'Wort nach rechts:
        FOR i% = markerStart + 1 TO LEN(text) + 1
            IF MID$(text, i%, 1) = " " THEN EXIT FOR
        NEXT
        IF i% > LEN(text) THEN i% = LEN(text) + 1
        markerStart = i%
   
    CASE "ctrl-end"
        markerStart = LEN(text) + 1
        oldText = ""

    CASE "ctrl-home"
        markerStart = 1
        oldText = ""
   
    CASE "ctrl-left"
        'Wort nach links:
        FOR i% = markerStart - 1 TO 1 STEP -1
            IF i% <= 1 OR MID$(text, i%, 1) = " " THEN EXIT FOR
        NEXT
        IF i% < 1 THEN i% = 1
        markerStart = i%

    CASE "home"
        markerStart = CurrentLineStart
        merkerEnd = markerStart
        

    CASE "right"
        markerStart = markerStart + 1
        IF markerStart > LEN(text) + 1 THEN
            markerStart = LEN(text) + 1
        END IF

    CASE "end"
        'Line-End:
        markerStart = CurrentLineStart + CurrentLineLen - 1
        IF markerStart = LEN(text) THEN
            markerStart = markerStart + 1
        END IF

    CASE "up"
        'Durchschnittliche Linie aufwÑrts:
        IF markerStart - iAnzSpalten > 1 THEN
            markerStart = CurrentLineStart - FIX(iAnzSpalten / 2)
        ELSE
            markerStart = 1
        END IF

    CASE "down"
        'Durchschnittliche Linie runter:
        IF markerStart + iAnzSpalten <= LEN(text) THEN
            markerStart = CurrentLineStart + FIX(iAnzSpalten / 2) + CurrentLineLen
        ELSE
            markerStart = LEN(text) + 1
        END IF

    CASE "left"
        markerStart = markerStart - 1
        IF markerStart < 1 THEN markerStart = 1

    CASE "insert"
        InsertMode = True - InsertMode

    CASE "delete"
        text = LEFT$(text, markerStart - 1) + MID$(text, markerStart + 1)

    CASE "backspace"
        IF markerStart > 1 THEN
            text = LEFT$(text, markerStart - 2) + MID$(text, markerStart)
            markerStart = markerStart - 1
        END IF
    CASE ELSE
        'Anderer Tastendruck:
        ' -> Buchstaben hinzu! (Die Markierung wird durch Buchstaben ersetzt)
        IF NOT (iMaxZeichen > 0 AND LEN(text) + LEN(a$) > iMaxZeichen) THEN
            IF InsertMode THEN
                text = LEFT$(text, markerStart - 1) + a$ + MID$(text, markerStart + LEN(a$))
            ELSE
                text = LEFT$(text, markerStart - 1) + a$ + MID$(text, markerStart)
            END IF
            'Die Markierung erlischt!
            markerStart = markerStart + LEN(a$)
        END IF

    END SELECT

    GOTO zEditBoxCalculateCursor

zEditBoxTerminate:
    InversFarbe = 0
    PutString sName$, text
    Controls(iCtrl).Value = markerStart
    MID$(Controls(iCtrl).tag, 3, 2) = MKI$(topperLine)
END SUB

'Wird ausgefÅhrt, wenn auf ein Steuerelement geklickt wird
'(Die Maustaste wurde losgelassen...)
'iCtrl = Die ControlNr
'iButton = Die Maustaste (1 = links, 2 = rechts, -1 = Doppelclick links)
SUB zEventClick (iCtrl, iButton)

    tag$ = RTRIM$(Controls(iCtrl).tag)

    SELECT CASE Controls(iCtrl).CtrlType
    CASE zScrollBar
        ActiveCtrl = CVI(MID$(tag$, 6, 2))

    CASE zSliderBar, zButton
        zDrawThisControl iCtrl, False, 0

    CASE zExitFocus
        keyb = "passfocus"

    CASE zMouseClicker
        keyb = tag$
        zDrawThisControl iCtrl, False, 0

    END SELECT

END SUB

'Wird ausgefÅhrt, wenn auf ein Objekt doppelgeklickt wurde.
SUB zEventDblClick (iCtrl)

    SELECT CASE Controls(iCtrl).CtrlType
    CASE zListBox, zIconList
        keyb = "enter"
    END SELECT
END SUB

'Wird ausgefÅhrt, wenn ein Mausbutton auf einem Steuerelement
'heruntergedrÅckt wird.
'iCtrl = Die ControlNr
'iButton = Taste 1 = links, 2 = Rechts
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
                Mouse.clicked = iButton
                zDrawThisControl iCtrl, False, 0
                Mouse.clicked = 0
            END IF
        END IF

    CASE zScrollBar
      MID$(Controls(iCtrl).tag, 9, 1) = "1"
      IF (w > 0 AND Mouse.x < x + 12) OR (w = 0 AND Mouse.y < y + 12) THEN
        Controls(iCtrl).Value = Value - CVI(MID$(tag$, 10, 2))
        zDrawThisControl iCtrl, False, 0
        iCtrlParent = CVI(MID$(tag$, 6, 2))
        RaiseScroll iCtrl, RTRIM$(Controls(iCtrl).Caption)
      ELSEIF (w > 0 AND Mouse.x > x2 - 12) OR (w = 0 AND Mouse.y > y2 - 12) THEN
        Controls(iCtrl).Value = Value + CVI(MID$(tag$, 10, 2))
        zDrawThisControl iCtrl, False, 0
        iCtrlParent = CVI(MID$(tag$, 6, 2))
        RaiseScroll iCtrl, RTRIM$(Controls(iCtrl).Caption)
      ELSE
        MID$(Controls(iCtrl).tag, 9, 1) = "0"
        zEventMouseMove iCtrl, iButton
        EXIT SUB
      END IF
      IF iCtrlParent > 0 THEN
        ActiveCtrl = iCtrlParent
        keyb = "passfocus"
        Controls(iCtrlParent).Value = Controls(iCtrl).Value
        zDrawThisControl iCtrlParent, 1, 0
      END IF


    CASE zSliderBar
      zEventMouseMove iCtrl, iButton

    END SELECT

END SUB

'Wird ausgefÅhrt, wenn mit der Maus Åber ein Steuerelement gefahren wird.
'iCtrl = die ContrlNr des Steuerelementes
'iButton = Die gedrÅckte Maustaste (0 = keine, 1 = links, 2 = rechts)
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
            Mouse.clicked = True
            zDrawThisControl iCtrl, False, 0
            Mouse.clicked = False
        END IF

    CASE zSliderBar, zScrollBar
        IF zMouseDown = iCtrl AND iButton = 1 THEN
            Min = CVI(MID$(tag$, 2, 2))
            stp = CVI(MID$(tag$, 10, 2))
            Max = INT(((CVI(MID$(tag$, 4, 2)) - Min + 1) / stp) * -1) * -1 + 1
            'Keine Bewegungen akzeptieren, wenn zuvor auf einen der Pfeile
            'gedrÅckt wurde:
            IF MID$(Controls(iCtrl).tag, 9, 1) = "1" THEN EXIT SUB
            IF w > 0 THEN
                'horizontal:
                IF Typ = zScrollBar THEN x = x + 12: x2 = x2 - 12
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
                IF Typ = zScrollBar THEN y = y + 12: y2 = y2 - 12
                nv = (Mouse.y - y)
                IF nv > y2 - y THEN nv = y2 - y
                IF nv > 0 THEN
                    nv = Min + (nv * CLNG((Max - Min)) / (y2 - y))
                    nv = (nv * stp) - stp + (Value MOD stp)
                ELSE
                    nv = Min
                END IF
            END IF
            Max = CVI(MID$(tag$, 4, 2))
            IF nv > Max THEN nv = Max
            Controls(iCtrl).Value = nv
            iCtrlParent = CVI(MID$(tag$, 6, 2))
            IF Typ = zSliderBar THEN
                zDrawThisControl iCtrl, True, 0
            END IF
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

END SUB

'Liest die Mauskoordinaten aus
'b = Status des Mausbuttons
'x,y = Die Koordinaten der Maus
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

'Sichert die wichtigsten Variablen, oder setzt sie zurÅck. Beim setzen
'werden die originalen Variablen auf einen Standardwert gesetzt.
'iRestore:  False = Die Variablen werden gesichert
'           True = Die ursprÅnglichen Variablenwerte werden wieder zurÅckgeholt
'
SUB zGetOriginalValues (iRestore)
    SHARED l, t, r, d, TA, cx, cy, im
    IF iRestore THEN
        Left = l
        Top = t
        Right = r
        Down = d
        TextAlignment = TA
        CurrentX = cx
        CurrentY = cy
        InversFarbe = im
    ELSE
        l = Left
        t = Top
        r = Right
        d = Down
        TA = TextAlignment
        cx = CurrentX
        cy = CurrentY
        im = InversFarbe
        'Standardwerte werden gesetzt:
        InversFarbe = 0
        TextAlignment = AlignLeft
        Left = 2
        Top = 2
        Right = 637
        Down = 347
        CurrentX = 3
        CurrentY = 3
    END IF
END SUB

'Handelt Klick-Ereignisse des aktiven Standard-Steuerelementes
'sGroup$ = Die Steuerelemente-Gruppe
'iChanges = RÅckgabewert: TRUE = Es wurden énderungen vorgenommen.
SUB zHandleControls (sGroup$, iChanges)
iChanges = False
    SELECT CASE Controls(ActiveCtrl).CtrlType
    CASE zCheckBox
        'Wert wechselt:
        Controls(ActiveCtrl).Value = True - Controls(ActiveCtrl).Value
        zDrawThisControl ActiveCtrl, False, 0
        iChanges = True

    CASE zRadioButton
        'alle deaktivieren:
        FOR i% = 1 TO UBOUND(Controls)
            IF sGroup$ = "" OR RTRIM$(Controls(i%).Group) = sGroup$ THEN
                IF Controls(i%).CtrlType = zRadioButton THEN
                    IF Controls(i%).Value = True AND ActiveCtrl <> i% THEN
                        Controls(i%).Value = False
                        zDrawThisControl i%, False, 0
                    ELSEIF ActiveCtrl = i% THEN
                        Controls(i%).Value = True
                        zDrawThisControl i%, False, 0
                        iChanges = True
                    END IF
                END IF
            END IF
        NEXT
    END SELECT
END SUB

'Handler fÅr die Symbolliste
SUB zIconListHandler (sGroup$, iCtrl, iRefresh)
DIM Hintergrund(505)

l = Controls(iCtrl).x
t = Controls(iCtrl).y
r = Controls(iCtrl).x2
d = Controls(iCtrl).y2

sName$ = RTRIM$(Controls(iCtrl).Caption)

'aktiviertes Symbol:
sv = Controls(iCtrl).Value

'altes Sichtfenster:
otopper = CVI(MID$(Controls(iCtrl).tag, 1, 2))
MaxItems = CVI(MID$(Controls(iCtrl).tag, 5, 2))

topperIcon = 1
bottomIcon = 0
nr = 0
col = POINT(l, t)

IF Mouse.clicked = 2 THEN
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
refresh = False
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
kennung$ = MID$(a$, 13)
IF x + 41 > r OR x + LEN(kennung$) * 8 > r THEN x = l: y = y + 65
IF LEN(kennung$) < 6 THEN x = x + (6 - LEN(kennung$)) * 4
sx = x + INT((LEN(kennung$) * 8 - 41) / 2)
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
  IF Mouse.clicked AND iCtrl = ActiveCtrl THEN
    IF Mouse.x >= x AND Mouse.x <= x + LEN(kennung$) * 8 THEN
        IF Mouse.y > y - 43 AND Mouse.y <= y + 16 THEN
            sv = nr
            Mouse.clicked = 0
            mc = 0
            Value = nr
            GOTO zIcListAct
        END IF
    END IF
  END IF
END IF
  IF (Value < topperIcon OR Value > bottomIcon AND sv = 0) OR refresh THEN
    Farbe = tcol
    InversFarbe = col
    zWriteTextXY x, y + 2, kennung$
    InversFarbe = 0
    IF col > 0 THEN c = col ELSE c = icfcol
    VIEW (sx, y - 42)-(sx + 40, y - 1), c, c
    IF oldicon$ <> name$ THEN
        IF LCASE$(RIGHT$(name$, 3)) = "icn" THEN
          VIEW
          zShowPicture sx, y - 42, laosdir + "icons\" + name$
        ELSE 'lsb
          VIEW (sx, y - 42)-(sx + 40, y - 1), c, c
          zShowPicture 0, 0, laosdir + "icons\" + name$
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
x = x + LEN(kennung$) * 8 + 16
nr = nr + 1
LOOP
IF sv <> 0 THEN
    IF sv <> Value AND Value < MaxItems THEN
        Value = Value + 1
    ELSE
        sv = 0
        nr = topperIcon
        IF otopper <> topperIcon THEN
            refresh = True
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
zDrawThisControl sb, 0, 0
'Die Scrollbar hat die Schrittweite der aktuellen Seite:
MID$(Controls(sb).tag, 10, 2) = MKI$(bottomIcon - topperIcon + 1)

Controls(iCtrl).Value = Value
MID$(Controls(iCtrl).tag, 1, 2) = MKI$(topperIcon)

zIcListPopUp:
IF ppm AND ActiveCtrl = iCtrl THEN zRspm iCtrl: ppm = 0: Mouse.ClickX = 0: EXIT SUB

MID$(Controls(iCtrl).tag, 5, 2) = MKI$(MaxItems)

IF iRefresh THEN EXIT SUB

a$ = GetKey(sGroup$)

IF Value <> Controls(iCtrl).Value THEN
    Value = Controls(iCtrl).Value
    topperIcon = CVI(MID$(Controls(iCtrl).tag, 1, 2))
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

'Aktiviert das vorherige Control
'Es werden dabei nur Controls mit demselben Typ wie das aktive Control
'beinflusst.
SUB zLastControl (sGroup$)
    iOld = ActiveCtrl
    'Suche nÑchstes Control:
    FOR z% = ActiveCtrl - 1 TO 1 STEP -1
        IF RTRIM$(Controls(z%).Group) = sGroup$ OR sGroup$ = "" THEN
            IF Controls(z%).CtrlType = Controls(iOld).CtrlType THEN
                ActiveCtrl = z%
                zDrawThisControl iOld, False, 0
                zDrawThisControl z%, False, 0
                EXIT FOR
            END IF
        END IF
    NEXT
END SUB

'Lauscht auf Maus-Events
'- Setzt den Fokus auf die Caption des Steuerelementes, auf dieses geklickt wurde!
'- Gibt True zurÅck, falls auf ein Steuerelement geklickt wurde!
'sGroup$ = Die Steuerelement-Gruppe, deren Mausereignisse abgefragt werden
'         ("" = alle)
FUNCTION zLauscheEvents% (sGroup$)
    IF MouseOn = 0 THEN EXIT FUNCTION

    zLauscheEvents% = False
    iOverControl = False

    DEF SEG = VARSEG(iMS(0))
    POKE VARPTR(iMS(0)) + 26, &H33
    CALL ABSOLUTE(3, b, x, y, VARPTR(iMS(0)))

    IF x + 14 > 639 THEN x2 = x + (639 - x) ELSE x2 = x + 14
    IF y + 24 > 349 THEN y2 = y + (349 - y) ELSE y2 = y + 24

    IF (Mouse.oldx <> x OR Mouse.oldy <> y AND zMHG(0) > 0) OR Mouse.Cursor < 0 THEN
        IF Mouse.Cursor < 0 THEN Mouse.Cursor = Mouse.Cursor * -1
        PUT (Mouse.oldx, Mouse.oldy), zMHG, PSET
        GET (x, y)-(x2, y2), zMHG

        SELECT CASE Mouse.Cursor
        CASE 1: DRAW "BM" + STR$(x) + "," + STR$(y) + zMCS
        CASE 2: DRAW "BM" + STR$(x) + "," + STR$(y) + "c" + STR$(tbtcol) + "r2l1d13l1r2"
        END SELECT
    END IF

    Mouse.x = x
    Mouse.y = y

    IF x <> Mouse.oldx AND y <> Mouse.oldy THEN Mouse.time = TIMER

    IF MouseOn = 2 THEN PUT (x, y), zMHG, PSET: MouseOn = -1: i% = ActiveCtrl: GOTO zLEClick

    'Gehe alle Controls durch, und schaue auf Events:
    FOR i% = UBOUND(Controls) TO 1 STEP -1
      IF Controls(i%).CtrlType > 0 THEN
            IF RTRIM$(Controls(i%).Group) = sGroup$ OR sGroup$ = "" THEN
                IF x >= Controls(i%).x AND x <= Controls(i%).x2 THEN
                    IF y >= Controls(i%).y AND y <= Controls(i%).y2 THEN
                        'Maus ist Åber einem Control positioniert!
                        iOverControl = True
                        IF zMouseDown = i% AND b = 0 THEN
                          'Maustaste wurde auf dem Objekt losgelassen!
                          ' - Click-Ereignis!
                            PUT (x, y), zMHG, PSET
                            zMouseDown = 0
                            ActiveCtrl = i%
                            Focus = RTRIM$(Controls(i%).Caption)
                            IF Controls(i%).CtrlType > zControlTypeCount THEN
                                keyb = "nofocus"
                            END IF
                            Mouse.clicked = Mouse.Down
                            Mouse.Down = 0
                            zEventClick i%, Mouse.clicked
                            'PrÅfe auf Doppelklick:
                            IF Mouse.time > 0 AND Mouse.clicked = 1 THEN
                              IF TIMER - Mouse.time < .8 THEN
                                IF x > Mouse.ClickX - 2 AND x < Mouse.ClickX + 2 THEN
                                  IF y > Mouse.ClickY - 2 AND y < Mouse.ClickY + 2 THEN
zLEClick:
                                    IF x >= Controls(ActiveCtrl).x AND x <= Controls(ActiveCtrl).x2 THEN
                                      IF y >= Controls(ActiveCtrl).y AND y <= Controls(ActiveCtrl).y2 THEN
                                        Mouse.clicked = -1
                                        zEventDblClick i%
                                      END IF
                                    END IF
                                  END IF
                                END IF
                              END IF
                            END IF
                            Mouse.time = TIMER
                            x = Mouse.x
                            y = Mouse.y
                            GET (x, y)-(x2, y2), zMHG
                            SELECT CASE Mouse.Cursor
                            CASE 1: DRAW "BM" + STR$(x) + "," + STR$(y) + zMCS
                            CASE 2: DRAW "BM" + STR$(x) + "," + STR$(y) + "c" + STR$(tbtcol) + "r2l1d13l1r2"
                            END SELECT
                            IF Mouse.clicked THEN zLauscheEvents% = True
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
                                iOld = ActiveCtrl
                                IF Controls(i%).CtrlType <= zControlTypeCount THEN
                                    ActiveCtrl = i%
                                END IF
                                PUT (x, y), zMHG, PSET
                                zMouseDown = i%
                                Mouse.Down = b
                                IF NOT Controls(i%).SelfHandler THEN zDrawThisControl i%, True, 0
                                IF NOT Controls(iOld).SelfHandler AND iOld <> i% THEN zDrawThisControl iOld, False, 0
                                zEventMouseDown i%, b
                                x = Mouse.x
                                y = Mouse.y
                                GET (x, y)-(x2, y2), zMHG
                                SELECT CASE Mouse.Cursor
                                CASE 1: DRAW "BM" + STR$(x) + "," + STR$(y) + zMCS
                                CASE 2: DRAW "BM" + STR$(x) + "," + STR$(y) + "c" + STR$(tbtcol) + "r2l1d13l1r2"
                                END SELECT
                                IF keyb <> "" THEN zLauscheEvents% = True
                            'Mousedown wiederholen nach .5 Sekunden...
                            ELSEIF TIMER - Mouse.time > .5 AND Controls(i%).CtrlType = zScrollBar THEN
                              Mouse.time = TIMER
                              GOTO zLEMd
                            END IF
                            GOTO zLEventsTerminate

                        ELSEIF b = 0 THEN
                            Mouse.clicked = 0
                            'Taste ist nicht gedrÅckt!
                            IF Mouse.x <> Mouse.oldx OR Mouse.y <> Mouse.oldy THEN
                                PUT (x, y), zMHG, PSET
                                zEventMouseMove i%, b
                                x = Mouse.x
                                y = Mouse.y
                                GET (x, y)-(x2, y2), zMHG
                                SELECT CASE Mouse.Cursor
                                CASE 1: DRAW "BM" + STR$(x) + "," + STR$(y) + zMCS
                                CASE 2: DRAW "BM" + STR$(x) + "," + STR$(y) + "c" + STR$(tbtcol) + "r2l1d13l1r2"
                                END SELECT
                            ELSE
                              IF TIMER - Mouse.time > 1 THEN
                                RaiseToolTipText i%, RTRIM$(Controls(i%).Caption)
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
            zDrawThisControl zMouseDown, False, 0
            GET (x, y)-(x2, y2), zMHG
            SELECT CASE Mouse.Cursor
            CASE 1: DRAW "BM" + STR$(x) + "," + STR$(y) + zMCS
            CASE 2: DRAW "BM" + STR$(x) + "," + STR$(y) + "c" + STR$(tbtcol) + "r2l1d13l1r2"
            END SELECT
            zMouseDown = 0
        END IF
    ELSE
        'Control ziehen und ablegen... (Mousemove mit gedrÅckter Taste)
        IF (x <> Mouse.oldx OR y <> Mouse.oldy) AND zMouseDown > 0 THEN
            PUT (x, y), zMHG, PSET
            zEventMouseMove zMouseDown, b
            GET (x, y)-(x2, y2), zMHG
            SELECT CASE Mouse.Cursor
            CASE 1: DRAW "BM" + STR$(x) + "," + STR$(y) + zMCS
            CASE 2: DRAW "BM" + STR$(x) + "," + STR$(y) + "c" + STR$(tbtcol) + "r2l1d13l1r2"
            END SELECT
            IF keyb <> "" THEN zLauscheEvents% = True
        END IF
    END IF
    Mouse.oldx = x
    Mouse.oldy = y
END FUNCTION

SUB zListboxHandler (sGroup$, iCtrl, x, y, x2, y2, iAnzSpalten, iAnzZeilen, iRefreshOnly)

DIM sName$, Value

'Der markierte Eintrag:
Value = Controls(iCtrl).Value

'Der Controlname:
sName$ = Controls(iCtrl).Caption

'Checked:
IF MID$(Controls(iCtrl).tag, 2, 1) = "C" THEN iCheckBoxes = True

'MenuStyle:
MenuStyle = MID$(Controls(iCtrl).tag, 1, 1) = "M"

sb = CVI(MID$(Controls(iCtrl).tag, 3, 2))
MaxItems = CVI(MID$(Controls(sb).tag, 4, 2))

zListBoxReDraw:

IF Value <= 0 THEN Value = 1
IF Value > MaxItems AND MaxItems > 0 THEN Value = MaxItems

iTopvisible = (FIX((Value - 1) / iAnzZeilen) * iAnzZeilen) + 1
iDownVisible = iTopvisible + iAnzZeilen - 1

IF iCtrl = ActiveCtrl AND Mouse.clicked THEN vstop = True ELSE vstop = False

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
                        vstop = False
                        IF NOT iRefreshOnly AND iCheckBoxes AND Mouse.x < x + 17 THEN
                            'CheckBox wurde angeklickt
                            zRessourceSet sName$, z%, "", AutoVal
                            checked = True - checked
                        END IF
                    END IF
                END IF
            END IF
            IF z% = Value AND a$ <> "-" AND NOT vstop THEN
                Farbe = lbtcol
                IF (NOT iRefreshOnly AND NOT MenuStyle) OR iCtrl = ActiveCtrl THEN
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
              zWriteTextXY x + 2, y + ytop, LEFT$(a$, iAnzSpalten)
            ELSE
              LINE (x + 2, y + ytop + 7)-(x2 - 2, y + ytop + 7), Farbe
            END IF
            COLOR Farbe
            IF iCheckBoxes THEN
                IF checked = True THEN
                    LINE (x + 3, y + ytop + 1)-STEP(11, 11), , B
                    LINE (x + 3, y + ytop + 1)-STEP(11, 11)
                    LINE (x + 3, y + ytop + 12)-STEP(11, -11)
                ELSEIF checked = False THEN
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
zDrawThisControl sb, 0, 0

'RestoreScreen
IF iRefreshOnly THEN
    'Value zurÅckspeichern:
    Controls(iCtrl).Value = Value
    EXIT SUB
END IF

Controls(iCtrl).Value = Value

a$ = GetKey(sGroup$)
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
        found = False
        FOR i% = Value TO MaxItems
            zRessourceGet sName$, i%, a$, 0
            IF LCASE$(LEFT$(a$, LEN(keybuffer$))) = keybuffer$ THEN Value = i%: found = True: EXIT FOR
        NEXT
        IF NOT found THEN
            FOR i% = 1 TO Value - 1
                zRessourceGet sName$, i%, a$, 0
                IF LCASE$(LEFT$(a$, LEN(keybuffer$))) = keybuffer$ THEN Value = i%: found = True: EXIT FOR
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
iRefreshOnly = True
iCtrl = 0
GOTO zListBoxReDraw

END SUB

'Bewegt einen Bildbereich von einem Ort an den anderen
'x1,y1,x2,y2 = Die Koordinaten des zu verschiebenden Bildbereiches
'toX,toY = Die Koordinaten, wo der Bildbereich hinkommen soll.
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

'Aktiviert das nÑchste Control
'Es werden dabei nur Controls mit demselben Typ wie das aktive Control
'beinflusst.
SUB zNextCtrl (sGroup$)
    iOld = ActiveCtrl
    'Suche nÑchstes Control:
    FOR z% = ActiveCtrl + 1 TO UBOUND(Controls)
        IF RTRIM$(Controls(z%).Group) = sGroup$ OR sGroup$ = "" THEN
            IF Controls(z%).CtrlType = Controls(iOld).CtrlType THEN
                ActiveCtrl = z%
                zDrawThisControl iOld, False, 0
                zDrawThisControl z%, False, 0
                EXIT FOR
            END IF
        END IF
    NEXT
END SUB

'Gibt True zurÅck, wenn das Steuerelement die Kontrolle abgeben soll.
FUNCTION zQuitKey
  SELECT CASE keyb
  CASE "esc", "enter", "tab", "passfocus", "alt", "popup"
    zQuitKey = True
  CASE IS <> ""
    zQuitKey = (INSTR("," + ShortKeys + ",", "," + keyb + ",") > 0)
  END SELECT
END FUNCTION

DEFSNG A-Z
'Ersetzt im text$ den Ausdruck find$ durch den Ausdruck replce$
SUB zReplace (text$, find$, replce$)
    DO
        i = i + 1
        IF MID$(text$, i, LEN(find$)) = find$ THEN
            text$ = LEFT$(text$, i - 1) + replce$ + MID$(text$, i + LEN(find$))
            i = i + LEN(find$)
        END IF
    LOOP UNTIL i > LEN(text$)
END SUB

DEFINT A-Z
'Liest den Text und den Wert einer Ressource aus
'sGroup$ = Die Ressourcengruppe
'index = Der Index der Ressource, die abgerufen werden soll
'text$ = RÅckgabewert fÅr den Text
'value = RÅckgabewert fÅr den Wert
SUB zRessourceGet (sGroup$, Index, text$, Value)
SHARED zoldsgroup AS STRING * 8
    text$ = ""
    Value = 0
    DIM rsc AS zRessourceType
    IF RTRIM$(zoldsgroup) <> RTRIM$(sGroup$) OR LOC(RscFile) < 0 THEN
        CLOSE RscFile
        OPEN tempdir + RTRIM$(sGroup$) + ".rs~" FOR BINARY AS RscFile LEN = LEN(rsc)
        zoldsgroup = sGroup$
    END IF
    GET RscFile, (Index - 1) * LEN(rsc) + 1, rsc
    IF rsc.text <> STRING$(80, 0) THEN text$ = RTRIM$(rsc.text)
    Value = rsc.Value
END SUB

'fÅgt eine Ressource hinzu. Siehe auch: ->zRessourceGet
'sGroup$ = Die Ressourcengruppe, zu der der Eintrag gehîrt. (max 8 Zeichen)
'index = Der Index, an dessen Position der Eintrag gespeichert werden soll (>0)
'sText = Der Text, der hinzugefÅgt werden soll. (max 80 Zeichen)
'value = Der Wert, der der Ressource zugewiesen werden soll.
'Wichtig:
'-Falls Value auf AutoVal gesetzt wird, so wird Value von False (0) auf True
' (-1) gesetzt und umgekehrt.
'-Falls text$ = "", dann wird der alte Text belassen, und nur der Value auf
' den neuen Wert gesetzt.
SUB zRessourceSet (sGroup$, Index, text$, Value)
SHARED zoldsgroup AS STRING * 8
    IF Index < 1 THEN Dialog "UngÅltiger Index: " + STR$(Index)
    'Ressourcen in File speichern:
    DIM rsc AS zRessourceType
    IF RTRIM$(zoldsgroup) <> RTRIM$(sGroup$) OR LOC(RscFile) < 0 THEN
        CLOSE RscFile
        OPEN tempdir + RTRIM$(sGroup$) + ".rs~" FOR BINARY AS RscFile LEN = LEN(rsc)
        zoldsgroup = sGroup$
    END IF
    IF LEN(text$) = 0 OR Value = AutoVal THEN
      GET RscFile, (Index - 1) * LEN(rsc) + 1, rsc
    END IF
    IF Value = AutoVal THEN Value = True - rsc.Value
    rsc.Value = Value
    IF LEN(text$) > 0 THEN rsc.text = text$
    PUT RscFile, (Index - 1) * LEN(rsc) + 1, rsc
END SUB

'Setzt einen Farbwert der Farbpalette auf einen bestimmten RGB-Wert.
'slot% = Ein Farbwert von 0 - 15
'r%, g%, b% = Der Farbwert in RGB
SUB zRGBPalette (slot%, r%, g%, b%)

SELECT CASE slot%
    CASE IS <= 5: s% = slot%
    CASE 6: s% = 20
    CASE 7: s% = 7
    CASE IS >= 8: s% = 48 + slot%
END SELECT

'Farben hineinschreiben:
    OUT &H3C8, s%
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%

END SUB

'Wird intern verwendet fÅr das aufrufen der PopupMenÅs.
SUB zRspm (iCtrl)
  RaisePopUpMenu iCtrl, RTRIM$(Controls(iCtrl).Caption), q
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
  FOR i% = 0 TO 3
    OUT &H3CE, 4
    OUT &H3CF, i%
    BSAVE tempdir + "temp" + ".sc" + CHR$(49 + i%), 0, bytes
  NEXT
ELSE
  PCOPY 0, 1
END IF

zGetMouse b, x, y
IF b <> 0 THEN EXIT SUB

DIM stars(100) AS startype
'Sterne setzen:
FOR i% = 1 TO UBOUND(stars)
    x% = INT(RND * 639)
    y% = INT(RND * 349)
    col% = 15
    stars(i%).x = x%
    stars(i%).y = y%
    stars(i%).col = col%
NEXT
CLS
COLOR , 0
DO
    nz! = TIMER + .1
    'Sterne zeichnen:
    FOR i% = 1 TO UBOUND(stars)
        IF stars(i%).col <> 0 THEN
            WHILE stars(i%).x > 315 AND stars(i%).x < 325 AND stars(i%).y > 170 AND stars(i%).y < 180
                stars(i%).x = INT(RND * 639)
                stars(i%).y = INT(RND * 349)
            WEND

            'alte Position Åbermalen:
            LINE ((stars(i%).x - 320) / 1.1 + 320 - stars(i%).float - 1, (stars(i%).y - 175) / 1.1 + 175 - stars(i%).float - 1)-((stars(i%).x - 320) / 1.1 + 320 + stars(i%).float + 1, (stars(i%).y - 175) / 1.1 + 175 + stars(i%).float + 1), 0, BF

            LINE (stars(i%).x, stars(i%).y - stars(i%).float)-(stars(i%).x, stars(i%).y + stars(i%).float), stars(i%).col
            LINE (stars(i%).x - stars(i%).float, stars(i%).y)-(stars(i%).x + stars(i%).float, stars(i%).y), stars(i%).col

            LINE (stars(i%).x + (stars(i%).float / 1.5), stars(i%).y + stars(i%).float / 1.5)-(stars(i%).x - stars(i%).float / 1.5, stars(i%).y - stars(i%).float / 1.5), stars(i%).col
            LINE (stars(i%).x - stars(i%).float / 1.5, stars(i%).y + stars(i%).float / 1.5)-(stars(i%).x + stars(i%).float / 1.5, stars(i%).y - stars(i%).float / 1.5), stars(i%).col
        END IF
    NEXT

    'Sterne verschieben:
    FOR i% = 1 TO UBOUND(stars)
        IF stars(i%).col <> 0 THEN
            stars(i%).x = (stars(i%).x - 320) * 1.1 + 320
            stars(i%).y = (stars(i%).y - 175) * 1.1 + 175
            stars(i%).float = stars(i%).float + .1
            SELECT CASE stars(i%).x
            CASE IS > 639, IS < 0
                'alte Position Åbermalen:
                LINE ((stars(i%).x - 320) / 1.1 + 320 - stars(i%).float - 1, (stars(i%).y - 175) / 1.1 + 175 - stars(i%).float - 1)-((stars(i%).x - 320) / 1.1 + 320 + stars(i%).float + 1, (stars(i%).y - 175) / 1.1 + 175 + stars(i%).float + 1), 0, BF

                x% = INT(RND * 639)
                y% = INT(RND * 349)
                col% = 15
                stars(i%).x = x%
                stars(i%).y = y%
                stars(i%).col = col%
                stars(i%).float = 0
            END SELECT
            SELECT CASE stars(i%).y
            CASE IS > 349, IS < 0
                'alte Position Åbermalen:
                LINE ((stars(i%).x - 320) / 1.1 + 320 - stars(i%).float - 1, (stars(i%).y - 175) / 1.1 + 175 - stars(i%).float - 1)-((stars(i%).x - 320) / 1.1 + 320 + stars(i%).float + 1, (stars(i%).y - 175) / 1.1 + 175 + stars(i%).float + 1), 0, BF

                x% = INT(RND * 639)
                y% = INT(RND * 349)
                col% = 15
                stars(i%).x = x%
                stars(i%).y = y%
                stars(i%).col = col%
                stars(i%).float = 0
            END SELECT

        END IF
    NEXT
    DO WHILE TIMER < nz!: LOOP

    zGetMouse b, nx, ny
LOOP UNTIL INKEY$ <> "" OR (nx <> Mouse.x) OR b <> 0 OR (ny <> Mouse.y)

IF SaveScreenMode THEN
    DEF SEG = &HA000
    FOR i% = 0 TO 3
        OUT &H3C4, 2
        OUT &H3C5, 2 ^ i%
        BLOAD tempdir + "temp" + ".sc" + CHR$(49 + i%), 0
    NEXT
ELSE
  PCOPY 1, 0
END IF
COLOR Farbe, bcol
END SUB

'öbergibt das Handeln eines Controls an eine externe Prozedur
'FÅr aufwÑndige Controls...
'sGroup$ = Die aktive Steuerelement-Gruppe
'iCtrlType = Der Typ des Steuerelementes
FUNCTION zSelfHandleControl (sGroup$, iCtrlType)

DIM text AS STRING

    i% = ActiveCtrl
    x = Controls(i%).x
    y = Controls(i%).y
    x2 = Controls(i%).x2
    y2 = Controls(i%).y2
    w = Controls(i%).w
    h = Controls(i%).h
    cap$ = Controls(i%).Caption
    Value = Controls(i%).Value
    zSelfHandleControl = False

    SELECT CASE iCtrlType
    CASE zIconList
        zIconListHandler sGroup$, i%, False
        zSelfHandleControl = True

    CASE zListBox
        zListboxHandler sGroup$, i%, x, y, x2, y2, w, h, False
        zSelfHandleControl = True

    CASE zEditBox
        zEditBoxHandler sGroup$, cap$, i%, x + 3, y + 1, x2, y2, w, h, text, False
        zSelfHandleControl = True

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
        OPEN laosdir + sCursor$ + ".cur" FOR BINARY AS f%
        GET f%, 1, l
        zMCS = SPACE$(l)
        GET f%, , zMCS
        CLOSE f%
    END IF

    'Muss Maschinenprogramm erst geladen werden?
    IF iMS(0) = 0 THEN
        RESTORE zMouseData
        DEF SEG = VARSEG(iMS(0))

        FOR i% = 0 TO 99
         READ Byte$
         IF Byte$ = "-" THEN EXIT FOR
         POKE VARPTR(iMS(0)) + i%, VAL("&H" + Byte$)
        NEXT i%
    END IF

    DEF SEG = VARSEG(iMS(0))

    'Maustreiber initialisieren:
    POKE VARPTR(iMS(0)) + 26, &H33
    CALL ABSOLUTE(0, 0, 0, 0, VARPTR(iMS(0)))


    'Mauszeiger unsichtbar:
    POKE VARPTR(iMS(0)) + 26, &H33
    CALL ABSOLUTE(2, 0, 0, 0, VARPTR(iMS(0)))

    'Maustempo setzen:
    POKE VARPTR(iMS(0)) + 26, &H33
    CALL ABSOLUTE(4, 0, Mouse.Speed, Mouse.Speed * 2, VARPTR(iMS(0)))

    'Mauscursor setzen:
    POKE VARPTR(iMS(0)) + 26, &H33
    CALL ABSOLUTE(4, 0, Mouse.x, Mouse.y, VARPTR(iMS(0)))

END SUB

'setzt die Originalpalette
SUB zSetOrigPalette
    RESTORE zColors
    FOR i% = 0 TO 15
        READ r, g, b
        zRGBPalette i%, r, g, b
    NEXT
END SUB

'Zeigt ein beliebiges Bild an.
'UnterstÅtzt werden die folgenden Formate:
'- *.ICO (Windows Symbole)
'- *.LDS (LaOS Draw Script)
'- *.LSB (LaOS LDS Symbole)
'xx,yy = Die Koordinaten geben an, wo das Bild gezeichnet wird.
'sFileName = Der Dateiname der Grafik
SUB zShowPicture (xx, yy, sFileName AS STRING)

    f% = FREEFILE
    sFileName = RTRIM$(sFileName)

    SELECT CASE LCASE$(RIGHT$(sFileName, 4))
    CASE ".icn"
      IF NOT exists(sFileName) THEN GOTO zSpnf
      DIM ico(0 TO 505)
      DEF SEG = VARSEG(ico(0))
      BLOAD sFileName, VARPTR(ico(0))
      LINE (xx, yy)-STEP(40, 41), 0, BF
      PUT (xx, yy), ico, XOR

    CASE ".lds", ".lsb"
zSpnf:
        DIM Befehle(5) AS SINGLE

        OPEN sFileName FOR BINARY AS f%: GET f%, 1, Befehle(0)
        IF Befehle(0) = 0 THEN CLOSE f%: OPEN laosdir + "icons\" + "sys.lsb" FOR BINARY AS f%
        SEEK f%, 1
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

    CASE ".ico"
        OPEN sFileName FOR BINARY AS f%

        SEEK #f%, 127
        b$ = " "
        GET #f%, , b$
        IF ASC(b$) <> 0 THEN
            'Neue Windows-Icons
            SEEK #f%, 143
        ELSE
            'Alte Windows-Icons
            SEEK #f%, 127
        END IF
        FOR y = 1 TO 32
            FOR x = 1 TO 32 STEP 2
                b$ = " "
                GET #f%, , b$
                col2 = ASC(b$) MOD 16
                col1 = (ASC(b$) - col2) / 16
                SELECT CASE col1
                CASE 11: col1 = 14
                CASE 10: col1 = 12
                CASE 14: col1 = 11
                CASE 12: col1 = 9
                CASE 8: col1 = 6 '0 8
                CASE 3: col1 = 4
                CASE 1: col1 = 4
                CASE 6: col1 = 3
                CASE 4: col1 = 1
                CASE 0: col1 = 8 '0
                END SELECT
                SELECT CASE col2
                CASE 11: col2 = 14
                CASE 10: col2 = 12
                CASE 14: col2 = 11
                CASE 12: col2 = 9
                CASE 8: col2 = 6 '0 '8
                CASE 3: col2 = 4
                CASE 1: col2 = 4
                CASE 6: col2 = 3
                CASE 4: col2 = 1
                CASE 0: col2 = 8 '0
                END SELECT

                IF NOT ((ASC(b$) - col2) / 16 = -.5 AND ASC(b$) MOD 16 = 0) THEN
                    PSET (x + 1, 32 - y), col2
                    PSET (x, 32 - y), col1
                END IF
            NEXT x
        NEXT y
    CASE ELSE
        GOTO zSpnf
    END SELECT

zShowPictureTerminate:
    CLOSE f%
    VIEW

END SUB

'Handler fÅr das Zeitereignis
'Nr = die Nr. des Ereignisses
SUB zTimeEventHandler (nr)

    of = Farbe
    om = InversFarbe

    SELECT CASE nr
    CASE 0  'Bildschirmschoner
        zScreenSaver

    CASE 1  'Zeit aktualisieren
        IF SilentScreenMode THEN EXIT SUB
        IF zDebugMode THEN
            LINE (2, 332)-(637, 347), btscol, BF
            LINE (2, 332)-(637, 332), Farbe
            Farbe = bttcol
            InversFarbe = btscol
            zWriteTextXY 3, 333, "Maximale Datenfeldgrîsse:" + STR$(FRE(-1)) + " Stack:" + STR$(FRE(-2)) + " Zeichenfolgen:" + STR$(FRE(0))
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
          zWriteTextXY x + 2, y, LEFT$(TIME$, 5)
          LINE (x, y)-STEP(43, y + 9), btrcol, B
        END IF
    END SELECT

    Farbe = of
    InversFarbe = om

END SUB

'Ruft ein Tooltiptext auf
'Text$  Der Text, der erscheinen soll...
'Tipp:
'In der SUB RaiseToolTipText kann fÅr ihre Controls ein Tooltiptext
'hinzugefÅgt werden.
SUB zToolTipText (text$)

  zGetOriginalValues False
  x = Mouse.x + 5
  y = Mouse.y + 5
  x2 = x + TextWidth(text$) + 4
  y2 = y + TextHeight(text$) + 2
  zGetOriginalValues True

  IF x2 > 637 THEN x = x + (637 - x2): x2 = x2 + (637 - x2)
  IF y2 > 347 THEN y = y + (347 - y2): y2 = y2 + (347 - y2)

  g = 4 + INT(((PMAP(x2, 0) - PMAP(x, 0) + 1) * (1) + 7) / 8) * 2 * (PMAP(y2, 1) - PMAP(y, 1) + 1)
  DIM bg(g)
  GET (x, y)-(x2, y2), bg
 
  Farbe = tttcol
  LINE (x, y)-(x2, y2), ttfcol, BF
  LINE (x, y)-(x2, y2), ttrcol, B
  Label x + 1, y + 1, x2, y2, text$, 0, ttfcol

  ax = Mouse.x
  ay = Mouse.y
 
  DO
  LOOP UNTIL INKEY$ = ""
  DEF SEG = VARSEG(iMS(0))
  DO
    zGetMouse b, nx, ny
    k = INP(&H60)
    zDoEvents
  LOOP UNTIL nx <> ax OR b <> 0 OR ny <> ay OR k < 128

  PUT (x, y), bg, PSET
  Mouse.time = TIMER

END SUB

'Schreibt einen Text ohne BerÅcksichtigung der Bereichsbegrenzungen und ohne
'Zeilenumbruch! Siehe auch ->Echo und ->EchoXY
'text = Text
'x, y = Die Position
SUB zWriteTextXY (x, y, text AS STRING)

IF LEN(text) = 0 THEN EXIT SUB

DIM g AS LONG

    IF x + TextLen(text) > 639 THEN x2 = 639 ELSE x2 = x + TextLen(text) - 1
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
    PRINT text
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

