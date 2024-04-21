'Das Baujahr 18.01.1987

DECLARE SUB CLW ()
SCREEN 9, , 1, 1
COLOR 0, 63
CLS
CLW
x = 200: y = 130: l = 80: c = 6: B = 14: bb = 40: pp = 1
GOSUB tag: GOSUB gross: GOSUB load
c = 3: GOSUB datum: c = 4: GOTO zeit
datum:
' ---------------------- Datum ----------------------------------
t1$ = MID$(DATE$, 1, 1)
t2$ = MID$(DATE$, 2, 1)
t3$ = MID$(DATE$, 4, 1)
t4$ = MID$(DATE$, 5, 1)
t5$ = MID$(DATE$, 7, 1)
t6$ = MID$(DATE$, 8, 1)
t7$ = MID$(DATE$, 9, 1)
t8$ = MID$(DATE$, 10, 1)

y = 40: l = 30: B = 8: bb = 40:
LINE (215, 100)-(225, 110), c, BF
LINE (365, 100)-(375, 110), c, BF

x = 550: z = VAL(t8$): GOSUB go
x = 500: z = VAL(t7$): GOSUB go
x = 450: z = VAL(t6$): GOSUB go
x = 400: z = VAL(t5$): GOSUB go
x = 300: z = VAL(t2$): GOSUB go
x = 250: z = VAL(t1$): GOSUB go
x = 150: z = VAL(t4$): GOSUB go
x = 100: z = VAL(t3$): GOSUB go
   c = 1: y = 37
x = 553: z = VAL(t8$): GOSUB go
x = 503: z = VAL(t7$): GOSUB go
x = 453: z = VAL(t6$): GOSUB go
x = 403: z = VAL(t5$): GOSUB go
x = 303: z = VAL(t2$): GOSUB go
x = 253: z = VAL(t1$): GOSUB go
x = 153: z = VAL(t4$): GOSUB go
x = 103: z = VAL(t3$): GOSUB go
LINE (218, 98)-(228, 108), c, BF
LINE (368, 98)-(378, 108), c, BF
RETURN

zeit:
'----------------------------- Zeit --------------------------------
t1$ = MID$(TIME$, 1, 1)
t2$ = MID$(TIME$, 2, 1)
t3$ = MID$(TIME$, 4, 1)
t4$ = MID$(TIME$, 5, 1)
t5$ = MID$(TIME$, 7, 1)
t6$ = MID$(TIME$, 8, 1)

x = 550: y = 130: l = 40: B = 10: bb = 40: z = VAL(t6$): GOSUB go
x = 500: y = 130: l = 40:  B = 10: bb = 40: z = VAL(t5$): GOSUB go
x = 380: y = 130: l = 70:  B = 14: bb = 70: z = VAL(t4$): GOSUB go
x = 280: y = 130: l = 70:  B = 14: bb = 70: z = VAL(t3$): GOSUB go
x = 150: y = 130: l = 70: B = 14: bb = 70: z = VAL(t2$): GOSUB go
x = 50: y = 130: l = 70:  B = 14: bb = 70: z = VAL(t1$): GOSUB go
CIRCLE (250, 180), 6, c:  PAINT (250, 180), c
CIRCLE (250, 230), 6, c:  PAINT (250, 230), c

10 :
REM LOCATE 17, 67: PRINT TIME$
a$ = INKEY$
IF a$ = CHR$(27) THEN GOTO ende:

IF start = 0 THEN GOSUB load: PALETTE 14, pal4: PALETTE 0, pal3: PALETTE 1, pal2: PALETTE 4, pal: start = 1
IF TIME$ = "00:00:00" THEN c = 3: GOSUB datum: GOSUB tag: GOSUB gross: c = 4: star = 0:

t1$ = MID$(TIME$, 1, 1)
t2$ = MID$(TIME$, 2, 1)
t3$ = MID$(TIME$, 4, 1)
t4$ = MID$(TIME$, 5, 1)
t5$ = MID$(TIME$, 7, 1)
t6$ = MID$(TIME$, 8, 1)
c = 4: x = 550: y = 130: l = 40: B = 10: bb = 40: z = VAL(t6$): GOSUB go

IF sd = 1 THEN GOTO 30
IF t6$ = t12$ AND sd = 0 THEN  ELSE t12$ = t6$

30 :
IF t6$ = "0" THEN x = 500: y = 130: l = 40:  B = 10: bb = 40: z = VAL(t5$): GOSUB go
IF t5$ = "0" THEN x = 380: y = 130: l = 70:  B = 14: bb = 70: z = VAL(t4$): GOSUB go
IF t4$ = "0" THEN x = 280: y = 130: l = 70:  B = 14: bb = 70: z = VAL(t3$): GOSUB go
IF t3$ = "0" THEN x = 150: y = 130: l = 70:  B = 14: bb = 70: z = VAL(t2$): GOSUB go
IF t2$ = "0" THEN x = 50: y = 130: l = 70: B = 14: bb = 70: z = VAL(t1$): GOSUB go
sty = INT(RND * 300) + 1
stx = INT(RND * 500) + 1
du = INT(RND * 100) + 1
cr = INT(RND * 16) + 1
IF star = 1 THEN
CIRCLE (250, 180), 6, pal4:  PAINT (250, 180), pal4
CIRCLE (250, 230), 6, pal4:  PAINT (250, 230), pal4
END IF
GOTO 10

go:
IF z = 0 THEN bin$ = "1011111": bir = bir + 1
IF z = 1 THEN bin$ = "0000011"
IF z = 2 THEN bin$ = "1110110"
IF z = 3 THEN bin$ = "1110011"
IF z = 4 THEN bin$ = "0101011"
IF z = 5 THEN bin$ = "1111001"
IF z = 6 THEN bin$ = "1111101"
IF z = 7 THEN bin$ = "1000011"
IF z = 8 THEN bin$ = "1111111"
IF z = 9 THEN bin$ = "1111011"
IF MID$(bin$, 1, 1) = "1" THEN co = c ELSE co = 0
p = 0: FOR a = 1 TO B: p = p + pp
LINE (2 + x + p, y + a - 5)-(x + bb - p - 2, y + a - 5), co

NEXT:                                                                                                                             IF bi = 1 AND bir > 300 THEN CLS : SYSTEM
IF MID$(bin$, 2, 1) = "1" THEN co = c ELSE co = 0
p = 0: FOR a = 1 TO B / 2: p = p + pp
LINE (x + 6 + p, y + l + 2 + a)-(x + bb - 6 - p, y + l + 2 + a), co
NEXT
p = 0: FOR a = B / 2 TO 1 STEP -1: p = p + pp
LINE (x + 6 + p, y + l + a - (B / 2) + 2)-(x + bb - 6 - p, y + l + a - (B / 2) + 2), co
NEXT

IF MID$(bin$, 3, 1) = "1" THEN co = c ELSE co = 0
p = 0: FOR a = 1 TO B: p = p + pp
LINE (2 + x + p, y - a + l + l + 5)-(x + bb - p - 2, y - a + l + l + 5), co
NEXT:

IF MID$(bin$, 4, 1) = "1" THEN co = c ELSE co = 0
p = 0: FOR a = 1 TO B: p = p + pp
LINE (x + a, y + p)-(x + a, l + y - p), co
NEXT:

IF MID$(bin$, 5, 1) = "1" THEN co = c ELSE co = 0
p = 0: FOR a = 1 TO B: p = p + pp
LINE (x + a, 4 + y + p + l)-(x + a, l + l + y - p), co
NEXT

IF MID$(bin$, 6, 1) = "1" THEN co = c ELSE co = 0
p = 0: FOR a = 1 TO B: p = p + pp
LINE (bb + x - a, y + p)-(bb + x - a, l + y - p), co
NEXT

IF MID$(bin$, 7, 1) = "1" THEN co = c ELSE co = 0
p = 0: FOR a = 1 TO B: p = p + pp
LINE (bb + x - a, 4 + y + l + p)-(bb + x - a, l + l + y - p), co
NEXT
RETURN

ende:
'CHAIN "c:\laos7\run.bas"
RUN "c:\laos7\run.bas"
SYSTEM

tag:
REM DATE$ = "01-23-2000"
da$ = DATE$: da$(1) = MID$(da$, 1, 2): da$(2) = MID$(da$, 4, 2)
da$(3) = MID$(da$, 7, 4): da$ = da$(2) + "." + da$(1) + "." + da$(3)
REM da$ = "12.03.2000"
jahr = VAL(RIGHT$(da$, 2)): monat = VAL(MID$(da$, 4, 2)): tag = VAL(LEFT$(da$, 2))
DIM zahl$(260), a$(260), jaa$(260)
FOR ja = 1 TO jahr + 1
FOR jar = 1 TO 12
READ zahl
FOR r = 1 TO jahr + 1 STEP 4
IF jar = 2 AND ja = r THEN zahl = 29
NEXT
zahl$(jar) = STR$(zahl)
NEXT:
a$(1) = LTRIM$(zahl$(1)):
FOR an = 1 TO 12
a$(an + 1) = a$(an) + LTRIM$(zahl$(an + 1))
NEXT: jaa$ = a$(an):
jaa$(ja) = "  " + jaa$
RESTORE:  NEXT
FOR r = 1 TO jahr
FOR m = 3 TO 26 STEP 2
ze = VAL(MID$(jaa$(r), m, 2))
re = re + ze
NEXT: NEXT
FOR r = 3 TO (monat * 2) STEP 2
ze = VAL(MID$(jaa$(jahr + 1), r, 2))
re = re + ze
NEXT
re = re + tag
B = re
FOR c = 1 TO B STEP 7
NEXT
t = c - B
IF t = 5 THEN ta$ = "Montag": ta = 1
IF t = 4 THEN ta$ = "Dienstag": ta = 2
IF t = 3 THEN ta$ = "Mittwoch": ta = 3
IF t = 2 THEN ta$ = "Donnerstag": ta = 4
IF t = 1 THEN ta$ = "Freitag": ta = 5
IF t = 7 THEN ta$ = "Samstag": ta = 6
IF t = 6 THEN ta$ = "Sonntag": ta = 7
DATA 31,28,31,30,31,30,31,31,30,31,30,31
pr$ = ta$
y = 23: x = 2
x1 = (x * 8) - 12
y1 = (y * 13) + (y) - 14
x2 = (8 * LEN(pr$)) + (x * 8) - 4
y2 = y1 + 14
RETURN

gross:
PALETTE 7, 0
xa = 180:
IF t = 2 THEN xa = 130
pas = 2
ya = -155: text$ = pr$: GOSUB go2
RETURN

'--------------------------- TEXT GROSS -------------------------
go2:
'IF pas = 1 THEN GOTO go3
xbo = LEN(text$) * 37
LINE (xa + 5, 295)-(xa + xbo + 5, 340), 3, BF
LINE (xa, 290)-(xa + xbo, 335), 1, BF
LINE (xa, 290)-(xa + xbo, 335), 4, B
COLOR 7: LOCATE 22, 2: PRINT text$
cor = 14
x = 0: y = 0:
IF pas = 1 THEN g = 2:  g2 = 1
IF pas = 2 THEN g = 3: g2 = 4
FOR y2 = 232 TO 447
FOR x2 = 1 TO LEN(text$) * 8 + 6
IF POINT(x2, y2) = 7 THEN x = x2 * g2 - 10: y = y2 * g - 440: GOSUB text: GOTO 35: PSET (x2, y2), 4:
35 :
NEXT: NEXT
COLOR 7: LOCATE 22, 2: PRINT SPACE$(LEN(text$))
RETURN

text:
IF pas = 1 THEN
LINE (x + 2 + xa, y + 4 + ya)-(x + 0 + xa, y + g + 0 + ya), 4
LINE (x + 0 + xa, y + 3 + ya)-(x + 0 + xa, y + g - 0 + ya), cor, B
END IF

IF pas = 2 THEN
LINE (x + 0 + xa, y + 0 + ya)-(x + 2 + xa, y + g - 0 + ya), cor, BF
LINE (x + 0 + xa, y + 0 + ya)-(x + 2 + xa, y + g - 0 + ya), 4, B
END IF
RETURN

load:
pal = 4: pal2 = 1: pal3 = 63: pal4 = 62:
RETURN

SUB CLW
'Fenster:
LINE (2, 2)-(637, 347), 0, BF
LINE (0, 0)-(638, 348), 4, B
'Schatten:
LINE (1, 1)-(637, 1), 1
LINE (1, 1)-(1, 347), 1
'Rand:
LINE (639, 349)-(0, 349), 11
LINE (639, 349)-(639, 0), 11
END SUB

