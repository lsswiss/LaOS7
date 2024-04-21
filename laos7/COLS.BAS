DECLARE SUB use (a!, b!, c!)
CLS
COLOR 12
use 0, 0, 255
use 255, 0, 255
use 0, 255, 255
use 255, 255, 255
use 0, 0, 128
use 128, 0, 128
use 0, 128, 128
use 128, 128, 128
use 2, 67, 250
use 240, 33, 21
use 135, 0, 124
COLOR 14
PRINT
use 68, 142, 68
PRINT "------------------------------------------------------------------------------"
COLOR 9
use 0, 128, 0
use 128, 128, 0

SUB use (a, b, c)
  'PRINT ((a + b) / 2) + ((b + c) / 2) + ((a + b + c) / 3)
  PRINT a, b, c,
  a = a + 1
  b = b + 1
  c = c + 1
  IF b > a THEN
    s1 = (b * 100 / a)
  ELSEIF a > b THEN
    s1 = (a * 100 / b) * -1
  ELSE
    s1 = 0
  END IF
  IF c > b THEN
    s2 = (c * 100 / b)
  ELSEIF b > c THEN
    s2 = (b * 100 / c) * -1
  ELSE
    s2 = 0
  END IF
  PRINT a * s1 + b * s2 + c * ((s1 + s2) / 2)
END SUB

