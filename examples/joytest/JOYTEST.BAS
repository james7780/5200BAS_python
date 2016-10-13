TITLE "joystick test"
'if title text is in lower case it rainbows.  If caps = solid
AUTHOR "   (C) 2014 SIO2    "
'AUTHOR STATEMENT MUST CONTAIN 20 CHARS OR WILL NOT COMPILE
'Program Automatically procedes after TITLE display for a brief moment
'
'
'Value   Key
'$00 	0 
'$01 	1 
'$02 	2 
'$03 	3 
'$04 	4 
'$05 	5 
'$06 	6 
'$07 	7 
'$08 	8 
'$09 	9 
'$0A 	* 
'$0B 	# 
'$0C 	Start 
'$0D 	Pause 
'$0E 	Reset 
'$FF 	No key waiting 
'
'
DEFINE PX,$FC
DEFINE PY,$FA
SCREEN 2
SPRITES ON 
'PALETTE 5,$96 'CHANGES THE COLOR PLAYER1 AND MISSILE 1 TO BLUE


'player
PX = 128
PY = 128

GOSUB drawplayer0:
GOSUB drawplayer1:


CLS
LOCATE 1,1
PRINT "    5200 JOYSTICK TEST BY SIO2 2014"
DO
A=$FF
A=INKEY
SELECT CASE A 'process key
CASE $00
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "0"
CASE $01 
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "1"}
CASE $02 
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "2"}
CASE $03
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "3"}
CASE $04
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "4"}
CASE $05
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "5"}
CASE $06
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "6"}
CASE $07
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "7"}
CASE $08
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "8"}
CASE $09
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1 
  PRINT "9"}
CASE $0A
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "*"}
CASE $0B
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "#"}
CASE $0C
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "START"}
CASE $0D
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "PAUSE"}
CASE $0E
  LOCATE 3,1
  PRINT "          "
  LOCATE 3,1
  PRINT "RESET"}
END SELECT
A=JOYTRIG(0)
IF A=$01 THEN 
LOCATE 6,1
PRINT "        "
ELSE
LOCATE 6,1
PRINT "FIRE"
END IF
A=JOYTRIG2
IF A=$08 THEN 
LOCATE 5,1
PRINT "        "
ELSE
LOCATE 5,1
PRINT "TRIGGER"
END IF
    A = JOYX(0)
A = A/2  'DIVIDE BY 2 SO THAT RANGE FITS ON SCREEN
PX = A
GOSUB drawplayer0:   
    A = JOYY(0)
A = A/2
PY = A
GOSUB drawplayer1:  
ATTRACT
LOOP

drawplayer0:
    A = PX
    A = A - 1
    PX = A
    PUT (PX,56),$93,0,8         'draw player 0 (horizontal)
'56 BECAUSE 0 TO 255 MIDDLE IS 128 AND WE SCALED BY / 2 SO HALF 128 IS 64 MINUS HALF THE WIDTH IS 56   
    A = PX
    A = A + 1
    PX = A
RETURN

drawplayer1:
    A = PY
    A = A - 1
    PY = A
    PUT (59,PY),$90,4,8   'draw player 1 (vertical) actually missile (4)
'59 BECAUSE SAME AS ABOVE BUT LESS WIDTH.  
'LOOKS CENTER IN EMULATOR
    A = PY
    A = A + 1
    PY = A
RETURN