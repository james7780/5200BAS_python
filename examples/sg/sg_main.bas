'
' Shooting Gallery
'            
'          an Atari 5200 game by Sebastian Mihai, 2012
'
' Move the joystick upwards to shoot!
' 
' Keep shooting down the targets until you get bored :)
' This example game demonstrates sprites, missiles, joystick input, collision, and sounds
'
' Quirk: Do _NOT_ use tabs in your source code. Only use spaces, or the compilation will fail
'

'variable definitions
'counters
DEFINE I,$FF
DEFINE J,$FE
DEFINE K,$FD

'player X
DEFINE PX,$FC

'bullet coordinates
DEFINE BX,$FB
DEFINE BY,$FA

'targets X coordinates
DEFINE TX1,$F9
DEFINE TX2,$F8
DEFINE TX3,$F7
'targets direction (0=left, 1=right)
DEFINE TD1,$F6
DEFINE TD2,$F5
DEFINE TD3,$F4

'sound counter - counts down after the volume is turned on for channel 0
'once it reaches 0, the volume is turned off, so the sound is only heard
'for a little while
DEFINE SNDCNT,$F3

'TITLE "Shooting Gallery"
TITLE OFF
SCREEN 7:SPRITES ON

' Colour information is stored in one byte
'[7 6 5 4  3 2 1 0]
'[  hue   | brightness ]
'
' hue     : 0=grey, 1=gold, 2=gold-orange, 3=red-orange, 4=orange, 5=magenta, 6=purple-blue
'           7=blue, 8=blue, 9=cyan, 10=blue-green, 11=blue-green, 12=green, 13=yellow-green
'           14=yellow, 15=yellow-red
'
' brightness: 0, 2, 4, 6, 8, 10, 12, 14 (only even values permitted)

'Sprite/missile palettes (5 is for first, 6 for second, etc.)
PALETTE 5,$1E:PALETTE 6,$6A:PALETTE 7,$96:PALETTE 8,$B6
'Background palette
PALETTE 0,$C0


'the accumulator, A,  is often used for simple arithmetic operations, and to read in values
'from various ports

'Initialize things

'player
PX = 75

'bullet begins on top of the player, or "offscreen"
A = PX
A = A + 2
BX = A

'any value less than 6 puts the bullet "offscreen"
BY = 0

TX1 = 10
TX2 = 50
TX3 = 90

TD1 = 1
TD2 = 1
TD3 = 1

drawplayer:
    A = PX
    A = A - 1
    PX = A
    PUT (PX,200),$93,0,8         'draw player    
    A = PX
    A = A + 1
    PX = A

mainloop:

    'decrement sound counter
    A = SNDCNT
    A = A - 1
    SNDCNT = A
    'once sound counter reaches 0, we stop the sound, so it doesn't last indefinitely
    IF A = 0 THEN: VOLUME 0, 2, 0: END IF
    
    'move bullet up if there's still room
    A = BY
    IF A > 5 THEN: A = BY: A = A - 5: BY = A: PUT (BX,BY),$90,4,8: END IF

    'if bullet is "offscreen", then draw it on top of player to hide it :D
    A = BY
    IF A < 6 THEN: A = PX: A = A + 3: K = A: PUT (K,200),$90,4,8: END IF
    
    'attempt to move targets to the right
    A = TD1
    IF A = 1 THEN: A = TX1: A = A + 1: TX1 = A: PUT (TX1,10),$96,1,8: END IF
    A = TD2
    IF A = 1 THEN: A = TX2: A = A + 1: TX2 = A: PUT (TX2,10),$96,2,8: END IF
    A = TD3
    IF A = 1 THEN: A = TX3: A = A + 1: TX3 = A: PUT (TX3,10),$96,3,8: END IF
    
    'attempt to move targets to the left
    A = TD1
    IF A = 0 THEN: A = TX1: A = A - 1: TX1 = A: PUT (TX1,10),$96,1,8: END IF
    A = TD2
    IF A = 0 THEN: A = TX2: A = A - 1: TX2 = A: PUT (TX2,10),$96,2,8: END IF
    A = TD3
    IF A = 0 THEN: A = TX3: A = A - 1: TX3 = A: PUT (TX3,10),$96,3,8: END IF

    'did targets reach right side of screen?
    A = TX1
    IF A = 150 THEN: TD1 = 0: END IF
    A = TX2
    IF A = 150 THEN: TD2 = 0: END IF
    A = TX3
    IF A = 150 THEN: TD3 = 0: END IF

    'did targets reach left side of screen?
    A = TX1
    IF A = 0 THEN: TD1 = 1: END IF
    A = TX2
    IF A = 0 THEN: TD2 = 1: END IF
    A = TX3
    IF A = 0 THEN: TD3 = 1: END IF
    
    
    A = JOYX(0)
    'was joystick moved right?
    IF A > 185 THEN:
        A = PX
        'check collision with right edge
        IF A < 150 THEN: 
            A = A + 1: PX = A: GOTO drawplayer: 
        END IF
    END IF
    
    'was joystick moved left?
    IF A < 38 THEN: 
        A = PX
        'check collision with left edge
        IF A > 2 THEN: 
            A = A - 1: PX = A: GOTO drawplayer: 
        END IF
    END IF
    
    A = JOYY(0)
    'was joystick moved upwards (shoot)?
    IF A < 38 THEN:
        A = BY
        'is the bullet not shown currently?
        IF A < 6 THEN: 
           BY = 200: A = PX: A = A + 2: BX = A: 
        END IF
    END IF
    
    
    A = BY
	'is the bullet shown?
    IF A >= 6 THEN:
        'is the bullet almost at the top?	
        IF A < 13 THEN:
            'did it hit target 1?
            X = BX
            IF X >= TX1 THEN:
                A = BX
                A = A - 6
                X = A
                IF X <= TX1 THEN: 
                    GOTO hittarget1
                END IF
            END IF
            
            'did it hit target 2?
            X = BX
            IF X >= TX2 THEN:
                A = BX
                A = A - 6
                X = A
                IF X <= TX2 THEN: 
                    GOTO hittarget2
                END IF
            END IF
            
            'did it hit target 3?
            X = BX
            IF X >= TX3 THEN:
                A = BX
                A = A - 6
                X = A
                IF X <= TX3 THEN: 
                    GOTO hittarget3
                END IF
            END IF
        END IF
    END IF
    
    	
GOTO mainloop

hittarget1:
    'play sound and move target off screen, so it reappears again soon
    SNDCNT = 60
    VOLUME 0, 8, 14
    SOUND 0, 50
    TX1 = 215

GOTO mainloop

hittarget2:
    'play sound and move target off screen, so it reappears again soon
    SNDCNT = 60
    VOLUME 0, 8, 14
    SOUND 0, 125
    TX2 = 215

GOTO mainloop

hittarget3:
    'play sound and move target off screen, so it reappears again soon
    SNDCNT = 60
    VOLUME 0, 8, 14
    SOUND 0, 200
    TX3 = 215

GOTO mainloop

'these are commented out and left here just as example calls
'num - 0-3 are sprites, 4-7 are missiles
'label is the segment where tiles begin ($9000 in this case, so segment is $90)
'PUT ( <x-coord>,<y-coord> ), <label>, <num>, [<height> OR -<height> for upside down]
'PUT (10,10),$90,7,5         'missile 3
'PUT (14,14),$90,6,5         'missile 2
'PUT (18,18),$90,5,5         'missile 1
'PUT (22,22),$90,4,5         'missile 0
'PUT (42,42),$90,0,5         'missile 0
'PUT (42,52),$90,1,5         'missile 0
'PUT (42,62),$90,2,12        'missile 0
'PUT (42,72),$90,3,5         'missile 0

	
'TILE INCLUDES GO HERE
.ORG    $9000
#INCLUDE sg_bullet.inc

.ORG    $9300
#INCLUDE sg_player.inc

.ORG    $9600
#INCLUDE sg_target.inc
