    processor 6502
    
    ; Include required files with VCS register memory mapping and macros
    include "vcs.h"
    include "macro.h"
    
    ; Declare the variables starting from memory address $80
    seg.u Variables
    org $80

JetXPos            byte    ; player0 x-position
JetYPos            byte    ; player0 y-position
BomberXPos         byte    ; player1 x-position
BomberYPos         byte    ; player1 y-position
MissileXPox        byte    ; missile x-position
MissileYPox        byte    ; missile y-position
Score              byte    ; 2-digit score stored as BCD
Timer              byte    ; 2-digit timer stored as BCD
Temp               byte    ; auxiliary variable to store temp values
OnesDigitOffset    word    ; lookup table offset for the score 1's digit
TensDigitOffset    word    ; lookup table offset for the score 10's digit
JetSpritePtr       word    ; pointer to player0 sprite table
JetColorPtr        word    ; pointer to player0 color table
BomberSpritePtr    word    ; pointer to player1 sprite table
BomberColorPtr     word    ; pointer to player1 color table
JetAnimationOffset word    ; player0 sprite frame offset for amination
Random             byte    ; random number generated
ScoreSprite        byte    ; store the sprite bit pattern for score
TimerSprite        byte    ; store the sprite bit pattern for timer
TerrainColor       byte    ; store the color of the terrain
RiverColor         byte    ; store the color of the river


    ; define constants
JET_HEIGHT    = 9          ; player0 sprite hegiht 
BOMBER_HEIGHT = 9          ; player1 sprite hegiht
DIGITS_HEIGHT = 5          ; scoreboard digit height

    ; Start our ROM code at memory address $F000
    seg code
    org $F000

Reset:
    CLEAN_START            ; class macro to reset memory and registers

    ; initialize RAM variables and TIA registers
    lda #10
    sta JetYPos            ; JetYPos = 10
    lda #60
    sta JetXPos            ; JetXPos = 60
    lda #83
    sta BomberYPos         ; BomberYPos = 83
    lda #54
    sta BomberXPos         ; BomberXPos = 54
    lda #%11010100
    sta Random             ; Random = $D4
    lda #0
    sta Score
    sta Timer              ; Score = Timer = 0

    ; Declare a macro to check if we display the missile
    MAC DRAW_MISSILE
        ;...x has the number of the scanline
        lda #%00000000
        cpx MissileYPox     ; compare x with missile y-position
        bne .SkipMissileDraw ; if x is not equal to missile y-position skip draw
.DrawMissile:
        lda #%00000010
        inc MissileYPox
.SkipMissileDraw:
        sta ENAM0
    ENDM

    ; initialize the pointers to the correct lookup table adresses
    lda #<PlayerSprite0
    sta JetSpritePtr       ; lo-byte pointer for jet sprite lookup table
    lda #>PlayerSprite0
    sta JetSpritePtr+1     ; hi-byte pointer for jet sprite lookup table

    lda #<PlayerColor0
    sta JetColorPtr        ; lo-byte pointer for jet color lookup table
    lda #>PlayerColor0
    sta JetColorPtr+1      ; hi-byte pointer for jet color lookup table

    lda #<BomberSprite
    sta BomberSpritePtr    ; lo-byte pointer for bomber sprite lookup table
    lda #>BomberSprite
    sta BomberSpritePtr+1  ; hi-byte pointer for bomber sprite lookup table

    lda #<BomberColor
    sta BomberColorPtr     ; lo-byte pointer for bomber color lookup table
    lda #>BomberColor
    sta BomberColorPtr+1   ; hi-byte pointer for bomber color lookup table

    ; Start the main display loop and frame rendering
StartFrame:
   


    ; Display VSYNC and VBLANK
    lda #2
    sta VBLANK            ; turn on VBLANK
    sta VSYNC             ; turn on VSYNC
    REPEAT 3
        sta WSYNC         ; display the 3 recomended line of VSYNC
    REPEND
    lda #0
    sta VSYNC             ; turn off VSYNC

    REPEAT 32
        sta WSYNC        ; display the 37 recomended line of VSYNC
    REPEND

;**************************************************************************************
    ; Calculate and tasks performed in the VBLANK
    lda JetXPos
    ldy #0
    jsr SetObjectXPos     ; set player0 horizonta position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos     ; set player1 horizonta position
    
    lda MissileXPox
    ldy #2
    jsr SetObjectXPos    ; set missile horizonta position

    jsr CalculateDigitOffset ; calculate the scoreboard digit lookup table offset
    
    jsr GeneratrJetSound     ; configure and enable JetEgnine audio

    sta WSYNC
    sta HMOVE             ; apply the horizonta positions

    lda #0
    sta VBLANK           ; turn off VBLANK

;*************************************************************************************
    ; Display the scoreboard lines
    lda #0               ; clear TIA registers before each frame
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF

    lda #$1E
    sta COLUPF
    
    ldx #DIGITS_HEIGHT   ; start the X counter whit the value 5

.ScoreDigitLoop:
    ldy TensDigitOffset     ; get the ten's digit offset for the score
    lda Digits,Y           ; load the bit pattern from lookup table
    and #$F0                ; mask the hight-bits
    sta ScoreSprite         ; save the score tens digit in a variable

    ldy OnesDigitOffset
    lda Digits,Y
    and #$0F
    ora ScoreSprite         ; add the new bits with the bit that i save before
    sta ScoreSprite         ; save it
    sta WSYNC               ; wait for scanline
    sta PF1

    ldy TensDigitOffset+1   ; get the left digit offset for the timer
    lda Digits,Y
    and #$F0
    sta TimerSprite

    ldy OnesDigitOffset+1
    lda Digits,Y
    and #$0F
    ora TimerSprite
    sta TimerSprite

    jsr Sleep12Cycles       ; waste 12 cycles

    sta PF1                 ; update playfield for timer display

    ldy ScoreSprite         ; preload for the next scanline
    sta WSYNC

    sty PF1                 ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles

    dex                     ; X--
    sta PF1
    bne .ScoreDigitLoop     ; if dex != 0, then branch to ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC


    ; Display the 96 visible scan lines of our main game (beacouse 2-line kernel) 
GameVisibleLines:
    lda TerrainColor
    sta COLUPF
    lda RiverColor
    sta COLUBK
    lda #%00000001       ; enable playfield reflection
    sta CTRLPF
    lda #$F0
    sta PF0
    lda #$FC       
    sta PF1
    lda #0
    sta PF2

    ldx #85               ; X count the number of remainig scanlines
.GameLineLoop:
    DRAW_MISSILE          ; macro to check if we should draw the missile

;********************************************************************************
.AreWeInsideJetSprite:
    txa                 ; transfer x to A
    sec                 ; make sure the carry flag is set before subtraction
    sbc JetYPos         ; subtract sprite Y-coord
    cmp #JET_HEIGHT      ; are we inside the sprite
    bcc .DrawSpriteP0   ; if result < SpriteHeight, call the draw routine
    lda #0              ; else, set lookup index to zero
.DrawSpriteP0:
    clc                     ; clear carry-flag before addition
    adc JetAnimationOffset  ; jump to the correct sprite frame address in memory 
    tay                     ; load Y so we can work whit the pointer
    lda (JetSpritePtr),Y    ; load player0 bitmap from lookup table
    sta WSYNC               ; wait for scanLine
    sta GRP0                ; set graphics for player0
    lda (JetColorPtr),Y     ; load player0 color from lookup table
    sta COLUP0              ; set color for player0
;********************************************************************************
.AreWeInsideBomberSprite:
    txa                    ; transfer x to A
    sec                    ; make sure the carry flag is set before subtraction
    sbc BomberYPos         ; subtract sprite Y-coord
    cmp #BOMBER_HEIGHT      ; are we inside the sprite
    bcc .DrawSpriteP1      ; if result < SpriteHeight, call the draw routine
    lda #0                 ; else, set lookup index to zero
.DrawSpriteP1:
    tay                     ; load Y so we can work whit the pointer
    
    lda #%00000101
    sta NUSIZ1              ; stretch player 1 sprite

    lda (BomberSpritePtr),Y ; load player1 bitmap from lookup table
    sta WSYNC               ; wait for scanLine
    sta GRP1                ; set graphics for player1
    lda (BomberColorPtr),Y  ; load player1 color from lookup table
    sta COLUP1              ; set color for player1
;********************************************************************************

    dex                  ; X--
    bne .GameLineLoop    ; repeat next main game scanline until finished

    lda #0
    sta JetAnimationOffset

    sta WSYNC

    ; Display Overscan
    lda #2
    sta VBLANK           ; turn on VBLANK again
    REPEAT 30
        sta WSYNC        ; Display 30 recomended scanlines
    REPEND
    lda #0
    sta VBLANK           ; turn off VBLANK

    ; Process joystick input for player0
CheckP0Up:
    lda #%00010000       ; player0 joystick up
    bit SWCHA
    bne CheckP0Down      ; if bit pattern doesnt match bypass
.P0IsUp:
    lda JetYPos
    cmp #70
    bpl CheckP0Down
    inc JetYPos
    lda #0
    sta JetAnimationOffset  

CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
.P0IsDown:
    lda JetYPos
    cmp #5
    bmi CheckP0Left
    dec JetYPos
    lda #0
    sta JetAnimationOffset 

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
.P0IsLeft:
    lda JetXPos
    cmp #35
    bmi CheckP0Right
    dec JetXPos
    lda #JET_HEIGHT           ; 9
    sta JetAnimationOffset   ; set JetAnimationOffset to the second frame

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne CheckButtonPress
.P0IsRight:
    lda JetXPos
    cmp #100
    bpl CheckButtonPress
    inc JetXPos
    lda #JET_HEIGHT          ; 9
    sta JetAnimationOffset  ; set JetAnimationOffset to the second frame

CheckButtonPress:
    lda #%10000000
    bit INPT4
    bne EndInput
.ButtonIsPress:
    lda JetXPos           ; load the x and y position of the Jet to set the Missile position
    clc
    adc #4
    sta MissileXPox
    lda JetYPos
    clc
    adc #7
    sta MissileYPox

EndInput:               ; fallback when no input

    ; Calculations to update positions form next frame
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0                          ; compare bomber Y position whit zero
    bmi .ResetBomberPosition        ; if is < 0, reset y pos back to the top
    dec BomberYPos                  ; else, decrement enemy y position
    jmp EndPositionUpdate
.ResetBomberPosition:
    jsr GetRandomBomberPosition     ; call subroutine for next random enemy x position

    ; use BCD to show friendly decimal values for the score and timer
.SetScoreValues:
    sed                      ; set decimal mode for score and timer

    lda Timer
    clc
    adc #1
    sta Timer

    cld                     ; disble decimal mode 

EndPositionUpdate:          ; fallback for the position update

    ; Check for object collitision
CheckCollitionP0P1:
    lda #%10000000         ; CXPPMM bit 7 detects P0 and P1 collition
    bit CXPPMM             ; check CXPPMM bit 7 with the above pattern
    bne .CollitionP0P1     ; collition hapend
    jsr SetTerrainRiverColor ; set playfield color to green and blue
    jmp CheckCollitionM0P1
.CollitionP0P1:
    jsr GameOver

CheckCollitionM0P1:
    lda #%10000000
    bit CXM0P
    bne .CollitionM0P1
    jmp EndCollitionCheck
.CollitionM0P1:
    sed
    lda Score
    clc
    adc #1
    sta Score            ; adds one to the score using decimal mode
    cld
    lda #0
    sta MissileYPox

EndCollitionCheck:         ; fallback
     sta CXCLR             ; reset collition flags

    ; Loop back to Start a brand new frame
    jmp StartFrame       ; continue to display next frame

; subroutine to generate audio for the Jet base on the Jet y-position
GeneratrJetSound subroutine
    lda #3
    sta AUDV0 

    lda JetYPos
    lsr 
    lsr
    lsr
    sta Temp
    lda #25
    sec
    sbc Temp
    sta AUDF0

    lda #8
    sta AUDC0

    rts


SetTerrainRiverColor subroutine
    lda #$C2
    sta TerrainColor
    lda #$84
    sta RiverColor
    rts

; subroutine to handle horizonta position
SetObjectXPos subroutine
    sta WSYNC            ; start a fresh new scanline
    sec                  ; make sure carry-flag is set before subs
.Div15Loop
    sbc #15              ; subtract 15 from the accumulator
    bcs .Div15Loop       ; loop until carry-flag is clear
    eor #7               ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                  ; four shift lefts to get only the top 4 bits
    sta HMP0,Y           ; store the fine offset to the correct HMxx
    sta RESP0,Y          ; fix object position in 15-step increment
    rts

; game over subroutine
GameOver subroutine
    lda #$30
    sta TerrainColor       ; set terrain color to red
    sta RiverColor         ; set river color to red
    lda #0
    sta Score              ; Score = 0
    rts

; subroutine to generate random number
GetRandomBomberPosition subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random
    lsr
    lsr                  ; divide the value by 4 with 2 right shift
    sta BomberXPos       ; save it to the variable BomberXPos
    lda #30
    adc BomberXPos       ; adds 30 + BomberXPos to compensate for left PF
    sta BomberXPos       ; sets the new value to the BomberXPos

    lda #96
    sta BomberYPos       ; sets the BomberYPos to the top of the screen
    rts

; subroutine to handle scoreboard digits
; convert the hegiht and low nibbles of the variable Score and Timer
; into the offset if digits lookup table so the values cn by display

; for the low nibble we need to mutiply by 5
CalculateDigitOffset subroutine
    ldx #1                  ; X register is the loop counter
.PrepareScoreLoop:          ; this will loop twice, first X=1, and then X=0
    lda Score,X             ; load A whit the Timer when X=1 or Score when X=0
    and #$0F                ; remove the 10's digit by masking the bits 00001111
    sta Temp                ; save the value of the acumulator in to the Temp variable
    asl                     ; shift left
    asl                     ; shift left N*4
    adc Temp                ; add the value saved in A
    sta OnesDigitOffset,X   ; save A in OnesDigitOffset+1 or OnesDigitOffset+0

    lda Score,X             ; load A whit the timer
    and #$F0                ; remove the 1's digit by masking bits 11110000
    lsr                     ; shift right n/2
    lsr                     ; shift right n/4
    sta Temp
    lsr
    lsr
    adc Temp                ; add the values (n/16 + n/4)
    sta TensDigitOffset,X  ; save A in TensDigitOffset+1 or TensDigitOffset+0

    dex                     ; X--
    bpl .PrepareScoreLoop   ; while X >= 0, loop to pass a second time 
    rts
;Subroutine to waste 12 cycles
;jsr = 6 cycles
;rts = 6 cycles 
Sleep12Cycles subroutine
    rts
    

    ; Declare ROM lookup tables
Digits:
    .byte %01110111
    .byte %01010101
    .byte %01010101
    .byte %01010101
    .byte %01110111

    .byte %00010001
    .byte %00010001
    .byte %00010001
    .byte %00010001
    .byte %00010001

    .byte %01110111
    .byte %00010001
    .byte %01110111
    .byte %01000100
    .byte %01110111

    .byte %01110111
    .byte %00010001
    .byte %00110011
    .byte %00010001
    .byte %01110111

    .byte %01010101
    .byte %01010101
    .byte %01110111
    .byte %00010001
    .byte %00010001

    .byte %01110111
    .byte %01000100
    .byte %01110111
    .byte %00010001
    .byte %01110111

    .byte %01110111
    .byte %01000100
    .byte %01110111
    .byte %01010101
    .byte %01110111

    .byte %01110111
    .byte %00010001
    .byte %00010001
    .byte %00010001
    .byte %00010001

    .byte %01110111
    .byte %01010101
    .byte %01110111
    .byte %01010101
    .byte %01110111

    .byte %01110111
    .byte %01010101
    .byte %01110111
    .byte %00010001
    .byte %01110111

    .byte %00100010
    .byte %01010101
    .byte %01110111
    .byte %01010101
    .byte %01010101

    .byte %01110111
    .byte %01010101
    .byte %01100110
    .byte %01010101
    .byte %01110111

    .byte %01110111
    .byte %01000100
    .byte %01000100
    .byte %01000100
    .byte %01110111

    .byte %01100110
    .byte %01010101
    .byte %01010101
    .byte %01010101
    .byte %01100110

    .byte %01110111
    .byte %01000100
    .byte %01110111
    .byte %01000100
    .byte %01110111

    .byte %01110111
    .byte %01000100
    .byte %01110111
    .byte %01000100
    .byte %01000100

PlayerSprite0
        .byte #%00000000
        .byte #%01000100
        .byte #%01111100
        .byte #%00111000
        .byte #%00111000
        .byte #%00111000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
PlayerSprite1
        .byte #%00000000
        .byte #%00101000
        .byte #%00111000
        .byte #%00111000
        .byte #%00111000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
        .byte #%00010000
PlayerColor0
        .byte #$00
        .byte #$06
        .byte #$08
        .byte #$0E
        .byte #$0C
        .byte #$42
        .byte #$1A
        .byte #$08
        .byte #$0C
PlayerColor1
        .byte #$00
        .byte #$06
        .byte #$08
        .byte #$0E
        .byte #$0C
        .byte #$42
        .byte #$1A
        .byte #$08
        .byte #$0C
BomberSprite
        .byte #%00000000
        .byte #%00001000
        .byte #%01011101
        .byte #%00111110
        .byte #%00011100
        .byte #%01011101
        .byte #%01001001
        .byte #%00111110
        .byte #%00011100
BomberColor
        .byte #$00
        .byte #$30
        .byte #$42
        .byte #$46
        .byte #$8A
        .byte #$42
        .byte #$42
        .byte #$48
        .byte #$32

    ; Complete ROM size with 4KB
    org $FFFC            ; move origin to position $FFFC
        word Reset       ; write 2 bytes with the program reset address
        word Reset
