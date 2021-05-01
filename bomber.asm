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
JetSpritePtr       word    ; pointer to player0 sprite table
JetColorPtr        word    ; pointer to player0 color table
BomberSpritePtr    word    ; pointer to player1 sprite table
BomberColorPtr     word    ; pointer to player1 color table
JetAnimationOffset word    ; player0 sprite frame offset for amination

    ; define constants
JET_HEIGHT    = 9          ; player0 sprite hegiht 
BOMBER_HEIGHT = 9          ; player1 sprite hegiht

    ; Start our ROM code at memory address $F000
    seg code
    org $F000

Reset:
    CLEAN_START            ; class macro to reset memory and registers

    ; initialize RAM variables and TIA registers
    lda #10
    sta JetYPos            ; JetYPos = 10
    lda #0
    sta JetXPos            ; JetXPos = 60
    lda #83
    sta BomberYPos         ; BomberYPos = 83
    lda #54
    sta BomberXPos         ; BomberXPos = 54

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
   
    ; Calculate and tasks performed in the pre-VBLANK
    lda JetXPos
    ldy #0
    jsr SetObjectXPos     ; set player0 horizonta position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos     ; set player1 horizonta position

    sta WSYNC
    sta HMOVE             ; apply the horizonta positions

    ; Display VSYNC and VBLANK
    lda #2
    sta VBLANK            ; turn on VBLANK
    sta VSYNC             ; turn on VSYNC
    REPEAT 3
        sta WSYNC         ; display the 3 recomended line of VSYNC
    REPEND
    lda #0
    sta VSYNC             ; turn off VSYNC

    REPEAT 37
        sta WSYNC        ; display the 37 recomended line of VSYNC
    REPEND
    sta VBLANK           ; turn off VBLANK

    ; Display the 96 visible scan lines of our main game (beacouse 2-line kernel) 
GameVisibleLines:
    lda #$84             ; set color background blue
    sta COLUBK
    lda #$C2             ; set the playfield/grass color to green
    sta COLUPF

    lda #%00000001       ; enable playfield reflection
    sta CTRLPF

    lda #$F0
    sta PF0
    lda #$FC       
    sta PF1
    lda #0
    sta PF2

    ldx #96              ; X count the number of remainig scanlines
.GameLineLoop:

;********************************************************************************
.AreWeInsideJetSprite:
    txa                 ; transfer x to A
    sec                 ; make sure the carry flag is set before subtraction
    sbc JetYPos         ; subtract sprite Y-coord
    cmp JET_HEIGHT      ; are we inside the sprite
    bcc .DrawSpriteP0   ; if result < SpriteHeight, call the draw routine
    lda #0              ; else, set lookup index to zero
.DrawSpriteP0:
    clc                     ; clear carry-flag before addition
    adc JetAnimationOffset  ; jump to the correct sprite frame address in memory 
    tay                  ; load Y so we can work whit the pointer
    lda (JetSpritePtr),Y ; load player0 bitmap from lookup table
    sta WSYNC            ; wait for scanLine
    sta GRP0             ; set graphics for player0
    lda (JetColorPtr),Y  ; load player0 color from lookup table
    sta COLUP0           ; set color for player0
;********************************************************************************
.AreWeInsideBomberSprite:
    txa                    ; transfer x to A
    sec                    ; make sure the carry flag is set before subtraction
    sbc BomberYPos         ; subtract sprite Y-coord
    cmp BOMBER_HEIGHT      ; are we inside the sprite
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
    inc JetYPos
    lda #0
    sta JetAnimationOffset  

CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    dec JetYPos
    lda #0
    sta JetAnimationOffset 

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    dec JetXPos
    lda JET_HEIGHT           ; 9
    sta JetAnimationOffset   ; set JetAnimationOffset to the second frame

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne EndInput
    inc JetXPos
    lda JET_HEIGHT          ; 9
    sta JetAnimationOffset  ; set JetAnimationOffset to the second frame

EndInput:               ; fallback when no input

    ; Calculations to update positions form next frame
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0                          ; compare bomber Y position whit zero
    bmi .ResetBomberPosition        ; if is < 0, reset y pos back to the top
    dec BomberYPos                  ; else, decrement enemy y position
    jmp EndPositionUpdate
.ResetBomberPosition
    lda #96
    sta BomberYPos
EndPositionUpdate:          ; fallback for the position update

    ; Loop back to Start a brand new frame
    jmp StartFrame       ; continue to display next frame

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

    ; Declare ROM lookup tables
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
