.org $800

main:
    lda #$10
    ldx #$4
loop:
    adc $20,x
    dex
    bpl loop
   .byte $02
