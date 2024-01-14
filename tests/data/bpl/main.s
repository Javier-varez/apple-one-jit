.org 0x800

main:
    lda #0x10
    ldx #0x4
loop:
    adc 0x20,x
    dex
    bpl loop
   .byte 0x02
