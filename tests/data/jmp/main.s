.org $800

main:
   jmp add
   brk
   nop
   nop

add:
   lda #$1
   adc #$7f
   .byte $02
