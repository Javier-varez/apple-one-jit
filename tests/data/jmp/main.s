.org 0x800

main:
   jmp add
   brk
   nop
   nop

add:
   lda #0x1
   adc #0x7f
   .byte 0x02
