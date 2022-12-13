.macro prepare_mem_call
    sub sp, sp, 0x60
    stp x8, x30, [sp, #0x50]
    stp x6, x7,  [sp, #0x40]
    stp x4, x5,  [sp, #0x30]
    stp x2, x3,  [sp, #0x20]
    stp x0, x1,  [sp, #0x10]
    // Save flags
    mrs x0, nzcv
    str x0, [sp, #0x00]
.endmacro

.macro exit_mem_call
    // Restore flags
    ldr x0, [sp, #0x00]
    msr nzcv, x0
    ldp x0,  x1,  [sp, #0x10]
    ldp x2,  x3,  [sp, #0x20]
    ldp x4,  x5,  [sp, #0x30]
    ldp x6,  x7,  [sp, #0x40]
    ldp x8,  x30, [sp, #0x50]
    add sp, sp, 0x60
    ret
.endmacro

.global memory_write_8_bits_asm
memory_write_8_bits_asm:
    prepare_mem_call

    mov x0, x5
    mov x1, x9
    mov x2, x10
    bl memory_write_8_bits

    exit_mem_call

.global memory_write_16_bits_asm
memory_write_16_bits_asm:
    prepare_mem_call

    mov x0, x5
    mov x1, x9
    mov x2, x10
    bl memory_write_16_bits

    exit_mem_call

.global memory_read_8_bits_asm
memory_read_8_bits_asm:
    prepare_mem_call

    mov x0, x5
    mov x1, x9
    bl memory_read_8_bits
    mov x9, x0

    exit_mem_call

.global memory_read_16_bits_asm
memory_read_16_bits_asm:
    prepare_mem_call

    mov x0, x5
    mov x1, x9
    bl memory_read_16_bits
    mov x9, x0

    exit_mem_call
