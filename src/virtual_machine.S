.global jump_to_emulator
jump_to_emulator:
    sub sp, sp, 0x60
    stp x30, x1,  [sp, #0x00]
    stp x27, x28, [sp, #0x10]
    stp x25, x26, [sp, #0x20]
    stp x23, x24, [sp, #0x30]
    stp x21, x22, [sp, #0x40]
    stp x19, x20, [sp, #0x50]

    // Restore state from r1
    // X24 = memory interface ptr
    mov x24, x2
    // x19 = acc
    ldr x19, [x1, 0]
    // x20 = x reg
    ldr x20, [x1, 8]
    // x21 = y reg
    ldr x21, [x1, 16]
    // x22 = sp reg
    ldr x22, [x1, 24]
    // Restore PC
    ldr x23, [x1, 32]
    // Restore Flags into nzcv
    ldr x9, [x1, 40]
    msr nzcv, x9

    // Jump to target code
    blr x0

    ldp x30, x1, [sp, #0x00]

    // Restore Flags into nzcv
    mrs x9, nzcv
    // acc = x19
    str x19, [x1, 0]
    // x reg = x1
    str x20, [x1, 8]
    // y reg = x2
    str x21, [x1, 16]
    // sp reg = x3
    str x22, [x1, 24]
    // pc reg = x4
    str x23, [x1, 32]
    // flags = nzcv
    str x9, [x1, 40]

    ldp x27, x28, [sp, #0x10]
    ldp x25, x26, [sp, #0x20]
    ldp x23, x24, [sp, #0x30]
    ldp x21, x22, [sp, #0x40]
    ldp x19, x20, [sp, #0x50]

    add sp, sp, 0x60
    ret
