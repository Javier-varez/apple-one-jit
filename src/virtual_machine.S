.global _jumpToEmulatedCode
_jumpToEmulatedCode:
    // Push all general purpose registers on the stack.
    // TODO: This is definitely overkill. To be optimized later. Technically we only need to push
    // those registers that are callee-saved AND we change... This keeps larger compatiblity and
    // allows to reuse this code regardless of the ABI... At a clear performance cost...
    sub sp, sp, 0x120
    str x30, [sp, #0x110]
    stp x28, x29, [sp, #0x100]
    stp x26, x27, [sp, #0xF0]
    stp x24, x25, [sp, #0xE0]
    stp x22, x23, [sp, #0xD0]
    stp x20, x21, [sp, #0xC0]
    stp x18, x19, [sp, #0xB0]
    stp x16, x17, [sp, #0xA0]
    stp x14, x15, [sp, #0x90]
    stp x12, x13, [sp, #0x80]
    stp x10, x11, [sp, #0x70]
    stp x8,  x9,  [sp, #0x60]
    stp x6,  x7,  [sp, #0x50]
    stp x4,  x5,  [sp, #0x40]
    stp x2,  x3,  [sp, #0x30]
    stp x0,  x1,  [sp, #0x20]

    mov x10, x0
    mov x11, x1

    // Restore state from r1
    // X5 = memory base
    mov x5, x2
    // x0 = acc
    ldr x0, [x11, 0]
    // x1 = x reg
    ldr x1, [x11, 8]
    // x2 = y reg
    ldr x2, [x11, 16]
    // x3 = sp reg
    ldr x3, [x11, 24]
    // Restore PC
    ldr x4, [x11, 32]
    // Restore Flags into nzcv
    ldr x7, [x11, 40]
    msr nzcv, x7

    // Jump to target code
    blr x10

    ldp x10, x11, [sp, #0x20]

    // Restore Flags into nzcv
    mrs x7, nzcv
    // acc = x0
    str x0, [x11, 0]
    // x reg = x1
    str x1, [x11, 8]
    // y reg = x2
    str x2, [x11, 16]
    // sp reg = x3
    str x3, [x11, 24]
    // pc reg = x4
    str x4, [x11, 32]
    // flags = nzcv
    str x7, [x11, 40]

    ldp x0,  x1,  [sp, #0x20]
    ldp x2,  x3,  [sp, #0x30]
    ldp x4,  x5,  [sp, #0x40]
    ldp x6,  x7,  [sp, #0x50]
    ldp x8,  x9,  [sp, #0x60]
    ldp x10, x11, [sp, #0x70]
    ldp x12, x13, [sp, #0x80]
    ldp x14, x15, [sp, #0x90]
    ldp x16, x17, [sp, #0xA0]
    ldp x18, x19, [sp, #0xB0]
    ldp x20, x21, [sp, #0xC0]
    ldp x22, x23, [sp, #0xD0]
    ldp x24, x25, [sp, #0xE0]
    ldp x26, x27, [sp, #0xF0]
    ldp x28, x29, [sp, #0x100]
    ldr x30, [sp, #0x110]

    add sp, sp, 0x120
    ret
