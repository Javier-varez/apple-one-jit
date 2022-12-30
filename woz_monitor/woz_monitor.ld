MEMORY {
    monitor (rw) : ORIGIN = 0xFF00, LENGTH = 0x100
}

SECTIONS {
    .text : {
        *(.text*)
    } > monitor

    .data : {
        *(.data*)
    } > monitor

    .bss : {
        *(.data*)
    } > monitor
}
