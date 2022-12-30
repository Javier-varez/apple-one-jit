MEMORY {
    ram (rw) : ORIGIN = 0x0000, LENGTH = 0x10000
}

SECTIONS {
    .text : {
        *(.text*)
    } > ram
}
