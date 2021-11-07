/* Default linker script for smol */

_RomSize    = 0x200; /* Program mem section - 2KB */
_RamSize    = 0x200; /* RAM section - 2KB */
_StackSize  = 0x100; /* Reserve half of RAM for the stack - 1KB */

_RomStart   = 0x0;
_RamStart   = _RomSize;

MEMORY {
    rom (rwx)   : ORIGIN = _RomStart, LENGTH = _RomSize - 1
    ram (rwx)   : ORIGIN = _RamStart, LENGTH = _RamSize
}

ENTRY(_start)

SECTIONS {
    .text : {
        *(.text .text*)
        *(.rodata .rodata*)
        _etext = .;
    } > rom

    .bss (NOLOAD) :
    {
        _sbss = .;
        *(.bss .bss*)
        *(COMMON)
        _ebss = .;
    } > ram

    .data :
    {
        _sdata = .;
        *(.data .data*)
        _edata = .;
    } > ram AT > rom

    .stack (NOLOAD) :
    {
        . = ALIGN(4);
        _sstack = .;
        . = . + _StackSize;
        . = ALIGN(4);
        _estack = .;
    } > ram

    _end = .;
}