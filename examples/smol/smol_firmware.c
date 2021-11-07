#include <stdint.h>

void main(void);
void _start(void);

// Define the linker script variables
extern uint32_t _etext;
extern uint32_t _sbss;
extern uint32_t _ebss;
extern uint32_t _sdata;
extern uint32_t _edata;
extern uint32_t _estack;

typedef uint32_t u32_t;

// SSEG Value Mappings
// 0-9
#define SSEG_0 0b0111111
#define SSEG_1 0b0000110
#define SSEG_2 0b1011011
#define SSEG_3 0b1001111
#define SSEG_4 0b1100110
#define SSEG_5 0b1101101
#define SSEG_6 0b1111101
#define SSEG_7 0b0000111
#define SSEG_8 0b1111111
#define SSEG_9 0b1100111
// A-F
#define SSEG_A 0b1110111
#define SSEG_B 0b1111100
#define SSEG_C 0b0111001
#define SSEG_D 0b1011110
#define SSEG_E 0b1111001
#define SSEG_F 0b1110001

// Begin user code execution here
void _start(void)
{
    // Define ptrs to access linker script variables
    u32_t *pre_def_vals_ptr = &_etext;
    u32_t *data_sec_ptr = &_sdata;
    u32_t *bss_sec_ptr;

    // Copy data from LMA to VMA in the .data section
    if (pre_def_vals_ptr != data_sec_ptr) {
        for (; data_sec_ptr < &_edata;) {
            *data_sec_ptr++ = *pre_def_vals_ptr++;
        }
    }

    // Fill zeros in the .bss section
    for (bss_sec_ptr = &_sbss; bss_sec_ptr < &_ebss;) {
        *bss_sec_ptr++ = 0;
    }

    // Init the stack pointer value
    asm volatile("la sp, _estack");

    // Jump to the main program
    main();
}

// Main program - a simple "hello world"
void main(void) {
    // Set values for SSEG digits
    volatile u32_t *sseg0 = (u32_t*)0x00003010;
    volatile u32_t *sseg1 = (u32_t*)0x00003014;

    // Setting digits to "54"
    *sseg0 = SSEG_5;
    *sseg1 = SSEG_4;

    // Loop forever
    for(;;);
}
