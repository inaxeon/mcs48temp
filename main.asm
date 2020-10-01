;
;   MCS-48 Based Dual Temperature Sensor
;   UPI Slave Implementation
;
;   Created:    Thu Sep 03 08:23:38 2020
;   Author:     Matthew Millman
;               http://tech.mattmillman.com/
;
;   Compiler:   http://asm48.sourceforge.net/
;
;   CPU:        Intel 8048
;

; ----------------------------------------------------------------------------
;   Data memory addresses
;
    .equ    digit_1,            0x20    ; Top left
    .equ    digit_2,            0x21    ; Top middle
    .equ    digit_3,            0x22    ; Top right
    .equ    digit_4,            0x23    ; Bottom left
    .equ    digit_5,            0x24    ; Bottom midle
    .equ    digit_6,            0x25    ; Bottom right

    .equ    decicelsius_1,      0x28
    .equ    decicelsius_2,      0x2A

;   End of constants
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Vectors
;
    .org    0x0000          ; Reset
    jmp     main
    .org    0x0003          ; External interrupt
    jmp     external_interrupt
    .org    0x0007          ; Timer interrupt
    jmp     timer_interrupt
;
;   End of vectors
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   7-Segment digit lookup table
;
    .org    0x000A
    .db     0xFC    ; 0
    .db     0x60    ; 1
    .db     0xDA    ; 2
    .db     0xF2    ; 3
    .db     0x66    ; 4
    .db     0xB6    ; 5
    .db     0xBE    ; 6
    .db     0xE0    ; 7
    .db     0xFE    ; 8
    .db     0xF6    ; 9
    .db     0xEE    ; A
    .db     0x3E    ; b
    .db     0x9C    ; C
    .db     0x7A    ; d
    .db     0x9E    ; E
    .db     0x8E    ; F
    .db     0x02    ; -
    .db     0x0A    ; r (To display 'Err')
    .db     0x00    ; Off
;
;   End of lookup table
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine:    'external_interrupt'
;
external_interrupt:
    dis     I
    sel     RB1
    mov     R3,     A       ; Save 'A'

    mov     R0,     0x29
    inc     @R0

    mov     R0,     0x2B
    .db     0x22;       in      A,      DBB
    mov     @R0,    A

    mov     A,      R3      ; Restore 'A'
    sel     RB0
    en      I
    retr
;
;   End of routine 'external_interrupt'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine:    'timer_interrupt'
;
;   Registers (Bank 1)
;   R0:     Data memory pointer
;   R1:     Byte to write to P2 to select display
;   R2:     Byte to write to P1 to select digits
;   R3:     Saved copy of 'A' register (for this)
;   R4:     Saved copy of 'A' register (for 'onewire_timer_sync')
;   R6:     Display index
;   R7:     Synchronisation flag
;
timer_interrupt:
    sel     RB1
    mov     R3,     A       ; Save 'A'
    mov     A,      R6
    swap    A
    mov     R0,     A
    in      A,      P2
    anl     A,      0x8F
    orl     A,      R0
    mov     R1,     A
    mov     A,      R6
    add     A,      digit_1
    mov     R0,     A
    inc     R6
    mov     A,      R6
    cpl     A
    inc     A
    add     A,      0x06
    jz      _reset_count
    jmp     _render_segments
_reset_count:
    mov     R6,     0x00    ; Last display
_render_segments:
    mov     A,      @R0
    anl     A,      0x80
    clr     F0
    jz      _no_dp
    cpl     F0
_no_dp:
    mov     A,      @R0
    anl     A,      0x7F
    add     A,      0x0A
    movp    A,      @A      ; Load byte from lookup table above
    jf0     _set_dp
    jmp     _no_set_dp
_set_dp:
    inc     A
_no_set_dp:
    mov     R2,     A
    mov     A,      R1
    anl     P1,     0x00    ; Turn off all segments before display changeover
    outl    P2,     A       ; Select display
    mov     A,      R2
    outl    P1,     A       ; Select segments
    mov     A,      R6
    mov     A,      0xF0
    mov     T,      A
    mov     A,      R3      ; Restore 'A'
    mov     R7,     0x00    ; Timer done. All clear to continue.
    sel     RB0
    retr
;
;   End of routine 'timer_interrupt'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine:    'main'
;
main:
    mov     A,      0x00
    outl    P2,     A
    outl    P1,     A
    sel     RB1
    mov     R6,     0x00        ; Clear current dispaly register
    sel     RB0
;   Set all digits to '-'
    mov     R1,     0x06
    mov     R0,     digit_1
_blank_digits_loop:
    mov     @R0,    0x10
    inc     R0
    djnz    R1,     _blank_digits_loop
;   Start timer
    en      TCNTI
    en      I
    strt    T
;   Clear decicelsius
    mov     R1,     4
    mov     R0,     decicelsius_1
_zero_decicelsius_loop:
    mov     @R0,    0x00
    inc     R0
    djnz    R1,     _zero_decicelsius_loop
; Main loop
_again:
    mov     R0,     decicelsius_1
    mov     A,      @R0
    mov     R1,     A
    inc     R0
    mov     A,      @R0
    mov     R2,     A
    call    celsius_to_fahrenheit
    mov     R0,     digit_1
    call    write_decicelsius_to_display
    mov     R0,     decicelsius_2
    mov     A,      @R0
    mov     R1,     A
    inc     R0
    mov     A,      @R0
    mov     R2,     A
    call    celsius_to_fahrenheit
    mov     R0,     digit_4
    call    write_decicelsius_to_display
    jmp     _again
;
;   End of routine 'main'
; ----------------------------------------------------------------------------

    .org    0x0100 ; Start of bank 1

; ----------------------------------------------------------------------------
;   Routine     'show_error'
;
;   Input:          R0 (address of first display digit to write to)
;
show_error:
    mov     @R0,    0x0E    ; 'E'
    inc     R0
    mov     @R0,    0x11    ; 'r'
    inc     R0
    mov     @R0,    0x11    ; 'r'
    ret
;
;   End of routine 'show_error'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'clear_display'
;
;   Input:          R0 (address of first display digit to write to)
;
clear_display:
    mov     @R0,    0x12    ; Off
    inc     R0
    mov     @R0,    0x12    ; Off
    inc     R0
    mov     @R0,    0x12    ; Off
    ret
;
;   End of routine 'clear_display'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'multiply_8x8r16_x10'
;   True multiply routine (fixed x10 multiplier)
;
;   Input:          R3
;   Overwrites:     R1, R2, R4
;   Returns:        R1 (msb), R2 (lsb)
;
multiply_8x8r16_x10:
    mov     R4,     9
    mov     R2,     10
    clr     A
    clr     C
_mul_loop:
    rrc     A
    xch     A,      R2
    rrc     A
    xch     A,      R2
    jnc     _mul_noadd
    add     A,      R3  
_mul_noadd:
    djnz    R4,     _mul_loop
    mov     R1,     A
    ret
;
;   End of routine 'multiply_8x8r16'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'negate_16r16'
;
;   Input:          R1 (msb), R2 (lsb)
;   Overwrites:     R1, R2
;   Returns:        R1 (msb), R2 (lsb)
;
negate_16r16:
    mov     A,      R1
    xrl     A,      0xFF
    mov     R1,     A
    mov     A,      R2
    xrl     A,      0xFF
    mov     R2,     A
    mov     A,      R2
    add     A,      1
    mov     R2,     A
    mov     A,      R1
    addc    A,      0
    mov     R1,     A
    ret
;
;   End of routine 'negate_16r16'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'write_decicelsius_to_display'
;   Renders a twos compliment fixed point number to one of the 7-segment banks
;
;   Input:          R0 (pointer to first display byte), R1 (msb), R2 (lsb)
;   Overwrites:     R0, R1, R2, R3, R4, R5, R6, R7, F0
;   Returns:        Converted digits in memory address starting from R0
;
write_decicelsius_to_display:
    clr     F0
    mov     A,      R1
    anl     A,      0x80    ; Negative number?
    jz      _positive_number
    cpl     F0              ; Negative number. Note it and make it positive
    call    negate_16r16
_positive_number:
    mov     R7,     0x00
    mov     R3,     100
    call    divide_16x8r8
    mov     R6,     A
    mov     A,      R2
    mov     R5,     A
    cpl     A
    inc     A
    add     A,      9
    jnc     _divide_first_digit
    jmp     _no_divide_first_digit
_divide_first_digit:
    mov     R1,     0x00
    mov     A,      R5
    mov     R2,     A
    mov     R3,     10
    call    divide_16x8r8
    mov     R5,     A
    mov     A,      R2
    mov     R7,     A
_no_divide_first_digit:
    mov     R1,     0x00
    mov     A,      R6
    mov     R2,     A
    mov     R3,     10
    call    divide_16x8r8
    mov     R3,     A
    mov     A,      R7
    jnz     _greater_or_equal_to_1000 ; > 100 degrees
    jmp     _less_than_1000
_greater_or_equal_to_1000:
    mov     @R0,    A
    jmp     _drop_last_digit
_less_than_1000:
    jf0     _set_sign
    mov     A,      R5
    jnz     _not_leading_zero
    mov     A,      0x12    ; Clear leading zero
_not_leading_zero:
    jmp     _no_set_sign
_set_sign:
    mov     A,      0x10
_no_set_sign:
    mov     @R0,    A
    jf0     _check_drop_decimal
    jmp     _no_drop_last_digit
_check_drop_decimal:
    mov     A,      R5
    jz     _no_drop_last_digit
_drop_last_digit:
    inc     R0
    mov     A,      R5
    mov     @R0,    A
    inc     R0
    mov     A,      R2
    mov     @R0,    A
    jmp     _conversion_done
_no_drop_last_digit:
    inc     R0
    mov     A,      R2
    orl     A,      0x80
    mov     @R0,    A
    inc     R0
    mov     A,      R3
    mov     @R0,    A
_conversion_done:
    ret
;
;   End of routine 'write_decicelsius_to_display'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'divide_16x8r8'
;   Taken from a paper copy ISIS-II manual.
;   TODO: Figure out how this works and extend it to have a 16-bit result
;
;   Input:          R1 (msb), R2 (lsb), R3 (divisor)
;   Overwrites:     R1, R2, R4
;   Returns:        A (remainder), R2 (8 bit result), C (set if overflow)
;
divide_16x8r8:
    mov     A,      R2
    xch     A,      R1
    mov     R4,     0x08
    cpl     A
    add     A,      R3
    cpl     A
    jc      _div_a
    cpl     C
    jmp     _div_b
_div_a:
    add     A,      R3
_div_lp:
    clr     C
    xch     A,      R1
    rlc     A
    xch     A,      R1
    rlc     A
    jnc     _div_e
    cpl     A
    add     A,      R3
    cpl     A
    jmp     _div_c
_div_e:
    cpl     A
    add     A,      R3
    cpl     A
    jnc     _div_c
    add     A,      R3
    jmp     _div_d
_div_c:
    inc     R1
_div_d:
    djnz    R4,  _div_lp
    clr     C
_div_b:
    xch     A,      R1
    mov     R2,     A
    mov     A,      R1
    ret
;
;   End of routine 'divide_16x8r8'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'multiply_16x8r16'
;   Pseudo multiply routine. Takes a 16-bit input unlike 
;   multiply_8x8r16_x10 and is slightly quicker if multiplier (< 10)
;
;   Input:          R1 (msb), R2 (lsb), R0 (multiplier)
;   Overwrites:     R1, R2, R3, R4
;   Returns:        R1 (msb), R2 (lsb)
;
multiply_16x8r16:
    dec     R0
    mov     A,      R1
    mov     R3,     A
    mov     A,      R2
    mov     R4,     A
_mul_i16_loop:
    call    add_16x16r16_nocarry
    djnz    R0,     _mul_i16_loop
    ret
;
;   End of routine 'multiply_16x8r16'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'add_16x16r16_nocarry'
;
;   Input:          R1 (msb), R2 (lsb), R3 (addend msb), R4 (addend lsb)
;   Overwrites:     R1, R2
;   Returns:        R1 (msb), R2 (lsb)
;
add_16x16r16_nocarry:
    mov     A,      R2
    add     A,      R4
    mov     R2,     A
    mov     A,      R1
    addc    A,      R3
    mov     R1,     A
    ret
;
;   End of routine 'add_16x16r16_nocarry'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'celsius_to_fahrenheit'
;   The most complicated and expensive routine in this project.
;   Execution takes 2.1ms @ 6 MHz
;
;   Input:          R1 (msb), R2 (lsb)
;   Overwrites:     R0, R1, R2, R3, R4, R5, R6, R7
;   Returns:        R1 (msb), R2 (lsb)
;
celsius_to_fahrenheit:
    jt1     _do_conversion
    ret
_do_conversion:
    clr     F0
    mov     A,      R1
    anl     A,      0x80    ; Negative number?
    jz      _df_positive_number
    cpl     F0              ; Negative number. Note it and make it positive
    call    negate_16r16
_df_positive_number:
    mov     R0,     9       ; Celsius * 9
    call    multiply_16x8r16
    mov     A,      R1
    mov     R6,     A
    mov     A,      R2
    mov     R7,     A
    mov     R3,     50      ; / 50 (divide_16x8r8 overflows with a dividend of 5)
    call    divide_16x8r8
    mov     R5,     A
    mov     R1,     0x00
    mov     R0,     10      ; * 10 (multiply it up again)
    call    multiply_16x8r16
    mov     A,      R1
    mov     R6,     A
    mov     A,      R2
    mov     R7,     A
    mov     R1,     0x00
    mov     A,      R5
    mov     R2,     A
    mov     R0,     10      ; Remainder * 10
    call    multiply_16x8r16
    mov     R3,     50      ; Remainder / 50
    call    divide_16x8r8
    mov     R1,     0x00
    mov     A,      R6
    mov     R3,     A
    mov     A,      R7
    mov     R4,     A
    call    add_16x16r16_nocarry   ; Add remainder
    jf0     _df_negate_result
    jmp     _df_no_negate_result
_df_negate_result:
    call    negate_16r16
_df_no_negate_result:
    mov     R3,     0x01
    mov     R4,     0x40
    call    add_16x16r16_nocarry    ; Add 320
    mov     A,      R2
    addc    A,      0x00            ; (carry)
    mov     R2,     A
    ret
;
;   End of routine 'celsius_to_fahrenheit'
; ----------------------------------------------------------------------------
