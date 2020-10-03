;
;   MCS-48 Based Dual Temperature Sensor
;   UPI Master
;
;   Created:    Thu Sep 03 08:23:38 2020
;   Author:     Matthew Millman
;               http://tech.mattmillman.com/
;
;   Compiler:   http://asm48.sourceforge.net/
;
;   CPU:        Intel 8049
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

    .equ    ow_num_sensors,     0x26
    .equ    ow_sensors_to_read, 0x27

    .equ    ow_scratch,         0x28
    .equ    ow_scratch_lsb,     0x28
    .equ    ow_scratch_msb,     0x29    ; + 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F

    .equ    data_ptr_1          0x30

    .equ    disp_3_status       0x32
    .equ    disp_3_reading_msb  0x33
    .equ    disp_3_reading_lsb  0x34
    .equ    disp_4_status       0x35
    .equ    disp_4_reading_msb  0x36
    .equ    disp_4_reading_lsb  0x37

    .equ    ow_address_1,       0x38    ; One-wire temperature sensor 1 address
    .equ    ow_address_2,       0x48    ; One-wire temperature sensor 2 address
    .equ    ow_address_3,       0x48    ; One-wire temperature sensor 3 address
    .equ    ow_address_4,       0x50    ; One-wire temperature sensor 4 address

;   Register constants
;
    .equ    OW_SEARCH_ROM,      0xF0
    .equ    OW_MATCH_ROM,       0x55
    .equ    OW_SKIP_ROM,        0xCC
    .equ    OW_PRESENSE_ERROR   0x80
    .equ    DS18X20_READ,       0xBE
    .equ    DS18X20_CONVERT_T,  0x44
    .equ    DS18X20_ROM_SIZE    0x40

;   Constatns
;
    .equ    NUM_EXTERNAL_DISPLAYS   2
;
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
    strt    T
;   Search sensors
    mov     R0,     ow_num_sensors
    mov     @R0,    0x00
    mov     A,      0xFF
    mov     R0,     ow_address_1
_search_sensor:
    call    onewire_rom_search
    clr     F0
    jz      _no_more_sensors
    jmp     _check_presense_error    
_no_more_sensors:
    cpl     F0
    jmp     _next_sensor
_check_presense_error:
    mov     R2,     A
    jb7     _search_complete    ; Presense error
_next_sensor:
    mov     R1,     ow_num_sensors
    inc     @R1
    mov     A,      @R1
    cpl     A
    inc     A
    add     A,      0x04
    jz      _search_complete    ; 4 sensors found.
    jf0     _search_complete    ; No more sensors on bus.
    mov     A,      R2
    jmp     _search_sensor
_search_complete:
    ; Main loop
_again:
    mov     R0,     ow_sensors_to_read
    mov     R1,     ow_num_sensors
    mov     A,      @R1
    mov     @R0,    A
    mov     R0,     digit_1
    jz      _no_sensors_error
    call    ds18b20_start_measurement
    ; Wait for measurement (approx 1 second)
    mov     R2,     0xFF    ; Inner loop
    mov     R3,     0xFF    ; Middle loop
    mov     R4,     0x03    ; Outer loop
_main_delay_loop:
    djnz    R2,     _main_delay_loop
    djnz    R3,     _main_delay_loop
    djnz    R4,     _main_delay_loop
    clr     A
    call    ds18b20_read_scratchpad
    jb7     _sensor_1_error
    call    ds18b20_calculate_decicelsius
    mov     A,      0x00
    call    celsius_to_fahrenheit
    mov     R0,     digit_1
    call    write_decicelsius_to_display
    jmp     _sensor_2
_sensor_1_error:
    mov     R0,     digit_1
    call    show_error
_sensor_2:
    call    decrement_sensors_to_read
    jz      _no_more_sensors_to_read
    mov     A,      0x01
    call    ds18b20_read_scratchpad
    jb7     _sensor_2_error
    call    ds18b20_calculate_decicelsius
    mov     A,      0x01
    call    celsius_to_fahrenheit
    mov     R0,     digit_4
    call    write_decicelsius_to_display
    call    decrement_sensors_to_read
    jmp     _end_of_sensors_loop
_sensor_2_error:
    mov     R0,     digit_4
    call    show_error
    jmp     _end_of_sensors_loop
_no_more_sensors_to_read:
    mov     R0,     digit_4
    call    clear_display
    jmp     _end_of_sensors_loop
_no_sensors_error:
    mov     R0,     digit_1
    call    show_error
    jmp     _no_sensors_error
_end_of_sensors_loop:
    call    read_remaining_sensors_to_bus
    jmp     _again
;
;   End of routine 'main'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine:    'decrement_sensors_to_read'
;
decrement_sensors_to_read:
    mov     R0,     ow_sensors_to_read
    mov     A,      @R0
    dec     A
    mov     @R0,    A
    ret
;
;   End of routine 'decrement_sensors_to_read'
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
;   Routine     'ds18b20_read_scratchpad'
;
;   Input:          A (Sensor index)
;   Overwrites:     A, R0, R1 (indirect), R2, R3
;
ds18b20_read_scratchpad:
    call    ds18b20_select_sensor
    jb7     _read_error_exit
    mov     A,      DS18X20_READ
    call    onewire_byte_io
    mov     R2,     0x08
    mov     R0,     ow_scratch
_scratchpad_byte_loop:
    mov     A,      0xFF
    call    onewire_byte_io
    mov     @R0,    A
    inc     R0
    djnz    R2,     _scratchpad_byte_loop
_read_error_exit:
    ret
;
;   End of routine 'ds18b20_read_scratchpad'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'ds18b20_select_sensor'
;
;   Input:          A (Sensor index)
;   Overwrites:     A, R1 (indirect), R2
;   Returns:        A (0x00 = success, OW_PRESENSE_ERROR)
;
ds18b20_select_sensor:
    inc     A
    mov     R2,     A
    call    onewire_bus_reset
    jf0     _selsensor_devices_present
    jmp     _selsensor_presense_error
_selsensor_devices_present:
    mov     A,      ow_address_1
    jmp     _start_sensor_index_loop
_sensor_index_loop:
    add     A,      0x08
_start_sensor_index_loop:
    djnz    R2,     _sensor_index_loop
    mov     R0,     A
    mov     A,      OW_MATCH_ROM
    call    onewire_byte_io
    mov     R2,     0x08
_sensor_address_loop:
    mov     A,      @R0
    call    onewire_byte_io
    inc     R0
    djnz    R2,     _sensor_address_loop
    clr     A
    ret
_selsensor_presense_error:
    mov     A,      OW_PRESENSE_ERROR
    ret
;
;   End of routine 'ds18b20_select_sensor'
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
;   Routine     'onewire_bus_reset'
;
;   Returns:        F0 ('1' if devices present)
;   Overwrites:     R1, F0
;
onewire_bus_reset:
    call    onewire_timer_sync
    orl     P2,     0x80
    mov     R1,     0x5E    ; 480 uS
_reset_delay:
    djnz    R1,     _reset_delay
    anl     P2,     0x7F
    mov     R1,     0xB     ; 64 uS
_ack_delay:
    djnz    R1,     _ack_delay
    clr     F0
    jt0     _no_ack
    cpl     F0              ; Something appears to have responded
_no_ack:
    mov     R1,     0x5E    ; 480 uS
_presense_delay:
    djnz    R1,     _presense_delay
    jt0     _line_ok
    clr     F0              ; Line should have been released by now. Fail.
_line_ok:
    ret
;
;   End of routine 'onewire_bus_reset'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine 'onewire_bit_io'
;
;   Input:          A (bit 0, writes '1' to line if set)
;   Returns:        F0
;   Overwrites:     A, R3, F0
onewire_bit_io:
    call    onewire_timer_sync
    mov     R3,     A
    clr     F0
    jb0     _write_one
; write zero
    orl     P2,     0x80    ; Pull bus low
    jmp     _read_bit
_write_one:
    orl     P2,     0x80    ; Pull bus low
    anl     P2,     0x7F    ; Float bus high
_read_bit:
    nop
    nop
    jnt0    _in_low
    cpl     F0
_in_low:
    mov     A,      R3
    mov     R3,     0x6
_slot_delay:
    djnz    R3,     _slot_delay
    anl     P2,     0x7F    ; Float bus high
    mov	    R3,     0x6
_slot_end_delay:
    djnz    R3,     _slot_end_delay
    ret
;
;   end of routine 'onewire_bit_io'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine: 'onewire_rom_search'
;
;   Inputs:         A (last diff), R0 (ROM ID pointer)
;   Returns:        A (index of last collision bit, 0x00 at end of search, 0x80 on error)
;                   R0 (ROM ID pointer for next sensor)
;   Overwrites:     R0, R1, R2, R3 (indirect), R4, F0 (indirect), F1
;
onewire_rom_search:
    mov     R4,     A
    call    onewire_bus_reset
    jf0     _devices_present
    jmp     _presense_error
    ret
_devices_present:
    mov     R2,     DS18X20_ROM_SIZE
    mov     A,      OW_SEARCH_ROM
    call    onewire_byte_io
    mov     R1,     0x00
    clr     F0
_bit_loop:
    jf0     _no_clear
    mov     @R0,    0x00    ; Clear out existing memory
_no_clear:
    mov     A,      0x01
    call    onewire_bit_io  ; Read bit
    clr     F1
    cpl     F1
    jf0     _first_read_high
    clr     F1
_first_read_high:
    call    onewire_bit_io  ; Read compliment
    jf0     _compliment_high
    jmp     _compliment_low
_compliment_high:
    jf1     _presense_error
    jmp     _write_bit
_compliment_low:
    jf1     _write_bit
    mov     A,      R4
    cpl     A
    inc     A
    add     A,      R2
    jnc     _one_path
    mov     A,      8
    cpl     A
    inc     A
    add     A,      R0
    mov     R0,     A
    mov     A,      @R0
    xch     A,      R0
    add     A,      8
    xch     A,      R0
    anl     A,      0x01
    jz      _write_bit
    mov     A,      R4
    cpl     A
    inc     A
    add     A,      R2
    jz      _write_bit
_one_path:
    clr     F1
    cpl     F1
    mov     A,      R2
    mov     R1,     A
_write_bit:
    mov     A,      0x01
    jf1     _write_high
    clr     A
_write_high:
    call    onewire_bit_io  ; Write direction bit
    mov     A,      @R0
    rr      A
    jf1     _store_one
    jmp     _store_zero
_store_one:
    orl     A,      0x80
_store_zero:
    mov     @R0,    A
    clr     F0
    cpl     F0
    mov     A,      R2
    dec     A
    anl     A,      0x7
    jnz     _continue_loop
    inc     R0              ; Increment address pointer
    clr     F0
_continue_loop:
    djnz    R2,     _bit_loop
    mov     A,      R1
    ret
_presense_error:
    mov     A,      OW_PRESENSE_ERROR
    ret
;
;   End of routine 'onewire_rom_search'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'ds18b20_start_measurement'
;
;   Input:          A (Sensor index)
;   Overwrites:     A, R0, R1 (indirect)
;
ds18b20_start_measurement:
    call    ds18b20_select_all
    jb7     _start_error_exit
    mov     A,      DS18X20_CONVERT_T
    call    onewire_byte_io
_start_error_exit:
    ret
;
;   End of routine 'ds18b20_start_measurement'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine 'onewire_timer_sync'
;
onewire_timer_sync:
    sel     RB1
    mov     R4,     A
    mov     R7,     0x01
_timer_sync_loop:
    mov     A,      R7
    jnz     _timer_sync_loop
    sel     RB1
    mov     A,      R4
    sel     RB0
    ret
;
;   End of routine 'onewire_timer_sync'
; ----------------------------------------------------------------------------

    .org    0x0200  ; Start of bank 2

; ----------------------------------------------------------------------------
;   Routine     'ds18b20_select_all'
;
;   Overwrites:     A, R1 (indirect), R2
;   Returns:        A (0x00 = success, OW_PRESENSE_ERROR)
;
ds18b20_select_all:
    call    onewire_bus_reset
    jf0     _selsensorall_devices_present
    jmp     _selsensorall_presense_error
_selsensorall_devices_present:
    mov     A,      OW_SKIP_ROM
    call    onewire_byte_io
    clr     A
    ret
_selsensorall_presense_error:
    mov     A,      OW_PRESENSE_ERROR
    ret
;
;   End of routine 'ds18b20_select_all'
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
;   Routine 'onewire_byte_io'
;
;   Input:          A (byte to write to line)
;   Returns:        A (byte read from line)
;   Overwrites:     A, R1, R3 (indirect), F0 (indirect)
;
onewire_byte_io:
    mov     R1,     8
_onewire_byte_loop:
    call    onewire_bit_io
    rr      A
    anl     A,      0x7F
    jf0     _one_result
    jmp     _zero_result
_one_result:
    orl     A,      0x80
_zero_result:
    djnz    R1,     _onewire_byte_loop
_onewire_byte_done:
    ret
;
;   End of routine 'onewire_byte_io'
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
;   Routine     'ds18b20_calculate_decicelsius'
;   Calculates decimalised degrees celsius from the scratchpad i.e. 31.5 = 315
;
;   Input:          ow_scratch_msb, ow_scratch_lsb
;   Overwrites:     R0, R1, R2, R3, R4 (indirect), R5, R6, R7
;   Returns:        R1 (msb), R2 (lsb)
;
ds18b20_calculate_decicelsius:
    ; Load from scratchpad
    mov     R0,     ow_scratch_msb
    mov     A,      @R0
    mov     R1,     A
    mov     R0,     ow_scratch_lsb
    mov     A,      @R0
    mov     R2,     A
    ; Negative temperature read from sensor?
    mov     A,      R1
    anl     A,      0x80
    clr     F0
    jz      _scratch_is_positive
    cpl     F0          ; Scratchpad contains negative temperature
    call    negate_16r16
_scratch_is_positive:
    ; Copy bits 0-3 into R5
    mov     A,      R2
    anl     A,      0x0F
    mov     R5,     A
    ; Move bits 4-11 into R7
    mov     A,      R2
    anl     A,      0xF0
    swap    A
    mov     R3,     A
    mov     A,      R1
    anl     A,      0x0F
    swap    A
    orl     A,      R3
    mov     R7,     A
    ; Multiply fraction by 10
    mov     A,      R5
    mov     R3,     A
    call    multiply_8x8r16_x10
    ; Adjust fraction if negative
    mov     A,      R2
    jf0     _adjust_negative
    jmp     _no_adjust_negative
_adjust_negative:
    add     A,      0x08
_no_adjust_negative:
    mov     R2,     A
    mov     R1,     0x00
    mov     R3,     16
    call    divide_16x8r8
    mov     A,      R2
    mov     R5,     A
    ; Multiply first two digits by 10
    mov     A,      R7
    mov     R3,     A
    call    multiply_8x8r16_x10
    ; Add fraction to result
    mov     A,      R5
    add     A,      R2
    mov     R2,     A
    mov     A,      0x00
    addc    A,      R1
    mov     R1,     A
    jf0     _negate_result
    jmp     _no_negate_result
_negate_result:
    call    negate_16r16
_no_negate_result:
    ret
;
;   End of routine 'ds18b20_calculate_decicelsius'
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

    .org    0x0300 ; Start of bank 3

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

; ----------------------------------------------------------------------------
;   Routine     'read_remaining_sensors_to_bus'
;
;   Inputs:         ow_sensors_to_read
;   Overwrites:     F0, R0, R1, R2, R3/R4/R5/R6/R7 (indirect)
;
read_remaining_sensors_to_bus:
    ; Setup pointer to temp read memory
    mov     R0,     data_ptr_1
    mov     A,      disp_3_status
    mov     @R0,    A
    mov     R0,     A
    ; Clear temp display data
    mov     R1,     NUM_EXTERNAL_DISPLAYS
_clear_extdisplay_loop:
    mov     R2,     3           ; Status+MSB+LSB
_clear_extdisplay_loop_inner:
    mov     @R0,    0x00
    inc     R0
    djnz    R2,     _clear_extdisplay_loop_inner
    djnz    R1,     _clear_extdisplay_loop
_extdisplay_read_loop:
    mov     R0,     ow_sensors_to_read
    mov     A,      @R0
    jz      _write_data_to_bus
    cpl     A
    inc     A
    mov     R0,     ow_num_sensors
    add     A,      @R0         ; (ow_num_sensors - ow_sensors_to_read)
    call    ds18b20_read_scratchpad
    jb7     _extdisplay_sensor_error
    call    ds18b20_calculate_decicelsius
    mov     R0,     data_ptr_1
    mov     A,      @R0
    mov     R0,     A
    mov     @R0,    0x02        ; Status OK
    inc     R0
    mov     A,      R1
    mov     @R0,    A
    inc     R0
    mov     A,      R2
    mov     @R0,    A
    inc     R0
    mov     R1,     data_ptr_1
    mov     A,      R0
    mov     @R1,    A
    jmp     _end_of_read_extdisplays_loop
_extdisplay_sensor_error:
    mov     R0,     data_ptr_1
    mov     A,      @R0
    mov     R0,     A
    mov     @R0,    0x01        ; Status Error
    inc     R0
    inc     R0
    inc     R0
    mov     R1,     data_ptr_1
    mov     A,      R0
    mov     @R1,    A
_end_of_read_extdisplays_loop:
    call    decrement_sensors_to_read
    jmp     _extdisplay_read_loop
_write_data_to_bus:
    mov     R0,     disp_3_status
    mov     R1,     NUM_EXTERNAL_DISPLAYS
    mov     R2,     0
_extdisplay_write_loop:
    clr     F0
    mov     A,      R2
    inc     R2
    call    write_to_upi        ; Display index
    cpl     F0
    mov     A,      @R0
    call    write_to_upi        ; Display status
    inc     R0
    mov     A,      @R0
    call    write_to_upi        ; Display msb
    inc     R0
    mov     A,      @R0
    call    write_to_upi        ; Display lsb
    inc     R0
    djnz    R1,     _extdisplay_write_loop
    ret
;
;   End of routine 'read_remaining_sensors_to_bus'
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
;   Routine     'write_to_upi'
;
;   Inputs:         F0 (0 = address, 1 = data), A (data to write)
;   Overwrites:     R7
;
write_to_upi:
    jf0     _write_upi_data
    anl     P2,     0xFE
    jmp     _write_upi_address
_write_upi_data:
    orl     P2,     0x01
_write_upi_address:
    outl    BUS,    A
    call    write_upi_delay
    ret
write_upi_delay:
    mov     R7,     0x40
_write_upi_delay_loop:
    djnz    R7,     _write_upi_delay_loop
;
;   End of routine 'write_to_upi'
; ----------------------------------------------------------------------------