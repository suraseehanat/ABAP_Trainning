CLASS zcl_booking_hello_world_003 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_booking_hello_world_003 IMPLEMENTATION.
    METHOD if_oo_adt_classrun~main.
    out->write( 'Hello ABAP Cloud' ).
    DATA(lv_result) = 10 + 20.
    out->write( |ผลลัพธ์ : { lv_result }| ).
    ENDMETHOD.
ENDCLASS. 
