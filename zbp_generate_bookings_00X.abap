CLASS zbp_generate_bookings_003 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    " Method สำหรับ Insert bookings
    METHODS: insert_bookings
      IMPORTING
        out             TYPE REF TO if_oo_adt_classrun_out OPTIONAL
      RETURNING
        VALUE(rv_count) TYPE i.

    " Method สำหรับ Delete booking
    METHODS: delete_booking
      IMPORTING
        iv_booking_number TYPE c
        out               TYPE REF TO if_oo_adt_classrun_out OPTIONAL
      RETURNING
        VALUE(rv_success) TYPE abap_bool.

    METHODS: delete_all_bookings
      IMPORTING
        out               TYPE REF TO if_oo_adt_classrun_out OPTIONAL
      RETURNING
        VALUE(rv_deleted) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zbp_generate_bookings_003 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    "================================================
    " MAIN METHOD - เลือก Method ที่ต้องการ Run
    "================================================

    DATA: lv_action TYPE string.

    " *** เปลี่ยนค่า lv_action เพื่อเลือก method ที่ต้องการ run ***
    " ตัวเลือก: 'INSERT', 'DELETE', 'DELETE_ALL',
    lv_action = 'DELETE_ALL'.  " <-- เปลี่ยนค่านี้

    out->write( |========================================| ).
    out->write( |   BOOKING MANAGEMENT SYSTEM           | ).
    out->write( |========================================| ).
    out->write( | | ).

    CASE lv_action.
      WHEN 'INSERT'.
        "------------------------------------------------
        " Run เฉพาะ INSERT method
        "------------------------------------------------
        out->write( |Action: INSERT NEW BOOKINGS| ).
        out->write( |----------------------------------------| ).

        DATA(lv_inserted) = insert_bookings( out ).

        out->write( | | ).
        out->write( |Total bookings inserted: { lv_inserted }| ).

      WHEN 'DELETE'.
        "------------------------------------------------
        " Run เฉพาะ DELETE method
        "------------------------------------------------
        out->write( |Action: DELETE BOOKING| ).
        out->write( |----------------------------------------| ).

        DATA(lv_booking_to_delete) = '3'.  " <-- เปลี่ยน booking number ที่ต้องการลบ
        DATA(lv_result) = delete_booking(
          iv_booking_number = lv_booking_to_delete
          out = out
        ).

        IF lv_result = abap_true.
          out->write( |✓ Booking { lv_booking_to_delete } deleted successfully!| ).
        ELSE.
          out->write( |✗ Failed to delete booking { lv_booking_to_delete }| ).
        ENDIF.

       WHEN 'DELETE_ALL'.
        "------------------------------------------------
        " ลบข้อมูลทั้งหมดในตาราง
        "------------------------------------------------
        out->write( |Action: DELETE ALL BOOKINGS| ).
        out->write( |----------------------------------------| ).
        out->write( |⚠️  WARNING: This will delete ALL bookings!| ).
        out->write( | | ).

        DATA(lv_deleted) = delete_all_bookings( out ).

        out->write( | | ).
        IF lv_deleted > 0.
          out->write( |✓ Successfully deleted { lv_deleted } bookings| ).
        ELSE.
          out->write( |ℹ️  No bookings to delete| ).
        ENDIF.

    ENDCASE.

    out->write( | | ).
    out->write( |========================================| ).
    out->write( |          PROCESS COMPLETED             | ).
    out->write( |========================================| ).


  ENDMETHOD.

  METHOD insert_bookings.
    DATA: it_bookings TYPE TABLE OF ztbooking_002,
          lv_ts       TYPE timestampl,
          lv_now      TYPE timestampl.

    " Get current timestamp
    GET TIME STAMP FIELD DATA(lv_tsl).
    GET TIME STAMP FIELD lv_ts.
    GET TIME STAMP FIELD lv_now.

    " Prepare booking data
    it_bookings = VALUE #(
      ( booking = '1'
        customername = 'Buchholm'
        numberofpassengers = '3'
        emailaddress = 'tester1@flight.example.com'
        country = 'Germany'
        dateofbooking = lv_now
        dateoftravel = lv_ts
        cost = '546'
        currencycode = 'EUR'
        lastchangedat = lv_tsl )

      ( booking = '2'
        customername = 'Jeremias'
        numberofpassengers = '1'
        emailaddress = 'tester2@flight.example.com'
        country = 'USA'
        dateofbooking = lv_now
        dateoftravel = lv_ts
        cost = '1373'
        currencycode = 'USD'
        lastchangedat = lv_tsl )

      ( booking = '3'
        customername = 'Tanaka'
        numberofpassengers = '2'
        emailaddress = 'tester3@flight.example.com'
        country = 'Japan'
        dateofbooking = lv_now
        dateoftravel = lv_ts
        cost = '2150'
        currencycode = 'JPY'
        lastchangedat = lv_tsl )

      ( booking = '4'
        customername = 'Martinez'
        numberofpassengers = '4'
        emailaddress = 'tester4@flight.example.com'
        country = 'Spain'
        dateofbooking = lv_now
        dateoftravel = lv_ts
        cost = '890'
        currencycode = 'EUR'
        lastchangedat = lv_tsl )

      ( booking = '5'
        customername = 'Smith'
        numberofpassengers = '2'
        emailaddress = 'tester5@flight.example.com'
        country = 'UK'
        dateofbooking = lv_now
        dateoftravel = lv_ts
        cost = '1120'
        currencycode = 'GBP'
        lastchangedat = lv_tsl )
    ).

    " Insert new bookings
    INSERT ztb_booking_003 FROM TABLE @it_bookings.
    IF sy-subrc = 0.
      COMMIT WORK.
      rv_count = sy-dbcnt.

      IF out IS BOUND.
        out->write( |Inserting { lines( it_bookings ) } bookings...| ).
        LOOP AT it_bookings INTO DATA(ls_booking).
          out->write( |  - Booking { ls_booking-booking }: { ls_booking-customername } ({ ls_booking-country })| ).
        ENDLOOP.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      rv_count = 0.

      IF out IS BOUND.
        out->write( |ERROR: Failed to insert bookings| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD delete_booking.
    " Initialize return value
    rv_success = abap_false.

    " Check if booking exists
    SELECT SINGLE booking, customername FROM ztb_booking_003
      WHERE booking = @iv_booking_number
      INTO @DATA(ls_existing).

    IF sy-subrc = 0.
      " Booking exists, proceed with deletion
      DELETE FROM ztb_booking_003 WHERE booking = @iv_booking_number.

      IF sy-subrc = 0.
        COMMIT WORK.
        rv_success = abap_true.
        IF out IS BOUND.
          out->write( |Deleting booking { iv_booking_number }: { ls_existing-customername }...| ).
          out->write( |Deleted successfully| ).
        ENDIF.
      ELSE.
        ROLLBACK WORK.

        IF out IS BOUND.
          out->write( |ERROR: Failed to delete booking { iv_booking_number }| ).
        ENDIF.
      ENDIF.
    ELSE.
      IF out IS BOUND.
        out->write( |WARNING: Booking { iv_booking_number } not found| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD delete_all_bookings.

  " ลบข้อมูลทั้งหมด
    

  ENDMETHOD.

ENDCLASS.
