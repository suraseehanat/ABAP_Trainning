CLASS zcl_zmik_customer_01_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zmik_customer_01_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS customerset_get_entityset
        REDEFINITION .
    METHODS customerset_create_entity
        REDEFINITION .
    METHODS customerset_update_entity
        REDEFINITION .
    METHODS customerset_delete_entity
        REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zmik_customer_01_dpc_ext IMPLEMENTATION.


  METHOD customerset_create_entity.
    DATA : ls_customer TYPE zdemocustomer01,
           ls_entity   TYPE zcl_zmik_customer_01_mpc=>ts_customer.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).

    " Validate Required Fields
    IF ls_entity-customerid IS INITIAL.
      " Generate Customer ID - Simple Method
      SELECT MAX( customer_id ) FROM zdemocustomer01 INTO @DATA(lv_max_id).
      IF lv_max_id IS INITIAL.
        ls_entity-customerid = 'CUST000001'.
      ELSE.
        " Extract numeric part and increment
        DATA(lv_num) = CONV i( lv_max_id+4(6) ).
        lv_num = lv_num + 1.
        ls_entity-customerid = |CUST{ lv_num WIDTH = 6 PAD = '0' }|.
      ENDIF.
    ENDIF.

    " Check if Customer ID already exists
    SELECT SINGLE customer_id FROM zdemocustomer01
      INTO @DATA(lv_existing)
      WHERE customer_id = @ls_entity-customerid.
    IF sy-subrc = 0.
      " Raise Error - Customer already exists
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid           = /iwbep/cx_mgw_busi_exception=>business_error
          entity_type      = 'Customer'
          http_status_code = '400'.
    ENDIF.
    " Convert Entity to Table Structure
    ls_customer-customer_id = ls_entity-customerid.
    ls_customer-name = ls_entity-name.
    ls_customer-email = ls_entity-email.
    ls_customer-phone = ls_entity-phone.
    ls_customer-city = ls_entity-city.

    " Insert into Database
    INSERT zdemocustomer01 FROM ls_customer.

    IF sy-subrc = 0.
      COMMIT WORK.
      " Return Created Entity
      er_entity = ls_entity.
    ELSE.
      ROLLBACK WORK.
      " Raise Error for Insert Failure
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid           = /iwbep/cx_mgw_busi_exception=>business_error
          entity_type      = 'Customer'
          http_status_code = '500'.
    ENDIF.
  ENDMETHOD.

  METHOD customerset_get_entityset.
    DATA: lt_customer  TYPE STANDARD TABLE OF zdemocustomer01,
          ls_customer  TYPE zdemocustomer01,
          ls_entity    TYPE zcl_zmik_customer_01_mpc=>ts_customer,
          lt_entityset TYPE zcl_zmik_customer_01_mpc=>tt_customer.

    DATA: lv_filter_string TYPE string,
          lt_filters       TYPE /iwbep/t_mgw_select_option,
          lr_customer_id   TYPE RANGE OF zdemocustomer01-customer_id,
          lr_name          TYPE RANGE OF zdemocustomer01-name,
          lr_city          TYPE RANGE OF zdemocustomer01-city.

    " อ่าน Filter Parameters
    lt_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    " Process Filters
    LOOP AT lt_filters INTO DATA(ls_filter).
      CASE ls_filter-property.
        WHEN 'CustomerID'.
          MOVE-CORRESPONDING ls_filter-select_options TO lr_customer_id.
        WHEN 'Name'.
          MOVE-CORRESPONDING ls_filter-select_options TO lr_name.
        WHEN 'City'.
          MOVE-CORRESPONDING ls_filter-select_options TO lr_city.
      ENDCASE.
    ENDLOOP.

    " Query Database with Filters
    SELECT * FROM zdemocustomer01
      INTO TABLE lt_customer
      WHERE customer_id IN lr_customer_id
        AND name IN lr_name
        AND city IN lr_city.

    LOOP AT lt_customer INTO ls_customer.
      ls_entity-customerid = ls_customer-customer_id.
      ls_entity-name = ls_customer-name.
      ls_entity-email = ls_customer-email.
      ls_entity-phone = ls_customer-phone.
      ls_entity-city = ls_customer-city.
      APPEND ls_entity TO lt_entityset.
    ENDLOOP.

    " Return Data
    et_entityset = lt_entityset.
  ENDMETHOD.

  METHOD customerset_update_entity.
    DATA: ls_customer    TYPE zdemocustomer01,
          ls_entity      TYPE zcl_zmik_customer_01_mpc=>ts_customer,
          lv_customer_id TYPE zdemocustomer01-customer_id.

    " อ่านข้อมูลจาก Request
    io_data_provider->read_entry_data( IMPORTING es_data = ls_entity ).

    " ดึง Customer ID จาก Key
    " Method 1: จาก URL Key
    DATA(lt_keys) = io_tech_request_context->get_keys( ).
    READ TABLE lt_keys INTO DATA(ls_key) WITH KEY name = 'CUSTOMERID'.
    IF sy-subrc = 0.
      lv_customer_id = ls_key-value.
    ENDIF.

    " Method 2: จาก Entity (backup)
    IF lv_customer_id IS INITIAL.
      lv_customer_id = ls_entity-customerid.
    ENDIF.

    " ตรวจสอบว่า Customer มีอยู่จริง
    SELECT SINGLE * FROM zdemocustomer01
      INTO @ls_customer
      WHERE customer_id = @lv_customer_id.

     IF sy-dbcnt = 0.
       RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
         EXPORTING
           textid           = /iwbep/cx_mgw_busi_exception=>business_error
           entity_type      = 'Customer'
           http_status_code = '404'
           message          = |Customer { lv_customer_id } not found|.
     ENDIF.

    " Validate ข้อมูลที่จะ Update (Optional)
    IF ls_entity-email IS NOT INITIAL.
      IF ls_entity-email NS '@' OR ls_entity-email NS '.'.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid           = /iwbep/cx_mgw_busi_exception=>business_error
            entity_type      = 'Customer'
            http_status_code = '400'.
      ENDIF.
    ENDIF.

    " Update ข้อมูลที่เปลี่ยนแปลง (Partial Update Support)
    IF ls_entity-name IS NOT INITIAL.
      ls_customer-name = ls_entity-name.
    ENDIF.

    IF ls_entity-email IS NOT INITIAL.
      ls_customer-email = ls_entity-email.
    ENDIF.

    IF ls_entity-phone IS NOT INITIAL.
      ls_customer-phone = ls_entity-phone.
    ENDIF.

    IF ls_entity-city IS NOT INITIAL.
      ls_customer-city = ls_entity-city.
    ENDIF.

    " Update Database
    UPDATE zdemocustomer01 SET
      name = @ls_customer-name,
      email = @ls_customer-email,
      phone = @ls_customer-phone,
      city = @ls_customer-city
    WHERE customer_id = @lv_customer_id.

    IF sy-subrc = 0.
      COMMIT WORK.

      " Return Updated Entity
      er_entity-customerid = ls_customer-customer_id.
      er_entity-name = ls_customer-name.
      er_entity-email = ls_customer-email.
      er_entity-phone = ls_customer-phone.
      er_entity-city = ls_customer-city.
    ELSE.
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid           = /iwbep/cx_mgw_busi_exception=>business_error
        entity_type      = 'Customer'
        http_status_code = '500'
        message          = 'Failed to update customer in database'.
    ENDIF.
  ENDMETHOD.

  METHOD customerset_delete_entity.
  DATA: lv_customer_id TYPE zdemocustomer01-customer_id.

  TRY.
    " ดึง Customer ID จาก URL Key
    DATA(lt_keys) = io_tech_request_context->get_keys( ).
    READ TABLE lt_keys INTO DATA(ls_key) WITH KEY name = 'CUSTOMERID'.
    IF sy-subrc = 0.
      lv_customer_id = ls_key-value.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid           = /iwbep/cx_mgw_busi_exception=>business_error
          entity_type      = 'Customer'
          http_status_code = '400'
          message          = 'Customer ID is required for delete operation'.
    ENDIF.

    " ตรวจสอบว่า Customer มีอยู่จริงก่อนลบ
    SELECT COUNT(*) FROM zdemocustomer01
      WHERE customer_id = @lv_customer_id.

    IF sy-dbcnt = 0.
      " Customer ไม่มีอยู่
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid           = /iwbep/cx_mgw_busi_exception=>business_error
          entity_type      = 'Customer'
          http_status_code = '404'
          message          = |Customer { lv_customer_id } not found|.
    ENDIF.

    " ลบข้อมูลจาก Database
    DELETE FROM zdemocustomer01
      WHERE customer_id = @lv_customer_id.

    IF sy-subrc = 0.
      COMMIT WORK.
      " DELETE method ไม่ต้อง return data
      " HTTP 204 No Content จะถูกส่งกลับโดยอัตโนมัติ
    ELSE.
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid           = /iwbep/cx_mgw_busi_exception=>business_error
          entity_type      = 'Customer'
          http_status_code = '500'
          message          = |Failed to delete customer { lv_customer_id }|.
    ENDIF.

  CATCH cx_root INTO DATA(lx_error).
    ROLLBACK WORK.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid           = /iwbep/cx_mgw_busi_exception=>business_error
        entity_type      = 'Customer'
        http_status_code = '500'
        message          = |Error deleting customer: { lx_error->get_text( ) }|.
  ENDTRY.
ENDMETHOD.

ENDCLASS.
