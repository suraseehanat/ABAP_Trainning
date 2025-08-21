REPORT Z_XXX_EX03.

" ประกาศ TABLES สำหรับใช้กับ SELECT-OPTIONS
TABLES: sflight, spfli.

"--------------------- Selection Screen ------------------------------"
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS:
  p_carrid TYPE sflight-carrid OBLIGATORY DEFAULT 'LH',  "สายการบิน
  p_max    TYPE i DEFAULT 100.                            "จำกัดจำนวนแถว

SELECT-OPTIONS:
  s_connid FOR sflight-connid,                            "เลขเที่ยวบิน
  s_fldate FOR sflight-fldate,                            "วันบิน
  s_price  FOR sflight-price.                             "ช่วงราคา

SELECTION-SCREEN END OF BLOCK b1.

"--------------------- โครงสร้างผลลัพธ์ (เลือกเฉพาะฟิลด์ที่ใช้) ----"
TYPES: BEGIN OF ty_flight,
         carrid   TYPE sflight-carrid,                    "สายการบิน
         connid   TYPE sflight-connid,                    "เลขเที่ยวบิน
         fldate   TYPE sflight-fldate,                    "วันบิน
         price    TYPE sflight-price,                     "ราคา
         currency TYPE sflight-currency,                  "สกุลเงิน
         cityfrom TYPE spfli-cityfrom,                    "เมืองต้นทาง
         cityto   TYPE spfli-cityto,                      "เมืองปลายทาง
         airpfrom TYPE spfli-airpfrom,                    "สนามบินต้นทาง
         airpto   TYPE spfli-airpto,                      "สนามบินปลายทาง
       END OF ty_flight.

DATA:
  gt_flight TYPE STANDARD TABLE OF ty_flight WITH EMPTY KEY,
  gs_flight TYPE ty_flight.

" ตัวแปรสำหรับ SALV
DATA: lo_alv TYPE REF TO cl_salv_table.

"--------------------- Main Logic ------------------------------------"
START-OF-SELECTION.

  " ตรวจ input
  IF p_max <= 0.
    MESSAGE 'กรุณาระบุ MAX มากกว่า 0' TYPE 'E'.
  ENDIF.

  "------------------ ดึงข้อมูลด้วย JOIN -----------------------------"
  SELECT
    a~carrid, a~connid, a~fldate, a~price, a~currency,
    b~cityfrom, b~cityto, b~airpfrom, b~airpto
    FROM sflight AS a
    INNER JOIN spfli AS b
      ON a~carrid = b~carrid
      AND a~connid = b~connid
    WHERE a~carrid = @p_carrid
      AND a~connid IN @s_connid      "ใช้ SELECT-OPTIONS
      AND a~fldate IN @s_fldate      "ใช้ SELECT-OPTIONS
      AND a~price  IN @s_price       "ใช้ SELECT-OPTIONS
    ORDER BY a~fldate, a~connid
    INTO TABLE @gt_flight
    UP TO @p_max ROWS.

  " ตรวจผลลัพธ์ + สาธิต IF/ELSE และ sy-subrc
  IF sy-subrc <> 0 OR gt_flight IS INITIAL.
    MESSAGE 'ไม่พบเที่ยวบินตามเงื่อนไข' TYPE 'S'.
    RETURN.
  ELSE.
    MESSAGE |พบข้อมูล { lines( gt_flight ) } แถว| TYPE 'S'.
  ENDIF.

  "------------------ แสดงผลด้วย ALV (SALV) --------------------------"
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_flight ).

      lo_alv->get_functions( )->set_all( abap_true ).           "ปุ่มมาตรฐานครบ
      lo_alv->get_columns( )->set_optimize( abap_true ).        "ปรับความกว้างอัตโนมัติ
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).

      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx).
      MESSAGE lx->get_text( ) TYPE 'E'.
  ENDTRY.
