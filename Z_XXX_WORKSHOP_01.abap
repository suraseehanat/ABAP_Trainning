REPORT Z_XXX_WORKSHOP_01.

"--------------------- Selection Screen ------------------------------"
TABLES:mara.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS:
  p_mtart  TYPE mara-mtart DEFAULT 'FERT',        "ประเภท Material
  p_spras  TYPE makt-spras DEFAULT 'E',            "ภาษา
  p_max    TYPE i DEFAULT 500.                     "จำกัดจำนวนแถว

SELECT-OPTIONS:
  s_matnr  FOR mara-matnr,                         "รหัส Material
  s_ersda  FOR mara-ersda.                         "วันที่สร้าง
SELECTION-SCREEN END OF BLOCK b1.

"--------------------- โครงสร้างผลลัพธ์ (เลือกเฉพาะฟิลด์ที่ใช้) ----"
TYPES: BEGIN OF ty_material,
         matnr    TYPE mara-matnr,                 "รหัส Material
         mtart    TYPE mara-mtart,                 "ประเภท Material
         matkl    TYPE mara-matkl,                 "กลุ่ม Material
         meins    TYPE mara-meins,                 "หน่วยพื้นฐาน
         ersda    TYPE mara-ersda,                 "วันที่สร้าง
         maktx    TYPE makt-maktx,                 "คำอธิบาย Material
       END OF ty_material.

DATA:
  gt_material TYPE STANDARD TABLE OF ty_material WITH EMPTY KEY,
  gs_material TYPE ty_material.

" ตัวแปรสำหรับ SALV
DATA: lo_alv TYPE REF TO cl_salv_table.

"--------------------- Main Logic ------------------------------------"
START-OF-SELECTION.

  " ตรวจ input
  IF p_max <= 0.
    MESSAGE 'กรุณาระบุจำนวนแถวมากกว่า 0' TYPE 'E'.
  ENDIF.

  "------------------ ดึงข้อมูลด้วย JOIN -----------------------------"
  SELECT
    a~matnr, a~mtart, a~matkl, a~meins, a~ersda,
    b~maktx
    FROM mara AS a
    INNER JOIN makt AS b
      ON a~matnr = b~matnr
    WHERE a~matnr IN @s_matnr
      AND a~mtart = @p_mtart
      AND a~ersda IN @s_ersda
      AND b~spras = @p_spras
    ORDER BY a~ersda DESCENDING, a~matnr
    INTO TABLE @gt_material
    UP TO @p_max ROWS.

  " ตรวจผลลัพธ์ + สาธิต IF/ELSE และ sy-subrc
  IF sy-subrc <> 0 OR gt_material IS INITIAL.
    MESSAGE 'ไม่พบข้อมูล Material ตามเงื่อนไข' TYPE 'S'.
    RETURN.
  ELSE.
    MESSAGE |พบข้อมูล { lines( gt_material ) } แถว| TYPE 'S'.
  ENDIF.

  "------------------ แสดงผลด้วย ALV (SALV) --------------------------"
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_material ).

      lo_alv->get_functions( )->set_all( abap_true ).           "ปุ่มมาตรฐานครบ
      lo_alv->get_columns( )->set_optimize( abap_true ).        "ปรับความกว้างอัตโนมัติ
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).

      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx).
      MESSAGE lx->get_text( ) TYPE 'E'.
  ENDTRY.
