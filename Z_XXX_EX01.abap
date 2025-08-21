REPORT z_mick_ex01_demo.
*
*DATA(lv_firstname) = |Surasee|.
*DATA(lv_lastname) = |Suraseehanat|.
*DATA lv_fullname  TYPE string.
*
*lv_fullname = |{ lv_firstname } { lv_lastname }|.
*WRITE : / |Fullname : { lv_fullname }|.

DATA lv_qty TYPE i value 7.
DATA(lv_price) = CONV decfloat34( '199.00' ).
DATA lv_disc TYPE p DECIMALS 2 VALUE '50.00'.
DATA lv_total TYPE p DECIMALS 2.

lv_total = lv_qty * lv_price - lv_disc.

write: / |Net : { lv_qty } * { lv_price } - {  lv_disc } = { lv_total }|.
