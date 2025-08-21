REPORT z_mick_ex02_demo.

TYPES : BEGIN OF ty_student,
          id    TYPE i,
          name  TYPE c LENGTH 30,
          score TYPE i,
        END OF ty_student.

DATA: gt_student TYPE TABLE OF ty_student,
      gs_student TYPE ty_student.

gs_student-id = 1.
gs_student-name = 'Somchai'.
gs_student-score = 85.
APPEND gs_student TO gt_student.

gs_student-id = 2.
gs_student-name = 'Suda'.
gs_student-score = 72.
APPEND gs_student TO gt_student.

gs_student-id = 3.
gs_student-name = 'Anan'.
gs_student-score = 60.
APPEND gs_student TO gt_student.

LOOP AT gt_student INTO gs_student.
  WRITE : / 'ID:', gs_student-id,
            'Name:' , gs_student-name,
            'Score:', gs_student-score.
  IF gs_student-score >= 80.
    WRITE: 'Grade: A'.
  ELSEIF gs_student-score >= 70.
    WRITE: 'Grade: B'.
  ELSE.
    WRITE: 'Grade: C'.
  ENDIF.
ENDLOOP.
