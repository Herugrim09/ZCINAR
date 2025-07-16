*&---------------------------------------------------------------------*
*& Report zmdg_create_table
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmdg_create_table.


* Structure for holding field definitions
TYPES: BEGIN OF ty_fields,
         fieldname TYPE fieldname,
         rollname  TYPE rollname,
         is_key    TYPE flag,
       END OF ty_fields.

* Global data declarations
DATA: gt_fields TYPE TABLE OF ty_fields,
      gs_fields TYPE ty_fields.

* Selection Screen Definition
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_tname  TYPE tabname OBLIGATORY,
              p_pkg    TYPE devclass OBLIGATORY,
              p_desc   TYPE as4text OBLIGATORY,
              p_dclass TYPE dd02v-exclass OBLIGATORY DEFAULT 'C',
              p_log    AS CHECKBOX DEFAULT 'X',
              p_buff   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

* Field Definition Block
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_field1 TYPE fieldname OBLIGATORY,
              p_type1  TYPE rollname OBLIGATORY,
              p_key1   AS CHECKBOX DEFAULT 'X',

              p_field2 TYPE fieldname,
              p_type2  TYPE rollname,
              p_key2   AS CHECKBOX,

              p_field3 TYPE fieldname,
              p_type3  TYPE rollname,
              p_key3   AS CHECKBOX,

              p_field4 TYPE fieldname,
              p_type4  TYPE rollname,
              p_key4   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

* Additional Fields Block
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_atr1   TYPE fieldname,
              p_atype1 TYPE rollname,

              p_atr2   TYPE fieldname,
              p_atype2 TYPE rollname,

              p_atr3   TYPE fieldname,
              p_atype3 TYPE rollname.
SELECTION-SCREEN END OF BLOCK b3.

* Initialization
INITIALIZATION.
  p_field1 = 'MANDT'.
  p_type1  = 'MANDT'.

* Form to validate package
FORM check_package.
  DATA: ls_package TYPE tdevc,
        lv_rc      TYPE sy-subrc.

  SELECT SINGLE * FROM tdevc INTO ls_package
    WHERE devclass = p_pkg.

  IF sy-subrc <> 0.
    MESSAGE e000(00) WITH 'Package' p_pkg 'does not exist'.
  ENDIF.
ENDFORM.

* AT SELECTION-SCREEN for package validation
AT SELECTION-SCREEN.
  PERFORM check_package.

* Form to collect field definitions
FORM collect_fields.
  CLEAR gt_fields.

  " Add mandatory client field
  IF p_field1 IS NOT INITIAL AND p_type1 IS NOT INITIAL.
    gs_fields-fieldname = p_field1.
    gs_fields-rollname  = p_type1.
    gs_fields-is_key    = p_key1.
    APPEND gs_fields TO gt_fields.
  ENDIF.

  " Add optional key fields
  IF p_field2 IS NOT INITIAL AND p_type2 IS NOT INITIAL.
    gs_fields-fieldname = p_field2.
    gs_fields-rollname  = p_type2.
    gs_fields-is_key    = p_key2.
    APPEND gs_fields TO gt_fields.
  ENDIF.

  IF p_field3 IS NOT INITIAL AND p_type3 IS NOT INITIAL.
    gs_fields-fieldname = p_field3.
    gs_fields-rollname  = p_type3.
    gs_fields-is_key    = p_key3.
    APPEND gs_fields TO gt_fields.
  ENDIF.

  IF p_field4 IS NOT INITIAL AND p_type4 IS NOT INITIAL.
    gs_fields-fieldname = p_field4.
    gs_fields-rollname  = p_type4.
    gs_fields-is_key    = p_key4.
    APPEND gs_fields TO gt_fields.
  ENDIF.

  " Add attributes (non-key fields)
  IF p_atr1 IS NOT INITIAL AND p_atype1 IS NOT INITIAL.
    gs_fields-fieldname = p_atr1.
    gs_fields-rollname  = p_atype1.
    gs_fields-is_key    = ''.
    APPEND gs_fields TO gt_fields.
  ENDIF.

  IF p_atr2 IS NOT INITIAL AND p_atype2 IS NOT INITIAL.
    gs_fields-fieldname = p_atr2.
    gs_fields-rollname  = p_atype2.
    gs_fields-is_key    = ''.
    APPEND gs_fields TO gt_fields.
  ENDIF.

  IF p_atr3 IS NOT INITIAL AND p_atype3 IS NOT INITIAL.
    gs_fields-fieldname = p_atr3.
    gs_fields-rollname  = p_atype3.
    gs_fields-is_key    = ''.
    APPEND gs_fields TO gt_fields.
  ENDIF.
ENDFORM.

* Main execution logic
START-OF-SELECTION.
  PERFORM collect_fields.
  PERFORM add_to_transport USING p_tname p_pkg 'MESK900047'. " Replace 'MESK900047' with dynamic input
  PERFORM create_table.

* Form to create the table
FORM create_table.
  DATA: ls_dd02v TYPE dd02v,
        ls_dd09l TYPE dd09l,
        lt_dd03p TYPE TABLE OF dd03p,
        ls_dd03p TYPE dd03p.
  DATA: lv_position TYPE tabfdpos,
        lv_pgmid    TYPE e071-pgmid,
        lv_object   TYPE e071-object,
        lv_obj_name TYPE e071-obj_name.

  " First, add table to package
  lv_pgmid    = 'R3TR'.
  lv_object   = 'TABL'.
  lv_obj_name = p_pkg.

  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = 'DEVC'
      obj_name          = lv_obj_name
    EXCEPTIONS
      object_not_found  = 1
      object_not_local  = 2
      object_exists     = 3
      object_locked     = 4
      wrong_object_name = 5
      OTHERS            = 6.

  DATA: ls_objects TYPE trexreqob.
  DATA: lt_objects TYPE TABLE OF trexreqob.

  ls_objects-obj_name = p_tname.
  ls_objects-object = 'TABL'.
  ls_objects-pgmid = 'R3TR'.

  APPEND ls_objects TO lt_objects.


  CALL FUNCTION 'TR_EXT_INSERT_IN_REQUEST'
    EXPORTING
      iv_req_id  = 'MESK900047'               " Request/task
*    IMPORTING
*     ev_exception =                  " Name of exception
*     es_msg     =                  " Correction Requests: Error Messages
    TABLES
      it_objects = lt_objects.                " Correction Requests: Object List of a Request/Task

  IF sy-subrc <> 0 AND sy-subrc <> 3.  " Ignore if object already exists
    MESSAGE e000(00) WITH 'Error adding table to package:' sy-subrc.
    RETURN.
  ENDIF.

  " Prepare table header (DD02V)
  CLEAR ls_dd02v.
  ls_dd02v-tabname    = p_tname.
  ls_dd02v-ddlanguage = sy-langu.
  ls_dd02v-tabclass   = 'TRANSP'.
  ls_dd02v-mainflag   = 'X'.
  ls_dd02v-contflag   = 'A'.
  ls_dd02v-exclass    = p_dclass.
  ls_dd02v-ddtext     = p_desc.

  " Technical Settings (DD09L)
  CLEAR ls_dd09l.
  ls_dd09l-tabname  = p_tname.
  ls_dd09l-as4local = 'A'.
  ls_dd09l-tabkat   = '0'.
  ls_dd09l-tabart   = 'APPL0'.
  ls_dd09l-protokoll = p_log.
  ls_dd09l-bufallow = p_buff.

  " Create field definitions
  CLEAR: lt_dd03p, lv_position.
  LOOP AT gt_fields INTO gs_fields.
    lv_position = lv_position + 1.

    CLEAR ls_dd03p.
    ls_dd03p-tabname   = p_tname.
    ls_dd03p-fieldname = gs_fields-fieldname.
    ls_dd03p-rollname  = gs_fields-rollname.
    ls_dd03p-keyflag   = gs_fields-is_key.
    ls_dd03p-position  = lv_position.
    APPEND ls_dd03p TO lt_dd03p.
  ENDLOOP.

  " Create the table
  CALL FUNCTION 'DDIF_TABL_PUT'
    EXPORTING
      name              = p_tname
      dd02v_wa          = ls_dd02v
      dd09l_wa          = ls_dd09l
    TABLES
      dd03p_tab         = lt_dd03p
    EXCEPTIONS
      tabl_not_found    = 1
      name_inconsistent = 2
      tabl_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.

  IF sy-subrc = 0.
    WRITE: / 'Table definition created successfully. Activating...'.

    " Activate the table
    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = p_tname
        auth_chk    = 'X'
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.

    IF sy-subrc = 0.
      WRITE: / 'Table created and activated successfully in package!'.
    ELSE.
      WRITE: / 'Error during table activation!'.
    ENDIF.
  ELSE.
    WRITE: / 'Error occurred during table creation!'.
    CASE sy-subrc.
      WHEN 1. WRITE: / 'Table not found'.
      WHEN 2. WRITE: / 'Name inconsistent'.
      WHEN 3. WRITE: / 'Table inconsistent'.
      WHEN 4. WRITE: / 'Put failure'.
      WHEN 5. WRITE: / 'Put refused'.
      WHEN OTHERS. WRITE: / 'Unknown error'.
    ENDCASE.
  ENDIF.
ENDFORM.
* Form to add the table to the transport request
FORM add_to_transport USING p_tname TYPE tabname
                             p_pkg   TYPE devclass
                             p_req   TYPE trkorr.

  DATA: lv_pgmid    TYPE e071-pgmid,
        lv_object   TYPE e071-object,
        lv_obj_name TYPE e071-obj_name,
        lv_trkorr   TYPE e070-trkorr.

  " Validate the transport request
  IF p_req IS INITIAL.
    MESSAGE 'Please provide a transport request.' TYPE 'E'.
    RETURN.
  ENDIF.

  " Define transport object details
  lv_pgmid    = 'R3TR'.
  lv_object   = 'TABL'. " Table object
  lv_obj_name = p_tname.
  lv_trkorr   = p_req.

  " Add the table to the transport request using RS_CORR_INSERT
  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object_class        = lv_object     " Program ID (e.g., R3TR)
      object              = lv_obj_name     " Object type (e.g., TABL)
*     obj_name            = lv_obj_name   " Object name (table name)
      global_lock         = 'X'
      korrnum             = lv_trkorr     " Transport request number
      devclass            = 'ZMERVE'
    EXCEPTIONS
      object_not_found    = 1             " Object does not exist
      corr_not_found      = 2             " Transport request does not exist
      corr_no_authority   = 3             " No authorization for request
      already_in_corr     = 4             " Object already in request
      illegal_pgmid       = 5             " Invalid Program ID
      illegal_object_type = 6             " Invalid Object Type
      internal_error      = 7             " Internal error
      OTHERS              = 8.

  " Handle errors
  CASE sy-subrc.
    WHEN 0.
      WRITE: / 'Table added to transport request successfully.'.
    WHEN 1.
      MESSAGE 'Object not found.' TYPE 'E'.
    WHEN 2.
      MESSAGE 'Transport request not found.' TYPE 'E'.
    WHEN 3.
      MESSAGE 'No authorization for the transport request.' TYPE 'E'.
    WHEN 4.
      MESSAGE 'Object already exists in the transport request.' TYPE 'I'.
    WHEN 5.
      MESSAGE 'Invalid Program ID.' TYPE 'E'.
    WHEN 6.
      MESSAGE 'Invalid Object Type.' TYPE 'E'.
    WHEN 7.
      MESSAGE 'Internal error occurred.' TYPE 'E'.
    WHEN OTHERS.
      MESSAGE 'Unknown error occurred while adding to transport request.' TYPE 'E'.
  ENDCASE.

ENDFORM.
