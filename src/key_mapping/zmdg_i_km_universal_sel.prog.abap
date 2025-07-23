*&---------------------------------------------------------------------*
*& Include          ZMDG_I_KM_UNIVERSAL_SEL
*&---------------------------------------------------------------------*
*& Universal Key Mapping Program - Selection Screen
*&---------------------------------------------------------------------*

"----------------------------------------------------------------------
" Block 1: Object Type and Scheme Selection
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_otc  TYPE mdg_object_type_code_bs OBLIGATORY.
  PARAMETERS: p_idsc TYPE mdg_ids_type_code_bs OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

"----------------------------------------------------------------------
" Block 2: Operation Mode Selection
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: rb_disp RADIOBUTTON GROUP op1 DEFAULT 'X' USER-COMMAND mode.
    SELECTION-SCREEN COMMENT 3(25) TEXT-006.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: rb_crt RADIOBUTTON GROUP op1.
    SELECTION-SCREEN COMMENT 3(25) TEXT-007.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: rb_upd RADIOBUTTON GROUP op1.
    SELECTION-SCREEN COMMENT 3(25) TEXT-008.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: rb_del RADIOBUTTON GROUP op1.
    SELECTION-SCREEN COMMENT 3(25) TEXT-009.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

"----------------------------------------------------------------------
" Block 3: Filter Criteria (Visible for Display Mode)
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: s_srcsy FOR gwa_business_system-business_system_id MODIF ID dis,
                  s_tgtsy FOR gwa_business_system-business_system_id MODIF ID dis,
                  s_srcid FOR gwa_key_mapping_display-source_id_value MODIF ID dis,
                  s_tgtid FOR gwa_key_mapping_display-target_id_value MODIF ID dis.
SELECTION-SCREEN END OF BLOCK b3.

"----------------------------------------------------------------------
" Block 4: File Upload Options (Visible for Create/Update/Delete Modes)
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_file TYPE string MODIF ID upl.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(15) FOR FIELD p_format MODIF ID upl.
    PARAMETERS: p_format TYPE char4 DEFAULT 'XLSX' MODIF ID upl.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(20) FOR FIELD p_header MODIF ID upl.
    PARAMETERS: p_header AS CHECKBOX DEFAULT 'X' MODIF ID upl.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b4.

"----------------------------------------------------------------------
" Block 5: Processing Options
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_test AS CHECKBOX MODIF ID opt.
    SELECTION-SCREEN COMMENT 3(25) FOR FIELD p_test.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_valid AS CHECKBOX MODIF ID opt.
    SELECTION-SCREEN COMMENT 3(25) FOR FIELD p_valid.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(15) FOR FIELD p_batch.
    PARAMETERS: p_batch TYPE i DEFAULT 1000 MODIF ID opt.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b5.

"----------------------------------------------------------------------
" Selection Screen Events
"----------------------------------------------------------------------

" At Selection Screen Output - Control Field Visibility

AT SELECTION-SCREEN OUTPUT.
  PERFORM control_screen_visibility.

  " Value Request for Object Type

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_otc.
  PERFORM f4_object_type CHANGING p_otc.

  " Value Request for ID Scheme Type

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_idsc.
  PERFORM f4_ids_type USING p_otc CHANGING p_idsc.

  " Value Request for File Path

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file_path CHANGING p_file.

  " Value Request for File Format

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_format.
  PERFORM f4_file_format CHANGING p_format.

  " Validation when Object Type changes

AT SELECTION-SCREEN ON p_otc.
  PERFORM validate_object_type USING p_otc.
  CLEAR p_idsc. " Reset ID scheme when object type changes

  " Validation when ID Scheme changes

AT SELECTION-SCREEN ON p_idsc.
  PERFORM validate_ids_type USING p_otc p_idsc.

  " Final validation before processing

*AT SELECTION-SCREEN.
*  PERFORM validate_selection_screen.
*

START-OF-SELECTION.
  " Validate file path only when actually executing
  IF gv_operation_mode = gc_mode_create OR
     gv_operation_mode = gc_mode_update OR
     gv_operation_mode = gc_mode_delete.
    IF gv_file_path IS INITIAL.
      MESSAGE e003(zmdg_km) WITH 'File path is required for' gv_operation_mode 'operation'.
      RETURN.
    ENDIF.
  ENDIF.
  "----------------------------------------------------------------------
  " Screen Control Forms
  "----------------------------------------------------------------------

FORM control_screen_visibility.

  " Determine operation mode
  IF rb_disp = abap_true.
    gv_operation_mode = gc_mode_display.
  ELSEIF rb_crt = abap_true.
    gv_operation_mode = gc_mode_create.
  ELSEIF rb_upd = abap_true.
    gv_operation_mode = gc_mode_update.
  ELSEIF rb_del = abap_true.
    gv_operation_mode = gc_mode_delete.
  ENDIF.

  " Control field visibility based on operation mode
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'DIS'. " Display mode fields
        IF gv_operation_mode = gc_mode_display.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.

      WHEN 'UPL'. " Upload mode fields
        IF gv_operation_mode = gc_mode_create OR
           gv_operation_mode = gc_mode_update OR
           gv_operation_mode = gc_mode_delete.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.

      WHEN 'OPT'. " Optional fields
        screen-active = 1.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

FORM f4_object_type CHANGING cv_object_type TYPE mdg_object_type_code_bs.

  TYPES: BEGIN OF ty_f4_object_type,
           object_type_code TYPE mdg_object_type_code_bs,
           description      TYPE mdg_object_type_code_desc_bs,
         END OF ty_f4_object_type.

  DATA: lt_object_types TYPE TABLE OF ty_f4_object_type,
        lt_return_tab   TYPE TABLE OF ddshretval,
        lwa_return      TYPE ddshretval.

  " Load object types if not already loaded
  IF gt_object_types IS INITIAL.
    PERFORM load_object_types.
  ENDIF.

  " Convert to local table structure
  LOOP AT gt_object_types INTO DATA(lwa_otc).
    APPEND VALUE ty_f4_object_type( object_type_code = lwa_otc-object_type_code
                                   description = lwa_otc-description ) TO lt_object_types.
  ENDLOOP.

  " Prepare value help data
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJECT_TYPE_CODE'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      window_title    = 'Select Object Type'
      value_org       = 'S'
    TABLES
      value_tab       = lt_object_types
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE lt_return_tab INTO lwa_return INDEX 1.
    IF sy-subrc = 0.
      cv_object_type = lwa_return-fieldval.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f4_ids_type USING iv_object_type TYPE mdg_object_type_code_bs
                 CHANGING cv_ids_type TYPE mdg_ids_type_code_bs.

  TYPES: BEGIN OF ty_f4_ids_type,
           ids_type_code    TYPE mdg_ids_type_code_bs,
           object_type_code TYPE mdg_object_type_code_bs,
           description      TYPE mdg_ids_type_code_desc_bs,
         END OF ty_f4_ids_type.

  DATA: lt_ids_types  TYPE TABLE OF ty_f4_ids_type,
        lt_return_tab TYPE TABLE OF ddshretval,
        lwa_return    TYPE ddshretval.

  " Load ID scheme types if not already loaded
  IF gt_ids_types IS INITIAL.
    PERFORM load_ids_types.
  ENDIF.

  " Filter by object type and convert to local structure
  LOOP AT gt_ids_types INTO DATA(lwa_ids) WHERE object_type_code = iv_object_type.
    APPEND VALUE ty_f4_ids_type( ids_type_code = lwa_ids-ids_type_code
                                object_type_code = lwa_ids-object_type_code
                                description = lwa_ids-description ) TO lt_ids_types.
  ENDLOOP.

  IF lt_ids_types IS INITIAL.
    MESSAGE 'No ID scheme types found for selected object type' TYPE 'I'.
    RETURN.
  ENDIF.

  " Prepare value help data
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'IDS_TYPE_CODE'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      window_title    = 'Select ID Scheme Type'
      value_org       = 'S'
    TABLES
      value_tab       = lt_ids_types
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE lt_return_tab INTO lwa_return INDEX 1.
    IF sy-subrc = 0.
      cv_ids_type = lwa_return-fieldval.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f4_file_path CHANGING cv_file_path TYPE string.

  DATA: lt_file_table TYPE filetable,
        lv_rc         TYPE i,
        lv_action     TYPE i.

  " File open dialog
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select File for Upload'
      default_extension       = COND #( WHEN p_format = 'XLSX' THEN '*.xlsx'
                                        WHEN p_format = 'CSV'  THEN '*.csv'
                                        ELSE '*.txt' )
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
      user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0 AND lv_action = cl_gui_frontend_services=>action_ok.
    READ TABLE lt_file_table INTO DATA(lwa_file) INDEX 1.
    IF sy-subrc = 0.
      cv_file_path = lwa_file-filename.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f4_file_format CHANGING cv_format TYPE char4.

  DATA: lt_formats    TYPE TABLE OF string,
        lt_return_tab TYPE TABLE OF ddshretval,
        lwa_return    TYPE ddshretval.

  " Prepare format list
  APPEND 'XLSX' TO lt_formats.
  APPEND 'CSV' TO lt_formats.
  APPEND 'TXT' TO lt_formats.

  " Show value help
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TABLE_LINE'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      window_title    = 'Select File Format'
      value_org       = 'S'
    TABLES
      value_tab       = lt_formats
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    READ TABLE lt_return_tab INTO lwa_return INDEX 1.
    IF sy-subrc = 0.
      cv_format = lwa_return-fieldval.
    ENDIF.
  ENDIF.

ENDFORM.

FORM validate_object_type USING iv_object_type TYPE mdg_object_type_code_bs.

  " Load object types if not loaded
  IF gt_object_types IS INITIAL.
    PERFORM load_object_types.
  ENDIF.

  " Validate object type exists
  READ TABLE gt_object_types TRANSPORTING NO FIELDS
       WITH KEY object_type_code = iv_object_type.
  IF sy-subrc <> 0.
    MESSAGE e001(zmdg_km) WITH 'Invalid object type code:' iv_object_type.
  ENDIF.

ENDFORM.

FORM validate_ids_type USING iv_object_type TYPE mdg_object_type_code_bs
                              iv_ids_type    TYPE mdg_ids_type_code_bs.

  " Load ID scheme types if not loaded
  IF gt_ids_types IS INITIAL.
    PERFORM load_ids_types.
  ENDIF.

  " Validate ID scheme type exists for object type
  READ TABLE gt_ids_types TRANSPORTING NO FIELDS
       WITH KEY object_type_code = iv_object_type
                ids_type_code    = iv_ids_type.
  IF sy-subrc <> 0.
    MESSAGE e002(zmdg_km) WITH 'Invalid ID scheme for object type:' iv_ids_type iv_object_type.
  ENDIF.

ENDFORM.

FORM validate_selection_screen.

  " Store selected values in global variables
  gv_object_type_code = p_otc.
  gv_ids_type_code    = p_idsc.
  gv_file_path        = p_file.
  gv_has_header       = p_header.
  gv_file_format      = p_format.
  gv_test_mode        = p_test.
  gv_validate_only    = p_valid.
  gv_batch_size       = p_batch.

  " Copy ranges to global variables
  gr_source_system[] = s_srcsy[].
  gr_target_system[] = s_tgtsy[].
  gr_source_id[]     = s_srcid[].
  gr_target_id[]     = s_tgtid[].

  " Validate file path for upload operations
  IF gv_operation_mode = gc_mode_create OR
     gv_operation_mode = gc_mode_update OR
     gv_operation_mode = gc_mode_delete.

    IF gv_file_path IS INITIAL.
      MESSAGE e003(zmdg_km) WITH 'File path is required for' gv_operation_mode 'operation'.
    ENDIF.

    " Check file exists
    DATA: lv_exists TYPE abap_bool.

    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = gv_file_path
      RECEIVING
        result               = lv_exists
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.

    IF sy-subrc <> 0 OR lv_exists = abap_false.
      MESSAGE e004(zmdg_km) WITH 'File does not exist:' gv_file_path.
    ENDIF.
  ENDIF.

ENDFORM.
