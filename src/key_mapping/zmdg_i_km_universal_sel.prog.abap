*&---------------------------------------------------------------------*
*& Include          ZMDG_I_KM_UNIVERSAL_SEL
*&---------------------------------------------------------------------*
*& Universal Key Mapping Program - Selection Screen with Dynamic Popup
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
" Block 6: Dynamic Component Filter Button
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-030.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(40) pb_comp USER-COMMAND comp_filter MODIF ID cmp.
  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN COMMENT 1(40) FOR FIELD pb_comp MODIF ID cmp.
*  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b6.

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

AT SELECTION-SCREEN OUTPUT.
  PERFORM control_screen_visibility.
  PERFORM set_component_filter_button.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'COMP_FILTER'.
      PERFORM show_component_filter_screen.
  ENDCASE.

  "----------------------------------------------------------------------
  " Value Request Events
  "----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_otc.
  PERFORM f4_object_type CHANGING p_otc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_idsc.
  PERFORM f4_ids_type USING p_otc CHANGING p_idsc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file_path CHANGING p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_format.
  PERFORM f4_file_format CHANGING p_format.

  "----------------------------------------------------------------------
  " Validation Events
  "----------------------------------------------------------------------

AT SELECTION-SCREEN ON p_otc.
  PERFORM validate_object_type USING p_otc.
  CLEAR p_idsc.
  " Clear component filters when object type changes
  CLEAR: gt_component_filters, gv_comp_filter_set, gv_comp_filter_count.

AT SELECTION-SCREEN ON p_idsc.
  PERFORM validate_ids_type USING p_otc p_idsc.
  " Clear component filters when ID scheme changes
  CLEAR: gt_component_filters, gv_comp_filter_set, gv_comp_filter_count.

  "----------------------------------------------------------------------
  " Start of Selection Processing
  "----------------------------------------------------------------------

START-OF-SELECTION.
  " Store selection screen values in global variables
  PERFORM store_selection_values.

  " Validate required fields for file operations
  IF gv_operation_mode = gc_mode_create OR
     gv_operation_mode = gc_mode_update OR
     gv_operation_mode = gc_mode_delete.
    IF gv_file_path IS INITIAL.
      MESSAGE e003(zmdg_km) WITH 'File path is required for' gv_operation_mode 'operation'.
      RETURN.
    ENDIF.

    " Check if file exists
    PERFORM validate_file_exists.
  ENDIF.

  "----------------------------------------------------------------------
  " Form Implementations
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

      WHEN 'CMP'. " Component filter fields
        IF gv_operation_mode = gc_mode_display AND
           p_otc IS NOT INITIAL AND
           p_idsc IS NOT INITIAL.
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

FORM set_component_filter_button.

  DATA: lv_button_text TYPE string.

  " Set button text based on filter status
  IF gv_comp_filter_set = abap_true AND gv_comp_filter_count > 0.
    lv_button_text = |Component Filters ({ gv_comp_filter_count } active)|.
  ELSE.
    lv_button_text = 'Set Component Filters'.
  ENDIF.

  " Update button text
  pb_comp = lv_button_text.

ENDFORM.

FORM show_component_filter_screen.

  DATA: lv_multi_key    TYPE boole_d,
        lt_key_fields   TYPE ddfields,
        lv_no_key_struc TYPE boole_d,
        ls_oitc_data    TYPE mdg_s_ids_code_bs,
        lt_temp_filters TYPE TABLE OF ty_component_filter.

  " Validate prerequisites
  IF p_otc IS INITIAL OR p_idsc IS INITIAL.
    MESSAGE 'Please select Object Type and ID Scheme first' TYPE 'I'.
    RETURN.
  ENDIF.
  " Fetch key structure fields for current IDS type
  TRY.
      cl_mdg_id_matching_tools=>process_rtti_key_struc(
        EXPORTING
          iv_oitc = p_idsc
        IMPORTING
          ev_multi_key    = lv_multi_key
          et_key_fields   = lt_key_fields
          ev_no_key_struc = lv_no_key_struc
          es_oitc_data    = ls_oitc_data
      ).

    CATCH cx_root INTO DATA(lx_error).
      MESSAGE |DEBUG: ERROR in process_rtti_key_struc: { lx_error->get_text( ) }| TYPE 'E'.
  ENDTRY.

  " Check if key structure analysis was successful
  IF lv_no_key_struc = abap_true.
    RETURN.
  ENDIF.

  " Build component filter table from key structure
  LOOP AT lt_key_fields INTO DATA(ls_field).


    " Determine field label from DDIC
    DATA(lv_field_label) = ls_field-fieldtext.
    IF lv_field_label IS INITIAL.
      SELECT SINGLE ddtext INTO @lv_field_label
        FROM dd03t
        WHERE tabname   = @ls_field-tabname
          AND fieldname = @ls_field-fieldname
          AND ddlanguage = @sy-langu.
      IF sy-subrc <> 0 OR lv_field_label IS INITIAL.
        lv_field_label = ls_field-fieldname.
      ENDIF.
    ENDIF.

    " Add to temp filter table with metadata
    APPEND VALUE ty_component_filter(
      field_name    = ls_field-fieldname
      field_label   = lv_field_label
      field_value   = ''
      data_type     = ls_field-datatype
      field_length  = ls_field-leng
      convexit      = ls_field-convexit
    ) TO lt_temp_filters.

  ENDLOOP.


  " Copy existing filter values if available
  IF gt_component_filters IS NOT INITIAL.
    LOOP AT lt_temp_filters INTO DATA(lwa_temp).
      READ TABLE gt_component_filters INTO DATA(lwa_existing)
           WITH KEY field_name = lwa_temp-field_name.
      IF sy-subrc = 0.
        lwa_temp-field_value = lwa_existing-field_value.
        MODIFY lt_temp_filters FROM lwa_temp.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " Show popup for component filter input
  PERFORM show_component_filter_popup USING lt_temp_filters
                                     CHANGING gt_component_filters
                                              gv_comp_filter_set
                                              gv_comp_filter_count.

ENDFORM.
FORM show_component_filter_popup USING    it_filters TYPE ty_t_component_filter
                                 CHANGING ct_filters TYPE ty_t_component_filter
                                          cv_filter_set TYPE abap_bool
                                          cv_filter_count TYPE i.

  DATA: lt_fields       TYPE TABLE OF sval,
        lv_returncode   TYPE sy-subrc,
        lv_title        TYPE string,
        lt_key_fields   TYPE ddfields,
        lv_multi_key    TYPE boole_d,
        lv_no_key_struc TYPE boole_d,
        ls_oitc_data    TYPE mdg_s_ids_code_bs.

  " Get the key structure information to use proper table/field references
  TRY.
      cl_mdg_id_matching_tools=>process_rtti_key_struc(
        EXPORTING
          iv_oitc = p_idsc
        IMPORTING
          ev_multi_key    = lv_multi_key
          et_key_fields   = lt_key_fields
          ev_no_key_struc = lv_no_key_struc
          es_oitc_data    = ls_oitc_data
      ).
    CATCH cx_root INTO DATA(lx_error).
      MESSAGE |Error getting key structure: { lx_error->get_text( ) }| TYPE 'E'.
      RETURN.
  ENDTRY.

  " Build fields table for popup
  LOOP AT it_filters INTO DATA(lwa_filter).

    " Find the corresponding key field to get domain information
    READ TABLE lt_key_fields INTO DATA(ls_key_field)
         WITH KEY fieldname = lwa_filter-field_name.

    DATA: lv_tabname   TYPE tabname,
          lv_fieldname TYPE fieldname.

    IF sy-subrc = 0 AND ls_key_field-domname IS NOT INITIAL.

      " Get value table for the domain from DD01L
      SELECT SINGLE entitytab
        FROM dd01l
        INTO @lv_tabname
        WHERE domname = @ls_key_field-domname
          AND as4local = 'A'.

      IF sy-subrc = 0 AND lv_tabname IS NOT INITIAL.
        " Use the value table and the original field name
        lv_fieldname = ls_key_field-fieldname.
      ENDIF.
    ENDIF.

    " Determine field type for popup
    DATA(lv_field_type) = COND string(
      WHEN lwa_filter-data_type = 'DATS' THEN 'D'
      WHEN lwa_filter-data_type = 'TIMS' THEN 'T'
      WHEN lwa_filter-data_type = 'NUMC' THEN 'N'
      WHEN lwa_filter-data_type = 'CURR' OR lwa_filter-data_type = 'DEC' THEN 'P'
      ELSE 'C'
    ).

    " Build SVAL entry - always with valid table/field to ensure popup displays
    APPEND VALUE sval(
      tabname    = lv_tabname     " Always has a value now
      fieldname  = lv_fieldname  " Always has a value now
      fieldtext  = lwa_filter-field_label
      value      = lwa_filter-field_value
      field_obl  = ' '
      field_attr = lv_field_type
    ) TO lt_fields.
  ENDLOOP.

  " Set popup title
  lv_title = |Component Filters for { p_idsc } ({ lines( it_filters ) } fields)|.

  " Force GUI refresh before showing popup
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = 'Preparing component filters...'
    EXCEPTIONS
      OTHERS     = 1.

  " Show popup input screen with guaranteed valid fields
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = lv_title
      start_column    = '20'
      start_row       = '8'
      no_value_check  = 'X'
    IMPORTING
      returncode      = lv_returncode
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  " Process results
  IF lv_returncode = 0.  " User clicked OK
    " Update filter table with entered values
    CLEAR: ct_filters, cv_filter_count.

    DATA(lv_field_idx) = 0.
    LOOP AT it_filters INTO DATA(lwa_orig_filter).
      " Get corresponding field from popup result
      lv_field_idx = sy-tabix.
      READ TABLE lt_fields INTO DATA(lwa_field) INDEX lv_field_idx.
      IF sy-subrc = 0.

        " Create filter entry
        DATA(lwa_new_filter) = VALUE ty_component_filter(
          field_name    = lwa_orig_filter-field_name
          field_label   = lwa_orig_filter-field_label
          field_value   = lwa_field-value
          data_type     = lwa_orig_filter-data_type
          field_length  = lwa_orig_filter-field_length
          convexit      = lwa_orig_filter-convexit
        ).

        APPEND lwa_new_filter TO ct_filters.

      ENDIF.
    ENDLOOP.
    cv_filter_count = lv_field_idx.

    " Set filter status and show appropriate message
    IF cv_filter_count > 0.
      cv_filter_set = abap_true.
      MESSAGE |{ cv_filter_count } component filter(s) set successfully| TYPE 'S'.
    ELSE.
      cv_filter_set = abap_false.
      MESSAGE 'All component filters cleared' TYPE 'I'.
    ENDIF.

  ELSEIF lv_returncode = 1.
    " User cancelled
    MESSAGE 'Component filter cancelled by user' TYPE 'I'.
  ELSE.
    " Error occurred
    MESSAGE |Popup error - Return code: { lv_returncode }, SY-SUBRC: { sy-subrc }| TYPE 'W'.
  ENDIF.

ENDFORM.
"----------------------------------------------------------------------
" F4 Help Forms
"----------------------------------------------------------------------

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

  " Prepare F4 data
  LOOP AT gt_object_types INTO DATA(lwa_otc).
    APPEND VALUE ty_f4_object_type(
      object_type_code = lwa_otc-object_type_code
      description = lwa_otc-description
    ) TO lt_object_types.
  ENDLOOP.

  " Show F4 help
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

  " Filter by object type
  LOOP AT gt_ids_types INTO DATA(lwa_ids) WHERE object_type_code = iv_object_type.
    APPEND VALUE ty_f4_ids_type(
      ids_type_code = lwa_ids-ids_type_code
      object_type_code = lwa_ids-object_type_code
      description = lwa_ids-description
    ) TO lt_ids_types.
  ENDLOOP.

  IF lt_ids_types IS INITIAL.
    MESSAGE 'No ID scheme types found for selected object type' TYPE 'I'.
    RETURN.
  ENDIF.

  " Show F4 help
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

  " Determine file extension based on format
  DATA(lv_extension) = COND string(
    WHEN p_format = 'XLSX' THEN '*.xlsx'
    WHEN p_format = 'CSV'  THEN '*.csv'
    ELSE '*.txt'
  ).

  " Show file open dialog
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select File for Upload'
      default_extension       = lv_extension
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

  " Show F4 help
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

"----------------------------------------------------------------------
" Validation Forms
"----------------------------------------------------------------------

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

FORM validate_file_exists.

  DATA: lv_exists TYPE abap_bool.

  " Check if file exists
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

ENDFORM.

FORM store_selection_values.

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

ENDFORM.

"----------------------------------------------------------------------
" Component Filter Application (for use in main program)
"----------------------------------------------------------------------

FORM apply_component_filters CHANGING ct_mappings TYPE gtty_key_mapping_display.

  " Only apply if filters are set
  IF gv_comp_filter_set = abap_false OR gt_component_filters IS INITIAL.
    RETURN.
  ENDIF.

  DATA: lt_filtered_mappings TYPE gtty_key_mapping_display.

  " Loop through each mapping record
  LOOP AT ct_mappings INTO DATA(lwa_mapping).

    DATA: lv_match_all_filters TYPE abap_bool VALUE abap_true.

    " Check each component filter that has a value
    LOOP AT gt_component_filters INTO DATA(lwa_filter)
         WHERE field_value <> ''.

      " Split the source ID value into components
      DATA: lt_components TYPE mdg_t_km_field_value.

      TRY.
          cl_mdg_id_matching_tools_ext=>split_id_value_into_comp(
            EXPORTING
              iv_id_value = lwa_mapping-source_id_value
              iv_oitc     = lwa_mapping-ids_type_code
            IMPORTING
              et_id_value_data = lt_components
          ).
        CATCH cx_root.
          " If split fails, skip this record
          lv_match_all_filters = abap_false.
          EXIT.
      ENDTRY.

      " Check if the component matches the filter
      READ TABLE lt_components INTO DATA(lwa_component)
           WITH KEY comp_name = lwa_filter-field_name.
      IF sy-subrc = 0.

        " Apply filter logic based on data type
        CASE lwa_filter-data_type.
          WHEN 'CHAR' OR 'STRING'.
            " String contains logic
            IF lwa_component-value_ext_format NS lwa_filter-field_value.
              lv_match_all_filters = abap_false.
              EXIT.
            ENDIF.

          WHEN OTHERS.
            " Exact match for other types
            IF lwa_component-value_ext_format <> lwa_filter-field_value.
              lv_match_all_filters = abap_false.
              EXIT.
            ENDIF.
        ENDCASE.

      ELSE.
        " Component not found, doesn't match
        lv_match_all_filters = abap_false.
        EXIT.
      ENDIF.

    ENDLOOP.

    " If all filters match, keep this record
    IF lv_match_all_filters = abap_true.
      APPEND lwa_mapping TO lt_filtered_mappings.
    ENDIF.

  ENDLOOP.

  " Replace original table with filtered results
  ct_mappings = lt_filtered_mappings.

  " Show filter results
  MESSAGE |Component filters applied: { lines( ct_mappings ) } records match| TYPE 'I'.

ENDFORM.

" Also fix the other form signature:
FORM get_component_filter_info CHANGING cv_filter_set TYPE abap_bool
                                        cv_filter_count TYPE i
                                        ct_filters TYPE table of ty_component_filter.

  " Export component filter information for use in main program
  cv_filter_set = gv_comp_filter_set.
  cv_filter_count = gv_comp_filter_count.
  ct_filters = gt_component_filters.

ENDFORM.
