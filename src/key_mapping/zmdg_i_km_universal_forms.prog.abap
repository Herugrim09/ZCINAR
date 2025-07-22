*&---------------------------------------------------------------------*
*& Include          ZMDG_I_KM_UNIVERSAL_FORMS
*&---------------------------------------------------------------------*
*& Universal Key Mapping Program - Processing Forms
*&---------------------------------------------------------------------*

"----------------------------------------------------------------------
" Initialization and Master Data Loading
"----------------------------------------------------------------------
FORM load_object_types.

  " Load object types into global table
  gt_object_types = lcl_data_loader=>load_object_types( ).

ENDFORM.

FORM load_ids_types.

  " Load ID scheme types into global table
  gt_ids_types = lcl_data_loader=>load_ids_types( ).

ENDFORM.

FORM load_business_systems.

  " Load business systems into global table
  gt_business_systems = lcl_data_loader=>load_business_systems( ).

ENDFORM.

FORM initialize_program.

  " Load all master data
  PERFORM load_object_types.
  PERFORM load_ids_types.
  PERFORM load_business_systems.

  " Initialize utility classes
  go_km_utility = zcl_med_mdg_km_utility=>get_instance( ).
  CREATE OBJECT go_tools_ext.

ENDFORM.

"----------------------------------------------------------------------
" File Processing Forms
"----------------------------------------------------------------------
FORM process_excel_file CHANGING ct_input TYPE gtty_key_mapping_input.

  DATA: lt_raw_data   TYPE truxs_t_text_data,
        lt_excel_data TYPE TABLE OF gty_key_mapping_input.

  " Read Excel file
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = gv_has_header
      i_tab_raw_data       = lt_raw_data
      i_filename           = gv_file_path
    TABLES
      i_tab_converted_data = lt_excel_data
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error reading Excel file'(e08) TYPE 'E'.
    RETURN.
  ENDIF.

  " Validate and enrich data
  LOOP AT lt_excel_data INTO DATA(lwa_excel).

    " Set default values if missing
    IF lwa_excel-object_type_code IS INITIAL.
      lwa_excel-object_type_code = gv_object_type_code.
    ENDIF.

    IF lwa_excel-ids_type_code IS INITIAL.
      lwa_excel-ids_type_code = gv_ids_type_code.
    ENDIF.

    " Set operation based on global mode
    lwa_excel-operation = gv_operation_mode.

    APPEND lwa_excel TO ct_input.
  ENDLOOP.

ENDFORM.

FORM process_csv_file CHANGING ct_input TYPE gtty_key_mapping_input.

  DATA: lt_csv_data TYPE TABLE OF string,
        lt_fields   TYPE TABLE OF string,
        lwa_input   TYPE gty_key_mapping_input.

  " Read CSV file
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = gv_file_path
      filetype            = 'ASC'
      has_field_separator = 'X'
    CHANGING
      data_tab            = lt_csv_data
    EXCEPTIONS
      file_open_error     = 1
      OTHERS              = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error reading CSV file'(e08) TYPE 'E'.
    RETURN.
  ENDIF.

  " Process CSV data
  LOOP AT lt_csv_data INTO DATA(lv_line).

    " Skip header line if specified
    IF sy-tabix = 1 AND gv_has_header = abap_true.
      CONTINUE.
    ENDIF.

    " Split CSV line into fields
    SPLIT lv_line AT ',' INTO TABLE lt_fields.

    " Map fields to structure
    CLEAR lwa_input.

    " Assuming CSV format: OTC,IDSC,SrcSys,SrcID,TgtSys,TgtID
    READ TABLE lt_fields INTO DATA(lv_field) INDEX 1.
    lwa_input-object_type_code = lv_field.

    READ TABLE lt_fields INTO lv_field INDEX 2.
    lwa_input-ids_type_code = lv_field.

    READ TABLE lt_fields INTO lv_field INDEX 3.
    lwa_input-source_system = lv_field.

    READ TABLE lt_fields INTO lv_field INDEX 4.
    lwa_input-source_id_value = lv_field.

    READ TABLE lt_fields INTO lv_field INDEX 5.
    lwa_input-target_system = lv_field.

    READ TABLE lt_fields INTO lv_field INDEX 6.
    lwa_input-target_id_value = lv_field.

    lwa_input-operation = gv_operation_mode.

    APPEND lwa_input TO ct_input.
  ENDLOOP.

ENDFORM.

FORM process_text_file CHANGING ct_input TYPE gtty_key_mapping_input.

  " Similar to CSV but with tab or other delimiter
  " Implementation similar to CSV processing

ENDFORM.

"----------------------------------------------------------------------
" ALV Event Handler Forms
"----------------------------------------------------------------------
FORM handle_create_function.

  " Get selected rows
  DATA: lt_selected_rows TYPE salv_t_row,
        lt_input         TYPE gtty_key_mapping_input.

  lt_selected_rows = go_selections->get_selected_rows( ).

  IF lt_selected_rows IS INITIAL.
    MESSAGE 'Please select at least one row' TYPE 'I'.
    RETURN.
  ENDIF.

  " Convert selected rows to input format
  LOOP AT lt_selected_rows INTO DATA(lv_row).
    READ TABLE gt_key_mappings_display INTO DATA(lwa_display) INDEX lv_row.
    IF sy-subrc = 0.
      APPEND VALUE #(
        object_type_code = lwa_display-object_type_code
        ids_type_code    = lwa_display-ids_type_code
        source_system    = lwa_display-source_system
        source_id_value  = lwa_display-source_id_value
        target_system    = lwa_display-target_system
        target_id_value  = lwa_display-target_id_value
        operation        = gc_mode_create
      ) TO lt_input.
    ENDIF.
  ENDLOOP.

  " Show create dialog
  PERFORM show_create_dialog USING lt_input.

ENDFORM.

FORM handle_update_function.

  " Similar to create but for update operation
  " Get selected rows and show update dialog

ENDFORM.

FORM handle_delete_function.

  DATA: lt_selected_rows TYPE salv_t_row,
        lv_answer        TYPE char1.

  lt_selected_rows = go_selections->get_selected_rows( ).

  IF lt_selected_rows IS INITIAL.
    MESSAGE 'Please select at least one row for deletion' TYPE 'I'.
    RETURN.
  ENDIF.

  " Confirmation popup
  CALL FUNCTION 'POPUP_TO_DECIDE'
    EXPORTING
      textline1    = 'Do you want to delete selected key mappings?'
      textline2    = 'This action cannot be undone!'
      titel        = 'Confirm Deletion'
      start_column = 25
      start_row    = 6
    IMPORTING
      answer       = lv_answer
    EXCEPTIONS
      OTHERS       = 1.

  IF lv_answer = '1'. " Yes
    PERFORM delete_selected_mappings USING lt_selected_rows.
  ENDIF.

ENDFORM.

FORM refresh_display.

  " Refresh ALV display
  DATA: lo_processor TYPE REF TO lcl_km_processor.

  CREATE OBJECT lo_processor.

  " Reload data based on current operation mode
  CASE gv_operation_mode.
    WHEN gc_mode_display.
      gt_key_mappings_display = lo_processor->process_display( ).
  ENDCASE.

  " Refresh ALV
  IF go_alv_grid IS BOUND.
    go_alv_grid->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDIF.

ENDFORM.

FORM export_data.

  DATA: lv_file_path TYPE string,
        lv_filename  TYPE string.

  " Get file path from user
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Export Key Mappings'
      default_extension = '*.xlsx'
      default_file_name = 'KeyMappings_Export.xlsx'
    CHANGING
      filename          = lv_filename
      path              = lv_file_path
    EXCEPTIONS
      OTHERS            = 1.

  IF sy-subrc = 0 AND lv_file_path IS NOT INITIAL.
    " Export data to selected file
    PERFORM export_to_excel USING lv_file_path.
  ENDIF.

ENDFORM.

FORM import_data.

  " Show file selection dialog and process import
  PERFORM f4_file_path CHANGING gv_file_path.

  IF gv_file_path IS NOT INITIAL.
    " Process file based on operation mode
    DATA: lo_processor TYPE REF TO lcl_km_processor,
          lt_input     TYPE gtty_key_mapping_input.

    CREATE OBJECT lo_processor.
    lt_input = lo_processor->process_file_upload( ).

    " Show import preview
    PERFORM show_import_preview USING lt_input.
  ENDIF.

ENDFORM.

FORM generate_template.

  DATA: lo_processor TYPE REF TO lcl_km_processor.

  CREATE OBJECT lo_processor.

  DATA(lv_success) = lo_processor->generate_template( ).

  IF lv_success = abap_true.
    MESSAGE 'Template generated successfully' TYPE 'S'.
  ELSE.
    MESSAGE 'Error generating template' TYPE 'E'.
  ENDIF.

ENDFORM.
