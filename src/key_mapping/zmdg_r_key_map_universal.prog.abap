*&---------------------------------------------------------------------*
*& Report ZMDG_R_KEY_MAP_UNIVERSAL
*&---------------------------------------------------------------------*
*& Universal Key Mapping CRUD Program
*&
*& Purpose: Universal program for managing key mappings across all
*&          MDG object types using the enhanced utility class
*&
*& Author: [Your Name]
*& Date: [Current Date]
*&---------------------------------------------------------------------*

REPORT zmdg_r_key_map_universal.

"----------------------------------------------------------------------
" Include Files
"----------------------------------------------------------------------
INCLUDE zmdg_i_km_universal_top.     " Global data declarations
INCLUDE zmdg_i_km_universal_sel.     " Selection screen
INCLUDE zmdg_i_km_universal_class.   " Local classes
INCLUDE zmdg_i_km_universal_forms.   " Processing forms
INCLUDE zmdg_i_km_universal_alv.     " ALV display logic

"----------------------------------------------------------------------
" Initialization
"----------------------------------------------------------------------

INITIALIZATION.

  " Initialize program data
  PERFORM initialize_program.

  " Set default values
  p_otc = if_mdg_otc_const=>bpartner.     " Default to Business Partner
  p_idsc = if_mdg_idsc_const=>bpartner_nr. " Default to BP Number
  p_format = gc_format_excel.
  p_batch = 1000.

  "----------------------------------------------------------------------
  " Start of Selection
  "----------------------------------------------------------------------

START-OF-SELECTION.

  " Validate authorization
  PERFORM check_authorization.

  " Process based on operation mode
  CASE gv_operation_mode.

    WHEN gc_mode_display.
      " Display existing key mappings
      PERFORM process_display_mode.

    WHEN gc_mode_create.
      " Create new key mappings
      PERFORM process_create_mode.

    WHEN gc_mode_update.
      " Update existing key mappings
      PERFORM process_update_mode.

    WHEN gc_mode_delete.
      " Delete key mappings
      PERFORM process_delete_mode.

  ENDCASE.

  " Display results in ALV
  PERFORM display_alv_results.

  "----------------------------------------------------------------------
  " End of Selection
  "----------------------------------------------------------------------

END-OF-SELECTION.

  " Final processing and cleanup
  PERFORM finalize_processing.

  "----------------------------------------------------------------------
  " Main Processing Forms
  "----------------------------------------------------------------------

FORM check_authorization.

  " Check user authorization for key mapping operations
  " You can implement specific authorization checks here
  " For example:
  " AUTHORITY-CHECK OBJECT 'Z_MDG_KM'
  "   ID 'ACTVT' FIELD gv_operation_mode
  "   ID 'OTC'   FIELD gv_object_type_code.

  " For now, allow all operations

ENDFORM.

FORM process_display_mode.

  DATA: lo_processor TYPE REF TO lcl_km_processor.

  " Create processor instance
  CREATE OBJECT lo_processor.

  " Process display request
  gt_key_mappings_display = lo_processor->process_display( ).

  " Check if any data found
  IF gt_key_mappings_display IS INITIAL.
    MESSAGE 'No key mappings found for specified criteria' TYPE 'I'.
  ELSE.
    MESSAGE |{ lines( gt_key_mappings_display ) } key mapping(s) found| TYPE 'S'.
  ENDIF.

ENDFORM.

FORM process_create_mode.

  DATA: lo_processor TYPE REF TO lcl_km_processor,
        lt_input     TYPE gtty_key_mapping_input.

  " Create processor instance
  CREATE OBJECT lo_processor.

  " Process file upload to get input data
  lt_input = lo_processor->process_file_upload( ).

  IF lt_input IS INITIAL.
    MESSAGE 'No data found in upload file' TYPE 'E'.
    RETURN.
  ENDIF.

  " Validate input data if validation flag is set
  IF gv_validate_only = abap_true.
    lt_input = lo_processor->validate_input_data( lt_input ).
    gt_key_mappings_display = lo_processor->convert_to_display_format( lt_input ).
    MESSAGE 'Validation completed - check results for errors' TYPE 'I'.
    RETURN.
  ENDIF.

  " Process creation
  gt_key_mappings_display = lo_processor->process_create( lt_input ).

  " Show results summary
  DATA: lv_success  TYPE i,
        lv_errors   TYPE i,
        lv_warnings TYPE i.

  LOOP AT gt_key_mappings_display INTO DATA(lwa_result).
    CASE lwa_result-mapping_status.
      WHEN gc_status_active.
        lv_success = lv_success + 1.
      WHEN gc_status_error.
        lv_errors = lv_errors + 1.
      WHEN OTHERS.
        lv_warnings = lv_warnings + 1.
    ENDCASE.
  ENDLOOP.

  MESSAGE |Creation completed: { lv_success } successful, { lv_errors } errors, { lv_warnings } warnings| TYPE 'I'.

ENDFORM.

FORM process_update_mode.

  DATA: lo_processor TYPE REF TO lcl_km_processor,
        lt_input     TYPE gtty_key_mapping_input.

  " Create processor instance
  CREATE OBJECT lo_processor.

  " Process file upload to get input data
  lt_input = lo_processor->process_file_upload( ).

  IF lt_input IS INITIAL.
    MESSAGE 'No data found in upload file' TYPE 'E'.
    RETURN.
  ENDIF.

  " Validate input data if validation flag is set
  IF gv_validate_only = abap_true.
    lt_input = lo_processor->validate_input_data( lt_input ).
    gt_key_mappings_display = lo_processor->convert_to_display_format( lt_input ).
    MESSAGE 'Validation completed - check results for errors' TYPE 'I'.
    RETURN.
  ENDIF.

  " Process updates
  gt_key_mappings_display = lo_processor->process_update( lt_input ).

  MESSAGE 'Update processing completed - check results' TYPE 'I'.

ENDFORM.

FORM process_delete_mode.

  DATA: lo_processor TYPE REF TO lcl_km_processor,
        lt_input     TYPE gtty_key_mapping_input,
        lv_answer    TYPE char1.

  " Create processor instance
  CREATE OBJECT lo_processor.

  " Process file upload to get input data
  lt_input = lo_processor->process_file_upload( ).

  IF lt_input IS INITIAL.
    MESSAGE 'No data found in upload file' TYPE 'E'.
    RETURN.
  ENDIF.

  " Show confirmation dialog for delete operation
  CALL FUNCTION 'POPUP_TO_DECIDE'
    EXPORTING
      textline1    = |You are about to delete { lines( lt_input ) } key mapping(s)|
      textline2    = 'This action cannot be undone!'
      textline3    = 'Do you want to continue?'
      titel        = 'Confirm Mass Deletion'
      start_column = 25
      start_row    = 6
    IMPORTING
      answer       = lv_answer
    EXCEPTIONS
      OTHERS       = 1.

  IF lv_answer <> '1'. " Not Yes
    MESSAGE 'Deletion cancelled by user' TYPE 'I'.
    RETURN.
  ENDIF.

  " Validate input data if validation flag is set
  IF gv_validate_only = abap_true.
    lt_input = lo_processor->validate_input_data( lt_input ).
    gt_key_mappings_display = lo_processor->convert_to_display_format( lt_input ).
    MESSAGE 'Validation completed - check results for errors' TYPE 'I'.
    RETURN.
  ENDIF.

  " Process deletions
  gt_key_mappings_display = lo_processor->process_delete( lt_input ).

  MESSAGE 'Deletion processing completed - check results' TYPE 'I'.

ENDFORM.

FORM display_alv_results.

  " Only display ALV if we have data
  IF gt_key_mappings_display IS NOT INITIAL.
    PERFORM create_alv_display.
  ENDIF.

ENDFORM.

FORM finalize_processing.

  " Cleanup and final processing
  IF go_km_utility IS BOUND.
    " Any cleanup needed for utility class
  ENDIF.

  " Clear global variables if needed
  " (Usually not necessary as program ends)

ENDFORM.

"----------------------------------------------------------------------
" Additional Helper Forms
"----------------------------------------------------------------------

FORM show_create_dialog USING it_input TYPE gtty_key_mapping_input.

  " Show dialog for creating/editing key mappings
  " This could be implemented as a popup ALV or custom screen
  " For now, just show a message
  MESSAGE |{ lines( it_input ) } mapping(s) ready for creation| TYPE 'I'.

ENDFORM.

FORM show_import_preview USING it_input TYPE gtty_key_mapping_input.

  " Show preview of imported data before processing
  DATA: lo_processor TYPE REF TO lcl_km_processor.

  CREATE OBJECT lo_processor.

  " Validate the imported data
  DATA(lt_validated) = lo_processor->validate_input_data( it_input ).

  " Convert to display format
  gt_key_mappings_display = lo_processor->convert_to_display_format( lt_validated ).

  " Show in ALV with special title
  PERFORM create_alv_display_preview.

ENDFORM.

FORM show_mapping_details USING iwa_mapping TYPE gty_key_mapping_display.

  " Show detailed information about a specific mapping
  DATA: lt_details TYPE TABLE OF string.

  APPEND |Object Type: { iwa_mapping-object_type_code } - { iwa_mapping-object_type_desc }| TO lt_details.
  APPEND |ID Scheme: { iwa_mapping-ids_type_code } - { iwa_mapping-ids_type_desc }| TO lt_details.
  APPEND |Source System: { iwa_mapping-source_system }| TO lt_details.
  APPEND |Source ID: { iwa_mapping-source_id_value }| TO lt_details.
  APPEND |Target System: { iwa_mapping-target_system }| TO lt_details.
  APPEND |Target ID: { iwa_mapping-target_id_value }| TO lt_details.
  APPEND |Status: { iwa_mapping-mapping_status }| TO lt_details.
  APPEND |Created: { iwa_mapping-created_on } by { iwa_mapping-created_by }| TO lt_details.

  IF iwa_mapping-changed_on IS NOT INITIAL.
    APPEND |Changed: { iwa_mapping-changed_on } by { iwa_mapping-changed_by }| TO lt_details.
  ENDIF.

  " Show in popup
  CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
    EXPORTING
      titel     = 'Key Mapping Details'
      start_col = 10
      start_row = 5
      end_col   = 100
      end_row   = 20
    TABLES
      valuetab  = lt_details
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.

FORM delete_selected_mappings USING it_selected_rows TYPE salv_t_row.

  DATA: lo_processor TYPE REF TO lcl_km_processor,
        lt_input     TYPE gtty_key_mapping_input.

  " Convert selected rows to input format
  LOOP AT it_selected_rows INTO DATA(lv_row).
    READ TABLE gt_key_mappings_display INTO DATA(lwa_mapping) INDEX lv_row.
    IF sy-subrc = 0.
      APPEND VALUE #(
        object_type_code = lwa_mapping-object_type_code
        ids_type_code    = lwa_mapping-ids_type_code
        source_system    = lwa_mapping-source_system
        source_id_value  = lwa_mapping-source_id_value
        target_system    = lwa_mapping-target_system
        target_id_value  = lwa_mapping-target_id_value
        operation        = gc_mode_delete
      ) TO lt_input.
    ENDIF.
  ENDLOOP.

  " Process deletions
  CREATE OBJECT lo_processor.
  DATA(lt_results) = lo_processor->process_delete( lt_input ).

  " Update display
  LOOP AT lt_results INTO DATA(lwa_result).
    " Find corresponding row in display table and update status
    LOOP AT gt_key_mappings_display INTO DATA(lwa_display).
      IF lwa_display-object_type_code = lwa_result-object_type_code AND
         lwa_display-ids_type_code = lwa_result-ids_type_code AND
         lwa_display-target_system = lwa_result-target_system AND
         lwa_display-target_id_value = lwa_result-target_id_value.

        lwa_display-mapping_status = lwa_result-mapping_status.
        lwa_display-message = lwa_result-message.
        lwa_display-row_color = lwa_result-row_color.

        MODIFY gt_key_mappings_display FROM lwa_display.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  " Refresh ALV
  IF go_alv_grid IS BOUND.
    go_alv_grid->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDIF.

ENDFORM.

FORM export_to_excel USING iv_file_path TYPE string.

  " Export current ALV data to Excel
  DATA: lt_export_data TYPE TABLE OF gty_key_mapping_display.

  lt_export_data = gt_key_mappings_display.

  " Use SAP standard function or cl_salv_ex_util for export
  TRY.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename              = iv_file_path
          filetype              = 'DAT'
          write_field_separator = 'X'
        CHANGING
          data_tab              = lt_export_data
        EXCEPTIONS
          OTHERS                = 1.

      IF sy-subrc = 0.
        MESSAGE |Data exported successfully to { iv_file_path }| TYPE 'S'.
      ELSE.
        MESSAGE 'Error exporting data' TYPE 'E'.
      ENDIF.

    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

"----------------------------------------------------------------------
" Template Generation Forms
"----------------------------------------------------------------------

FORM generate_excel_template USING it_template TYPE gtty_key_mapping_input.

  " Generate Excel template with headers and sample data
  DATA: lv_file_path TYPE string,
        lv_filename  TYPE string.

  " Build filename
  lv_filename = |KeyMapping_Template_{ gv_object_type_code }_{ sy-datum }.xlsx|.

  " Get save location from user
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Save Template File'
      default_extension = '*.xlsx'
      default_file_name = lv_filename
    CHANGING
      filename          = lv_filename
      path              = lv_file_path
    EXCEPTIONS
      OTHERS            = 1.

  IF sy-subrc = 0 AND lv_file_path IS NOT INITIAL.
    " Export template data
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename              = lv_file_path
        filetype              = 'DAT'
        write_field_separator = 'X'
      CHANGING
        data_tab              = it_template
      EXCEPTIONS
        OTHERS                = 1.

    IF sy-subrc = 0.
      MESSAGE |Template saved to { lv_file_path }| TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.

FORM generate_csv_template USING it_template TYPE gtty_key_mapping_input.

  " Similar to Excel template but for CSV format
  " Implementation would be similar to generate_excel_template
  " but with CSV format specifics

ENDFORM.
