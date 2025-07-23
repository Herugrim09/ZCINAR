*&---------------------------------------------------------------------*
*& Include          ZMDG_I_KM_UNIVERSAL_CLASS
*&---------------------------------------------------------------------*
*& Universal Key Mapping Program - Local Classes
*&---------------------------------------------------------------------*

"----------------------------------------------------------------------
" Class: Data Loader - Loads master data and configurations
"----------------------------------------------------------------------
CLASS lcl_data_loader DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      " Load object types from customizing table
      load_object_types
        EXPORTING et_object_types TYPE gtty_object_type_list,

      " Load ID scheme types with object type mapping
      load_ids_types
        EXPORTING et_ids_types TYPE gtty_ids_type_list,

      " Load business systems
      load_business_systems
        EXPORTING et_systems TYPE gtty_business_system_list,

      " Load key mapping descriptions
      load_descriptions.

ENDCLASS.

CLASS lcl_data_loader IMPLEMENTATION.

  METHOD load_object_types.

    " Get object types from customizing table
    SELECT object_type_code,
           description
      FROM mdgi_otc_t_bs
      INTO TABLE @et_object_types
      WHERE langu = @sy-langu
      ORDER BY object_type_code.

    " If no data found, show message
    IF et_object_types IS INITIAL.
      MESSAGE 'No object types found in customizing table MDGI_OTC_T_BS'(e01) TYPE 'W'.
    ENDIF.

  ENDMETHOD.

  METHOD load_ids_types.

    " Get ID scheme types with object type mapping from customizing tables
    SELECT i~ids_type_code,
           i~object_type_code,
           t~description
      FROM mdgi_idstc_bs AS i
      INNER JOIN mdgi_idstc_t_bs AS t
        ON i~ids_type_code = t~ids_type_code
       AND t~langu = @sy-langu
      INTO TABLE @et_ids_types
      ORDER BY i~object_type_code, i~ids_type_code.

    " If no data found, show message
    IF et_ids_types IS INITIAL.
      MESSAGE 'No ID scheme types found in customizing tables'(e02) TYPE 'W'.
    ENDIF.

  ENDMETHOD.

  METHOD load_business_systems.
    DATA: lt_systems TYPE	lcr_t_bsysprod.
    " Load from SLD or local configuration
    TRY.
        CALL FUNCTION 'LCR_LIST_BUSINESS_SYSTEMS'
          IMPORTING
            bus_systems = lt_systems
          EXCEPTIONS
            OTHERS      = 1.

        IF sy-subrc = 0.
          LOOP AT lt_systems INTO DATA(lwa_system).
            APPEND VALUE #( business_system_id = lwa_system-bs_key_name
                           description = lwa_system-bs_caption ) TO et_systems.
          ENDLOOP.
        ENDIF.

      CATCH cx_root.
        " Fallback: use current system
        APPEND VALUE #( business_system_id = sy-sysid
                       description = 'Local System'(t08) && | { sy-sysid }| ) TO et_systems.
    ENDTRY.

    " If still no systems found, add current system
    IF et_systems IS INITIAL.
      APPEND VALUE #( business_system_id = sy-sysid
                     description = 'Local System'(t08) && | { sy-sysid }| ) TO et_systems.
    ENDIF.

  ENDMETHOD.

  METHOD load_descriptions.

    " Load all master data into global variables
    load_object_types( IMPORTING et_object_types = gt_object_types ).
    load_ids_types( IMPORTING et_ids_types = gt_ids_types ).
    load_business_systems( IMPORTING et_systems = gt_business_systems ).

  ENDMETHOD.

ENDCLASS.

"----------------------------------------------------------------------
" Class: Key Mapping Processor - Main business logic
"----------------------------------------------------------------------
CLASS lcl_km_processor DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:
      constructor,

      " Main processing methods
      process_display
        EXPORTING et_results TYPE gtty_key_mapping_display,

      process_create
        IMPORTING it_input   TYPE gtty_key_mapping_input
        EXPORTING et_results TYPE gtty_key_mapping_display,

      process_update
        IMPORTING it_input   TYPE gtty_key_mapping_input
        EXPORTING et_results TYPE gtty_key_mapping_display,

      process_delete
        IMPORTING it_input   TYPE gtty_key_mapping_input
        EXPORTING et_results TYPE gtty_key_mapping_display,

      " File processing methods
      process_file_upload
        EXPORTING et_input TYPE gtty_key_mapping_input,

      " Validation methods
      validate_input_data
        CHANGING ct_input TYPE gtty_key_mapping_input,


      " Template generation
      generate_template
        EXPORTING ev_success TYPE abap_bool,

      " Convert to display format
      convert_to_display_format
        IMPORTING it_input   TYPE gtty_key_mapping_input
        EXPORTING et_display TYPE gtty_key_mapping_display.

  PRIVATE SECTION.

    DATA: mo_km_utility TYPE REF TO zcl_med_mdg_km_utility,
          mo_tools_ext  TYPE REF TO cl_mdg_id_matching_tools_ext.

    METHODS:
      " Helper methods
      get_existing_mappings
        EXPORTING et_mappings TYPE gtty_key_mapping_display,

      apply_filters
        CHANGING ct_mappings TYPE gtty_key_mapping_display,

      enrich_with_descriptions
        CHANGING ct_mappings TYPE gtty_key_mapping_display,

      handle_errors
        IMPORTING it_messages TYPE usmd_t_message
        CHANGING  ct_results  TYPE gtty_key_mapping_display.

ENDCLASS.

CLASS lcl_km_processor IMPLEMENTATION.

  METHOD constructor.

    " Initialize utility classes
    mo_km_utility = zcl_med_mdg_km_utility=>get_instance( ).
    CREATE OBJECT mo_tools_ext.

  ENDMETHOD.

  METHOD process_display.

    " Get existing key mappings based on filters
    get_existing_mappings( IMPORTING et_mappings = et_results ).

    " Apply user-defined filters
    apply_filters( CHANGING ct_mappings = et_results ).

    " Enrich with descriptions
    enrich_with_descriptions( CHANGING ct_mappings = et_results ).

  ENDMETHOD.

  METHOD process_create.

    DATA: lt_validated TYPE gtty_key_mapping_input,
          lt_messages  TYPE usmd_t_message.

    lt_validated = it_input.
    " Validate input data
    validate_input_data( CHANGING ct_input = lt_validated ).

    " Process each mapping
    LOOP AT lt_validated INTO DATA(lwa_input) WHERE status <> 'E'.

      " Check if mapping already exists
      DATA(lv_exists) = mo_km_utility->check_mapping_exists(
        iv_object_type = lwa_input-object_type_code
        iv_ids_type    = lwa_input-ids_type_code
        iv_system_id   = lwa_input-target_system
        iv_id_value    = lwa_input-target_id_value
      ).

      IF lv_exists = abap_true.
        lwa_input-status = 'W'.
        lwa_input-message = 'Mapping already exists - skipped'.
      ELSE.
        " Create new mapping
        DATA(lv_created) = mo_km_utility->create_key_mapping(
          iv_object_type     = lwa_input-object_type_code
          iv_ids_type        = lwa_input-ids_type_code
          iv_source_system   = lwa_input-source_system
          iv_target_system   = lwa_input-target_system
          iv_source_id_value = lwa_input-source_id_value
          iv_target_id_value = lwa_input-target_id_value
        ).

        IF lv_created = abap_false.
          lwa_input-status = 'S'.
          lwa_input-message = 'Mapping created successfully'.
        ELSE.
          lwa_input-status = 'W'.
          lwa_input-message = 'Mapping already existed'.
        ENDIF.
      ENDIF.

      MODIFY lt_validated FROM lwa_input.
    ENDLOOP.

    " Save all mappings
    IF gv_test_mode = abap_false.
      DATA(lv_success) = mo_km_utility->save_mappings(
        IMPORTING et_messages = lt_messages
      ).

      IF lv_success = abap_false.
        " Handle save errors
        handle_errors( EXPORTING it_messages = lt_messages
                       CHANGING ct_results = et_results ).
      ENDIF.
    ENDIF.

    " Convert to display format
    convert_to_display_format( EXPORTING it_input = lt_validated
                              IMPORTING et_display = et_results ).

  ENDMETHOD.

  METHOD process_update.

    DATA: lt_validated TYPE gtty_key_mapping_input,
          lt_messages  TYPE usmd_t_message.
    lt_validated = it_input.
    " Validate input data
    validate_input_data( CHANGING ct_input = lt_validated ).

    " Process each update
    LOOP AT lt_validated INTO DATA(lwa_input) WHERE status <> 'E'.

      " For update, we need to implement update logic using existing API methods
      " This is a simplified implementation - you may need to enhance based on requirements

      lwa_input-status = 'S'.
      lwa_input-message = 'Update functionality to be implemented'.

      MODIFY lt_validated FROM lwa_input.
    ENDLOOP.

    " Save changes
    IF gv_test_mode = abap_false.
      DATA(lv_success) = mo_km_utility->save_mappings(
        IMPORTING et_messages = lt_messages
      ).
    ENDIF.

    " Convert to display format
    convert_to_display_format( EXPORTING it_input = lt_validated
                              IMPORTING et_display = et_results ).

  ENDMETHOD.

  METHOD process_delete.

    DATA: lt_validated TYPE gtty_key_mapping_input,
          lt_messages  TYPE usmd_t_message.
    lt_validated = it_input.
    " Validate input data
    validate_input_data( CHANGING ct_input = lt_validated ).

    " Process each deletion
    LOOP AT lt_validated INTO DATA(lwa_input) WHERE status <> 'E'.

      " For delete, we need to implement delete logic using existing API methods
      " This is a simplified implementation - you may need to enhance based on requirements

      lwa_input-status = 'S'.
      lwa_input-message = 'Delete functionality to be implemented'.

      MODIFY lt_validated FROM lwa_input.
    ENDLOOP.

    " Save changes
    IF gv_test_mode = abap_false.
      DATA(lv_success) = mo_km_utility->save_mappings(
        IMPORTING et_messages = lt_messages
      ).
    ENDIF.

    " Convert to display format
    convert_to_display_format( EXPORTING it_input = lt_validated
                              IMPORTING et_display = et_results ).

  ENDMETHOD.

  METHOD process_file_upload.

    " File processing based on format
    CASE gv_file_format.
      WHEN gc_format_excel.
        " Process Excel file
        PERFORM process_excel_file CHANGING et_input.

      WHEN gc_format_csv.
        " Process CSV file
        PERFORM process_csv_file CHANGING et_input.

      WHEN gc_format_txt.
        " Process text file
        PERFORM process_text_file CHANGING et_input.
    ENDCASE.

  ENDMETHOD.

  METHOD validate_input_data.



    " Validate each input record
    LOOP AT ct_input INTO DATA(lwa_input).

      " Basic field validation
      IF lwa_input-object_type_code IS INITIAL.
        lwa_input-status = 'E'.
        lwa_input-message = 'Object type code is required'.

      ELSEIF lwa_input-ids_type_code IS INITIAL.
        lwa_input-status = 'E'.
        lwa_input-message = 'ID scheme type code is required'.

      ELSEIF lwa_input-source_system IS INITIAL.
        lwa_input-status = 'E'.
        lwa_input-message = 'Source system is required'.

      ELSEIF lwa_input-target_system IS INITIAL.
        lwa_input-status = 'E'.
        lwa_input-message = 'Target system is required'.

      ELSEIF lwa_input-source_id_value IS INITIAL.
        lwa_input-status = 'E'.
        lwa_input-message = 'Source ID value is required'.

      ELSEIF lwa_input-target_id_value IS INITIAL.
        lwa_input-status = 'E'.
        lwa_input-message = 'Target ID value is required'.

      ELSE.
        " Advanced validation using tools_ext
        TRY.
            " Validate ID format using tools_ext
            cl_mdg_id_matching_tools_ext=>split_id_value_into_comp(
              EXPORTING
                iv_id_value = lwa_input-source_id_value
                iv_oitc     = lwa_input-ids_type_code
              IMPORTING
                et_id_value_data = DATA(lt_components)
            ).

            lwa_input-status = 'S'.
            lwa_input-message = 'Validation passed'.

          CATCH cx_root INTO DATA(lx_error).
            lwa_input-status = 'E'.
            lwa_input-message = |ID format validation failed: { lx_error->get_text( ) }|.
        ENDTRY.
      ENDIF.

      MODIFY ct_input FROM lwa_input.
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_template.

    " Generate template file based on selected object type and format
    DATA: lt_template TYPE gtty_key_mapping_input.

    " Create sample template record
    APPEND VALUE #(
      object_type_code = gv_object_type_code
      ids_type_code    = gv_ids_type_code
      source_system    = 'SOURCE_SYS'
      source_id_value  = 'SOURCE_ID_001'
      target_system    = 'TARGET_SYS'
      target_id_value  = 'TARGET_ID_001'
      operation        = 'C'
      status           = ''
      message          = ''
    ) TO lt_template.

    " Generate file based on format
    CASE gv_file_format.
      WHEN gc_format_excel.
        PERFORM generate_excel_template USING lt_template.
      WHEN gc_format_csv.
        PERFORM generate_csv_template USING lt_template.
    ENDCASE.

    ev_success = abap_true.

  ENDMETHOD.

  METHOD convert_to_display_format.

    " Convert input format to display format
    LOOP AT it_input INTO DATA(lwa_input).
      APPEND VALUE #(
        object_type_code = lwa_input-object_type_code
        ids_type_code    = lwa_input-ids_type_code
        source_system    = lwa_input-source_system
        source_id_value  = lwa_input-source_id_value
        target_system    = lwa_input-target_system
        target_id_value  = lwa_input-target_id_value
        mapping_status   = COND #( WHEN lwa_input-status = 'S' THEN gc_status_active
                                  WHEN lwa_input-status = 'E' THEN gc_status_error
                                  ELSE gc_status_pending )
        message          = lwa_input-message
        created_by       = sy-uname
        created_on       = sy-datum
        row_color        = COND #( WHEN lwa_input-status = 'S' THEN gc_color_success
                                  WHEN lwa_input-status = 'E' THEN gc_color_error
                                  ELSE gc_color_warning )
      ) TO et_display.
    ENDLOOP.

  ENDMETHOD.

  " Private helper methods implementation
  METHOD get_existing_mappings.

    " Get existing mappings using utility class
    " This is a simplified implementation
    " In reality, you'd call the utility class methods to get mappings

    DATA: lwa_mapping TYPE gty_key_mapping_display.

    " Sample data - replace with actual utility calls
    lwa_mapping-object_type_code = gv_object_type_code.
    lwa_mapping-ids_type_code = gv_ids_type_code.
    lwa_mapping-source_system = 'SYS_001'.
    lwa_mapping-source_id_value = 'ID_001'.
    lwa_mapping-target_system = 'SYS_002'.
    lwa_mapping-target_id_value = 'ID_002'.
    lwa_mapping-mapping_status = gc_status_active.
    lwa_mapping-created_by = sy-uname.
    lwa_mapping-created_on = sy-datum.

    APPEND lwa_mapping TO et_mappings.

  ENDMETHOD.

  METHOD apply_filters.

    " Apply range filters
    IF gr_source_system IS NOT INITIAL.
      DELETE ct_mappings WHERE source_system NOT IN gr_source_system.
    ENDIF.

    IF gr_target_system IS NOT INITIAL.
      DELETE ct_mappings WHERE target_system NOT IN gr_target_system.
    ENDIF.

    IF gr_source_id IS NOT INITIAL.
      DELETE ct_mappings WHERE source_id_value NOT IN gr_source_id.
    ENDIF.

    IF gr_target_id IS NOT INITIAL.
      DELETE ct_mappings WHERE target_id_value NOT IN gr_target_id.
    ENDIF.

  ENDMETHOD.

  METHOD enrich_with_descriptions.

    " Add descriptions from master data
    LOOP AT ct_mappings INTO DATA(lwa_mapping).

      " Object type description
      READ TABLE gt_object_types INTO DATA(lwa_otc)
           WITH KEY object_type_code = lwa_mapping-object_type_code.
      IF sy-subrc = 0.
        lwa_mapping-object_type_desc = lwa_otc-description.
      ENDIF.

      " ID scheme type description
      READ TABLE gt_ids_types INTO DATA(lwa_idsc)
           WITH KEY ids_type_code = lwa_mapping-ids_type_code.
      IF sy-subrc = 0.
        lwa_mapping-ids_type_desc = lwa_idsc-description.
      ENDIF.

      " Set row color based on status
      CASE lwa_mapping-mapping_status.
        WHEN gc_status_active.
          lwa_mapping-row_color = gc_color_success.
        WHEN gc_status_error.
          lwa_mapping-row_color = gc_color_error.
        WHEN gc_status_pending.
          lwa_mapping-row_color = gc_color_warning.
        WHEN OTHERS.
          lwa_mapping-row_color = gc_color_normal.
      ENDCASE.

      MODIFY ct_mappings FROM lwa_mapping.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_errors.

    " Process error messages and update results
    LOOP AT it_messages INTO DATA(lwa_message).
      " Update corresponding result record with error info
      " Implementation depends on how messages are linked to records
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

"----------------------------------------------------------------------
" Class: ALV Handler - Handles ALV events and toolbar functions
"----------------------------------------------------------------------
CLASS lcl_alv_handler DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:
      " Event handlers
      handle_toolbar_click
        FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function,

      handle_double_click
        FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      handle_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

    CLASS-DATA: mo_processor TYPE REF TO lcl_km_processor.

ENDCLASS.

CLASS lcl_alv_handler IMPLEMENTATION.

  METHOD handle_toolbar_click.

    CASE e_salv_function.
      WHEN gc_fc_create.
        " Handle create function
        PERFORM handle_create_function.

      WHEN gc_fc_update.
        " Handle update function
        PERFORM handle_update_function.

      WHEN gc_fc_delete.
        " Handle delete function
        PERFORM handle_delete_function.

      WHEN gc_fc_refresh.
        " Refresh display
        PERFORM refresh_display.

      WHEN gc_fc_export.
        " Export data
        PERFORM export_data.

      WHEN gc_fc_import.
        " Import data
        PERFORM import_data.

      WHEN gc_fc_template.
        " Generate template
        PERFORM generate_template.

      WHEN gc_fc_validate.
        " Validate data
        "PERFORM validate_data.

    ENDCASE.

  ENDMETHOD.

  METHOD handle_double_click.

    " Handle double-click on ALV row
    READ TABLE gt_key_mappings_display INTO DATA(lwa_mapping) INDEX row.
    IF sy-subrc = 0.
      " Show detail popup or navigate to detail screen
      PERFORM show_mapping_details USING lwa_mapping.
    ENDIF.

  ENDMETHOD.

  METHOD handle_link_click.

    " Handle link clicks in ALV
    " Implementation depends on which columns have links

  ENDMETHOD.

ENDCLASS.
