*&---------------------------------------------------------------------*
*& Include          ZMDG_I_KM_UNIVERSAL_ALV
*&---------------------------------------------------------------------*
*& Universal Key Mapping Program - ALV Display Logic
*&---------------------------------------------------------------------*

"----------------------------------------------------------------------
" ALV Creation and Setup
"----------------------------------------------------------------------
FORM create_alv_display.

  " Create ALV instance
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv_grid
        CHANGING
          t_table      = gt_key_mappings_display
      ).

    CATCH cx_salv_msg INTO DATA(lx_salv_error).
      MESSAGE lx_salv_error->get_text( ) TYPE 'E'.
      RETURN.
  ENDTRY.

  " Configure ALV
  PERFORM setup_alv_layout.
  PERFORM setup_alv_columns.
  PERFORM setup_alv_functions.
  PERFORM setup_alv_events.
  PERFORM setup_alv_toolbar.

  " Set ALV title
  DATA(lv_title) = |Key Mappings - { gv_object_type_code } ({ lines( gt_key_mappings_display ) } entries)|.
  go_alv_grid->get_display_settings( )->set_list_header( lv_title ).

  " Display ALV
  go_alv_grid->display( ).

ENDFORM.

FORM create_alv_display_preview.

  " Similar to main ALV but for preview mode
  PERFORM create_alv_display.

  " Override title for preview
  DATA(lv_title) = |Import Preview - { lines( gt_key_mappings_display ) } records|.
  go_alv_grid->get_display_settings( )->set_list_header( lv_title ).

ENDFORM.

"----------------------------------------------------------------------
" ALV Configuration
"----------------------------------------------------------------------
FORM setup_alv_layout.

  " Set general display settings
  DATA(lo_display) = go_alv_grid->get_display_settings( ).

  " Optimize column width
  lo_display->set_optimized_column_width( abap_true ).

  " Enable striped pattern
  lo_display->set_striped_pattern( abap_true ).

  " Set selection mode
  go_selections = go_alv_grid->get_selections( ).
  go_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

ENDFORM.

FORM setup_alv_columns.

  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column_table.

  " Get columns object
  lo_columns = go_alv_grid->get_columns( ).
  lo_columns->set_optimize( abap_true ).

  " Configure individual columns
  TRY.
      " Object Type Code
      lo_column ?= lo_columns->get_column( 'OBJECT_TYPE_CODE' ).
      lo_column->set_short_text( 'OTC' ).
      lo_column->set_medium_text( 'Object Type' ).
      lo_column->set_long_text( 'Object Type Code' ).
      lo_column->set_output_length( 10 ).

      " Object Type Description
      lo_column ?= lo_columns->get_column( 'OBJECT_TYPE_DESC' ).
      lo_column->set_short_text( 'OT Desc' ).
      lo_column->set_medium_text( 'Object Type Desc' ).
      lo_column->set_long_text( 'Object Type Description' ).
      lo_column->set_output_length( 25 ).

      " ID Scheme Type Code
      lo_column ?= lo_columns->get_column( 'IDS_TYPE_CODE' ).
      lo_column->set_short_text( 'IDSC' ).
      lo_column->set_medium_text( 'ID Scheme' ).
      lo_column->set_long_text( 'ID Scheme Type Code' ).
      lo_column->set_output_length( 10 ).

      " ID Scheme Type Description
      lo_column ?= lo_columns->get_column( 'IDS_TYPE_DESC' ).
      lo_column->set_short_text( 'IDS Desc' ).
      lo_column->set_medium_text( 'ID Scheme Desc' ).
      lo_column->set_long_text( 'ID Scheme Type Description' ).
      lo_column->set_output_length( 25 ).

      " Source System
      lo_column ?= lo_columns->get_column( 'SOURCE_SYSTEM' ).
      lo_column->set_short_text( 'Src Sys' ).
      lo_column->set_medium_text( 'Source System' ).
      lo_column->set_long_text( 'Source Business System' ).
      lo_column->set_output_length( 12 ).

      " Source ID Value
      lo_column ?= lo_columns->get_column( 'SOURCE_ID_VALUE' ).
      lo_column->set_short_text( 'Source ID' ).
      lo_column->set_medium_text( 'Source ID Value' ).
      lo_column->set_long_text( 'Source Identifier Value' ).
      lo_column->set_output_length( 20 ).

      " Target System
      lo_column ?= lo_columns->get_column( 'TARGET_SYSTEM' ).
      lo_column->set_short_text( 'Tgt Sys' ).
      lo_column->set_medium_text( 'Target System' ).
      lo_column->set_long_text( 'Target Business System' ).
      lo_column->set_output_length( 12 ).

      " Target ID Value
      lo_column ?= lo_columns->get_column( 'TARGET_ID_VALUE' ).
      lo_column->set_short_text( 'Target ID' ).
      lo_column->set_medium_text( 'Target ID Value' ).
      lo_column->set_long_text( 'Target Identifier Value' ).
      lo_column->set_output_length( 20 ).

      " Mapping Status
      lo_column ?= lo_columns->get_column( 'MAPPING_STATUS' ).
      lo_column->set_short_text( 'Status' ).
      lo_column->set_medium_text( 'Mapping Status' ).
      lo_column->set_long_text( 'Key Mapping Status' ).
      lo_column->set_output_length( 10 ).

      " Created By
      lo_column ?= lo_columns->get_column( 'CREATED_BY' ).
      lo_column->set_short_text( 'Created' ).
      lo_column->set_medium_text( 'Created By' ).
      lo_column->set_long_text( 'Created By User' ).
      lo_column->set_output_length( 12 ).

      " Created On
      lo_column ?= lo_columns->get_column( 'CREATED_ON' ).
      lo_column->set_short_text( 'Date' ).
      lo_column->set_medium_text( 'Created On' ).
      lo_column->set_long_text( 'Created On Date' ).
      lo_column->set_output_length( 10 ).

      " Changed By
      lo_column ?= lo_columns->get_column( 'CHANGED_BY' ).
      lo_column->set_short_text( 'Changed' ).
      lo_column->set_medium_text( 'Changed By' ).
      lo_column->set_long_text( 'Changed By User' ).
      lo_column->set_output_length( 12 ).

      " Changed On
      lo_column ?= lo_columns->get_column( 'CHANGED_ON' ).
      lo_column->set_short_text( 'Chg Date' ).
      lo_column->set_medium_text( 'Changed On' ).
      lo_column->set_long_text( 'Changed On Date' ).
      lo_column->set_output_length( 10 ).

      " Message
      lo_column ?= lo_columns->get_column( 'MESSAGE' ).
      lo_column->set_short_text( 'Message' ).
      lo_column->set_medium_text( 'Status Message' ).
      lo_column->set_long_text( 'Processing Status Message' ).
      lo_column->set_output_length( 50 ).

      " Hide row color column
      lo_column ?= lo_columns->get_column( 'ROW_COLOR' ).
      lo_column->set_visible( abap_false ).

    CATCH cx_salv_not_found.
      " Column not found - continue
  ENDTRY.

  " Set row color column
  TRY.
      lo_columns->set_color_column( 'ROW_COLOR' ).
    CATCH cx_salv_data_error.
      " Color column setup failed - continue without colors
  ENDTRY.

ENDFORM.

FORM setup_alv_functions.

  " Enable standard ALV functions
  DATA(lo_functions) = go_alv_grid->get_functions( ).

  " Enable all standard functions
  lo_functions->set_all( abap_true ).

  " Set default variant
  lo_functions->set_default( abap_true ).

ENDFORM.

FORM setup_alv_events.

  " Set up event handling
  go_alv_events = go_alv_grid->get_event( ).

  " Register event handlers
  SET HANDLER lcl_alv_handler=>handle_toolbar_click FOR go_alv_events.
  SET HANDLER lcl_alv_handler=>handle_double_click FOR go_alv_events.
  SET HANDLER lcl_alv_handler=>handle_link_click FOR go_alv_events.

ENDFORM.

FORM setup_alv_toolbar.

  " Add custom toolbar buttons based on operation mode
  DATA(lo_functions) = go_alv_grid->get_functions( ).

  " Add custom functions
  TRY.
      CASE gv_operation_mode.
        WHEN gc_mode_display.
          " Display mode buttons
          lo_functions->add_function(
            id      = gc_fc_create
            icon    = icon_create
            text    = 'Create New'
            tooltip = 'Create New Key Mapping'
            position = if_salv_c_function_position=>right_of_salv_functions
          ).

          lo_functions->add_function(
            id      = gc_fc_update
            icon    = icon_change
            text    = 'Update'
            tooltip = 'Update Selected Mapping'
            position = if_salv_c_function_position=>right_of_salv_functions
          ).

          lo_functions->add_function(
            id      = gc_fc_delete
            icon    = icon_delete
            text    = 'Delete'
            tooltip = 'Delete Selected Mappings'
            position = if_salv_c_function_position=>right_of_salv_functions
          ).

        WHEN OTHERS.
          " Processing mode buttons
          lo_functions->add_function(
            id      = gc_fc_validate
            icon    = icon_check
            text    = 'Validate'
            tooltip = 'Validate Current Data'
            position = if_salv_c_function_position=>right_of_salv_functions
          ).
      ENDCASE.

      " Common buttons for all modes
      lo_functions->add_function(
        id      = gc_fc_refresh
        icon    = icon_refresh
        text    = 'Refresh'
        tooltip = 'Refresh Display'
        position = if_salv_c_function_position=>right_of_salv_functions
      ).

      lo_functions->add_function(
        id      = gc_fc_export
        icon    = icon_export
        text    = 'Export'
        tooltip = 'Export to File'
        position = if_salv_c_function_position=>right_of_salv_functions
      ).

      lo_functions->add_function(
        id      = gc_fc_import
        icon    = icon_import
        text    = 'Import'
        tooltip = 'Import from File'
        position = if_salv_c_function_position=>right_of_salv_functions
      ).

      lo_functions->add_function(
        id      = gc_fc_template
        icon    = icon_create
        text    = 'Template'
        tooltip = 'Generate Template File'
        position = if_salv_c_function_position=>right_of_salv_functions
      ).

    CATCH cx_salv_existing cx_salv_wrong_call.
      " Function already exists or wrong call - continue
  ENDTRY.

ENDFORM.

"----------------------------------------------------------------------
" ALV Utility Forms
"----------------------------------------------------------------------
FORM get_selected_rows RETURNING VALUE(rt_selected) TYPE salv_t_row.

  " Get currently selected rows
  IF go_selections IS BOUND.
    rt_selected = go_selections->get_selected_rows( ).
  ENDIF.

ENDFORM.

FORM refresh_alv_display.

  " Refresh the ALV display
  IF go_alv_grid IS BOUND.
    go_alv_grid->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDIF.

ENDFORM.

FORM set_alv_title USING iv_title TYPE string.

  " Set ALV title
  IF go_alv_grid IS BOUND.
    go_alv_grid->get_display_settings( )->set_list_header( iv_title ).
  ENDIF.

ENDFORM.

FORM show_alv_message USING iv_message TYPE string
                            iv_type    TYPE char1.

  " Show message in ALV context
  MESSAGE iv_message TYPE iv_type.

ENDFORM.

FORM get_alv_selected_data changing ct_selected TYPE gtty_key_mapping_display.

  " Get selected rows data
  DATA(lt_rows) = COND salv_t_row( WHEN go_selections IS BOUND
                                   THEN go_selections->get_selected_rows( ) ).

  LOOP AT lt_rows INTO DATA(lv_row).
    READ TABLE gt_key_mappings_display INTO DATA(lwa_mapping) INDEX lv_row.
    IF sy-subrc = 0.
      APPEND lwa_mapping TO ct_selected.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM validate_alv_selection.

  " Validate that user has selected rows when required
  DATA(lt_selected) = COND salv_t_row( WHEN go_selections IS BOUND
                                       THEN go_selections->get_selected_rows( ) ).

  IF lt_selected IS INITIAL.
    MESSAGE 'Please select at least one row'(d007) TYPE 'I'.
  ENDIF.

ENDFORM.
