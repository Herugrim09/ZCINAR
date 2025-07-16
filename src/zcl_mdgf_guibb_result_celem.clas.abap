class ZCL_MDGF_GUIBB_RESULT_CELEM definition
  public
  inheriting from CL_MDGF_GUIBB_RESULT
  final
  create public .

public section.

  methods IF_FPM_GUIBB_LIST~GET_DEFINITION
    redefinition .
  methods IF_FPM_GUIBB_LIST~GET_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MDGF_GUIBB_RESULT_CELEM IMPLEMENTATION.


METHOD if_fpm_guibb_list~get_data.
  DATA: lri_services TYPE REF TO if_usmdz_appl_services,
        lv_celem_cat TYPE katyp,
        lv_class     TYPE usmdz1_co_kaint,
        lv_class_txt TYPE usmdz1_celem_class_txt.

  FIELD-SYMBOLS: <lt_data>               TYPE STANDARD TABLE,
                 <ls_row>                TYPE any,
                 <lv_celemcat>           TYPE any,
                 <lv_classification_txt> TYPE any.

  " Call parent method first - get all the standard data
  CALL METHOD super->if_fpm_guibb_list~get_data
    EXPORTING
      iv_eventid                = iv_eventid
      it_selected_fields        = it_selected_fields
      iv_raised_by_own_ui       = iv_raised_by_own_ui
      iv_visible_rows           = iv_visible_rows
      iv_edit_mode              = iv_edit_mode
      io_extended_ctrl          = io_extended_ctrl
    IMPORTING
      et_messages               = et_messages
      ev_data_changed           = ev_data_changed
      ev_field_usage_changed    = ev_field_usage_changed
      ev_action_usage_changed   = ev_action_usage_changed
      ev_selected_lines_changed = ev_selected_lines_changed
      ev_dnd_attr_changed       = ev_dnd_attr_changed
      eo_itab_change_log        = eo_itab_change_log
    CHANGING
      ct_data                   = ct_data
      ct_field_usage            = ct_field_usage
      ct_action_usage           = ct_action_usage
      ct_selected_lines         = ct_selected_lines
      cv_lead_index             = cv_lead_index
      cv_first_visible_row      = cv_first_visible_row
      cs_additional_info        = cs_additional_info
      ct_dnd_attributes         = ct_dnd_attributes.
  TRY.
      " Get the application services instance
      lri_services = cl_usmdz_object_factory=>get_instance_appl_services( ).
    CATCH cx_usmdz_exception INTO DATA(lrcx_usmdz_ex).
      "Loga yazdır
      RETURN.
  ENDTRY.
  " Now populate our custom field for each row
  LOOP AT ct_data ASSIGNING <ls_row>.

    " Get the cost element category from the current row
    ASSIGN COMPONENT 'CELEMCAT' OF STRUCTURE <ls_row> TO <lv_celemcat>.
    IF <lv_celemcat> IS ASSIGNED AND <lv_celemcat> IS NOT INITIAL.
      lv_celem_cat = <lv_celemcat>.

      " Get the classification text using the application service
      lri_services->get_celem_class(
        EXPORTING
          iv_celem_cat            = lv_celem_cat
        IMPORTING
          ev_cost_elem_class      = lv_class
          ev_cost_elem_class_text = lv_class_txt
      ).

      " Assign the classification text to our custom field
      ASSIGN COMPONENT 'CELEMCLSF__TXT' OF STRUCTURE <ls_row> TO <lv_classification_txt>.
      IF <lv_classification_txt> IS ASSIGNED.
        <lv_classification_txt> = lv_class_txt.
        ev_data_changed = abap_true.  " Tell the framework we changed data
      ENDIF.

    ENDIF.

  ENDLOOP.
ENDMETHOD.


METHOD if_fpm_guibb_list~get_definition.

  " Call parent method first to get existing definition
  CALL METHOD super->if_fpm_guibb_list~get_definition
    IMPORTING
      eo_field_catalog         = eo_field_catalog
      et_field_description     = et_field_description
      et_action_definition     = et_action_definition
      et_special_groups        = et_special_groups
      es_message               = es_message
      ev_additional_error_info = ev_additional_error_info
      et_dnd_definition        = et_dnd_definition
      et_row_actions           = et_row_actions
      es_options               = es_options.

  " Get existing components from the field catalog
  DATA: lt_components   TYPE cl_abap_structdescr=>component_table,
        ls_component    TYPE abap_componentdescr,
        lo_struct_descr TYPE REF TO cl_abap_structdescr.

  " Get the table line type and its components
  lo_struct_descr ?= eo_field_catalog->get_table_line_type( ).
  lt_components = lo_struct_descr->get_components( ).

  " Add your new component/column
  CLEAR ls_component.
  ls_component-name = 'CELEMCLSF__TXT'.        " Field name
  ls_component-type = cl_abap_elemdescr=>get_string( ).  " Field type - adjust as needed
  " For other types, use:
  " cl_abap_elemdescr=>get_c( 20 )     " Character field with length 20
  " cl_abap_elemdescr=>get_i( )        " Integer
  " cl_abap_elemdescr=>get_d( )        " Date
  " etc.

  APPEND ls_component TO lt_components.

  " Recreate the structure descriptor with the new component
  TRY.
      lo_struct_descr = cl_abap_structdescr=>create( lt_components ).
    CATCH cx_sy_struct_creation.
      " Handle exception if needed
  ENDTRY.

  " Recreate the table descriptor (field catalog) with the new structure
  TRY.
      eo_field_catalog = cl_abap_tabledescr=>create( lo_struct_descr ).
    CATCH cx_sy_table_creation.
      " Handle exception if needed
  ENDTRY.

  " Add corresponding field description for the new column
  DATA: ls_field_desc TYPE fpmgb_s_listfield_descr.

  CLEAR ls_field_desc.
  ls_field_desc-name                = 'CELEMCLSF__TXT'.
  ls_field_desc-visibility          = if_fpm_constants=>gc_visibility-visible.
  ls_field_desc-read_only           = abap_false.
  ls_field_desc-tooltip             = 'Cost Element Classification'.

  APPEND ls_field_desc TO et_field_description.

ENDMETHOD.
ENDCLASS.
