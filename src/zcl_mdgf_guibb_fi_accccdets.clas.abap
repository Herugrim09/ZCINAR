class ZCL_MDGF_GUIBB_FI_ACCCCDETS definition
  public
  inheriting from CL_MDGF_GUIBB_FI_ACCCCDETS
  final
  create public .

public section.

  methods IF_FPM_GUIBB_LIST~GET_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MDGF_GUIBB_FI_ACCCCDETS IMPLEMENTATION.


  METHOD if_fpm_guibb_list~get_data.
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
    IF mv_chart_of_accounts = 'CAFI'.
      READ TABLE ct_action_usage ASSIGNING FIELD-SYMBOL(<lwa_any_action>) WITH KEY id = 'CREATE_ROOT'.
      IF sy-subrc = 0.
        <lwa_any_action>-enabled = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
