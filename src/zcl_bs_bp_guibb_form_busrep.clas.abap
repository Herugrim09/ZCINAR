class ZCL_BS_BP_GUIBB_FORM_BUSREP definition
  public
  inheriting from CL_BS_BP_GUIBB_FORM
  final
  create public .

public section.

  methods IF_FPM_GUIBB_FORM~GET_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BS_BP_GUIBB_FORM_BUSREP IMPLEMENTATION.


  METHOD if_fpm_guibb_form~get_data.
    CALL METHOD super->if_fpm_guibb_form~get_data
      EXPORTING
        io_event                = io_event
        iv_raised_by_own_ui     = iv_raised_by_own_ui
        it_selected_fields      = it_selected_fields
        iv_edit_mode            = iv_edit_mode
        io_extended_ctrl        = io_extended_ctrl
      IMPORTING
        et_messages             = et_messages
        ev_data_changed         = ev_data_changed
        ev_field_usage_changed  = ev_field_usage_changed
        ev_action_usage_changed = ev_action_usage_changed
      CHANGING
        cs_data                 = cs_data
        ct_field_usage          = ct_field_usage
        ct_action_usage         = ct_action_usage.
    mo_collection->get_iterator(
      RECEIVING
        rv_result = DATA(lo_iterator)
    ).
    IF lo_iterator IS BOUND.
      lo_iterator->get_current(
        RECEIVING
          rv_result = DATA(lo_entity)
      ).
      IF lo_entity IS BOUND.
        ASSIGN COMPONENT 'ZZREPORT' OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_report>).
        IF <lv_report> IS ASSIGNED.
          lo_entity->set_property(
            iv_attr_name = 'ZZREPORT'                 " Component Name
            iv_value     = <lv_report>
          ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
