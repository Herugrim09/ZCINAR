class ZCL_MDGF_GUIBB_DQUERY definition
  public
  inheriting from CL_MDGF_GUIBB_DQUERY
  final
  create public .

public section.

  methods IF_FPM_GUIBB_SEARCH~GET_DATA
    redefinition .
  methods IF_FPM_GUIBB_SEARCH~PROCESS_EVENT
    redefinition .
protected section.

  methods CREATE_STRUCT_RTTI
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_MDGF_GUIBB_DQUERY IMPLEMENTATION.


  METHOD create_struct_rtti.
    DATA:
    lit_component        TYPE cl_abap_structdescr=>component_table.

    CALL METHOD super->create_struct_rtti.

    mo_struct_rtti->get_components(
      RECEIVING
        p_result = lit_component                  " Component Description Table
    ).

    APPEND INITIAL LINE TO lit_component ASSIGNING FIELD-SYMBOL(<lwa_component>).
    IF <lwa_component> IS ASSIGNED.
      <lwa_component>-name = 'COMP_TXT'.
      <lwa_component>-type ?= cl_abap_elemdescr=>describe_by_name( p_name = 'BUTXT' ).
    ENDIF.

    mo_struct_rtti = cl_abap_structdescr=>create( lit_component ).
  ENDMETHOD.


  METHOD if_fpm_guibb_search~get_data.

    DATA: lfd_butxt              TYPE text25,
          lit_selopt             TYPE STANDARD TABLE OF selopt,
          lit_visible_attributes TYPE fpmgb_t_selected_fields.


    READ TABLE ct_fpm_search_criteria ASSIGNING FIELD-SYMBOL(<lwa_fpm_search_criteria>) WITH KEY search_attribute = 'COMP_TXT'.
    IF <lwa_fpm_search_criteria> IS ASSIGNED.
      lfd_butxt = <lwa_fpm_search_criteria>-search_attribute.
      APPEND VALUE selopt( option = usmd0_cs_ra-option_eq sign = <lwa_fpm_search_criteria>-sign low = <lwa_fpm_search_criteria>-low ) TO lit_selopt.
      SELECT SINGLE bukrs FROM t001 WHERE butxt IN @lit_selopt INTO @DATA(lfd_bukrs).
      IF sy-subrc = 0.
        <lwa_fpm_search_criteria>-search_attribute = if_usmdz_cons_entitytypes=>gc_entity_company_code.
        <lwa_fpm_search_criteria>-low = lfd_bukrs.
*        lit_visible_attributes = CORRESPONDING #( it_visible_attributes ).
*        READ TABLE lit_visible_attributes ASSIGNING FIELD-SYMBOL(<lwa_visible_attr>) WITH KEY name = 'COMP_TXT'.
*        IF sy-subrc = 0.
*          <lwa_visible_attr>-name = 'COMPACC'.
*        ENDIF.

      ENDIF.
    ENDIF.

    CALL METHOD super->if_fpm_guibb_search~get_data
      EXPORTING
        io_event                      = io_event
        iv_raised_by_own_ui           = iv_raised_by_own_ui
        it_visible_attributes         = it_visible_attributes
        it_selected_columns_of_result = it_selected_columns_of_result
        it_selected_search_attributes = it_selected_search_attributes
        io_search_conversion          = io_search_conversion
        io_extended_ctrl              = io_extended_ctrl
      IMPORTING
        et_messages                   = et_messages
        ev_search_criteria_changed    = ev_search_criteria_changed
        et_result_list                = et_result_list
        ev_result_list_title          = ev_result_list_title
        ev_field_usage_changed        = ev_field_usage_changed
      CHANGING
        ct_fpm_search_criteria        = ct_fpm_search_criteria
        ct_field_usage                = ct_field_usage.
  ENDMETHOD.


  METHOD if_fpm_guibb_search~process_event.


    IF io_event->mv_event_id = 'FPM_EXECUTE_SEARCH'.
      DATA: lfd_butxt           TYPE text25,
            lit_selopt          TYPE STANDARD TABLE OF selopt,
            lit_search_criteria TYPE fpmgb_t_search_criteria.

      lit_search_criteria = it_fpm_search_criteria.

      READ TABLE lit_search_criteria ASSIGNING FIELD-SYMBOL(<lwa_fpm_search_criteria>) WITH KEY search_attribute = 'COMP_TXT'.
      IF <lwa_fpm_search_criteria> IS ASSIGNED.
        lfd_butxt = <lwa_fpm_search_criteria>-search_attribute.
        APPEND VALUE selopt( option = usmd0_cs_ra-option_eq sign = <lwa_fpm_search_criteria>-sign low = <lwa_fpm_search_criteria>-low ) TO lit_selopt.
        SELECT SINGLE bukrs FROM t001 WHERE butxt IN @lit_selopt INTO @DATA(lfd_bukrs).
        IF sy-subrc = 0.
          <lwa_fpm_search_criteria>-search_attribute = if_usmdz_cons_entitytypes=>gc_entity_company_code.
          <lwa_fpm_search_criteria>-low = lfd_bukrs.
*        lit_search_criteria = CORRESPONDING #( it_visible_attributes ).
*        READ TABLE lit_search_criteria ASSIGNING FIELD-SYMBOL(<lwa_visible_attr>) WITH KEY name = 'COMP_TXT'.
*        IF sy-subrc = 0.
*          <lwa_visible_attr>-name = 'COMPACC'.
*        ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
    CALL METHOD super->if_fpm_guibb_search~process_event
      EXPORTING
        io_event               = io_event
        it_fpm_search_criteria = lit_search_criteria
        iv_raised_by_own_ui    = iv_raised_by_own_ui
        iv_max_num_results     = iv_max_num_results
        io_search_conversion   = io_search_conversion
      IMPORTING
        et_messages            = et_messages
        ev_result              = ev_result.
  ENDMETHOD.
ENDCLASS.
