class ZCL_MDGF_GUIBB_FI_ACCOUNT definition
  public
  inheriting from CL_MDGF_GUIBB_FI_ACCOUNT
  final
  create public .

public section.

  methods IF_FPM_GUIBB_FORM~GET_DEFINITION
    redefinition .
  methods IF_FPM_GUIBB_FORM~GET_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MDGF_GUIBB_FI_ACCOUNT IMPLEMENTATION.


  METHOD if_fpm_guibb_form~get_data.
    DATA: lt_dom_fixed_val TYPE TABLE OF dd07v.
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

    ASSIGN COMPONENT 'ZZLCAT' OF STRUCTURE cs_data TO FIELD-SYMBOL(<lfd_any>).
    IF <lfd_any> IS ASSIGNED AND <lfd_any> IS NOT INITIAL.
      IF lt_dom_fixed_val IS INITIAL.
        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname   = 'ZMED_0G_LCAT'                 " Domain name
            text      = abap_true            " Default ' ': without texts, 'X': with, 'T': only text
*           langu     = space            " Language, default SY-LANGU, '*': all texts
*           bypass_buffer  = space
*        IMPORTING
*           rc        =
          TABLES
            dd07v_tab = lt_dom_fixed_val
*        EXCEPTIONS
*           wrong_textflag = 1                " Incorrect value in TEXT: Parameter (<> X, T ,'')
*           others    = 2
          .
        IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.
      READ TABLE lt_dom_fixed_val INTO DATA(lwa_fixed_val) WITH KEY domvalue_l = <lfd_any>.
      IF sy-subrc = 0.
        UNASSIGN <lfd_any>.
        ASSIGN COMPONENT 'ZZLCAT__TXT' OF STRUCTURE cs_data TO <lfd_any>.
        IF <lfd_any> IS ASSIGNED.
          <lfd_any> = lwa_fixed_val-ddtext.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~get_definition.
    DATA: lrc_data_descr TYPE REF TO cl_abap_typedescr,
          lfd_string     TYPE string.

    CALL METHOD super->if_fpm_guibb_form~get_definition
      IMPORTING
        eo_field_catalog         = eo_field_catalog
        et_field_description     = et_field_description
        et_action_definition     = et_action_definition
        et_special_groups        = et_special_groups
        et_dnd_definition        = et_dnd_definition
        es_options               = es_options
        es_message               = es_message
        ev_additional_error_info = ev_additional_error_info.

    APPEND INITIAL LINE TO et_field_description ASSIGNING FIELD-SYMBOL(<lwa_field_description>).
    IF <lwa_field_description> IS ASSIGNED.
      CONCATENATE 'ZZLCAT' if_usmdz10_constants=>gc_suffix_text INTO <lwa_field_description>-name.
      <lwa_field_description>-read_only = abap_true.
    ENDIF.

    "  lrc_data_descr ?= cl_abap_elemdescr=>describe_by_data( p_data = lfd_string ).

    DATA(lit_components) = eo_field_catalog->get_components( ).
    APPEND VALUE cl_abap_structdescr=>component( name = <lwa_field_description>-name type = cl_abap_elemdescr=>get_string( ) ) TO lit_components.

    eo_field_catalog ?= cl_abap_structdescr=>create( p_components = lit_components ).
  ENDMETHOD.
ENDCLASS.
