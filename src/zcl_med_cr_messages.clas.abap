class ZCL_MED_CR_MESSAGES definition
  public
  final
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_FORM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MED_CR_MESSAGES IMPLEMENTATION.


  method IF_FPM_GUIBB_FORM~CHECK_CONFIG.
  endmethod.


  method IF_FPM_GUIBB_FORM~FLUSH.
  endmethod.


  METHOD if_fpm_guibb_form~get_data.
    CHECK 1 = 1.
  ENDMETHOD.


  method IF_FPM_GUIBB_FORM~GET_DEFAULT_CONFIG.
  endmethod.


  METHOD if_fpm_guibb_form~get_definition.
    DATA: lit_component TYPE cl_abap_structdescr=>component_table.
    DATA: lwa_component  TYPE cl_abap_structdescr=>component.

    lwa_component-name = 'MESSAGE'.
    lwa_component-type ?= cl_abap_elemdescr=>describe_by_name( p_name = 'CHAR100' ).

    lit_component = VALUE #( ( lwa_component ) ).

    eo_field_catalog ?= cl_abap_structdescr=>create( p_components = lit_component ).
  ENDMETHOD.


  method IF_FPM_GUIBB_FORM~PROCESS_EVENT.
  endmethod.


  method IF_FPM_GUIBB~GET_PARAMETER_LIST.
  endmethod.


  method IF_FPM_GUIBB~INITIALIZE.
  endmethod.
ENDCLASS.
