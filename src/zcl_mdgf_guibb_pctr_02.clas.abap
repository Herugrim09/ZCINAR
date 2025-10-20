class ZCL_MDGF_GUIBB_PCTR_02 definition
  public
  inheriting from CL_MDGF_GUIBB_PCTR
  final
  create public .

public section.

  methods IF_FPM_GUIBB~INITIALIZE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MDGF_GUIBB_PCTR_02 IMPLEMENTATION.


  METHOD if_fpm_guibb~initialize.
    CALL METHOD super->if_fpm_guibb~initialize
      EXPORTING
        it_parameter      = it_parameter
        io_app_parameter  = io_app_parameter
        iv_component_name = iv_component_name
        is_config_key     = is_config_key
        iv_instance_id    = iv_instance_id.

    CHECK 2 = 2.
  ENDMETHOD.
ENDCLASS.
