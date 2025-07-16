class ZCL_MDGF_GUIBB_FI_ACCOUNT_COPY definition
  public
  inheriting from CL_MDGF_GUIBB_FI_ACCOUNT_COPY
  final
  create public .

public section.

  data FD_ALLOWED type ABAP_BOOLEAN value ABAP_TRUE ##NO_TEXT.

  methods IF_FPM_GUIBB_FORM~PROCESS_EVENT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MDGF_GUIBB_FI_ACCOUNT_COPY IMPLEMENTATION.


  METHOD if_fpm_guibb_form~process_event.

    IF io_event->mv_event_id = 'MDGF_COPY_TEMPLATE_EXEC' AND fd_allowed = abap_false.
      ev_result = 'FAILED'.

    ENDIF.
    CALL METHOD super->if_fpm_guibb_form~process_event
      EXPORTING
        io_event            = io_event
        iv_raised_by_own_ui = iv_raised_by_own_ui
      IMPORTING
        ev_result           = ev_result
        et_messages         = et_messages.


  ENDMETHOD.
ENDCLASS.
