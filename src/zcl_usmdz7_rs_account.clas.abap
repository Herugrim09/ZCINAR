class ZCL_USMDZ7_RS_ACCOUNT definition
  public
  inheriting from CL_USMDZ7_RS_ACCOUNT
  final
  create public .

public section.

  methods IF_EX_USMD_RULE_SERVICE~CHECK_ENTITY
    redefinition .
  methods IF_EX_USMD_RULE_SERVICE~DERIVE_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_USMDZ7_RS_ACCOUNT IMPLEMENTATION.


  METHOD if_ex_usmd_rule_service~check_entity.
    DATA: lfd_cr_type TYPE usmd_crequest_type.
    DATA: lri_context TYPE REF TO if_usmd_app_context.
    DATA: lwa_message TYPE usmd_s_message.
    CALL METHOD super->if_ex_usmd_rule_service~check_entity
      EXPORTING
        io_model        = io_model
        id_edition      = id_edition
        id_crequest     = id_crequest
        id_entitytype   = id_entitytype
        if_online_check = if_online_check
        it_data         = it_data
      IMPORTING
        et_message      = et_message.

    cl_usmd_crequest_util=>get_cr_type_by_cr(
      EXPORTING
        i_crequest           = id_crequest                 " Change Request
*      io_model             =                  " MDG Data Model for Access from SAP Standard Delivery
*      iv_skip_read_archive =
      RECEIVING
        e_cr_type            = lfd_cr_type                 " Type of Change Request
    ).


    lri_context = cl_usmd_app_context=>get_context( ).
    IF lri_context->mv_crequest_step = '90'.
      IF lfd_cr_type = 'ZACCAP1'.
        lwa_message-msgid = 'ZMDG_DQM_MESSAGES'.
        lwa_message-msgno = '005'.
        lwa_message-msgty = 'I'.
        lwa_message-msgv1 = lfd_cr_type.
        APPEND lwa_message TO et_message.
      ENDIF.
      IF lri_context->mv_crequest_type = 'ZACCAP1'.
        lwa_message-msgid = 'ZMDG_DQM_MESSAGES'.
        lwa_message-msgno = '002'.
        lwa_message-msgty = 'I'.
        "APPEND lwa_message TO et_message_info.
      ENDIF.


    ENDIF.
  ENDMETHOD.


  METHOD if_ex_usmd_rule_service~derive_entity.
    CALL METHOD super->if_ex_usmd_rule_service~derive_entity
      EXPORTING
        io_model      = io_model
        id_edition    = id_edition
        id_entitytype = id_entitytype
      IMPORTING
        et_message    = et_message
      CHANGING
        ct_data       = ct_data.
  ENDMETHOD.
ENDCLASS.
