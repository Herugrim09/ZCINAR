class ZCL_0G_FOLLOW_UP definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_USMD_SSW_SYST_METHOD_CALLER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_0G_FOLLOW_UP IMPLEMENTATION.


  METHOD if_usmd_ssw_syst_method_caller~call_system_method.
    DATA: lri_model_ext TYPE REF TO if_usmd_model_ext.
    DATA lit_crequest             TYPE usmd_cr_ts_root.
    DATA: lfd_description TYPE usmd_txtlg.
    DATA: lfd_createdby TYPE xubname.
    FIELD-SYMBOLS:
                   <lit_crequest> TYPE ANY TABLE.


    cl_usmd_model_ext=>get_instance(
      EXPORTING
        i_usmd_model = '0G'                  " Data model
      IMPORTING
        eo_instance  = lri_model_ext                 " MDM Data Model for Access from Non-SAP Standard
*        et_message   =                  " Messages
    ).
    CASE iv_service_name.
      WHEN 'ZCAC_0G_FOLLOW_UP'.
        "Optional: Read the reference change request (here in order to determine the creator of it)
        lri_model_ext->read_char_value(
          EXPORTING
            i_fieldname = usmd0_cs_fld-crequest
            it_sel      = VALUE #( ( fieldname = usmd0_cs_fld-crequest sign = 'I' option = 'EQ' low = iv_cr_number ) )
          IMPORTING
            et_data     = lit_crequest ).
        READ TABLE lit_crequest ASSIGNING FIELD-SYMBOL(<lwa_crequest>) WITH KEY usmd_crequest = iv_cr_number.
        IF <lwa_crequest> IS ASSIGNED.
          CONCATENATE <lwa_crequest>-usmd_creq_text '/' <lwa_crequest>-usmd_changed_by INTO lfd_description.
          lri_model_ext->create_follow_up_crequest(
            EXPORTING
              iv_ref_crequest_id       =  iv_cr_number                " ID of reference change request
*              iv_crequest_type         =                  " Type of follow-up change request
              iv_description           = lfd_description                 " Description of follow-up change request
*              iv_created_by            = SY-UNAME         " Creator of follow-up change request
*              iv_priority              =                  " Priority
*              iv_due_date              =                  " Due Date
*              iv_reason                =                  " Reason
*              iv_copy_notes            = abap_false       " Copy notes from reference change request
*              iv_copy_attachments      = abap_false       " Copy attachments from reference change request
*              iv_keep_note_creators    = abap_false       " X = Take over the note creators from reference cr
*              iv_skip_checks_at_save   = abap_false       " X = Checks at save are skipped
*              iv_keep_attachm_creators = abap_false       " X = Take over the attachment creators from reference cr
*              iv_perform_commit        = abap_false       " X = Perform an explicit commit work at save
            IMPORTING
              ev_fu_crequest_id        = DATA(lfd_crequest_re)                 " ID of follow-up change request
*              et_message               =                  " Messages
          ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
