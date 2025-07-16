CLASS zcl_cac_create_follow_up DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: c_pctr TYPE usmd_fieldname VALUE 'PCTR'.
    CONSTANTS: c_cctr TYPE usmd_fieldname VALUE 'CCTR'.
    CLASS-METHODS create_crequest_controlling
      IMPORTING
        !pfd_i_cr_number TYPE usmd_crequest
        !pfd_i_entity    TYPE usmd_entity.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cac_create_follow_up IMPLEMENTATION.
  METHOD create_crequest_controlling.
    DATA:
      lri_entity_key_str       TYPE REF TO data,
      lri_entity_key_langu     TYPE REF TO data,
      lri_entity_key_langu_tab TYPE REF TO data,
      lri_entity_key_att       TYPE REF TO data,
      lri_entity_key_att_tab   TYPE REF TO data,
      lri_entity_data_str      TYPE REF TO data,
      lri_gov_api              TYPE REF TO if_usmd_gov_api,
      lfd_crequest_id          TYPE usmd_crequest,
      lri_entity_key_tab       TYPE REF TO data,
      lri_entity_data_tab      TYPE REF TO data,
      lri_context              TYPE REF TO if_usmd_app_context,
      lri_model                TYPE REF TO if_usmd_model,
      lit_crequest_old         TYPE usmd_cr_ts_root,
      lit_crequest             TYPE usmd_cr_ts_root,
      lit_notes                TYPE usmd_t_crequest_note,
      lfd_fieldname            TYPE usmd_fieldname.

    FIELD-SYMBOLS:
      <lit_entity_key>       TYPE ANY TABLE,
      <lit_entity_data>      TYPE ANY TABLE,
      <lit_entity_key_langu> TYPE ANY TABLE,
      <lit_entity_key_att>   TYPE ANY TABLE,
      <lit_master_data>      TYPE ANY TABLE,
      <lit_text_data>        TYPE ANY TABLE,
      <lit_att_data>         TYPE ANY TABLE,
      <lwa_entity_key>       TYPE any,
      <lwa_entity_key_langu> TYPE any,
      <lwa_entity_data>      TYPE any,
      <lwa_entity_key_att>   TYPE any.

    " Get Model instance. This is needed since we have to update previous CR field in CR Master.
    cl_usmd_model=>get_instance(
      IMPORTING
        eo_instance = lri_model ).

    " Initialize context
    lri_context = cl_usmd_app_context=>get_context( ).
    IF lri_context IS NOT BOUND OR lri_context->mv_crequest_id NE pfd_i_cr_number.
      CALL METHOD cl_usmd_app_context=>discard_context( ).

      CALL METHOD cl_usmd_app_context=>init_context
        EXPORTING
          iv_crequest_id = pfd_i_cr_number.
      lri_context = cl_usmd_app_context=>get_context( ).
    ENDIF.

    " Determine fieldname based on entity type
    CASE pfd_i_entity.
      WHEN if_usmdz_cons_entitytypes=>gc_entity_cctr.
        lfd_fieldname = c_cctr.
      WHEN if_usmdz_cons_entitytypes=>gc_entity_pctr.
        lfd_fieldname = c_pctr.
    ENDCASE.

    " Read entity data from the original change request
    lri_context->mo_model->read_entity_data_all(
      EXPORTING
        i_fieldname    = lfd_fieldname
        if_active      = abap_false
        i_crequest     = pfd_i_cr_number
      IMPORTING
        et_data_entity = DATA(lit_data_entity) ).

    " If no data found, exit the method
    IF lit_data_entity IS INITIAL.
      RETURN. "If there is no entity data in staging, nothing has changed, so follow up is unnecessary.
    ENDIF.
    " Create an instance of the governance API
    TRY.
        lri_gov_api = cl_usmd_gov_api=>get_instance( iv_model_name = if_usmdz_cons_general=>gc_model_default ).
      CATCH cx_usmd_gov_api INTO DATA(lrcx_usmd_gov_api).
        "/s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
        "      pfd_i_crequest      = pfd_i_cr_number
        "      pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_fi
        "      pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
        "      pit_i_messages      = lrcx_usmd_gov_api->mt_messages
        "      pfd_i_exception_obj = lrcx_usmd_gov_api ).
    ENDTRY.

    " Read master data (key attributes)
    READ TABLE lit_data_entity WITH TABLE KEY usmd_entity = pfd_i_entity usmd_entity_cont = ''
                                  struct = lri_context->mo_model->gc_struct_key_attr
                                  INTO DATA(lwa_master_data).
    ASSIGN lwa_master_data-r_t_data->* TO <lit_master_data>.

    " Read text data
    READ TABLE lit_data_entity WITH TABLE KEY usmd_entity = pfd_i_entity usmd_entity_cont = ''
                                  struct = lri_context->mo_model->gc_struct_key_txt_langu
                                  INTO DATA(lwa_text_data).
    ASSIGN lwa_text_data-r_t_data->* TO <lit_text_data>.

    " Read attachment data
    READ TABLE lit_data_entity WITH TABLE KEY usmd_entity = pfd_i_entity usmd_entity_cont = ''
                                  struct = lri_context->mo_model->gc_struct_key_attach
                                  INTO DATA(lwa_att_data).
    ASSIGN lwa_att_data-r_t_data->* TO <lit_att_data>.

    " Get CR Attributes to read Edition
    lri_gov_api->get_crequest_attributes(
      EXPORTING
        iv_crequest_id = pfd_i_cr_number
      RECEIVING
        rs_crequest    = DATA(lwa_crequest)
    ).

    " Create data references
    lri_gov_api->create_data_reference(
      EXPORTING
        iv_entity_name = pfd_i_entity
        iv_struct      = lri_gov_api->gc_struct_key
      IMPORTING
        er_structure   = lri_entity_key_str
        er_table       = lri_entity_key_tab ).

    lri_gov_api->create_data_reference(
      EXPORTING
        iv_entity_name = pfd_i_entity
        iv_struct      = lri_gov_api->gc_struct_key_attr
      IMPORTING
        er_structure   = lri_entity_data_str
        er_table       = lri_entity_data_tab ).

    lri_gov_api->create_data_reference(
      EXPORTING
        iv_entity_name = pfd_i_entity
        iv_struct      = lri_gov_api->gc_struct_key_txt_langu
      IMPORTING
        er_structure   = lri_entity_key_langu
        er_table       = lri_entity_key_langu_tab ).

    lri_gov_api->create_data_reference(
      EXPORTING
        iv_entity_name = pfd_i_entity
        iv_struct      = lri_gov_api->gc_struct_key_attach_wo_cont
      IMPORTING
        er_structure   = lri_entity_key_att
        er_table       = lri_entity_key_att_tab ).

    " Assign field symbols
    ASSIGN lri_entity_key_str->* TO <lwa_entity_key>.
    ASSIGN lri_entity_key_tab->* TO <lit_entity_key>.
    ASSIGN lri_entity_data_str->* TO <lwa_entity_data>.
    ASSIGN lri_entity_data_tab->* TO <lit_entity_data>.
    ASSIGN lri_entity_key_langu->* TO <lwa_entity_key_langu>.
    ASSIGN lri_entity_key_langu_tab->* TO <lit_entity_key_langu>.
    ASSIGN lri_entity_key_att->* TO <lwa_entity_key_att>.
    ASSIGN lri_entity_key_att_tab->* TO <lit_entity_key_att>.

    " Process text data
    IF <lit_text_data> IS ASSIGNED AND <lit_text_data> IS NOT INITIAL.
      LOOP AT <lit_text_data> ASSIGNING FIELD-SYMBOL(<lwa_text_data>).
        IF <lwa_text_data> IS ASSIGNED AND <lwa_entity_key_langu> IS ASSIGNED AND <lit_entity_key_langu> IS ASSIGNED.
          MOVE-CORRESPONDING <lwa_text_data> TO <lwa_entity_key_langu>.
          INSERT <lwa_entity_key_langu> INTO TABLE <lit_entity_key_langu>.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Process entity data
    IF <lit_master_data> IS ASSIGNED AND <lit_master_data> IS NOT INITIAL.
      LOOP AT <lit_master_data> ASSIGNING FIELD-SYMBOL(<lwa_master_data>).
        IF <lwa_entity_data> IS ASSIGNED AND <lwa_entity_key> IS ASSIGNED AND <lwa_master_data> IS ASSIGNED.
          MOVE-CORRESPONDING <lwa_master_data> TO <lwa_entity_data>.
          MOVE-CORRESPONDING <lwa_master_data> TO <lwa_entity_key>.
          IF <lit_entity_data> IS ASSIGNED AND <lit_entity_key> IS ASSIGNED.
            INSERT <lwa_entity_data> INTO TABLE <lit_entity_data>.
            INSERT <lwa_entity_key> INTO TABLE <lit_entity_key>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Process attachment data
    IF <lit_att_data> IS ASSIGNED AND <lit_att_data> IS NOT INITIAL.
      LOOP AT <lit_att_data> ASSIGNING FIELD-SYMBOL(<lwa_att_data>).
        IF <lwa_entity_key_att> IS ASSIGNED AND <lit_entity_key_att> IS ASSIGNED AND <lwa_att_data> IS ASSIGNED.
          MOVE-CORRESPONDING <lwa_att_data> TO <lwa_entity_key_att>.
          INSERT <lwa_entity_key_att> INTO TABLE <lit_entity_key_att>.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Create change request
    DATA: lv_cr_type TYPE usmd_crequest_type.
    CASE pfd_i_entity.
      WHEN if_usmdz_cons_entitytypes=>gc_entity_cctr.
        lv_cr_type = 'ZCCT2P1'.
      WHEN if_usmdz_cons_entitytypes=>gc_entity_pctr.
        lv_cr_type = 'ZPCT2P1'.
    ENDCASE.

    TRY.
        lfd_crequest_id = lri_gov_api->create_crequest(
                           iv_crequest_type = lv_cr_type
                           iv_description   = |Create new { COND #( WHEN pfd_i_entity = if_usmdz_cons_entitytypes=>gc_entity_pctr
                                               THEN 'Profit Center' ELSE 'Cost Center' ) } via Follow-up CR|  ##NO_TEXT
                           iv_edition       = lwa_crequest-usmd_edition ).
      CATCH cx_usmd_gov_api INTO lrcx_usmd_gov_api.
        "/s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
        "    pfd_i_crequest      = lfd_crequest_id
        "    pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_fi
        "    pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
        "    pit_i_messages      = lrcx_usmd_gov_api->mt_messages
        "    pfd_i_exception_obj = lrcx_usmd_gov_api ).
    ENDTRY.

    " Enqueue change request
    lri_gov_api->enqueue_crequest(
                EXPORTING
                  iv_crequest_id = lfd_crequest_id ).

    " Enqueue entity
    TRY.
        lri_gov_api->enqueue_entity(
          EXPORTING
            iv_crequest_id = lfd_crequest_id
            iv_entity_name = pfd_i_entity
            it_data        = <lit_entity_key> ).
      CATCH cx_usmd_gov_api_entity_lock INTO DATA(lrcx_entity_lock).
        "/s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
        "    pfd_i_crequest      = lfd_crequest_id
        "    pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_fi
        "    pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
        "    pit_i_messages      = lrcx_entity_lock->mt_messages
        "    pfd_i_exception_obj = lrcx_entity_lock ).
      CATCH cx_usmd_gov_api INTO lrcx_usmd_gov_api.
*        ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*            pfd_i_crequest      = lfd_crequest_id
*            pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*            pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*            pit_i_messages      = lrcx_usmd_gov_api->mt_messages
*            pfd_i_exception_obj = lrcx_usmd_gov_api ).
    ENDTRY.

    " Write entity data
    IF <lit_entity_data> IS NOT INITIAL.
      TRY.
          lri_gov_api->write_entity(
            EXPORTING
              iv_crequest_id = lfd_crequest_id
              iv_entity_name = pfd_i_entity
              it_data        = <lit_entity_data> ).
        CATCH cx_usmd_gov_api_entity_write INTO DATA(lrcx_entity_write).
          "/s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
          "    pfd_i_crequest      = lfd_crequest_id
          "    pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_fi
          "    pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
          "    pit_i_messages      = lrcx_entity_write->mt_messages
          "    pfd_i_exception_obj = lrcx_entity_write ).
      ENDTRY.
    ENDIF.

    " Write text data
    IF <lit_entity_key_langu> IS NOT INITIAL.
      TRY.
          lri_gov_api->write_entity(
            EXPORTING
              iv_crequest_id = lfd_crequest_id
              iv_entity_name = pfd_i_entity
              it_data        = <lit_entity_key_langu> ).
        CATCH cx_usmd_gov_api_entity_write INTO lrcx_entity_write.
*          ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*              pfd_i_crequest      = lfd_crequest_id
*              pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*              pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*              pit_i_messages      = lrcx_entity_write->mt_messages
*              pfd_i_exception_obj = lrcx_entity_write ).
      ENDTRY.
    ENDIF.

    " Write attachment data if exists
    IF <lit_entity_key_att> IS NOT INITIAL.
      TRY.
          lri_gov_api->write_entity(
            EXPORTING
              iv_crequest_id = lfd_crequest_id
              iv_entity_name = pfd_i_entity
              it_data        = <lit_entity_key_att> ).
        CATCH cx_usmd_gov_api_entity_write INTO lrcx_entity_write.
*          ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*              pfd_i_crequest      = lfd_crequest_id
*              pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*              pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*              pit_i_messages      = lrcx_entity_write->mt_messages
*              pfd_i_exception_obj = lrcx_entity_write ).
      ENDTRY.
    ENDIF.

    " Update CR attributes to link to original CR
    DATA(lit_sel_ref) = VALUE usmd_ts_sel( ( fieldname = usmd0_cs_fld-crequest
                   sign = usmd0_cs_ra-sign_i option = usmd0_cs_ra-option_eq low = lfd_crequest_id ) ).

    " Read new CR Attributes
    lri_model->read_char_value(
      EXPORTING
        i_fieldname = usmd0_cs_fld-crequest
        it_sel      = lit_sel_ref
      IMPORTING
        et_data     = lit_crequest ).

    CLEAR: lit_sel_ref.
    lit_sel_ref = VALUE usmd_ts_sel( ( fieldname = usmd0_cs_fld-crequest
                   sign = usmd0_cs_ra-sign_i option = usmd0_cs_ra-option_eq low = pfd_i_cr_number ) ).
    lri_model->read_char_value(
      EXPORTING
        i_fieldname = usmd0_cs_fld-crequest
        it_sel      = lit_sel_ref
      IMPORTING
        et_data     = lit_crequest_old ).

    READ TABLE lit_crequest ASSIGNING FIELD-SYMBOL(<lwa_crequest>) WITH KEY usmd_crequest = lfd_crequest_id.

    IF <lwa_crequest> IS ASSIGNED.
      <lwa_crequest>-usmd_crequest_re = pfd_i_cr_number.
      <lwa_crequest>-usmd_reason = lit_crequest_old[ 1 ]-usmd_reason.
    ENDIF.

    lri_model->write_char_value(
      EXPORTING
        i_fieldname = usmd0_cs_fld-crequest
        it_data     = lit_crequest ).

    " Copy notes from previous CR
    TRY.
        lit_notes = lri_gov_api->get_notes( iv_crequest_id = pfd_i_cr_number ).
      CATCH cx_usmd_gov_api_core_error INTO DATA(lrcx_gov_api_core).
        "/s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
        "      pfd_i_crequest      = lfd_crequest_id
        "      pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_fi
        "      pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
        "      pit_i_messages      = lrcx_gov_api_core->mt_messages
        "      pfd_i_exception_obj = lrcx_gov_api_core ).
    ENDTRY.

    IF lit_notes IS NOT INITIAL.
      LOOP AT lit_notes ASSIGNING FIELD-SYMBOL(<lwa_note>).
        IF <lwa_note> IS ASSIGNED.
          TRY.
              lri_gov_api->write_note(
                EXPORTING
                  iv_crequest_id = lfd_crequest_id
                  iv_note        = <lwa_note>-usmd_note ).
            CATCH cx_usmd_gov_api_core_error INTO lrcx_gov_api_core.
              "/s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
              "      pfd_i_crequest      = lfd_crequest_id
              "      pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_fi
              "      pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
              "      pit_i_messages      = lrcx_gov_api_core->mt_messages
              "      pfd_i_exception_obj = lrcx_gov_api_core ).
            CATCH cx_usmd_gov_api INTO lrcx_usmd_gov_api.
              "/s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
              "      pfd_i_crequest      = lfd_crequest_id
              "      pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_fi
              "      pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
              "      pit_i_messages      = lrcx_usmd_gov_api->mt_messages
              "      pfd_i_exception_obj = lrcx_usmd_gov_api ).
          ENDTRY.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Copy attachments from original CR
    TRY.
        DATA(lit_attachment) = lri_gov_api->get_attachment_list(
                                 iv_crequest_id  = pfd_i_cr_number
                                 if_with_content = abap_true
                               ).
      CATCH cx_usmd_gov_api_core_error INTO lrcx_gov_api_core.
*        ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*                      pfd_i_crequest      = lfd_crequest_id
*                      pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*                      pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*                      pit_i_messages      = lrcx_gov_api_core->mt_messages
*                      pfd_i_exception_obj = lrcx_gov_api_core ).
    ENDTRY.

    IF lit_attachment IS NOT INITIAL.
      LOOP AT lit_attachment ASSIGNING FIELD-SYMBOL(<lwa_attachment>).
        IF <lwa_attachment> IS ASSIGNED.
          TRY.
              lri_gov_api->add_attachment(
                EXPORTING
                  iv_crequest_id = lfd_crequest_id
                  is_attachment  = <lwa_attachment>-data
              ).
            CATCH cx_usmd_gov_api_core_error INTO lrcx_gov_api_core.
*              ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*                    pfd_i_crequest      = lfd_crequest_id
*                    pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*                    pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*                    pit_i_messages      = lrcx_gov_api_core->mt_messages
*                    pfd_i_exception_obj = lrcx_gov_api_core ).
            CATCH cx_usmd_gov_api INTO lrcx_usmd_gov_api.
*              ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*                    pfd_i_crequest      = lfd_crequest_id
*                    pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*                    pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*                    pit_i_messages      = lrcx_usmd_gov_api->mt_messages
*                    pfd_i_exception_obj = lrcx_usmd_gov_api ).
          ENDTRY.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Save and dequeue
    lri_gov_api->save( i_mode = if_usmd_ui_services=>gc_save_mode_draft_no_check ).

    lri_gov_api->dequeue_entity(
      iv_crequest_id = lfd_crequest_id
      iv_entity_name = pfd_i_entity
      it_data        = <lit_entity_data> ).

    lri_gov_api->dequeue_crequest( EXPORTING iv_crequest_id = lfd_crequest_id ).

    " Check change request data
    TRY.
        lri_gov_api->check_crequest_data( iv_crequest_id = lfd_crequest_id ).
      CATCH cx_usmd_gov_api_core_error INTO lrcx_gov_api_core.
*        ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*            pfd_i_crequest      = lfd_crequest_id
*            pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*            pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*            pit_i_messages      = lrcx_gov_api_core->mt_messages
*            pfd_i_exception_obj = lrcx_gov_api_core ).
      CATCH cx_usmd_gov_api INTO lrcx_usmd_gov_api.
*        ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*            pfd_i_crequest      = lfd_crequest_id
*            pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*            pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*            pit_i_messages      = lrcx_usmd_gov_api->mt_messages
*            pfd_i_exception_obj = lrcx_usmd_gov_api ).
    ENDTRY.

    " Start workflow
    TRY.
        lri_gov_api->if_usmd_gov_api_process~start_workflow( EXPORTING iv_crequest_id = lfd_crequest_id ).
      CATCH cx_usmd_gov_api_core_error INTO lrcx_gov_api_core.
*        ZCL_MED_MDG_MSG_LOG_SERVICE=>write_application_log_simple(
*            pfd_i_crequest      = lfd_crequest_id
*            pfd_i_object        = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_object_fi
*            pfd_i_subobject     = ZCL_MED_MDG_MSG_LOG_SERVICE=>c_sub_object_wf
*            pit_i_messages      = lrcx_gov_api_core->mt_messages
*            pfd_i_exception_obj = lrcx_gov_api_core ).
    ENDTRY.

    " Commit work
    COMMIT WORK AND WAIT.

  ENDMETHOD.
ENDCLASS.
