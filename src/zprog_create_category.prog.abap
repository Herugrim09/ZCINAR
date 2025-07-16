*&---------------------------------------------------------------------*
*& Report ZPROG_CREATE_CATEGORY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprog_create_category.

DATA: lrc_hierarchy TYPE REF TO zcl_p40_mdg_0g_assign_hry.

lrc_hierarchy = zcl_p40_mdg_0g_assign_hry=>get_instance(
pfd_i_model     = 'ZA'
*                  pfd_i_cr_number =
).

lrc_hierarchy->assign_hierarchy(
  EXPORTING
    pfd_i_hierarchy     = 'ZSUPREME'
    pfd_i_hierarchy_key = '0000000001'
    pfd_i_entity        = 'ZCATEGORY'
    pfd_i_entity_key    = '0000000001'
*    pfd_i_parent_entity =
*    pfd_i_parent_key    =
*    pfd_i_previous      =
*    pfd_i_previous_key  =
  IMPORTING
    pfd_e_derived       = DATA(lfd_is_derived)
    pit_e_message_info  = DATA(lit_messages)
).

*DATA: lri_gov_api TYPE REF TO if_usmd_gov_api.
*
*lri_gov_api = cl_usmd_gov_api=>get_instance(
*                iv_model_name =  'ZA'                 " Data Model
**                iv_classname  = 'CL_USMD_GOV_API' " Object Type Name
*              ).
*DATA: lrd_category_data_tab TYPE REF TO data,
*      lrd_category_data_str TYPE REF TO data.
*
*lri_gov_api->create_data_reference(
*  EXPORTING
*    iv_entity_name =  'ZSUPREME'                " Entity Type
*    iv_struct      = lri_gov_api->gc_struct_key_attr   " Type of Data Structure
**    it_attribute   =                  " Number of Attributes to Be Returned (If Requested)
**    iv_edition     = abap_false       " Include edition into generated object?
*  IMPORTING
*    er_structure   =  lrd_category_data_str                " Structure
*    er_table       = lrd_category_data_tab                 " Sorted Table (Unique key)
*).
**CATCH cx_usmd_gov_api. " General Processing Error GOV_API
*
*FIELD-SYMBOLS: <lit_category_data> TYPE ANY TABLE,
*               <lwa_category_data> TYPE any.
*
*ASSIGN lrd_category_data_str->* TO <lwa_category_data>.
*ASSIGN lrd_category_data_tab->* TO <lit_category_data>.
*
*IF <lwa_category_data> IS ASSIGNED AND <lit_category_data> IS ASSIGNED.
*  ASSIGN COMPONENT 'ZSUPREME' OF STRUCTURE <lwa_category_data> TO FIELD-SYMBOL(<lfd_any>).
*  <lfd_any> = '0000000001'.
*  INSERT <lwa_category_data> INTO TABLE <lit_category_data>.
*ENDIF.
*
*DATA(lfd_crequest) = lri_gov_api->create_crequest(
*                       iv_crequest_type = 'ZTXSUP01'                 " Type of Change Request
**                       iv_description   =                  " Description (long text)
**                       iv_edition       =                  " Edition
*                     ).
*TRY.
*    lri_gov_api->enqueue_crequest(
*      iv_crequest_id = lfd_crequest                 " Change Request
**  iv_lock_mode   = 'E'              " Lock Mode
**  iv_scope       = '1'              " Lock Behavior
*    ).
*  CATCH cx_usmd_gov_api_core_error INTO DATA(lrcx_gov_api_core). " CX_USMD_CORE_DYNAMIC_CHECK
*  CATCH cx_usmd_gov_api INTO DATA(lrcx_gov_api).            " General Processing Error GOV_API
*ENDTRY.
*TRY.
*    lri_gov_api->enqueue_entity(
*      EXPORTING
*        iv_crequest_id = lfd_crequest                 " Change Request
*        iv_entity_name = 'ZSUPREME'                 " Entity Type of Storage and Use Type 1
*        it_data        = <lit_category_data>                 " Must Contain Entity Key
**    iv_lock_mode   = 'E'              " Block Mode
**    iv_scope       = '1'              " Block Behavior
**  IMPORTING
**    et_locked      =                  " Blocked Entities
*    ).
*  CATCH               cx_usmd_gov_api_core_error INTO lrcx_gov_api_core.  " CX_USMD_CORE_DYNAMIC_CHECK
*  CATCH  cx_usmd_gov_api_entity_lock INTO DATA(lrcx_entity_lock). " RESUMABLE Error While Blocking an Entity
*  CATCH               cx_usmd_gov_api INTO lrcx_gov_api.             " General Processing Error GOV_API
*ENDTRY.
*TRY.
*    lri_gov_api->write_entity(
*      iv_crequest_id = lfd_crequest                 " Change Request
*      iv_entity_name =  'ZSUPREME'                " Entity Type
*      it_data        =  <lit_category_data>                " Entity Keys and Attributes
**    it_attribute   =                  " List of Field Names with Changed Data
*    ).
*  CATCH               cx_usmd_gov_api_core_error INTO lrcx_gov_api_core.   " CX_USMD_CORE_DYNAMIC_CHECK
*  CATCH BEFORE UNWIND cx_usmd_gov_api_entity_write INTO DATA(lrcx_entity_write). " RESUMABLE Error While Writing an Entity
*  CATCH               cx_usmd_gov_api INTO lrcx_gov_api.              " General Processing Error GOV_API
*ENDTRY.
*TRY.
*    lri_gov_api->save(
**  i_mode = if_usmd_ui_services=>gc_save_mode_draft " Type of Save Mode (Declared in if_usmd_ui_services)
*    ).
*  CATCH cx_usmd_gov_api_core_error INTO lrcx_gov_api_core. " CX_USMD_CORE_DYNAMIC_CHECK
*ENDTRY.
*TRY.
*    lri_gov_api->dequeue_entity(
*      iv_crequest_id = lfd_crequest                  " Change Request
*      iv_entity_name = 'ZSUPREME'                 " Entity
*      it_data        =  <lit_category_data>                " Must Contain Entity Key
**      iv_lock_mode   = 'E'              " Block Mode
**      iv_scope       = '1'              " Block Behavior
*    ).
*  CATCH cx_usmd_gov_api INTO lrcx_gov_api. " General Processing Error GOV_API
*ENDTRY.
*TRY.
*    lri_gov_api->dequeue_crequest(
*      iv_crequest_id = lfd_crequest                 " Change Request
**        iv_lock_mode   = 'E'              " Lock Mode
**        iv_scope       = '1'              " Lock Behavior
*    ).
*  CATCH cx_usmd_gov_api INTO lrcx_gov_api. " General Processing Error GOV_API
*
*
*ENDTRY.
*TRY.
*    lri_gov_api->start_workflow( iv_crequest_id = lfd_crequest ).
*  CATCH cx_usmd_gov_api_core_error INTO lrcx_gov_api_core. " CX_USMD_CORE_DYNAMIC_CHECK
*ENDTRY.
*
*COMMIT WORK AND WAIT.
