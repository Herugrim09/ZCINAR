*&---------------------------------------------------------------------*
*& Report ZPROG_CREATE_CATEGORY_CONV
*&---------------------------------------------------------------------*
*&
*& MDG Category creation using Conversion SOM API
*&---------------------------------------------------------------------*
REPORT zprog_create_category_conv.

DATA: lo_conv_api    TYPE REF TO if_usmd_conv_som_gov_api,
      lv_crequest    TYPE usmd_crequest,
      lv_model       TYPE usmd_model VALUE 'ZA',
      lv_entity      TYPE usmd_entity VALUE 'ZCATEGORY',
      lv_cr_type     TYPE usmd_crequest_type VALUE 'ZTCCAT01',
      lv_category_id TYPE string VALUE '0000000004'.

" Data structures for category entity
DATA: lr_data_tab TYPE REF TO data,
      lr_data_str TYPE REF TO data.

FIELD-SYMBOLS: <lt_category_data> TYPE ANY TABLE,
               <ls_category_data> TYPE any.

TRY.
    " Get conversion SOM API instance
    lo_conv_api = cl_usmd_conv_som_gov_api=>get_instance(
      iv_model_name = lv_model
    ).

    " Set environment for change request creation
    lo_conv_api->set_environment(
      iv_crequest_type    = lv_cr_type
      iv_create_crequest  = abap_true
    ).

    " Create data references for the entity
    lo_conv_api->get_entity_structure(
      EXPORTING
        iv_entity_name = lv_entity
        iv_struct_type = lo_conv_api->gc_struct_key_attr
      IMPORTING
        er_structure   = lr_data_str
        er_table       = lr_data_tab
    ).

    " Assign field symbols to data references
    ASSIGN lr_data_str->* TO <ls_category_data>.
    ASSIGN lr_data_tab->* TO <lt_category_data>.

    IF <ls_category_data> IS ASSIGNED AND <lt_category_data> IS ASSIGNED.
      " Fill category data
      ASSIGN COMPONENT 'ZCATEGORY' OF STRUCTURE <ls_category_data> TO FIELD-SYMBOL(<lv_category>).
      IF <lv_category> IS ASSIGNED.
        <lv_category> = lv_category_id.
      ENDIF.

      " Insert data into table
      INSERT <ls_category_data> INTO TABLE <lt_category_data>.

      " Get the change request ID
      lv_crequest = lo_conv_api->get_crequest_id( ).
      lo_conv_api->set_crequest_attributes(
        iv_crequest_text   = 'Test CR for Category'
*        iv_priority        =
*        iv_due_date        =
*        iv_reason          =
*        iv_reason_rejected =
      ).
*      CATCH cx_usmd_gov_api_core_error. " General Processing Error (CX_USMD_CORE_DYNAMIC_CHECK)
*      CATCH cx_usmd_gov_api.            " General Processing Error GOV_API
      " Prepare entity data structure for writing
      DATA: lt_entity_data TYPE usmd_gov_api_ts_ent_data,
            ls_entity_data TYPE usmd_gov_api_s_ent_data.

      ls_entity_data-entity = lv_entity.
      ls_entity_data-entity_data = lr_data_tab.
      INSERT ls_entity_data INTO TABLE lt_entity_data.

      " Dequeue entity
      DATA: lt_entity_for_enqueue TYPE usmd_gov_api_ts_ent_tabl,
            ls_entity_for_enqueue TYPE usmd_gov_api_s_ent_tabl.

      ls_entity_for_enqueue-entity = lv_entity.
      ls_entity_for_enqueue-tabl = lr_data_tab.
      INSERT ls_entity_for_enqueue INTO TABLE lt_entity_for_enqueue.

      TRY.
          lo_conv_api->enqueue_crequest( iv_lock_mode = 'E' ).
        CATCH cx_usmd_gov_api_core_error INTO DATA(lrcx_core). " CX_USMD_CORE_DYNAMIC_CHECK
        CATCH cx_usmd_gov_api INTO DATA(lrcx_gov_api).            " General Processing Error GOV_API

      ENDTRY.

      TRY.
          lo_conv_api->enqueue_entity(
            it_entity_keys = lt_entity_for_enqueue
          ).
        CATCH cx_usmd_gov_api INTO lrcx_gov_api.
      ENDTRY.
      " Write entity data to change request
      lo_conv_api->write_entity_data(
        it_entity_data = lt_entity_data
      ).

      " Optional: Validate the change request
      lo_conv_api->check( ).

*      " Save the change request
*      TRY.
*          lo_conv_api->save(
*            if_include_check_in_save_only = abap_true
*          ).
*        CATCH cx_usmd_gov_api_core_error INTO lrcx_gov_api_core.
*      ENDTRY.


      TRY.
          lo_conv_api->dequeue_entity(
            it_entity_keys = lt_entity_for_enqueue
          ).
        CATCH cx_usmd_gov_api INTO lrcx_gov_api.
      ENDTRY.

      " Dequeue change request
      TRY.
          lo_conv_api->dequeue_crequest( ).
        CATCH cx_usmd_gov_api INTO lrcx_gov_api.
      ENDTRY.

      "HIERARCHY
      DATA: lv_path TYPE string.
      DATA: lo_model_ext    TYPE REF TO if_usmd_model_ext,
            lr_top_key      TYPE REF TO data,
            lr_top_key_str  TYPE REF TO data,
            lr_node_key     TYPE REF TO data,
            lr_node_key_str TYPE REF TO data.
      FIELD-SYMBOLS: <lt_top_key>  TYPE ANY TABLE,
                     <lt_node_key> TYPE ANY TABLE.
      DATA(lo_hier) = zcl_p40_mdg_0g_assign_hry=>get_instance(
                        pfd_i_model     = 'ZA'
                  pfd_i_cr_number = lv_crequest
*                  pfd_i_with_badi = ' '
                      ).
      cl_usmd_model_ext=>get_instance(
        EXPORTING
          i_usmd_model = 'ZA'                  " Data model
        IMPORTING
          eo_instance  = lo_model_ext                 " MDM Data Model for Access from Non-SAP Standard
*    et_message   =                  " Messages
      ).
      lo_model_ext->create_data_reference(
        EXPORTING
          i_fieldname          = 'ZSUPREME'                  " Field Name
          i_struct             = lo_model_ext->gc_struct_key     " Structure
*    it_attribute         =                   " Field Names
*    if_incl_active_fld   = space             " MDGAF: General Indicator
*    if_incl_edition_fld  = space             " MDGAF: General Indicator
*    if_incl_obsolete_flg = space             " Financial MDM: General Indicator
*    if_incl_actioncode   = space             " Financial MDM: General Indicator
*    if_table             = 'X'               " Financial MDM: General Indicator
*    if_edtn_number       = space
*    i_tabtype            = gc_tabtype_hashed " Single-Character Indicator
        IMPORTING
          er_data              = lr_top_key
*    et_message           =                   " Messages
      ).
      ASSIGN lr_top_key->* TO <lt_top_key>.
      CREATE DATA lr_top_key_str LIKE LINE OF <lt_top_key>.
      ASSIGN lr_top_key_str->* TO FIELD-SYMBOL(<ls_top_key>).
      ASSIGN COMPONENT 'ZSUPREME' OF STRUCTURE <ls_top_key> TO FIELD-SYMBOL(<lv_any>).
      <lv_any> = '0000000001'.
      lo_model_ext->create_data_reference(
        EXPORTING
          i_fieldname          = 'ZCATEGORY'                  " Field Name
          i_struct             = lo_model_ext->gc_struct_key     " Structure
*    it_attribute         =                   " Field Names
*    if_incl_active_fld   = space             " MDGAF: General Indicator
*    if_incl_edition_fld  = space             " MDGAF: General Indicator
*    if_incl_obsolete_flg = space             " Financial MDM: General Indicator
*    if_incl_actioncode   = space             " Financial MDM: General Indicator
*    if_table             = 'X'               " Financial MDM: General Indicator
*    if_edtn_number       = space
*    i_tabtype            = gc_tabtype_hashed " Single-Character Indicator
        IMPORTING
          er_data              = lr_node_key
*    et_message           =                   " Messages
      ).
      ASSIGN lr_node_key->* TO <lt_node_key>.
      CREATE DATA lr_node_key_str LIKE LINE OF <lt_node_key>.
      ASSIGN lr_node_key_str->* TO FIELD-SYMBOL(<ls_node_key>).
      ASSIGN COMPONENT 'ZCATEGORY' OF STRUCTURE <ls_node_key> TO FIELD-SYMBOL(<lv_any2>).
      <lv_any2> = lv_category_id.
      lo_hier->assign_hierarchy(
        EXPORTING
          pfd_i_hierarchy     = 'ZSUPREME'
          pfd_i_hierarchy_key = lr_top_key_str
          pfd_i_entity        = 'ZCATEGORY'
          pfd_i_entity_key    = lr_node_key_str
    pfd_i_parent_entity = 'ZSUPREME'
    pfd_i_parent_key    = lr_top_key_str
        IMPORTING
          pfd_e_derived       = DATA(lv_is_derived)
          pit_e_message_info  = DATA(lt_messages)
      ).


      lo_conv_api->save(
        if_include_check_in_save_only = abap_true
      ).


      WRITE: / |Change request { lv_crequest } created successfully for category { lv_category_id }|.

    ELSE.
      WRITE: / 'Error: Could not assign field symbols to data references'.
    ENDIF.

  CATCH cx_usmd_conv_som_gov_api INTO DATA(lx_conv_api).
    WRITE: / 'Conversion SOM API Error:', lx_conv_api->get_text( ).

  CATCH cx_usmd_gov_api INTO DATA(lx_gov_api).
    WRITE: / 'Government API Error:', lx_gov_api->get_text( ).

  CATCH cx_root INTO DATA(lx_root).
    WRITE: / 'General Error:', lx_root->get_text( ).

ENDTRY.

" Commit the transaction
COMMIT WORK AND WAIT.

BREAK-POINT.
" Optional: Hierarchy assignment (if needed)
" Uncomment and modify as per your hierarchy requirements
*DATA: lo_hierarchy TYPE REF TO zcl_p40_mdg_0g_assign_hry.
*
*lo_hierarchy = zcl_p40_mdg_0g_assign_hry=>get_instance(
*  pfd_i_model = lv_model
*).
*
*lo_hierarchy->assign_hierarchy(
*  EXPORTING
*    pfd_i_hierarchy     = 'ZSUPREME'
*    pfd_i_hierarchy_key = '0000000001'
*    pfd_i_entity        = lv_entity
*    pfd_i_entity_key    = lv_category_id
*  IMPORTING
*    pfd_e_derived       = DATA(lv_is_derived)
*    pit_e_message_info  = DATA(lt_hierarchy_messages)
*).
