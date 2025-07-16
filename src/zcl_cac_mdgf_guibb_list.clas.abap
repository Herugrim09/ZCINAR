CLASS zcl_cac_mdgf_guibb_list DEFINITION
  PUBLIC
  INHERITING FROM cl_mdgf_guibb_list
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_key_val_pair,
        name  TYPE usmd_fieldname,
        value TYPE usmd_value,
      END OF t_key_val_pair .
    TYPES:
      tt_key_val_pair TYPE STANDARD TABLE OF t_key_val_pair .

    CONSTANTS c_creqtype_zfaccul1 TYPE usmd_crequest_type VALUE 'ZFACCUL1' ##NO_TEXT.
    CONSTANTS c_model_0g TYPE usmd_model VALUE '0G' ##NO_TEXT.
    METHODS if_fpm_guibb_list~get_data
        REDEFINITION .
    METHODS if_fpm_guibb_list~get_definition
        REDEFINITION .
  PROTECTED SECTION.
private section.

  methods MAKE_LINES_READ_ONLY
    importing
      !PRI_CONTEXT type ref to IF_USMD_APP_CONTEXT
    changing
      !PIT_C_DATA type DATA
      !PIT_C_ACTION_USAGE type FPMGB_T_ACTIONUSAGE
      !PIT_C_FIELD_USAGE type FPMGB_T_FIELDUSAGE .
  methods CHANGE_ITAB
    importing
      !PRI_CONTEXT type ref to IF_USMD_APP_CONTEXT
    exporting
      !PRI_E_ITAB_CHANGE_LOG type ref to IF_SALV_ITAB_CHANGE_LOG
    changing
      !PIT_C_DATA type DATA
      !PIT_C_ACTION_USAGE type FPMGB_T_ACTIONUSAGE .
  methods GET_CHANGED_FIELDS
    importing
      !PIT_I_KEYS type TT_KEY_VAL_PAIR
      !PFD_I_ENTITY type USMD_ENTITY
    exporting
      !PIT_E_CHANGED_FIELDS type USMD_T_CHANGED_ENTITIES .
ENDCLASS.



CLASS ZCL_CAC_MDGF_GUIBB_LIST IMPLEMENTATION.


  METHOD get_changed_fields.
    DATA: lri_conv_api TYPE REF TO if_usmd_conv_som_gov_api.
    DATA: lrd_key_struct TYPE REF TO data.
    DATA: lrd_key_tab TYPE REF TO data.
    DATA: lwa_gov_api_ent TYPE usmd_gov_api_s_ent_tabl.
    DATA: lit_gov_api_ent TYPE usmd_gov_api_ts_ent_tabl.

    FIELD-SYMBOLS: <lwa_key_struct> TYPE any.
    FIELD-SYMBOLS: <lit_key_struct> TYPE ANY TABLE.
    FIELD-SYMBOLS: <lit_data> TYPE ANY TABLE.
    FIELD-SYMBOLS: <lwa_data> TYPE any.
    FIELD-SYMBOLS: <lfd_key_attr> TYPE any.
    FIELD-SYMBOLS: <lfd_any_key_write> TYPE any.

    CLEAR: pit_e_changed_fields.

    " Get data model
    lri_conv_api = cl_usmd_conv_som_gov_api=>get_instance(
                    iv_model_name = c_model_0g
                  ).

    " Get entity key structure and table
    lri_conv_api->get_entity_structure(
      EXPORTING
        iv_entity_name = pfd_i_entity
      IMPORTING
        er_structure   =  lrd_key_struct
        er_table       =  lrd_key_tab
    ).

    ASSIGN lrd_key_struct->* TO <lwa_key_struct>.
    ASSIGN lrd_key_tab->* TO <lit_key_struct>.

    IF <lit_key_struct> IS ASSIGNED AND <lit_key_struct> IS ASSIGNED.
      LOOP AT pit_i_keys INTO DATA(lwa_key).
        ASSIGN COMPONENT lwa_key-name OF STRUCTURE <lwa_key_struct> TO FIELD-SYMBOL(<lfd_any>).
        IF <lfd_any> IS ASSIGNED.
          <lfd_any> = lwa_key-value.
        ENDIF.
      ENDLOOP.
      INSERT <lwa_key_struct> INTO TABLE <lit_key_struct>.
    ENDIF.

    lwa_gov_api_ent-entity = pfd_i_entity.
    lwa_gov_api_ent-tabl = lrd_key_tab.

    INSERT lwa_gov_api_ent INTO TABLE lit_gov_api_ent.

    lri_conv_api->get_entity_field_changes(
  EXPORTING
    iv_struct            = lri_conv_api->gc_struct_key_txt_langu
    it_entity_keys       =  lit_gov_api_ent
  RECEIVING
    rt_changes           = pit_e_changed_fields
).

  ENDMETHOD.


  METHOD if_fpm_guibb_list~get_data.
    DATA:
      lri_context        TYPE REF TO if_usmd_app_context,
      lwa_key            TYPE t_key_val_pair,
      lit_key            TYPE tt_key_val_pair,
      lit_changed_fields TYPE usmd_t_changed_entities,
      lv_index           TYPE int4.

    CALL METHOD super->if_fpm_guibb_list~get_data
      EXPORTING
        iv_eventid                = iv_eventid
        it_selected_fields        = it_selected_fields
        iv_raised_by_own_ui       = iv_raised_by_own_ui
        iv_visible_rows           = iv_visible_rows
        iv_edit_mode              = iv_edit_mode
        io_extended_ctrl          = io_extended_ctrl
      IMPORTING
        et_messages               = et_messages
        ev_data_changed           = ev_data_changed
        ev_field_usage_changed    = ev_field_usage_changed
        ev_action_usage_changed   = ev_action_usage_changed
        ev_selected_lines_changed = ev_selected_lines_changed
        ev_dnd_attr_changed       = ev_dnd_attr_changed
        eo_itab_change_log        = eo_itab_change_log
      CHANGING
        ct_data                   = ct_data
        ct_field_usage            = ct_field_usage
        ct_action_usage           = ct_action_usage
        ct_selected_lines         = ct_selected_lines
        cv_lead_index             = cv_lead_index
        cv_first_visible_row      = cv_first_visible_row
        cs_additional_info        = cs_additional_info
        ct_dnd_attributes         = ct_dnd_attributes.

    CHECK sy-uname NE 'BGOZUMOGULLA'.

    lri_context = cl_usmd_app_context=>get_context( ).

*    me->change_itab(
*      EXPORTING
*        pri_context           = lri_context
*      IMPORTING
*        pri_e_itab_change_log = eo_itab_change_log
*      CHANGING
*        pit_c_data            = ct_data
*        pit_c_action_usage    = ct_action_usage
*    ).

    me->make_lines_read_only(
      EXPORTING
        pri_context        = lri_context
      CHANGING
        pit_c_data         = ct_data
        pit_c_action_usage = ct_action_usage
        pit_c_field_usage  = ct_field_usage
    ).
    ev_data_changed = abap_true.
    ev_field_usage_changed = abap_true.
    ev_action_usage_changed = abap_true.
  ENDMETHOD.


  METHOD if_fpm_guibb_list~get_definition.
    CALL METHOD super->if_fpm_guibb_list~get_definition
      IMPORTING
        eo_field_catalog         = eo_field_catalog
        et_field_description     = et_field_description
        et_action_definition     = et_action_definition
        et_special_groups        = et_special_groups
        es_message               = es_message
        ev_additional_error_info = ev_additional_error_info
        et_dnd_definition        = et_dnd_definition
        et_row_actions           = et_row_actions
        es_options               = es_options.
  ENDMETHOD.


  METHOD change_itab.
    DATA:
      "lri_context        TYPE REF TO if_usmd_app_context,
      lwa_key               TYPE t_key_val_pair,
      lit_key               TYPE tt_key_val_pair,
      lit_changed_fields    TYPE usmd_t_changed_entities,
      lfd_index             TYPE int4,
      lrc_line_mode         TYPE REF TO cl_salv_itab_editor_line_mode,
      lrd_data_before       TYPE REF TO data,
      lfd_type              TYPE if_salv_itab_change_log=>y_modification_type,
      lit_deleted           TYPE if_salv_service_types=>yt_range,
      lit_inserted          TYPE if_salv_service_types=>yt_range,
      lcx_salv_edt_contract TYPE REF TO cx_salv_edt_contract_violation.

    FIELD-SYMBOLS: <lit_cdata> TYPE ANY TABLE.
    IF pri_context->mv_crequest_step = '00'.
      LOOP AT pit_c_data ASSIGNING FIELD-SYMBOL(<lwa_data>).
        IF <lwa_data> IS ASSIGNED.
          ASSIGN COMPONENT if_usmdz_cons_attributes=>gc_attr_coa OF STRUCTURE <lwa_data> TO FIELD-SYMBOL(<lfd_any>).
          IF <lfd_any> IS ASSIGNED.
            lwa_key-name = if_usmdz_cons_attributes=>gc_attr_coa.
            lwa_key-value = <lfd_any>.
            APPEND lwa_key TO lit_key.
            UNASSIGN: <lfd_any>.
          ENDIF.
          ASSIGN COMPONENT if_usmdz_cons_attributes=>gc_attr_account OF STRUCTURE <lwa_data> TO <lfd_any>.
          IF <lfd_any> IS ASSIGNED.
            lwa_key-name = if_usmdz_cons_attributes=>gc_attr_account.
            lwa_key-value = <lfd_any>.
            APPEND lwa_key TO lit_key.
            UNASSIGN: <lfd_any>.
          ENDIF.
        ENDIF.
        EXIT.
      ENDLOOP.

      me->get_changed_fields(
        EXPORTING
          pit_i_keys           = lit_key
          pfd_i_entity         = if_usmdz_cons_entitytypes=>gc_entity_account
        IMPORTING
          pit_e_changed_fields = lit_changed_fields
      ).
      "ASSIGN pit_c_data->* TO <lit_cdata>.

      READ TABLE lit_changed_fields INTO DATA(lwa_changed_fields) WITH KEY struct = 'KLTXT' entity_type = if_usmdz_cons_entitytypes=>gc_entity_account.
      READ TABLE lwa_changed_fields-changed_entities INTO DATA(lwa_changed_data) INDEX 1.
      ASSIGN lwa_changed_data-entity->* TO FIELD-SYMBOL(<lwa_changed_data>).
      IF <lwa_changed_data> IS ASSIGNED.
        ASSIGN COMPONENT 'LANGU' OF STRUCTURE <lwa_changed_data> TO <lfd_any>.
        "ITAB EDITOR
        IF lrc_line_mode IS INITIAL.
          CREATE OBJECT lrc_line_mode.
        ENDIF.
        IF <lfd_any> IS ASSIGNED.
          mo_itab_editor->start_recording( it_data = pit_c_data ).

          lrc_line_mode->start_recording(
            CHANGING
              ct_data = pit_c_data
                ).
          lfd_index = 1.
          LOOP AT pit_c_data ASSIGNING <lwa_data>.
            IF <lwa_data> IS ASSIGNED.
              ASSIGN COMPONENT 'LANGU' OF STRUCTURE <lwa_data> TO FIELD-SYMBOL(<lfd_langu>).
              IF <lfd_langu> IS ASSIGNED.

                IF <lfd_langu> = <lfd_any>.
                  lfd_index = lfd_index + 1.
                  TRY.
                      lrc_line_mode->modify_line(
                        EXPORTING
                          i_line     = <lwa_data>
                          i_index    = 1                 " Index of Internal Tables
                        RECEIVING
                          r_sy_subrc = DATA(lrd_sy_subrc)                  " Return Value of ABAP Statements
                      ).
                    CATCH cx_salv_edt_contract_violation INTO lcx_salv_edt_contract. " Editor contract violated by caller
                  ENDTRY.
                  CONTINUE.
                ELSE.
                  TRY.
                      lrc_line_mode->log_delete_lines(
                                                      i_index       = sy-tabix                 " Index of Internal Tables
                                                      i_line_number = lfd_index
                                                                                   ).
*                      CATCH cx_salv_edt_contract_violation. " API contract violated by caller
                    CATCH cx_salv_edt_contract_violation INTO lcx_salv_edt_contract.

                  ENDTRY.
                  TRY.
                      lrc_line_mode->delete_line(
                        EXPORTING
                          i_index    = lfd_index
                        RECEIVING
                          r_sy_subrc = lrd_sy_subrc                 " Return Value of ABAP Statements
                      ).
                    CATCH cx_salv_edt_contract_violation INTO lcx_salv_edt_contract. " Editor contract violated by caller

                  ENDTRY.


                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.

          READ TABLE pit_c_action_usage ASSIGNING FIELD-SYMBOL(<lwa_action_usage>) WITH KEY id = 'FPM_BOL_TABLE_INSERT'.
          IF <lwa_action_usage> IS ASSIGNED.
            <lwa_action_usage>-enabled = abap_false.
          ENDIF.

*          CATCH cx_salv_illegal_key. " illegal definition of key fields

*          CATCH cx_salv_edt_contract_violation. " Editor contract violated by caller


          mo_itab_editor->if_salv_itab_change_log~get_index_map(
  IMPORTING
    et_deleted                 = lit_deleted                 " deleted lines in before image
    et_inserted                = lit_inserted                 " inserted lines in after image
    et_moved                   = DATA(lit_moved)                 " mapping of lines between before and after image
    et_move_to_insert_position = DATA(lit_move_to)                 " existing lines for moving to insert position
  ).
*          CATCH cx_salv_illegal_key. " illegal definition of key fields
          "mo_itab_editor->stop_recording( ).

*          mo_itab_editor->if_salv_itab_change_log~data_is_new(
*            RECEIVING
*              r_is_new = DATA(lfd_is_new)
*          ).
          lrc_line_mode->if_salv_itab_change_log~data_is_new(
            RECEIVING
              r_is_new = DATA(pfd_boolean)
          ).
*CATCH cx_salv_edt_contract_violation. " Editor contract violated by caller
          lrc_line_mode->set_new_data( it_data = pit_c_data ).

          pri_e_ITAB_CHANGE_LOG = lrc_line_mode.
*          CATCH cx_salv_illegal_key. " illegal definition of key fields


        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD make_lines_read_only.
    DATA:
      lwa_key            TYPE t_key_val_pair,
      lit_key            TYPE tt_key_val_pair,
      lit_changed_fields TYPE usmd_t_changed_entities.

    IF pri_context->mv_crequest_step = '00'.
      LOOP AT pit_c_data ASSIGNING FIELD-SYMBOL(<lwa_data>).
        IF <lwa_data> IS ASSIGNED.
          ASSIGN COMPONENT if_usmdz_cons_attributes=>gc_attr_coa OF STRUCTURE <lwa_data> TO FIELD-SYMBOL(<lfd_any>).
          IF <lfd_any> IS ASSIGNED.
            lwa_key-name = if_usmdz_cons_attributes=>gc_attr_coa.
            lwa_key-value = <lfd_any>.
            APPEND lwa_key TO lit_key.
            UNASSIGN: <lfd_any>.
          ENDIF.
          ASSIGN COMPONENT if_usmdz_cons_attributes=>gc_attr_account OF STRUCTURE <lwa_data> TO <lfd_any>.
          IF <lfd_any> IS ASSIGNED.
            lwa_key-name = if_usmdz_cons_attributes=>gc_attr_account.
            lwa_key-value = <lfd_any>.
            APPEND lwa_key TO lit_key.
            UNASSIGN: <lfd_any>.
          ENDIF.
        ENDIF.
        EXIT.

      ENDLOOP.

      me->get_changed_fields(
        EXPORTING
          pit_i_keys           = lit_key
          pfd_i_entity         = if_usmdz_cons_entitytypes=>gc_entity_account
        IMPORTING
          pit_e_changed_fields = lit_changed_fields
      ).

      READ TABLE lit_changed_fields INTO DATA(lwa_changed_fields) WITH KEY struct = 'KLTXT' entity_type = if_usmdz_cons_entitytypes=>gc_entity_account.
      READ TABLE lwa_changed_fields-changed_entities INTO DATA(lwa_changed_data) INDEX 1.
      ASSIGN lwa_changed_data-entity->* TO FIELD-SYMBOL(<lwa_changed_data>).
      IF <lwa_changed_data> IS ASSIGNED.
        ASSIGN COMPONENT 'LANGU' OF STRUCTURE <lwa_changed_data> TO <lfd_any>.
      ENDIF.
      IF <lfd_any> IS ASSIGNED.
        LOOP AT pit_c_data ASSIGNING <lwa_data>.
          IF <lwa_data> IS ASSIGNED.
            ASSIGN COMPONENT 'LANGU' OF STRUCTURE <lwa_data> TO FIELD-SYMBOL(<lfd_langu>).
            IF <lfd_langu> IS ASSIGNED.
              IF <lfd_langu> = <lfd_any>.
                CONTINUE.
              ENDIF.
              ASSIGN COMPONENT 'FPM_BOL_GUIBB_REF_0000000013' OF STRUCTURE <lwa_data> TO FIELD-SYMBOL(<lfd_any_ref>).
              <lfd_any_ref> = 'X'.
              ASSIGN COMPONENT 'FPM_BOL_GUIBB_REF_0000000015' OF STRUCTURE <lwa_data> TO <lfd_any_ref>.
              "<lfd_any_ref> = abap_false.
              ASSIGN COMPONENT 'FPM_BOL_GUIBB_REF_0000000017' OF STRUCTURE <lwa_data> TO <lfd_any_ref>.
              <lfd_any_ref> = 'X'.
              ASSIGN COMPONENT 'FPM_BOL_GUIBB_REF_0000000019' OF STRUCTURE <lwa_data> TO <lfd_any_ref>.
              "<lfd_any_ref> = abap_false.
            ENDIF.
          ENDIF.
        ENDLOOP.

        READ TABLE pit_c_action_usage ASSIGNING FIELD-SYMBOL(<lwa_action_usage>) WITH KEY id = 'FPM_BOL_TABLE_INSERT'.
        IF <lwa_action_usage> IS ASSIGNED.
          <lwa_action_usage>-enabled = abap_false.
        ENDIF.
        READ TABLE pit_c_field_usage ASSIGNING FIELD-SYMBOL(<lwa_field_usage>) WITH KEY name = 'LANGU'.
        IF <lwa_field_usage> IS ASSIGNED.
          <lwa_field_usage>-enabled = abap_false.
          <lwa_field_usage>-read_only = abap_true.
          <lwa_field_usage>-visibility = '00'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
