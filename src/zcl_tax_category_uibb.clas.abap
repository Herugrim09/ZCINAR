CLASS zcl_tax_category_uibb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_fpm_guibb .
    INTERFACES if_fpm_guibb_form .

    CLASS-DATA: gs_tax_attributes TYPE zmdg_user_lock.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_tax_category_uibb IMPLEMENTATION.

  METHOD if_fpm_guibb_form~check_config.
  ENDMETHOD.

  METHOD if_fpm_guibb_form~flush.
  ENDMETHOD.

  METHOD if_fpm_guibb_form~get_data.
    FIELD-SYMBOLS: <fs_field_usage> TYPE fpmgb_s_fieldusage.
    DATA: lv_time TYPE timestamp.
    " Process events for ZLOCK and ZUNLOCK actions
    IF io_event->mv_event_id = 'ZLOCK'.
      ASSIGN cs_data TO FIELD-SYMBOL(<ls_data>).

      IF <ls_data> IS ASSIGNED.
        " Update the global structure with values from the UI
        ASSIGN COMPONENT 'MAIN' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_main>).
        IF <lv_main> IS ASSIGNED.
          gs_tax_attributes-main = <lv_main>.
        ENDIF.

        ASSIGN COMPONENT 'SUB1' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_sub1>).
        IF <lv_sub1> IS ASSIGNED.
          gs_tax_attributes-sub1 = <lv_sub1>.
        ENDIF.

        ASSIGN COMPONENT 'SUB2' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_sub2>).
        IF <lv_sub2> IS ASSIGNED.
          gs_tax_attributes-sub2 = <lv_sub2>.
        ENDIF.

        GET TIME STAMP FIELD lv_time.

        gs_tax_attributes-timestamp = lv_time.
        gs_tax_attributes-username = sy-uname.
        gs_tax_attributes-mandt = sy-mandt.

        MODIFY zmdg_user_lock  FROM gs_tax_attributes.
      ENDIF.

      " Loop through all fields in the dynamic structure and set them to read-only
      LOOP AT ct_field_usage ASSIGNING <fs_field_usage>.
        <fs_field_usage>-visibility = '01'. " Set field to read-only
      ENDLOOP.

      " Update action usage: hide the lock button, show the unlock button
      LOOP AT ct_action_usage ASSIGNING FIELD-SYMBOL(<fs_action_usage>).
        CASE <fs_action_usage>-id.
          WHEN 'ZLOCK'.
            <fs_action_usage>-visible = '01'. " Hide the lock button
          WHEN 'ZUNLOCK'.
            <fs_action_usage>-visible = '02'. " Show the unlock button
        ENDCASE.
      ENDLOOP.

    ELSEIF io_event->mv_event_id = 'ZUNLOCK'.
      " Restore the original state of the fields
      LOOP AT ct_field_usage ASSIGNING <fs_field_usage>.
        " Reset the read-only status to its previous state
        <fs_field_usage>-visibility = '02'. " Allow editing again
      ENDLOOP.

      DELETE FROM zmdg_user_lock WHERE username = sy-uname.
      CLEAR: gs_tax_attributes.

      " Update action usage: show the lock button, hide the unlock button
      LOOP AT ct_action_usage ASSIGNING <fs_action_usage>.
        CASE <fs_action_usage>-id.
          WHEN 'ZLOCK'.
            <fs_action_usage>-visible = '02'. " Show the lock button
          WHEN 'ZUNLOCK'.
            <fs_action_usage>-visible = '01'. " Hide the unlock button
        ENDCASE.
      ENDLOOP.
    ENDIF.

    " Indicate that data has changed
    ev_data_changed = abap_true.
    ev_field_usage_changed = abap_true.
    ev_action_usage_changed = abap_true.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD if_fpm_guibb_form~get_default_config.
  ENDMETHOD.

  METHOD if_fpm_guibb_form~get_definition.

    " Define the structure of the form
    eo_field_catalog ?= cl_abap_structdescr=>describe_by_name( p_name = 'ZMDG_TAX_CAT_STR' ).

    " Define the ZLOCK action
    DATA: ls_action_definition TYPE fpmgb_s_actiondef.

    CLEAR ls_action_definition.
    ls_action_definition-id       = 'ZLOCK'.
    ls_action_definition-text     = 'Lock Form'.
    ls_action_definition-tooltip  = 'Click to lock the taxonomy attributes form'.
    ls_action_definition-enabled  = abap_true.
    ls_action_definition-visible  = '02'. " 02 = Always visible
    ls_action_definition-exposable = abap_true.
    APPEND ls_action_definition TO et_action_definition.

    " Define the ZUNLOCK action
    CLEAR ls_action_definition.
    ls_action_definition-id       = 'ZUNLOCK'.
    ls_action_definition-text     = 'Unlock Form'.
    ls_action_definition-tooltip  = 'Click to unlock the taxonomy attributes form'.
    ls_action_definition-enabled  = abap_true.
    ls_action_definition-visible  = '02'. " 02 = Always visible
    ls_action_definition-exposable = abap_true.
    APPEND ls_action_definition TO et_action_definition.

  ENDMETHOD.

  METHOD if_fpm_guibb_form~process_event.

    " Handle ZLOCK and ZUNLOCK actions
    IF io_event->mv_event_id = 'ZLOCK'.
      " Logic for locking the form
      MESSAGE 'The form has been locked. You can open Attribute View' TYPE 'I'.

    ELSEIF io_event->mv_event_id = 'ZUNLOCK'.
      " Logic for unlocking the form
      MESSAGE 'The form has been unlocked.' TYPE 'I'.
      DELETE zmdg_user_lock  FROM gs_tax_attributes.
      CLEAR: gs_tax_attributes.
    ENDIF.

  ENDMETHOD.

  METHOD if_fpm_guibb~get_parameter_list.
  ENDMETHOD.

  METHOD if_fpm_guibb~initialize.
  ENDMETHOD.

ENDCLASS.

