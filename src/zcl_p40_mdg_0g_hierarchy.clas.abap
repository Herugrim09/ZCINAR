CLASS zcl_p40_mdg_0g_hierarchy DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_p40_mdg_0g_hierarchy .

    DATA gv_model TYPE usmd_model VALUE '0G' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !iv_model    TYPE usmd_model
        !iv_crequest TYPE usmd_crequest OPTIONAL .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_model          TYPE usmd_model
        !iv_crequest       TYPE usmd_crequest OPTIONAL
        !iv_get_new        TYPE abap_boolean OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zif_p40_mdg_0g_hierarchy .
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_index_vs_parent,
        index TYPE i,
        node  TYPE REF TO data,
      END OF ty_index_vs_parent .
    TYPES:
      ty_index_vs_parent_tab TYPE SORTED TABLE OF ty_index_vs_parent WITH UNIQUE KEY index .

    CLASS-DATA go_instance TYPE REF TO zif_p40_mdg_0g_hierarchy .
    DATA go_context TYPE REF TO if_usmd_app_context .
    DATA go_gov_api TYPE REF TO if_usmd_gov_api .
    DATA gv_crequest TYPE usmd_crequest .
    DATA go_hierarchy_badi TYPE REF TO zmed_assign_hierarchy_badi .

    METHODS update_change_request
      IMPORTING
        !iv_cr_number TYPE usmd_crequest .
    METHODS get_badi
      IMPORTING
        !iv_entity       TYPE usmd_entity
        !iv_project_name TYPE char5 .
    METHODS fill_node_key
      IMPORTING
        !iv_entity_name    TYPE usmd_entity
        !is_key            TYPE any
      CHANGING
        !cs_assignment_key TYPE usmd_s_assignment_key .
    METHODS fill_top_hierarchy
      IMPORTING
        !iv_entity_name    TYPE usmd_entity
        !is_key            TYPE any
      CHANGING
        !cs_assignment_key TYPE usmd_s_assignment_key .
    METHODS get_gov_api .
    METHODS fill_parent_node
      IMPORTING
        !iv_entity_name    TYPE usmd_entity
        !is_key            TYPE any
      CHANGING
        !cs_assignment_key TYPE usmd_s_assignment_key .
    METHODS fill_previous_node
      IMPORTING
        !iv_entity_name TYPE usmd_entity
        !is_key         TYPE any
      CHANGING
        !cs_assignment  TYPE usmd_s_assignment .
    METHODS get_parent_key
      IMPORTING
        !iv_hierarchy_name   TYPE usmd_entity
        !is_hierarchy_key    TYPE any
        !iv_entity           TYPE usmd_entity
        !is_key              TYPE any OPTIONAL
        !iv_parent           TYPE usmd_entity
        !iv_edition          TYPE usmd_edition OPTIONAL
        !iv_index            TYPE i DEFAULT '1'
      RETURNING
        VALUE(rr_parent_key) TYPE REF TO data
      RAISING
        cx_usmd_gov_api_core_error .
    METHODS assign_hierarchy_with_badi
      IMPORTING
        !iv_hierarchy_name   TYPE usmd_entity
        !is_hierarchy_key    TYPE any
        !iv_entity           TYPE usmd_entity
        !is_key              TYPE any
        !iv_parent           TYPE usmd_entity
        !is_parent_key       TYPE any
        !iv_edition          TYPE usmd_edition
        !iv_previous         TYPE usmd_entity
        !is_previous_key     TYPE any
        !iv_project_name     TYPE char5
        !iv_force_assignment TYPE abap_boolean OPTIONAL
        !iv_attempt_number   TYPE i DEFAULT 2
      EXPORTING
        !et_message_info     TYPE usmd_t_message .
ENDCLASS.



CLASS ZCL_P40_MDG_0G_HIERARCHY IMPLEMENTATION.


  METHOD assign_hierarchy_with_badi.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Assign Hierarchy with BAdI Private Method
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    DATA:
      ls_assignment_key     TYPE usmd_s_assignment_key,
      ls_assignment         TYPE usmd_s_assignment,
      lv_hierarchy_name     TYPE usmd_entity,
      lr_hierarchy_key      TYPE REF TO data,
      lv_parent_entity      TYPE usmd_entity,
      lr_parent_key         TYPE REF TO data,
      lv_previous_entity    TYPE usmd_entity,
      lr_previous_key       TYPE REF TO data,
      lt_children           TYPE usmd_t_hry_edge_rkey,
      ls_parent_node        TYPE usmd_s_hry_node_rkey,
      lr_entity_key         TYPE REF TO data,
      lr_entity_key_tab     TYPE REF TO data,
      lv_assignment_written TYPE abap_boolean,
      lv_crequest_locked    TYPE abap_boolean,
      lv_entity_locked      TYPE abap_boolean,
      lv_assignment_locked  TYPE abap_boolean,
      lv_next_attempt_no    TYPE i,
      lt_retry_message_info TYPE usmd_t_message,
      lv_note               TYPE usmd_note.

    FIELD-SYMBOLS:
      <ls_hierarchy_key> TYPE any,
      <ls_parent_key>    TYPE any,
      <ls_previous_key>  TYPE any,
      <lt_entity_key>    TYPE ANY TABLE,
      <ls_entity_key>    TYPE any.
    CONSTANTS:
      lc_max_assignment_attempts TYPE i VALUE 10,
      lc_retry_wait_seconds      TYPE i VALUE 10.
    CLEAR et_message_info.

    me->get_gov_api( ).

    lv_hierarchy_name  = iv_hierarchy_name.
    lv_parent_entity   = iv_parent.
    lv_previous_entity = iv_previous.

*    TRY.
*
*        "------------------------------------------------------------
*        "1. Lock Change Request
*        "------------------------------------------------------------
*        go_gov_api->enqueue_crequest(
*          iv_crequest_id = me->gv_crequest
*        ).
*
*        lv_crequest_locked = abap_true.
*        "------------------------------------------------------------
*        "2. Get BAdI implementation
*        "------------------------------------------------------------
*        me->get_badi( iv_entity = iv_entity iv_project_name = iv_project_name ).
*
*        "------------------------------------------------------------
*        "3. Derive hierarchy type/key
*        "------------------------------------------------------------
*        IF go_hierarchy_badi IS BOUND.
*
*          go_gov_api->create_data_reference(
*            EXPORTING
*              iv_entity_name = lv_hierarchy_name
*            IMPORTING
*              er_structure   = lr_hierarchy_key
*          ).
*
*          ASSIGN lr_hierarchy_key->* TO <ls_hierarchy_key>.
*
*          IF <ls_hierarchy_key> IS ASSIGNED.
*
*            IF is_hierarchy_key IS NOT INITIAL.
*              <ls_hierarchy_key> = CORRESPONDING #( is_hierarchy_key ).
*            ENDIF.
*
*            CALL BADI me->go_hierarchy_badi->determine_hierarchy
*              CHANGING
*                cv_hierarchy_type = lv_hierarchy_name
*                cv_hierarchy_key  = <ls_hierarchy_key>.
*
*          ENDIF.
*
*          me->fill_top_hierarchy(
*            EXPORTING
*              iv_entity_name     = lv_hierarchy_name
*              is_key            = <ls_hierarchy_key>
*            CHANGING
*              cs_assignment_key = ls_assignment_key
*          ).
*
*          "------------------------------------------------------------
*          "4. Derive parent type/key
*          "------------------------------------------------------------
*          UNASSIGN <ls_parent_key>.
*
*          go_gov_api->create_data_reference(
*            EXPORTING
*              iv_entity_name = lv_parent_entity
*            IMPORTING
*              er_structure   = lr_parent_key
*          ).
*
*          ASSIGN lr_parent_key->* TO <ls_parent_key>.
*
*          IF <ls_parent_key> IS ASSIGNED.
*
*            IF is_parent_key IS NOT INITIAL.
*              <ls_parent_key> = CORRESPONDING #( is_parent_key ).
*            ENDIF.
*
*            CALL BADI me->go_hierarchy_badi->determine_parent
*              CHANGING
*                cv_parent_type = lv_parent_entity
*                cv_parent_key  = <ls_parent_key>.
*
*          ENDIF.
*
*          me->fill_parent_node(
*            EXPORTING
*              iv_entity_name     = lv_parent_entity
*              is_key            = <ls_parent_key>
*            CHANGING
*              cs_assignment_key = ls_assignment_key
*          ).
*
*        ELSE.
*
*          "No BAdI implementation: use incoming parameters directly
*          me->fill_top_hierarchy(
*            EXPORTING
*              iv_entity_name     = iv_hierarchy_name
*              is_key            = is_hierarchy_key
*            CHANGING
*              cs_assignment_key = ls_assignment_key
*          ).
*
*          me->fill_parent_node(
*            EXPORTING
*              iv_entity_name     = iv_parent
*              is_key            = is_parent_key
*            CHANGING
*              cs_assignment_key = ls_assignment_key
*          ).
*
*          lv_hierarchy_name = iv_hierarchy_name.
*          lv_parent_entity  = iv_parent.
*
*        ENDIF.
*
*        "------------------------------------------------------------
*        "5. Fill processing node / child node
*        "------------------------------------------------------------
*        me->fill_node_key(
*          EXPORTING
*            iv_entity_name     = iv_entity
*            is_key            = is_key
*          CHANGING
*            cs_assignment_key = ls_assignment_key
*        ).
*
*        go_gov_api->create_data_reference(
*          EXPORTING
*            iv_entity_name = iv_entity                 " Entity Type
*          IMPORTING
*            er_structure   = lr_entity_key                  " Structure
*            er_table       = lr_entity_key_tab                 " Sorted Table (Unique key)
*        ).
*
*        ASSIGN lr_entity_key_tab->* TO <lt_entity_key>.
*        IF <lt_entity_key> IS NOT ASSIGNED.
*          RETURN.
*        ENDIF.
*
*        ASSIGN lr_entity_key->* TO <ls_entity_key>.
*        IF <ls_entity_key> IS NOT ASSIGNED.
*          RETURN.
*        ENDIF.
*
*        <ls_entity_key> = CORRESPONDING #( is_key ).
*        INSERT <ls_entity_key> INTO TABLE <lt_entity_key>.
*
*        go_gov_api->enqueue_entity(
*          EXPORTING
*            iv_crequest_id = me->gv_crequest                  " Change Request
*            iv_entity_name = iv_entity                 " Entity Type of Storage and Use Type 1
*            it_data        = <lt_entity_key>                 " Must Contain Entity Key
*        ).
*
*        lv_entity_locked = abap_true.
*
*        "------------------------------------------------------------
*        "6. Lock assignment
*        "------------------------------------------------------------
*        go_gov_api->enqueue_assignment(
*          iv_crequest_id    = me->gv_crequest
*          iv_leading_entity = lv_parent_entity
*          is_assignment_key = ls_assignment_key
*          iv_edition        = iv_edition
*        ).
*
*        lv_assignment_locked = abap_true.
*
*        ls_assignment = CORRESPONDING #( ls_assignment_key ).
*
*        "------------------------------------------------------------
*        "7. Read children for previous node derivation
*        "------------------------------------------------------------
*        CLEAR lt_children.
*        ls_parent_node = CORRESPONDING #( ls_assignment-parent ).
*        go_gov_api->get_direct_children(
*         EXPORTING
*           iv_lead_entity  = ls_assignment_key-parent-entity                " Leading Entity Type of Hierarchy
*           iv_edition      = iv_edition                 " Edition
*           is_parent       = ls_parent_node                 " Key of Parent Node (as Data Reference)
*         IMPORTING
*           et_children     = lt_children ).                " Hierarchy Edges Parent -> Child (Keys as Data Refs)
*
*        "------------------------------------------------------------
*        "8. Derive previous node
*        "------------------------------------------------------------
*        UNASSIGN <ls_previous_key>.
*
*        IF go_hierarchy_badi IS BOUND.
*
*          IF lv_previous_entity IS INITIAL.
*            lv_previous_entity = iv_entity.
*          ENDIF.
*
*          go_gov_api->create_data_reference(
*            EXPORTING
*              iv_entity_name = lv_previous_entity
*            IMPORTING
*              er_structure   = lr_previous_key
*          ).
*
*          ASSIGN lr_previous_key->* TO <ls_previous_key>.
*
*          IF <ls_previous_key> IS ASSIGNED.
*
*            IF is_previous_key IS NOT INITIAL.
*              <ls_previous_key> = CORRESPONDING #( is_previous_key ).
*            ENDIF.
*
*            CALL BADI me->go_hierarchy_badi->determine_previous
*              EXPORTING
*                cv_key          = is_key
*                pit_i_children  = lt_children
*              CHANGING
*                pfd_c_prev_type = lv_previous_entity
*                pwa_c_prev_key  = <ls_previous_key>.
*
*          ENDIF.
*
*        ELSEIF iv_previous IS NOT INITIAL
*           AND is_previous_key IS NOT INITIAL.
*
*          lv_previous_entity = iv_previous.
*
*          go_gov_api->create_data_reference(
*            EXPORTING
*              iv_entity_name = lv_previous_entity
*            IMPORTING
*              er_structure   = lr_previous_key
*          ).
*
*          ASSIGN lr_previous_key->* TO <ls_previous_key>.
*
*          IF <ls_previous_key> IS ASSIGNED.
*            <ls_previous_key> = CORRESPONDING #( is_previous_key ).
*          ENDIF.
*
*        ENDIF.
*
*        IF lv_previous_entity IS NOT INITIAL
*        AND <ls_previous_key> IS ASSIGNED
*        AND <ls_previous_key> IS NOT INITIAL.
*
*          me->fill_previous_node(
*            EXPORTING
*              iv_entity_name = lv_previous_entity
*              is_key        = <ls_previous_key>
*            CHANGING
*              cs_assignment = ls_assignment
*          ).
*
*        ENDIF.
*
*        "------------------------------------------------------------
*        "9. Write assignment
*        "------------------------------------------------------------
*        go_gov_api->write_assignment(
*          iv_crequest_id     = me->gv_crequest
*          iv_leading_entity  = lv_parent_entity
*          is_assignment_data = ls_assignment
*        ).
*        lv_assignment_written = abap_true.
*        "------------------------------------------------------------
*        "10. Dequeue assignment
*        "------------------------------------------------------------
*        go_gov_api->dequeue_assignment(
*          iv_crequest_id    = me->gv_crequest
*          iv_leading_entity = lv_parent_entity
*          is_assignment_key = ls_assignment_key
*        ).
*        lv_assignment_locked = abap_false.
*
*        go_gov_api->dequeue_entity(
*          iv_crequest_id = me->gv_crequest                 " Change Request
*          iv_entity_name = iv_entity                 " Entity
*          it_data        = <lt_entity_key>                 " Must Contain Entity Key
*
*        ).
*
*        lv_entity_locked = abap_false.
*        "------------------------------------------------------------
*        "11. Dequeue Change Request
*        "------------------------------------------------------------
*        go_gov_api->dequeue_crequest(
*          iv_crequest_id = me->gv_crequest
*        ).
*
*        lv_crequest_locked = abap_false.
*
*      CATCH cx_usmd_gov_api_core_error INTO DATA(lx_gov_api_core).
*
*        /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*          pfd_i_crequest      = me->gv_crequest
*          pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*          pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*          pit_i_messages      = lx_gov_api_core->mt_messages
*          pfd_i_exception_obj = lx_gov_api_core
*        ).
*
*        APPEND LINES OF lx_gov_api_core->mt_messages TO et_message_info.
*
*      CATCH cx_usmd_gov_api INTO DATA(lx_gov_api).
*
*        /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*          pfd_i_crequest      = me->gv_crequest
*          pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*          pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*          pit_i_messages  = lx_gov_api->mt_messages
*          pfd_i_exception_obj = lx_gov_api
*        ).
*
*        APPEND LINES OF lx_gov_api->mt_messages TO et_message_info.
*    ENDTRY.
*
*    "------------------------------------------------------------
*    " Release locks after failed assignment attempt
*    "------------------------------------------------------------
*    IF lv_assignment_locked = abap_true.
*      TRY.
*          go_gov_api->dequeue_assignment(
*            iv_crequest_id    = me->gv_crequest
*            iv_leading_entity = lv_parent_entity
*            is_assignment_key = ls_assignment_key
*          ).
*
*          lv_assignment_locked = abap_false.
*
*        CATCH cx_usmd_gov_api_core_error INTO lx_gov_api_core.
*
*          /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*            pfd_i_crequest      = me->gv_crequest
*            pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*            pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*            pit_i_messages      = lx_gov_api_core->mt_messages
*            pfd_i_exception_obj = lx_gov_api_core
*          ).
*
*          APPEND LINES OF lx_gov_api_core->mt_messages TO et_message_info.
*
*        CATCH cx_usmd_gov_api INTO lx_gov_api.
*
*          /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*            pfd_i_crequest      = me->gv_crequest
*            pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*            pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*            pit_i_messages  = lx_gov_api->mt_messages
*            pfd_i_exception_obj = lx_gov_api
*          ).
*
*          APPEND LINES OF lx_gov_api->mt_messages TO et_message_info.
*      ENDTRY.
*    ENDIF.
*
*    IF lv_entity_locked = abap_true
*       AND <lt_entity_key> IS ASSIGNED.
*
*      TRY.
*          go_gov_api->dequeue_entity(
*            iv_crequest_id = me->gv_crequest
*            iv_entity_name = iv_entity
*            it_data        = <lt_entity_key>
*          ).
*
*          lv_entity_locked = abap_false.
*
*        CATCH cx_usmd_gov_api_core_error INTO lx_gov_api_core.
*
*          /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*            pfd_i_crequest      = me->gv_crequest
*            pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*            pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*            pit_i_messages      = lx_gov_api_core->mt_messages
*            pfd_i_exception_obj = lx_gov_api_core
*          ).
*
*          APPEND LINES OF lx_gov_api_core->mt_messages TO et_message_info.
*
*        CATCH cx_usmd_gov_api INTO lx_gov_api.
*
*          /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*            pfd_i_crequest      = me->gv_crequest
*            pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*            pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*            pit_i_messages  = lx_gov_api->mt_messages
*            pfd_i_exception_obj = lx_gov_api
*          ).
*
*          APPEND LINES OF lx_gov_api->mt_messages TO et_message_info.
*      ENDTRY.
*    ENDIF.
*
*    "Write the error as note to change request
*    IF et_message_info IS NOT INITIAL.
*      DATA(lo_util) = /s4e/cl_p40_mdg_0g_sg_util=>get_instance( ).
*      CALL FUNCTION 'FORMAT_MESSAGE'
*        EXPORTING
*          id        = et_message_info[ 1 ]-msgid        " Application Area
*          lang      = 'E'
*          no        = et_message_info[ 1 ]-msgno
*          v1        = et_message_info[ 1 ]-msgv1        " 1st parameter
*          v2        = et_message_info[ 1 ]-msgv2        " 2nd parameter
*          v3        = et_message_info[ 1 ]-msgv3        " 3rd parameter
*          v4        = et_message_info[ 1 ]-msgv4        " 4th Parameter
*        IMPORTING
*          msg       = lv_note
*        EXCEPTIONS
*          not_found = 1
*          OTHERS    = 2.
*      IF sy-subrc = 0.
*        lo_util->write_note(
*                  EXPORTING
*                    pfd_i_crequest     = gv_crequest                 " Change Request
*                    pfd_i_note         = lv_note                 " Note
*                    pri_i_gov_api      = go_gov_api                 " Governance API
*                ).
*      ENDIF.
*    ENDIF.
*
*    IF lv_crequest_locked = abap_true.
*      TRY.
*          go_gov_api->dequeue_crequest(
*            iv_crequest_id = me->gv_crequest
*          ).
*
*          lv_crequest_locked = abap_false.
*
*        CATCH cx_usmd_gov_api_core_error INTO lx_gov_api_core.
*
*          /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*            pfd_i_crequest      = me->gv_crequest
*            pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*            pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*            pit_i_messages      = lx_gov_api_core->mt_messages
*            pfd_i_exception_obj = lx_gov_api_core
*          ).
*
*          APPEND LINES OF lx_gov_api_core->mt_messages TO et_message_info.
*
*        CATCH cx_usmd_gov_api INTO lx_gov_api.
*
*          /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*            pfd_i_crequest      = me->gv_crequest
*            pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*            pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*            pit_i_messages  = lx_gov_api->mt_messages
*            pfd_i_exception_obj = lx_gov_api
*          ).
*
*          APPEND LINES OF lx_gov_api->mt_messages TO et_message_info.
*      ENDTRY.
*    ENDIF.
*
*    "------------------------------------------------------------
*    " Force assignment: maximum five total attempts
*    "------------------------------------------------------------
*    IF lv_assignment_written = abap_false
*       AND iv_force_assignment = abap_true
*       AND iv_attempt_number < lc_max_assignment_attempts.
*
*      lv_next_attempt_no = iv_attempt_number + 1.
*
*      WAIT UP TO lc_retry_wait_seconds SECONDS.
*
*      CLEAR lt_retry_message_info.
*
*      me->assign_hierarchy_with_badi_internal(
*        EXPORTING
*          iv_hierarchy_name   = iv_hierarchy_name
*          is_hierarchy_key    = is_hierarchy_key
*          iv_entity           = iv_entity
*          is_key              = is_key
*          iv_parent           = iv_parent
*          is_parent_key       = is_parent_key
*          iv_edition          = iv_edition
*          iv_previous         = iv_previous
*          is_previous_key     = is_previous_key
*          iv_project_name     = iv_project_name
*          iv_force_assignment = iv_force_assignment
*          iv_attempt_number    = lv_next_attempt_no
*        IMPORTING
*          et_message_info     = lt_retry_message_info
*      ).
*
*      et_message_info = lt_retry_message_info.
*
*    ENDIF.
  ENDMETHOD.


  METHOD constructor.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Constructor. Initiliaze instace attributes
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: Clean ABAP
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    me->gv_model = iv_model.
    IF me->go_context IS NOT BOUND.
      go_context = cl_usmd_app_context=>get_context( ).
    ENDIF.
    me->get_gov_api( ).
    IF iv_crequest IS SUPPLIED.
*-->4000006602
      update_change_request( iv_cr_number = iv_crequest ).
*<--4000006602
    ENDIF.
  ENDMETHOD.


  METHOD fill_node_key.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Fill node key of a hierarchy structure
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    FIELD-SYMBOLS: <ls_any_ref>     TYPE any.
    DATA: ls_node           TYPE usmd_s_hry_node_rkey,
          lr_entity_key_str TYPE REF TO data.

    go_gov_api->create_data_reference(
      EXPORTING
        iv_entity_name = iv_entity_name                 " Entity Type
      IMPORTING
        er_structure   = lr_entity_key_str                 " Structure
    ).

    ASSIGN lr_entity_key_str->* TO <ls_any_ref>.
    IF <ls_any_ref> IS ASSIGNED.
      <ls_any_ref> = CORRESPONDING #( is_key ).
    ENDIF.

    ls_node-entity = iv_entity_name.
    ls_node-key = lr_entity_key_str.
    cs_assignment_key-node = ls_node.

  ENDMETHOD.


  METHOD fill_parent_node.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Write parent node into class' buffer
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    FIELD-SYMBOLS:
      <ls_any_ref> TYPE any.

    DATA:
      ls_node           TYPE usmd_s_hry_pnode_rkey,
      lr_entity_key_str TYPE REF TO data.

    go_gov_api->create_data_reference(
      EXPORTING
        iv_entity_name = iv_entity_name
      IMPORTING
        er_structure   = lr_entity_key_str
    ).

    ASSIGN lr_entity_key_str->* TO <ls_any_ref>.

    IF <ls_any_ref> IS ASSIGNED.
      <ls_any_ref> = CORRESPONDING #( is_key ).
    ENDIF.

    ls_node-entity = iv_entity_name.
    ls_node-key    = lr_entity_key_str.

    cs_assignment_key-parent = ls_node.

  ENDMETHOD.


  METHOD fill_previous_node.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Write previous node into class' buffer
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    FIELD-SYMBOLS:
      <ls_any_ref> TYPE any.

    DATA:
      ls_node           TYPE usmd_s_hry_node_rkey,
      lr_entity_key_str TYPE REF TO data.

    go_gov_api->create_data_reference(
      EXPORTING
        iv_entity_name = iv_entity_name
      IMPORTING
        er_structure   = lr_entity_key_str
    ).

    ASSIGN lr_entity_key_str->* TO <ls_any_ref>.

    IF <ls_any_ref> IS ASSIGNED.
      <ls_any_ref> = CORRESPONDING #( is_key ).
    ENDIF.

    ls_node-entity = iv_entity_name.
    ls_node-key    = lr_entity_key_str.

    cs_assignment-predec = ls_node.

  ENDMETHOD.


  METHOD fill_top_hierarchy.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Fill top node of the hierarchy
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************

    FIELD-SYMBOLS:
      <ls_any_ref>     TYPE any.
    DATA:
      ls_node           TYPE usmd_s_hry_name_rkey,
      lr_entity_key_str TYPE REF TO data.

    go_gov_api->create_data_reference(
        EXPORTING
          iv_entity_name = iv_entity_name                 " Entity Type
        IMPORTING
          er_structure   = lr_entity_key_str                 " Structure
).

    ASSIGN lr_entity_key_str->* TO <ls_any_ref>.
    IF <ls_any_ref> IS ASSIGNED.
      <ls_any_ref> = CORRESPONDING #( is_key ).
    ENDIF.

    ls_node-entity = iv_entity_name.
    ls_node-key = lr_entity_key_str.
    cs_assignment_key-hryname = ls_node.

  ENDMETHOD.


  METHOD get_badi.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Hierarchy BAdI Initialization
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
*    TRY.
*        GET BADI go_hierarchy_badi
*          FILTERS
*            entity  = iv_entity
*            project = iv_project_name.
*
*      CATCH cx_badi INTO DATA(lx_badi).
*
*        DATA(lo_app_context) = cl_usmd_app_context=>get_context( ).
*        DATA(lv_crequest_id) = lo_app_context->mv_crequest_id.
*
*        /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*          pfd_i_crequest      = lv_crequest_id
*          pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*          pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*          pfd_i_message_text  = lx_badi->get_longtext( )
*          pfd_i_e vxception_obj = lx_badi
*        ).
*
*    ENDTRY.

  ENDMETHOD.


  METHOD get_gov_api.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Get governance API instance
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    IF go_gov_api IS NOT BOUND.
      go_gov_api = cl_usmd_gov_api=>get_instance(
                     iv_model_name = me->gv_model                  " Data Model
                   ).
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Get class instance
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: 4000006602
* Reason        : Enabling new instance of this class change request
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
*-->4000006602
    "To get new instances for every iteration, set iv_get_new to abap_true
    IF iv_get_new = abap_true.
      IF iv_crequest IS SUPPLIED.
        go_instance = NEW zcl_p40_mdg_0g_hierarchy( iv_model = iv_model iv_crequest = iv_crequest ).
      ELSE.
        go_instance = NEW zcl_p40_mdg_0g_hierarchy( iv_model = iv_model ).
      ENDIF.
      ro_instance = go_instance.
      RETURN.
    ENDIF.
*<--4000006602
    IF go_instance IS NOT BOUND.
      IF iv_crequest IS SUPPLIED.
        go_instance = NEW zcl_p40_mdg_0g_hierarchy( iv_model = iv_model iv_crequest = iv_crequest ).
      ELSE.
        go_instance = NEW zcl_p40_mdg_0g_hierarchy( iv_model = iv_model ).
      ENDIF.
    ENDIF.

    ro_instance = go_instance.
  ENDMETHOD.


  METHOD get_parent_key.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Private method for get_parent_nodes of the interface method
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: 4000006602
* Reason        : Commented code deletion
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************

    DATA:
      ls_assignment             TYPE usmd_s_assignment_key,
      lt_parent_nodes_container TYPE ty_index_vs_parent_tab,
      lv_index                  TYPE i.
    CONSTANTS lc_max_hierarchy_levels TYPE i VALUE 30.

    CLEAR: rr_parent_key.
    "0 index mean nothing
    IF iv_index = 0.
      RETURN.
    ENDIF.
*-->4000006602 "IF statement added
    IF is_key IS NOT INITIAL.
*<--4000006602
      "Fill node key
      me->fill_node_key(
        EXPORTING
          iv_entity_name     = iv_entity
          is_key            = is_key
        CHANGING
          cs_assignment_key = ls_assignment
      ).
    ENDIF.

    IF is_hierarchy_key IS NOT INITIAL. "4000006602 "IF statement added
      "Fill Hierarchy name key
      me->fill_top_hierarchy(
        EXPORTING
          iv_entity_name     = iv_hierarchy_name
          is_key            = is_hierarchy_key
        CHANGING
          cs_assignment_key = ls_assignment
      ).
    ENDIF.
    "Set index to 1
    lv_index = 1.
    "Start recursive method call. If the first parent is requested, read assignment will called just once.
    DO.
      "Read assignment
      me->go_gov_api->read_assignment(
        EXPORTING
          iv_crequest_id      = me->gv_crequest                  " Change Request
          iv_leading_entity   = iv_parent                " Hierarchy bearing Entity Type
          iv_edition          = iv_edition                 " Edition
          is_assignment_key   = ls_assignment                " Key of Hierarchy Assignment
        IMPORTING
          et_assignments_data = DATA(lt_assignment_data)                 " Parents/Predecessors of Hierarchy Node (Keys as Data Refs)
      ).

      "Get parent node from lt_assignment_data.
      READ TABLE lt_assignment_data ASSIGNING FIELD-SYMBOL(<ls_assignment_data>) WITH KEY hryname-entity = iv_hierarchy_name.
      CLEAR ls_assignment-node.
      IF <ls_assignment_data> IS ASSIGNED.
        "If index is found, return with data reference.
        IF iv_index = lv_index.
          rr_parent_key = <ls_assignment_data>-parent-key.
          RETURN.
        ENDIF.

        "Otherwise, collect parent nodes as data reference structures in a local buffer.
        INSERT VALUE ty_index_vs_parent( index = lv_index node = <ls_assignment_data>-parent-key ) INTO TABLE lt_parent_nodes_container.

        CLEAR ls_assignment-node.
        "Prepare the parameters for next read_assignment execution
        ASSIGN <ls_assignment_data>-parent-key->* TO FIELD-SYMBOL(<ls_key>).
        IF <ls_key> IS ASSIGNED.
          "If the levels end, exit.
          IF is_hierarchy_key = <ls_key>.
            EXIT.
          ENDIF.

          me->fill_node_key(
            EXPORTING
              iv_entity_name     = <ls_assignment_data>-parent-entity
              is_key            = <ls_key>
            CHANGING
              cs_assignment_key = ls_assignment
              ).
        ENDIF.
      ENDIF.
      lv_index = lv_index + 1.
      "Actually we will exit from DO loop whenever top node level reached. Nevertheless, we guarantee an exit condition with plan B.
      "Plan B: In case of <ls_key> cannot be assigned. No hierchy would consist of 30 node levels.
      IF lv_index = lc_max_hierarchy_levels.
        EXIT.
      ENDIF.
    ENDDO.
    "How many parent nodes are there?
    DATA(lv_final_index) = lines( lt_parent_nodes_container ).

    " Note: minus index means backward index. Example:
    " Lets say our hierarchy contain 10 levels of nodes and we need -4th of them ( last 4th from top level ).
    "Then our positive index should be 10 + (-4) + 1 = 7. Add 1 since local index starts with 1.
    IF iv_index < 0.
      "Add 1 since our index starts from 1.
      lv_final_index = lv_final_index + iv_index + 1.
      READ TABLE lt_parent_nodes_container ASSIGNING FIELD-SYMBOL(<ls_parent_node>) WITH KEY index = lv_final_index.
      IF <ls_parent_node> IS ASSIGNED.
        rr_parent_key = <ls_parent_node>-node.
      ENDIF.
*   If the given index is more than the number of node level, return with second last node.
    ELSE.
      lv_final_index = lv_final_index - 1.
      READ TABLE lt_parent_nodes_container ASSIGNING <ls_parent_node> WITH KEY index = lv_final_index.
      IF <ls_parent_node> IS ASSIGNED.
        rr_parent_key = <ls_parent_node>-node.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD update_change_request.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Update change request buffer
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    IF iv_cr_number <> me->gv_crequest.
      me->gv_crequest = iv_cr_number.
    ENDIF.
  ENDMETHOD.


  METHOD zif_p40_mdg_0g_hierarchy~assign_hierarchy.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Assign Hierarchy with governance API
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: 4000006602
* Reason        : Hierarchy Assignment method was introduced
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
*-->4000006602
    DATA:
      ls_assignment_key TYPE usmd_s_assignment_key,
      ls_assignment     TYPE usmd_s_assignment,
      lr_entity_key     TYPE REF TO data,
      lr_entity_key_tab TYPE REF TO data.

    FIELD-SYMBOLS: <lt_entity_key> TYPE ANY TABLE,
                   <ls_entity_key> TYPE any.

    CLEAR et_message_info.

    "Gov API instance should already be created in constructor,
    "but keep this defensive call.
    me->get_gov_api( ).

    IF me->gv_crequest IS INITIAL.
      "Gov API write_assignment needs CR context.
      "If CR is not supplied, assignment cannot be written safely.
*      APPEND VALUE usmd_s_message(
*        msgid = /s4e/if_p40_mdg_0g_constants=>c_msg_id_782
*        msgty = /s4e/if_p40_mdg_0g_constants=>c_msgty_w
*        msgno = /s4e/if_p40_mdg_0g_constants=>c_msg_no_14
*      ) TO et_message_info.

      RETURN.
    ENDIF.

    TRY.

        "1. Lock Change Request
        go_gov_api->enqueue_crequest(
          iv_crequest_id = me->gv_crequest
        ).


        "2. Construct assignment key: child node
        me->fill_node_key(
          EXPORTING
            iv_entity_name     = iv_entity
            is_key            = is_key
          CHANGING
            cs_assignment_key = ls_assignment_key
        ).

        "3. Construct assignment key: parent node
        me->fill_parent_node(
          EXPORTING
            iv_entity_name     = iv_parent
            is_key            = is_parent_key
          CHANGING
            cs_assignment_key = ls_assignment_key
        ).

        "4. Construct assignment key: hierarchy name/top hierarchy
        me->fill_top_hierarchy(
          EXPORTING
            iv_entity_name     = iv_hierarchy_name
            is_key            = is_hierarchy_key
          CHANGING
            cs_assignment_key = ls_assignment_key
        ).


        "Lock Processing Entity
        go_gov_api->create_data_reference(
          EXPORTING
            iv_entity_name = iv_entity                 " Entity Type
          IMPORTING
            er_structure   = lr_entity_key                  " Structure
            er_table       = lr_entity_key_tab                 " Sorted Table (Unique key)
        ).

        ASSIGN lr_entity_key_tab->* TO <lt_entity_key>.
        IF <lt_entity_key> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        ASSIGN lr_entity_key->* TO <ls_entity_key>.
        IF <ls_entity_key> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        <ls_entity_key> = CORRESPONDING #( is_key ).
        INSERT <ls_entity_key> INTO TABLE <lt_entity_key>.

        go_gov_api->enqueue_entity(
          EXPORTING
            iv_crequest_id = me->gv_crequest                  " Change Request
            iv_entity_name = iv_entity                 " Entity Type of Storage and Use Type 1
            it_data        = <lt_entity_key>                 " Must Contain Entity Key
        ).

        "5. Lock hierarchy assignment
        go_gov_api->enqueue_assignment(
          iv_crequest_id      = me->gv_crequest
          iv_leading_entity   = iv_parent
          is_assignment_key   = ls_assignment_key
        ).

        "6. Prepare assignment data
        ls_assignment = CORRESPONDING #( ls_assignment_key ).

        "7. Fill previous node if supplied
        IF iv_previous IS NOT INITIAL
        AND is_previous_key IS NOT INITIAL.

          me->fill_previous_node(
            EXPORTING
              iv_entity_name = iv_previous
              is_key        = is_previous_key
            CHANGING
              cs_assignment = ls_assignment
          ).

        ENDIF.

        "8. Write assignment by Governance API
        go_gov_api->write_assignment(
          iv_crequest_id      = me->gv_crequest
          iv_leading_entity   = iv_parent
          is_assignment_data  = ls_assignment
        ).

        "9. Dequeue assignment
        go_gov_api->dequeue_assignment(
          iv_crequest_id      = me->gv_crequest
          iv_leading_entity   = iv_parent
          is_assignment_key   = ls_assignment_key
        ).

        go_gov_api->dequeue_entity(
          iv_crequest_id = me->gv_crequest                  " Change Request
          iv_entity_name = iv_entity                     " Entity
          it_data        = <lt_entity_key>                 " Must Contain Entity Key
        ).

        "10. Dequeue CR
        go_gov_api->dequeue_crequest(
          iv_crequest_id = me->gv_crequest
        ).

      CATCH cx_usmd_gov_api_core_error INTO DATA(lx_gov_api_core).

*        /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*          pfd_i_crequest      = me->gv_crequest
*          pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*          pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*          pit_i_messages      = lx_gov_api_core->mt_messages
*          pfd_i_exception_obj = lx_gov_api_core
*        ).
        APPEND LINES OF lx_gov_api_core->mt_messages TO et_message_info.
      CATCH cx_usmd_gov_api INTO DATA(lx_gov_api).

*        /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*          pfd_i_crequest      = me->gv_crequest
*          pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*          pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*          pit_i_messages      = lx_gov_api->mt_messages
*          pfd_i_exception_obj = lx_gov_api
*        ).
        APPEND LINES OF lx_gov_api->mt_messages TO et_message_info.
    ENDTRY.
*<--4000006602
  ENDMETHOD.


  METHOD zif_p40_mdg_0g_hierarchy~assign_hierarchy_with_badi.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Assign Hierarchy using BAdI
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    me->assign_hierarchy_with_badi(
      EXPORTING
        iv_hierarchy_name    = iv_hierarchy_name
        is_hierarchy_key     = is_hierarchy_key
        iv_entity            = iv_entity
        is_key               = is_key
        iv_parent            = iv_parent
        is_parent_key        = is_parent_key
        iv_edition           = iv_edition
        iv_previous          = iv_previous
        is_previous_key      = is_previous_key
        iv_project_name      = iv_project_name
        iv_force_assignment  = iv_force_assignment
      IMPORTING
        et_message_info      = et_message_info
    ).
  ENDMETHOD.


  METHOD zif_p40_mdg_0g_hierarchy~create_data_reference.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Create GOV API Hierarchy Node structures
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    DATA: lr_key_struct TYPE REF TO data.
    CLEAR rr_structure.
    get_gov_api( ).
    TRY.
        go_gov_api->create_data_reference(
          EXPORTING
            iv_entity_name = iv_entity                 " Entity Type
          IMPORTING
            er_structure   = lr_key_struct                 " Structure
        ).
      CATCH cx_usmd_gov_api INTO DATA(lx_gov_api). " General Processing Error GOV_API
*        /s4e/cl_p40_mdg_0g_logging=>write_application_log_simple(
*          pfd_i_crequest      = me->gv_crequest
*          pfd_i_object        = /s4e/cl_p40_mdg_0g_logging=>c_object_mdg
*          pfd_i_subobject     = /s4e/cl_p40_mdg_0g_logging=>c_sub_object_wf
*          pit_i_messages  = lx_gov_api->mt_messages
*          pfd_i_exception_obj = lx_gov_api
*       ).
    ENDTRY.

    IF lr_key_struct IS BOUND.
      ASSIGN lr_key_struct->* TO FIELD-SYMBOL(<ls_key_struct>).
      IF <ls_key_struct> IS ASSIGNED.
        CREATE DATA rr_structure LIKE <ls_key_struct>.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_p40_mdg_0g_hierarchy~get_children.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Get Children Nodes
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    CLEAR et_children.
    RETURN.
  ENDMETHOD.


  METHOD zif_p40_mdg_0g_hierarchy~get_parent_nodes.
************************************************************************
* Extension ID  : E00782
* Project ID    : S4E
* Purpose       : Returns parent node of the processing node based on index. See below
************************************************************************
* --Changes-------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
* ----------------------------------------------------------------------
* Correction No.: <SolMan S4E Support Change or Workitem>
* Reason        : <Describe Reason(s) for Change>
************************************************************************
    " Note: minus index means backward index. Example:
    " Lets say our hierarchy contain 10 levels of nodes and we need -4th of them. Then
    " our positive index should be 10 + (-4) + 1 = 7. Add 1 since local index starts with 1.

    DATA: lr_parent_key TYPE REF TO data.

    CLEAR: rr_parent_key.
    " Get parent key from the child data
    lr_parent_key = get_parent_key(
                       iv_hierarchy_name = iv_hierarchy_name
                       is_hierarchy_key  = is_hierarchy_key
                       iv_parent         = iv_parent
                       iv_entity         = iv_entity
                       is_key            = is_key
                       iv_edition        = iv_edition
                       iv_index          = iv_index
                     ).

    rr_parent_key = lr_parent_key.
  ENDMETHOD.
ENDCLASS.
