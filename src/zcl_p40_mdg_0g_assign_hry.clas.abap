class ZCL_P40_MDG_0G_ASSIGN_HRY definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !PFD_I_MODEL type USMD_MODEL
      !PFD_I_CR_NUMBER type USMD_CREQUEST optional
      !PFD_I_WITH_BADI type ABAP_BOOLEAN default ' ' .
  class-methods GET_INSTANCE
    importing
      !PFD_I_MODEL type USMD_MODEL
      !PFD_I_CR_NUMBER type USMD_CREQUEST optional
      !PFD_I_WITH_BADI type ABAP_BOOLEAN default ' '
    returning
      value(PRC_R_OBJECT) type ref to ZCL_P40_MDG_0G_ASSIGN_HRY .
  methods ASSIGN_HIERARCHY
    importing
      !PFD_I_HIERARCHY type USMD_ENTITY optional
      !PFD_I_HIERARCHY_KEY type USMD_VALUE optional
      !PFD_I_ENTITY type USMD_ENTITY optional
      !PFD_I_ENTITY_KEY type USMD_VALUE optional
      !PFD_I_PARENT_ENTITY type USMD_ENTITY optional
      !PFD_I_PARENT_KEY type USMD_VALUE optional
      !PFD_I_PREVIOUS type USMD_ENTITY optional
      !PFD_I_PREVIOUS_KEY type USMD_VALUE optional
      !PFD_I_AUTO_PREV type ABAP_BOOLEAN default ' '
    exporting
      !PFD_E_DERIVED type ABAP_BOOLEAN
      !PIT_E_MESSAGE_INFO type USMD_T_MESSAGE .
  methods CHECK_THESE_FIELDS
    importing
      !PIT_I_FIELDS type USMD_T_ATTRIBUTE .
  PROTECTED SECTION.
    DATA:
      mv_model        TYPE usmd_model,
      fd_auto_prev    TYPE abap_boolean,
      rd_entity_key   TYPE REF TO data,
      rd_entity_tab   TYPE REF TO data,
      ri_conv_api     TYPE REF TO if_usmd_conv_som_gov_api,
      ri_model_ext    TYPE REF TO if_usmd_model_ext,
      mo_badi_handler TYPE REF TO zmed_assign_hierarchy_badi,
      wa_assignment   TYPE usmd_s_assignment_key,
      fd_cr_number    TYPE usmd_crequest,
      it_entity_key   TYPE usmd_gov_api_ts_ent_tabl,
      it_check_fields TYPE usmd_t_attribute.

    METHODS:
      get_entity_keys
        IMPORTING
          pfd_i_entity     TYPE usmd_entity
          pfd_i_value      TYPE usmd_value OPTIONAL
        EXPORTING
          prd_e_entity_key TYPE REF TO data,

      fill_node_key
        IMPORTING
          pfd_i_entiy_name     TYPE usmd_entity
          prd_i_key            TYPE usmd_value
        CHANGING
          pwa_c_assignment_key TYPE usmd_s_assignment_key,

      fill_top_hry
        IMPORTING
          pfd_i_entiy_name     TYPE usmd_entity
          pfd_i_key            TYPE usmd_value
        CHANGING
          pwa_c_assignment_key TYPE usmd_s_assignment_key,

      fill_parent_node
        IMPORTING
          pfd_i_entiy_name     TYPE usmd_entity
          pfd_i_key            TYPE usmd_value
        CHANGING
          pwa_c_assignment_key TYPE usmd_s_assignment_key,

      fill_previous_node
        IMPORTING
          pfd_i_entiy_name TYPE usmd_entity
          pfd_i_key        TYPE usmd_value
        CHANGING
          pwa_c_assignment TYPE usmd_s_assignment,

      check_fields
        IMPORTING
          pfd_i_entity            TYPE usmd_entity
        RETURNING
          VALUE(pfd_r_is_allowed) TYPE abap_boolean.

private section.

  data FD_WITH_BADI type ABAP_BOOLEAN value ' ' ##NO_TEXT.
  class-data GO_INSTANCE type ref to ZCL_P40_MDG_0G_ASSIGN_HRY .

  methods INITIALIZE_BADI
    importing
      !IV_ENTITY type USMD_ENTITY .
ENDCLASS.



CLASS ZCL_P40_MDG_0G_ASSIGN_HRY IMPLEMENTATION.


  METHOD assign_hierarchy.
    DATA: lrd_entity_key    TYPE REF TO data,
          lwa_assignment    TYPE usmd_s_assignment,
          lv_hierarchy_type TYPE usmd_entity,
          lv_hierarchy_key  TYPE usmd_value,
          lv_parent_type    TYPE usmd_entity,
          lv_parent_key     TYPE usmd_value,
          lv_previous_type  TYPE usmd_entity,
          lv_previous_key   TYPE usmd_value,
          lv_entity_key     TYPE usmd_value,
          lfd_is_allowed    TYPE abap_boolean.

    CLEAR: pfd_e_derived, pit_e_message_info.

    SET UPDATE TASK LOCAL.
    IF fd_cr_number IS NOT INITIAL.
      me->check_fields(
        EXPORTING
          pfd_i_entity     = pfd_i_entity
        RECEIVING
          pfd_r_is_allowed = lfd_is_allowed
      ).
    ELSE.
      lfd_is_allowed = abap_true.
    ENDIF.
    IF lfd_is_allowed = abap_false.
      RETURN.
    ENDIF.
    IF fd_cr_number IS NOT INITIAL.
      me->get_entity_keys(
     EXPORTING
       pfd_i_entity     = pfd_i_entity
     IMPORTING
       prd_e_entity_key = lrd_entity_key ).
    ENDIF.

    IF lfd_is_allowed = abap_true. "lrd_entity_key IS NOT INITIAL AND

      IF fd_with_badi = abap_true.
        " Initialize BAdI with current entity
        initialize_badi( pfd_i_entity ).
      ENDIF.
      " Convert entity key reference to value
      ASSIGN lrd_entity_key->* TO FIELD-SYMBOL(<ls_key>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT pfd_i_entity OF STRUCTURE <ls_key> TO FIELD-SYMBOL(<lv_key>).
        IF <lv_key> IS ASSIGNED.
          lv_entity_key = <lv_key>.
        ENDIF.
      ELSEIF pfd_i_entity_key IS SUPPLIED.
        lv_entity_key = pfd_i_entity_key.
      ENDIF.

      " Prepare and determine hierarchy values
      lv_hierarchy_type = pfd_i_hierarchy.
      lv_hierarchy_key  = pfd_i_hierarchy_key.

      IF ( pfd_i_hierarchy IS NOT SUPPLIED OR pfd_i_hierarchy_key IS NOT SUPPLIED  )
         AND mo_badi_handler IS BOUND.
        CALL BADI mo_badi_handler->determine_hierarchy
          CHANGING
            cv_hierarchy_type = lv_hierarchy_type
            cv_hierarchy_key  = lv_hierarchy_key.
      ENDIF.

      " Prepare and determine parent values
      lv_parent_type = pfd_i_parent_entity.
      lv_parent_key  = pfd_i_parent_key.

      IF ( pfd_i_parent_entity IS NOT SUPPLIED  OR pfd_i_parent_key IS NOT SUPPLIED  )
         AND mo_badi_handler IS BOUND.
        CALL BADI mo_badi_handler->determine_parent
          EXPORTING
            iv_entity_type = pfd_i_entity
            iv_entity_key  = lv_entity_key
          CHANGING
            cv_parent_type = lv_parent_type
            cv_parent_key  = lv_parent_key.
      ENDIF.


      " Prepare and determine previous values
      lv_previous_type = pfd_i_previous.
      lv_previous_key  = pfd_i_previous_key.


      IF ( pfd_i_previous IS NOT SUPPLIED  OR pfd_i_previous_key IS NOT SUPPLIED  )
         AND mo_badi_handler IS BOUND.
        CALL BADI mo_badi_handler->determine_previous
          EXPORTING
            iv_entity_type   = pfd_i_entity
            iv_entity_key    = lv_entity_key
          CHANGING
            cv_previous_type = lv_previous_type
            cv_previous_key  = lv_previous_key.
      ENDIF.

      " Fill assignment data
      IF pfd_i_entity IS SUPPLIED AND lv_entity_key IS NOT INITIAL.
        me->fill_node_key(
          EXPORTING
            pfd_i_entiy_name     = pfd_i_entity
            prd_i_key            = lv_entity_key
          CHANGING
            pwa_c_assignment_key = wa_assignment ).
      ENDIF.
      IF lv_hierarchy_key IS NOT INITIAL AND lv_hierarchy_key IS NOT INITIAL.
        me->fill_top_hry(
          EXPORTING
            pfd_i_entiy_name     = lv_hierarchy_type
            pfd_i_key            = lv_hierarchy_key
          CHANGING
            pwa_c_assignment_key = wa_assignment ).
      ENDIF.
      IF lv_parent_key IS NOT INITIAL AND lv_parent_key IS NOT INITIAL.
        me->fill_parent_node(
          EXPORTING
            pfd_i_entiy_name     = lv_parent_type
            pfd_i_key            = lv_parent_key
          CHANGING
            pwa_c_assignment_key = wa_assignment ).
      ENDIF.
      MOVE-CORRESPONDING wa_assignment TO lwa_assignment.

      IF lv_previous_type IS NOT INITIAL AND lv_previous_key IS NOT INITIAL.
        me->fill_previous_node(
          EXPORTING
            pfd_i_entiy_name = lv_previous_type
            pfd_i_key        = lv_previous_key
          CHANGING
            pwa_c_assignment = lwa_assignment
        ).
      ENDIF.

      ri_conv_api->read_assignment(
        EXPORTING
          iv_leading_entity   = pfd_i_parent_entity
          is_assignment_key   = wa_assignment
        IMPORTING
          et_assignments_data = DATA(lt_assignments) ).

      DATA(lv_assignment_count) = lines( lt_assignments ).
      CLEAR lt_assignments.

      ri_conv_api->enqueue_entity( it_entity_key ).

      TRY.
          ri_conv_api->enqueue_assignment(
            iv_leading_entity = lv_parent_type
            is_assignment_key = wa_assignment ).

          ri_conv_api->write_assignment(
            iv_leading_entity  = lv_parent_type
            is_assignment_data = lwa_assignment ).
        CATCH cx_usmd_gov_api INTO DATA(lrcx_usmd_gov_api).
      ENDTRY.
      TRY.
          ri_conv_api->read_assignment(
            EXPORTING
              iv_leading_entity   = lv_parent_type
              is_assignment_key   = wa_assignment
            IMPORTING
              et_assignments_data = lt_assignments ).

          IF lines( lt_assignments ) > lv_assignment_count.
            pfd_e_derived = abap_true.
          ENDIF.

        CATCH cx_usmd_gov_api INTO DATA(lo_error).
          IF lo_error->mt_messages IS NOT INITIAL.
            pit_e_message_info = lo_error->mt_messages.
          ENDIF.

          TRY.
              ri_conv_api->delete_assignment(
                iv_leading_entity  = lv_parent_type
                is_assignment_data = lwa_assignment ).
            CATCH cx_usmd_gov_api.
          ENDTRY.

          RETURN.
      ENDTRY.

      ri_conv_api->dequeue_assignment(
        iv_leading_entity = lv_parent_type
        is_assignment_key = wa_assignment ).

      ri_conv_api->dequeue_entity( it_entity_key ).

      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD check_fields.
    DATA: lfd_fieldname TYPE usmd_fieldname.
    pfd_r_is_allowed = abap_true.

    IF it_check_fields IS NOT INITIAL.
      lfd_fieldname = pfd_i_entity.

      ri_model_ext->read_entity_data_all(
        EXPORTING
          i_fieldname      = lfd_fieldname
          if_active        = abap_false
          i_crequest       = fd_cr_number
        IMPORTING
          et_data_entity   = DATA(lit_data_entity)
      ).

      READ TABLE lit_data_entity INTO DATA(lwa_data_entity) INDEX 1.

      ASSIGN lwa_data_entity-r_t_data->* TO FIELD-SYMBOL(<lit_data>).
      IF <lit_data> IS ASSIGNED.
        LOOP AT <lit_data> ASSIGNING FIELD-SYMBOL(<lwa_data>).
          LOOP AT it_check_fields ASSIGNING FIELD-SYMBOL(<lfd_check_field>).
            ASSIGN COMPONENT <lfd_check_field> OF STRUCTURE <lwa_data> TO FIELD-SYMBOL(<lfd_any>).
            IF <lfd_any> IS NOT ASSIGNED OR <lfd_any> IS INITIAL.
              pfd_r_is_allowed = abap_false.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        pfd_r_is_allowed = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_these_fields.
    IF it_check_fields IS INITIAL.
      APPEND LINES OF pit_i_fields TO it_check_fields.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mv_model = pfd_i_model.
    fd_cr_number = pfd_i_cr_number.
    fd_with_badi = pfd_i_with_badi.
    ri_conv_api = cl_usmd_conv_som_gov_api=>get_instance( iv_model_name = pfd_i_model ).

    cl_usmd_model_ext=>get_instance(
      EXPORTING
        i_usmd_model = pfd_i_model
      IMPORTING
        eo_instance  = ri_model_ext ).

  ENDMETHOD.


  METHOD fill_node_key.
    FIELD-SYMBOLS:
      <ls_any_ref> TYPE any,
      <lv_any>     TYPE any.

    ri_conv_api->get_entity_structure(
      EXPORTING
        iv_entity_name = pfd_i_entiy_name
      IMPORTING
        er_structure   = rd_entity_key
        er_table       = rd_entity_tab ).

    ASSIGN rd_entity_key->* TO <ls_any_ref>.

    me->get_entity_keys(
      EXPORTING
        pfd_i_entity     = pfd_i_entiy_name
*        pfd_i_value      = prd_i_key
      IMPORTING
        prd_e_entity_key = DATA(lrd_entity_key)
    ).


*    ASSIGN COMPONENT pfd_i_entiy_name OF STRUCTURE <ls_any_ref> TO <lv_any>.
*    IF sy-subrc = 0.
*      ASSIGN prd_i_key->* TO FIELD-SYMBOL(<lv_key>).
*      <lv_any> = <lv_key>.
*    ENDIF.

    pwa_c_assignment_key-node-entity = pfd_i_entiy_name.
    pwa_c_assignment_key-node-key = lrd_entity_key.

    ASSIGN rd_entity_tab->* TO FIELD-SYMBOL(<lt_entity_tab>).
    ASSIGN lrd_entity_key->* TO <ls_any_ref>.
    IF <ls_any_ref> IS ASSIGNED.
      INSERT <ls_any_ref> INTO TABLE <lt_entity_tab>.
    ENDIF.
    DATA: ls_entity_key TYPE usmd_gov_api_s_ent_tabl.
    ls_entity_key-entity = pfd_i_entiy_name.
    ls_entity_key-tabl = rd_entity_tab.
    INSERT ls_entity_key INTO TABLE it_entity_key.
  ENDMETHOD.


  METHOD fill_parent_node.
    FIELD-SYMBOLS:
      <ls_any_ref> TYPE any,
      <lv_any>     TYPE any.


    me->get_entity_keys(
  EXPORTING
    pfd_i_entity     = pfd_i_entiy_name
    pfd_i_value      = pfd_i_key
  IMPORTING
    prd_e_entity_key = DATA(lrd_key_struct)
).

*    ASSIGN COMPONENT pfd_i_entiy_name OF STRUCTURE <ls_any_ref> TO <lv_any>.
*    IF sy-subrc = 0.
*      <lv_any> = pfd_i_key.
*    ENDIF.

    pwa_c_assignment_key-parent-entity = pfd_i_entiy_name.
    pwa_c_assignment_key-parent-key = lrd_key_struct.
  ENDMETHOD.


  METHOD fill_previous_node.
    FIELD-SYMBOLS:
      <ls_any_ref> TYPE any,
      <lv_any>     TYPE any.

*    ri_conv_api->get_entity_structure(
*      EXPORTING
*        iv_entity_name = pfd_i_entiy_name
*      IMPORTING
*        er_structure   = rd_entity_key ).
*
*    ASSIGN rd_entity_key->* TO <ls_any_ref>.
*
*    ASSIGN COMPONENT pfd_i_entiy_name OF STRUCTURE <ls_any_ref> TO <lv_any>.
*    IF sy-subrc = 0.
*      <lv_any> = pfd_i_key.
*    ENDIF.

    me->get_entity_keys(
  EXPORTING
    pfd_i_entity     = pfd_i_entiy_name
    pfd_i_value      = pfd_i_key
  IMPORTING
    prd_e_entity_key = DATA(lrd_key_struct)
).

    pwa_c_assignment-predec-entity = pfd_i_entiy_name.
    pwa_c_assignment-predec-key = rd_entity_key.
  ENDMETHOD.


  METHOD fill_top_hry.
    FIELD-SYMBOLS:
      <ls_any_ref> TYPE any,
      <lv_any>     TYPE any.

    ri_conv_api->get_entity_structure(
      EXPORTING
        iv_entity_name = pfd_i_entiy_name
      IMPORTING
        er_structure   = rd_entity_key ).

    ASSIGN rd_entity_key->* TO <ls_any_ref>.

*   if <ls_any_ref> is assigned.
    me->get_entity_keys(
      EXPORTING
        pfd_i_entity     = pfd_i_entiy_name
        pfd_i_value      = pfd_i_key
      IMPORTING
        prd_e_entity_key = DATA(lrd_key_struct)
    ).

*    ASSIGN COMPONENT pfd_i_entiy_name OF STRUCTURE <ls_any_ref> TO <lv_any>.
*    IF sy-subrc = 0.
*      <lv_any> = pfd_i_key.
*    ENDIF.

    pwa_c_assignment_key-hryname-entity = pfd_i_entiy_name.
    pwa_c_assignment_key-hryname-key = lrd_key_struct.
*    ENDIF.
  ENDMETHOD.


  METHOD get_entity_keys.
    DATA: lv_fieldname TYPE usmd_fieldname.
    DATA: lv_fieldname2 TYPE fieldname.
    DATA: lrd_key_struct TYPE REF TO data.


    FIELD-SYMBOLS: <lwa_key> TYPE any.
    CLEAR: prd_e_entity_key.
    lv_fieldname2 = pfd_i_entity.
    lv_fieldname = pfd_i_entity.

    cl_usmd_crequest_util=>get_edition_from_cr(
      EXPORTING
        i_crequest       = fd_cr_number                 " Change Request
*        io_model         =                  " Can model also be spaces?
      IMPORTING
        e_edition        = DATA(lfd_edition)                 " Edition
*        e_edition_number =                  " Edition Number
    ).

    IF pfd_i_value IS SUPPLIED.
      ri_model_ext->read_entity_data_all(
        EXPORTING
          i_fieldname      = lv_fieldname                 " Financial MDM: Field Name
          if_active        = abap_true                 " Financial MDM: General Indicator
*          i_crequest       =                  " Change Request
          it_sel           = VALUE usmd_ts_sel( ( fieldname = lv_fieldname2 sign = 'I' option = 'EQ' low = pfd_i_value )
                                                 ( fieldname = 'USMD_EDITION' sign = 'I' option = 'EQ' low = lfd_edition ) )                 " Sorted Table: Selection Condition (Range per Field)
*          it_entity_filter =                  " Ent.Types for Which Data Is Expected; Default: All Ent.Types
        IMPORTING
          et_message       = DATA(lit_messages)                 " Messages
          et_data_entity   = DATA(lt_data_entity)                 " Data for Entity Types
      ).
    ELSE.
      ri_model_ext->read_entity_data_all(
        EXPORTING
          i_fieldname    = lv_fieldname
          if_active      = abap_false
          i_crequest     = fd_cr_number
        IMPORTING
          et_data_entity = lt_data_entity ).

    ENDIF.
    IF lt_data_entity IS NOT INITIAL.
      READ TABLE lt_data_entity WITH KEY usmd_entity = pfd_i_entity struct = if_usmd_model_ext=>gc_struct_key_attr
        INTO DATA(ls_data_entity).
      ri_conv_api->get_entity_structure(
      EXPORTING
        iv_entity_name = pfd_i_entity
      IMPORTING
        er_structure   = lrd_key_struct ).

      ASSIGN ls_data_entity-r_t_data->* TO FIELD-SYMBOL(<lt_data>).
      ASSIGN lrd_key_struct->* TO <lwa_key>.
      LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
        IF sy-subrc = 0 AND <ls_data> IS ASSIGNED.
          CREATE DATA prd_e_entity_key LIKE <lwa_key>.
          ASSIGN prd_e_entity_key->* TO <lwa_key>. "FIELD-SYMBOL(<ls_exporting>).
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <ls_data> TO <lwa_key>.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF go_instance IS NOT INITIAL.
      prc_r_object = go_instance.
      RETURN.
    ENDIF.

    CREATE OBJECT go_instance
      EXPORTING
        pfd_i_model     = pfd_i_model
        pfd_i_cr_number = pfd_i_cr_number
        pfd_i_with_badi = pfd_i_with_badi.

        prc_r_object    = go_instance.
  ENDMETHOD.


  METHOD initialize_badi.
    TRY.
        GET BADI mo_badi_handler
          FILTERS
            model  = mv_model
            entity = iv_entity.
      CATCH cx_badi.
*   Handle BAdI initialization error if needed
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
