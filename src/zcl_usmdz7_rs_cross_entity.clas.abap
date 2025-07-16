class ZCL_USMDZ7_RS_CROSS_ENTITY definition
  public
  inheriting from CL_USMDZ7_RS_CROSS_ENTITY
  final
  create public .

public section.

  methods CINARS_METHOD
    importing
      !IO_MODEL type ref to IF_USMD_MODEL_EXT
      !IO_CHANGED_DATA type ref to IF_USMD_DELTA_BUFFER_READ
      !IO_WRITE_DATA type ref to IF_USMD_DELTA_BUFFER_WRITE
    exporting
      !ET_MESSAGE_INFO type USMD_T_MESSAGE .
  methods DER_COST_CENTER_PERSON_RESP
    importing
      !IO_MODEL type ref to IF_USMD_MODEL_EXT
      !IO_CHANGED_DATA type ref to IF_USMD_DELTA_BUFFER_READ
      !IO_WRITE_DATA type ref to IF_USMD_DELTA_BUFFER_WRITE
    exporting
      !ET_MESSAGE_INFO type USMD_T_MESSAGE .
  methods DER_COST_CENTER_CATEGORY
    importing
      !IO_MODEL type ref to IF_USMD_MODEL_EXT
      !IO_CHANGED_DATA type ref to IF_USMD_DELTA_BUFFER_READ
      !IO_WRITE_DATA type ref to IF_USMD_DELTA_BUFFER_WRITE
    exporting
      !ET_MESSAGE_INFO type USMD_T_MESSAGE .

  methods IF_EX_USMD_RULE_SERVICE2~DERIVE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_USMDZ7_RS_CROSS_ENTITY IMPLEMENTATION.


  METHOD if_ex_usmd_rule_service2~derive.

    CALL METHOD super->if_ex_usmd_rule_service2~derive
      EXPORTING
        io_model        = io_model
        io_changed_data = io_changed_data
        io_write_data   = io_write_data
      IMPORTING
        et_message_info = et_message_info.

    CASE sy-uname.
      WHEN 'ACILEK'.
        me->cinars_method(
          EXPORTING
            io_model        = io_model                         " MDG Data Model for Access from Non-SAP Standard Code
            io_changed_data = io_changed_data                 " Read-Interface to Data Buffer
            io_write_data   = io_write_data                   " Write Interface to Data Buffer
          IMPORTING
            et_message_info = et_message_info                " Messages
        ).
      WHEN 'BGOZUMOGULLA'.
        me->der_cost_center_category(
          EXPORTING
            io_model        = io_model                        " MDG Data Model for Access from Non-SAP Standard Code
            io_changed_data = io_changed_data                 " Read-Interface to Data Buffer
            io_write_data   = io_write_data                   " Write Interface to Data Buffer
          IMPORTING
            et_message_info = et_message_info                 " Messages
        ).
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.


  METHOD cinars_method.
    DATA: lri_context TYPE REF TO if_usmd_app_context.
    DATA: lwa_message TYPE usmd_s_message.
    lri_context = cl_usmd_app_context=>get_context( ).
*    CALL METHOD super->if_ex_usmd_rule_service2~derive
*      EXPORTING
*        io_model        = io_model
*        io_changed_data = io_changed_data
*        io_write_data   = io_write_data
*      IMPORTING
*        et_message_info = et_message_info.


    io_changed_data->read_data(
      EXPORTING
        i_entity      = if_usmdz_cons_entitytypes=>gc_entity_account                                      " Entity Type
        i_struct      = if_usmd_model_ext=>gc_struct_key_attr " Type of Data Structure
      IMPORTING
        er_t_data_ins = DATA(lrd_data_tab_ins)                                      " Newly Added Data Records
        er_t_data_upd = DATA(lrd_data_tab_upd)                                     " Data Records Changed
*       er_t_data_del =                                       " Data Records Deleted
        er_t_data_mod = DATA(lrd_data_tab_mod)                                      " "Modified" Data Records (INSERT+UPDATE)
*       ef_t_data_upd_complete =                                       " Unchanged Attributes Are Also Filled
    ).

    io_model->get_cr_objectlist(
      EXPORTING
        i_crequest = lri_context->mv_crequest_id                 " Change Request
      IMPORTING
*       e_count    =                  " Total Number of Objects in Change Request
        et_key_all = DATA(lt_objects)                 " Keys of Objects in Change Request
*       et_hry_key_all =                  " Hierarchy node keys
    ).
    DATA:
      lo_fpm       TYPE REF TO if_fpm,
      lo_fpm_event TYPE REF TO cl_fpm_event,
      lt_messages  TYPE bapiret2_t,
      ls_message   TYPE bapiret2.

    " Get FPM instance
    lo_fpm = cl_fpm=>get_instance( ).

    " Prepare your message
    ls_message-type        = 'I'.        " S-Success, E-Error, W-Warning, I-Info
    ls_message-id          = 'ZMDG_DQM_MESSAGES'.     " Your message class
    ls_message-number      = '001'.      " Your message number
    " Either set individual parameters or use the message field
    APPEND ls_message TO lt_messages.

    " Create message event
    CREATE OBJECT lo_fpm_event
      EXPORTING
        iv_event_id = cl_fpm_event=>gc_event_message_navigate.

    " Add messages to the event data
    lo_fpm_event->mo_event_data->set_value(
      EXPORTING
        iv_key   = 'MESSAGES'  " Standard key for messages
        iv_value = lt_messages
    ).

    " Raise the event
    lo_fpm->raise_event( lo_fpm_event ).

    io_model->read_entity_data_all(
      EXPORTING
        i_fieldname    = 'ACCOUNT'                 " Financial MDM: Field Name
        if_active      = abap_true                 " Financial MDM: General Indicator
*       i_crequest     = lri_context->mv_crequest_id                 " Change Request
        it_sel         = VALUE usmd_ts_sel( ( fieldname = 'ACCOUNT'      sign = 'I' option = 'BT' low = '0000000000' high = '9999999999' )
                                            ( fieldname = 'USMD_EDITION' sign = 'I' option = 'EQ' low = 'DUMMY' ) )                   " Sorted Table: Selection Condition (Range per Field)
*       it_entity_filter =                  " Ent.Types for Which Data Is Expected; Default: All Ent.Types
      IMPORTING
*       et_message     =                  " Messages
        et_data_entity = DATA(lrd_data_entity)                 " Data for Entity Types
    ).

    IF lri_context->mv_crequest_step = '90'.

      IF lri_context->mv_crequest_type = 'ZACCAP1'.
        lwa_message-msgid = 'ZMDG_DQM_MESSAGES'.
        lwa_message-msgno = '002'.
        lwa_message-msgty = 'I'.
        "APPEND lwa_message TO et_message_info.
      ENDIF.

      DATA(ls_fpm_message) = cl_usmd_mc_assist_ui=>map_message_to_fpm( is_usmd_message = lwa_message ).
      lo_fpm->mo_message_manager->report_t100_message( iv_msgid       = ls_fpm_message-msgid
                                                       iv_msgno       = ls_fpm_message-msgno
                                                       iv_severity    = ls_fpm_message-severity
                                                       iv_parameter_1 = ls_fpm_message-parameter_1
                                                       iv_parameter_2 = ls_fpm_message-parameter_2
                                                       iv_parameter_3 = ls_fpm_message-parameter_3
                                                       iv_parameter_4 = ls_fpm_message-parameter_4 ).

    ENDIF.
    "IF (cl_usmd_conv_som_gov_api=>check_instance_exists( '0G' ) = abap_true.
    IF NOT lri_context->mv_crequest_type = 'ZCCT2P1'.
      zcl_p40_mdg_0g_assign_hry=>get_instance(
        EXPORTING
          pfd_i_model     = '0G'
          pfd_i_cr_number = lri_context->mv_crequest_id
        RECEIVING
          prc_r_object    = DATA(lrc_hierarch)
      ).

      lrc_hierarch->check_these_fields( pit_i_fields = VALUE #( ( 'COAREA' ) ( 'CCTR' ) ( 'CCTRCGY' ) ( 'CCODECCTR' ) ( 'CURRCCTR' ) ) ).

      lrc_hierarch->assign_hierarchy(
        EXPORTING
*         pfd_i_hierarchy     = 'CCTRH'
*         pfd_i_hierarchy_key = 'CCGH'
          pfd_i_entity = 'CCTR'
*         pfd_i_parent_entity = 'CCTRG'
*         pfd_i_parent_key    = 'CCG'
*         pfd_i_previous      =
*         pfd_i_previous_key  =
*      IMPORTING
*         pfd_e_derived       =
*         pit_e_message_info  =
      ).
      " ENDIF.
    ENDIF.


*    IF sy-uname = 'BGOZUMOGULLA'.
*      FIELD-SYMBOLS: <lt_data_ent>     TYPE ANY TABLE,
*                     <lt_derived_data> TYPE ANY TABLE.
*      DATA: lv_germany_existis TYPE boolean,
*            lr_entity_text_str TYPE REF TO data.
*
*      IF lrd_data_entity IS NOT INITIAL.
*        READ TABLE lrd_data_entity WITH KEY struct = 'KLTXT' INTO DATA(ls_text).
*        IF sy-subrc IS INITIAL.
*          ASSIGN ls_text-r_t_data->* TO <lt_data_ent>.
*          LOOP AT <lt_data_ent> ASSIGNING FIELD-SYMBOL(<ls_data_ent>).
*            ASSIGN COMPONENT 'LANGU' OF STRUCTURE <ls_data_ent> TO FIELD-SYMBOL(<lv_langu>).
*
*            IF <lv_langu> = 'D'.
*              lv_germany_existis = abap_true.
*              EXIT.
*            ENDIF.
*          ENDLOOP.
*
*          IF lv_germany_existis = abap_false.
*
*            CALL METHOD io_write_data->create_data_reference
*              EXPORTING
*                i_entity = 'ACCOUNT'
*                i_struct = 'KLTXT'
*              RECEIVING
*                er_data  = DATA(lr_entity_text_tab).
*
*            ASSIGN lr_entity_text_tab->* TO <lt_derived_data>.
*            CREATE DATA lr_entity_text_str LIKE LINE OF <lt_derived_data>.
*            ASSIGN lr_entity_text_str->* TO FIELD-SYMBOL(<ls_entity_key_attr>).
*
*            CHECK <ls_entity_key_attr> IS ASSIGNED AND <ls_data_ent> IS NOT INITIAL AND <ls_data_ent> IS ASSIGNED.
*
*            MOVE-CORRESPONDING <ls_data_ent> TO <ls_entity_key_attr>.
*            ASSIGN COMPONENT 'LANGU' OF STRUCTURE <ls_entity_key_attr> TO <lv_langu>.
*            <lv_langu> = 'D'.
*            INSERT <ls_entity_key_attr> INTO TABLE <lt_derived_data>.
*
*            TRY.
*                CALL METHOD io_write_data->write_data
*                  EXPORTING
*                    i_entity = 'ACCOUNT'
*                    it_data  = <lt_derived_data>.
*              CATCH cx_usmd_write_error .
*                cl_abap_unit_assert=>abort( msg  = 'Error while writing data for Entity'
*                                            quit = if_aunit_constants=>method ).
*            ENDTRY.
*
*
*          ENDIF.
*
*        ENDIF.
*
*
*      ENDIF.
*
*
*    ENDIF.
  ENDMETHOD.


  METHOD der_cost_center_category.

    DATA:
      lv_cctr             TYPE usmdz1_cctr,
      lv_coarea           TYPE kokrs,
      lv_edition          TYPE usmd_edition,
      lv_func_area        TYPE fkber,
      ls_key              TYPE usmd_s_value,
      lt_key              TYPE usmd_ts_value,
      lv_attribute        TYPE usmd_fieldname,
      lt_attribute        TYPE usmd_ts_fieldname,
      lx_usmd_write_error TYPE REF TO cx_usmd_write_error.

    FIELD-SYMBOLS:
      <lt_mod_data> TYPE ANY TABLE,
      <lt_cctr>     TYPE ANY TABLE.

    " Get changed Cost Center data
    io_changed_data->read_data(
      EXPORTING
        i_entity      = if_usmdz_cons_entitytypes=>gc_entity_cctr                                     " Entity Type
        i_struct      = if_usmd_model_ext=>gc_struct_key_attr " Type of Data Structure
      IMPORTING
        er_t_data_mod = DATA(lr_t_data_mod)             " "Modified" Data Records (INSERT+UPDATE)
    ).

    " If there is no modification in Cost Center entity then return
    IF lr_t_data_mod IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN lr_t_data_mod->* TO <lt_mod_data>.
    IF <lt_mod_data> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    " Loop over the modified data and maintain necessary informations
    LOOP AT <lt_mod_data> ASSIGNING FIELD-SYMBOL(<ls_mod_data>).

*      " If Cost Center Category is already filled by key user than don't overwrite it, return
*      ASSIGN COMPONENT 'CCTRCGY' OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_mod_cctrcgy>).
*      IF      <lv_mod_cctrcgy> IS ASSIGNED
*          AND <lv_mod_cctrcgy> IS NOT INITIAL.
*        RETURN.
*      ENDIF.

      " Maintain key attributes
      ASSIGN COMPONENT 'CCTR' OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_cctr_key>).
      IF      <lv_cctr_key> IS ASSIGNED
          AND <lv_cctr_key> IS NOT INITIAL.
        lv_cctr = <lv_cctr_key>.
      ENDIF.

      ASSIGN COMPONENT 'COAREA' OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_coarea_key>).
      IF      <lv_coarea_key> IS ASSIGNED
          AND <lv_coarea_key> IS NOT INITIAL.
        lv_coarea = <lv_coarea_key>.
      ENDIF.

      " Get edition information for further use
      ASSIGN COMPONENT usmd0_cs_fld-edition OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_edition>).
      IF      <lv_edition> IS ASSIGNED
          AND <lv_edition> IS NOT INITIAL.
        lv_edition = <lv_edition>.
      ENDIF.

      " Get updated fields information
      ASSIGN COMPONENT 'USMDX_S_UPDATE' OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<ls_updated_fields>).
      IF <ls_updated_fields> IS ASSIGNED.

        " If the Functional Area is updated then maintain this information, else return
        ASSIGN COMPONENT 'FUNCCCTR' OF STRUCTURE <ls_updated_fields> TO FIELD-SYMBOL(<lv_is_funccctr_updated>).
        IF      <lv_is_funccctr_updated> IS ASSIGNED
            AND <lv_is_funccctr_updated> IS NOT INITIAL.
          ASSIGN COMPONENT 'FUNCCCTR' OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_mod_funccctr>).
          IF <lv_mod_funccctr> IS ASSIGNED.
            lv_func_area = <lv_mod_funccctr>.
          ENDIF.
        ELSE.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " For those scenarios where Functional Area is cleared
    IF lv_func_area IS INITIAL.
      RETURN.
    ENDIF.

    " Get the regarding Cost Center Category of Functional Area
    SELECT SINGLE kosar FROM tka05
      WHERE func_area = @lv_func_area
      INTO @DATA(lv_cost_center_cat).

    " If there is a related Cost Center Category exists, continue to derivation
    IF sy-subrc EQ 0 AND lv_cost_center_cat IS NOT INITIAL.
      TRY.
*          DATA(test) = 'TEST'.
          DATA(lrt_cctr) = io_write_data->create_data_reference(
            EXPORTING
*             i_entity = CONV #( test )                " Entity Type
              i_entity = if_usmdz_cons_entitytypes=>gc_entity_cctr                 " Entity Type
              i_struct = io_model->gc_struct_key_attr                 " Type of Data Structure
          ).
        CATCH cx_usmd_write_error INTO lx_usmd_write_error. " Error while writing to buffer
          APPEND LINES OF lx_usmd_write_error->dt_message TO et_message_info.
          RETURN.
      ENDTRY.

      IF lrt_cctr IS BOUND.
        ASSIGN lrt_cctr->* TO <lt_cctr>.
      ENDIF.

      " Fill the reference data with current values
      IF <lt_cctr> IS ASSIGNED.
        MOVE-CORRESPONDING <lt_mod_data> TO <lt_cctr>.
        IF <lt_cctr> IS NOT INITIAL.
          LOOP AT <lt_cctr> ASSIGNING FIELD-SYMBOL(<ls_cctr>).
            ASSIGN COMPONENT 'CCTRCGY' OF STRUCTURE <ls_cctr> TO FIELD-SYMBOL(<lv_cctrcgy>).

            " Fill the reference data with the query result (Cost Center Category)
            IF <lv_cctrcgy> IS ASSIGNED.
              <lv_cctrcgy> = lv_cost_center_cat.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      " Do NOT contine if the related field is not maintained correctly
      IF      <lv_cctrcgy> IS NOT ASSIGNED
          AND <lv_cctrcgy> IS INITIAL.
        RETURN.
      ENDIF.

      " Fill the attribute table that corresponds which fields will be affected for derivation
      lv_attribute = if_usmdz_cons_attributes=>gc_attr_cctrcgy.
      INSERT lv_attribute INTO TABLE lt_attribute.

      " Fill the key values of entity & edition
      IF lv_cctr IS NOT INITIAL.
        CLEAR ls_key.
        ls_key-fieldname = 'CCTR'.
        ls_key-value = lv_cctr.
        INSERT ls_key INTO TABLE lt_key.
      ENDIF.
      IF lv_coarea IS NOT INITIAL.
        CLEAR ls_key.
        ls_key-fieldname = 'COAREA'.
        ls_key-value = lv_coarea.
        INSERT ls_key INTO TABLE lt_key.
      ENDIF.
      IF lv_edition IS NOT INITIAL.
        CLEAR ls_key.
        ls_key-fieldname = usmd0_cs_fld-edition.
        ls_key-value = lv_edition.
        INSERT ls_key INTO TABLE lt_key.
      ENDIF.

      " Do the derivation with updated Cost Center Category
      TRY.
          io_write_data->write_data(
            i_entity     = if_usmdz_cons_entitytypes=>gc_entity_cctr                 " Entity Type
            it_key       = lt_key       " Fixings
            it_attribute = lt_attribute                 " Financials MDM: Field Name
            it_data      = <lt_cctr>
          ).
        CATCH cx_usmd_write_error INTO lx_usmd_write_error. " Error while writing to buffer
          APPEND LINES OF lx_usmd_write_error->dt_message TO et_message_info.
          RETURN.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD DER_COST_CENTER_PERSON_RESP.

    DATA:
      lv_cctr             TYPE usmdz1_cctr,
      lv_coarea           TYPE kokrs,
      lv_edition          TYPE usmd_edition,
      lv_func_area        TYPE fkber,
      ls_key              TYPE usmd_s_value,
      lt_key              TYPE usmd_ts_value,
      lv_attribute        TYPE usmd_fieldname,
      lt_attribute        TYPE usmd_ts_fieldname,
      lx_usmd_write_error TYPE REF TO cx_usmd_write_error.

    FIELD-SYMBOLS:
      <lt_mod_data> TYPE ANY TABLE,
      <lt_cctr>     TYPE ANY TABLE.

    " Get changed Cost Center data
    io_changed_data->read_data(
      EXPORTING
        i_entity      = if_usmdz_cons_entitytypes=>gc_entity_cctr                                     " Entity Type
        i_struct      = if_usmd_model_ext=>gc_struct_key_attr " Type of Data Structure
      IMPORTING
        er_t_data_mod = DATA(lr_t_data_mod)             " "Modified" Data Records (INSERT+UPDATE)
    ).

    " If there is no modification in Cost Center entity then return
    IF lr_t_data_mod IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN lr_t_data_mod->* TO <lt_mod_data>.
    IF <lt_mod_data> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    " Loop over the modified data and maintain necessary informations
    LOOP AT <lt_mod_data> ASSIGNING FIELD-SYMBOL(<ls_mod_data>).

      " Maintain key attributes
      ASSIGN COMPONENT 'CCTR' OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_cctr_key>).
      IF      <lv_cctr_key> IS ASSIGNED
          AND <lv_cctr_key> IS NOT INITIAL.
        lv_cctr = <lv_cctr_key>.
      ENDIF.

      ASSIGN COMPONENT 'COAREA' OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_coarea_key>).
      IF      <lv_coarea_key> IS ASSIGNED
          AND <lv_coarea_key> IS NOT INITIAL.
        lv_coarea = <lv_coarea_key>.
      ENDIF.

      " Get edition information for further use
      ASSIGN COMPONENT usmd0_cs_fld-edition OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_edition>).
      IF      <lv_edition> IS ASSIGNED
          AND <lv_edition> IS NOT INITIAL.
        lv_edition = <lv_edition>.
      ENDIF.

      ASSIGN COMPONENT 'CCTRRESPP' OF STRUCTURE <ls_mod_data> TO FIELD-SYMBOL(<lv_cctrrespp>).

    ENDLOOP.

    " Get the regarding Cost Center Category of Functional Area
    SELECT SINGLE kosar FROM tka05
      WHERE func_area = @lv_func_area
      INTO @DATA(lv_cost_center_cat).

    " If there is a related Cost Center Category exists, continue to derivation
    IF sy-subrc EQ 0 AND lv_cost_center_cat IS NOT INITIAL.
      TRY.
*          DATA(test) = 'TEST'.
          DATA(lrt_cctr) = io_write_data->create_data_reference(
            EXPORTING
*             i_entity = CONV #( test )                " Entity Type
              i_entity = if_usmdz_cons_entitytypes=>gc_entity_cctr                 " Entity Type
              i_struct = io_model->gc_struct_key_attr                 " Type of Data Structure
          ).
        CATCH cx_usmd_write_error INTO lx_usmd_write_error. " Error while writing to buffer
          APPEND LINES OF lx_usmd_write_error->dt_message TO et_message_info.
          RETURN.
      ENDTRY.

      IF lrt_cctr IS BOUND.
        ASSIGN lrt_cctr->* TO <lt_cctr>.
      ENDIF.

      " Fill the reference data with current values
      IF <lt_cctr> IS ASSIGNED.
        MOVE-CORRESPONDING <lt_mod_data> TO <lt_cctr>.
        IF <lt_cctr> IS NOT INITIAL.
          LOOP AT <lt_cctr> ASSIGNING FIELD-SYMBOL(<ls_cctr>).
            ASSIGN COMPONENT 'CCTRCGY' OF STRUCTURE <ls_cctr> TO FIELD-SYMBOL(<lv_cctrcgy>).

            " Fill the reference data with the query result (Cost Center Category)
            IF <lv_cctrcgy> IS ASSIGNED.
              <lv_cctrcgy> = lv_cost_center_cat.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      " Do NOT contine if the related field is not maintained correctly
      IF      <lv_cctrcgy> IS NOT ASSIGNED
          AND <lv_cctrcgy> IS INITIAL.
        RETURN.
      ENDIF.

      " Fill the attribute table that corresponds which fields will be affected for derivation
      lv_attribute = if_usmdz_cons_attributes=>gc_attr_cctrcgy.
      INSERT lv_attribute INTO TABLE lt_attribute.

      " Fill the key values of entity & edition
      IF lv_cctr IS NOT INITIAL.
        CLEAR ls_key.
        ls_key-fieldname = 'CCTR'.
        ls_key-value = lv_cctr.
        INSERT ls_key INTO TABLE lt_key.
      ENDIF.
      IF lv_coarea IS NOT INITIAL.
        CLEAR ls_key.
        ls_key-fieldname = 'COAREA'.
        ls_key-value = lv_coarea.
        INSERT ls_key INTO TABLE lt_key.
      ENDIF.
      IF lv_edition IS NOT INITIAL.
        CLEAR ls_key.
        ls_key-fieldname = usmd0_cs_fld-edition.
        ls_key-value = lv_edition.
        INSERT ls_key INTO TABLE lt_key.
      ENDIF.

      " Do the derivation with updated Cost Center Category
      TRY.
          io_write_data->write_data(
            i_entity     = if_usmdz_cons_entitytypes=>gc_entity_cctr                 " Entity Type
            it_key       = lt_key       " Fixings
            it_attribute = lt_attribute                 " Financials MDM: Field Name
            it_data      = <lt_cctr>
          ).
        CATCH cx_usmd_write_error INTO lx_usmd_write_error. " Error while writing to buffer
          APPEND LINES OF lx_usmd_write_error->dt_message TO et_message_info.
          RETURN.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
