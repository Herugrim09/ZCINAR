class ZCL_USMD_CR_TYPE_EDITION_POPUP definition
  public
  inheriting from CL_USMD_CR_TYPE_EDITION_POPUP
  final
  create public .

public section.

  methods IF_FPM_GUIBB_FORM~GET_DEFINITION
    redefinition .
  methods IF_FPM_GUIBB_OVS~HANDLE_PHASE_2
    redefinition .
protected section.

  types:
    BEGIN OF ty_s_edition,
           edition TYPE usmd_edition,
         END OF ty_s_edition .
  types:
    ty_t_edition TYPE SORTED TABLE OF ty_s_edition WITH UNIQUE KEY edition .

  data MT_FILTERED_EDITIONS type TY_T_EDITION .
  data MV_CR_TYPE type USMD_CREQUEST_TYPE .

  methods GET_EDITIONS .

  methods OVS_HANDLE_PHASE_2
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_USMD_CR_TYPE_EDITION_POPUP IMPLEMENTATION.


  METHOD get_editions.
    CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '000C292C36471FE0A9BA6425DA509149'.
    DATA:lv_timestamp    TYPE timestamp,
         lt_name_value   TYPE abap_parmbind_tab,
         ls_name_value   TYPE abap_parmbind,
         lr_data         TYPE REF TO data,
         lx_fdt          TYPE REF TO cx_fdt,
         la_zmdg_cr_type TYPE if_fdt_types=>element_text,
         la_is_active    TYPE if_fdt_types=>element_boolean,
         lv_cr_type      TYPE usmd_crequest_type.
    FIELD-SYMBOLS <la_any> TYPE any.
****************************************************************************************************
* All method calls within one processing cycle calling the same function must use the same timestamp.
* For subsequent calls of the same function, we recommend to use the same timestamp for all calls.
* This is to improve the system performance.
****************************************************************************************************
* If you are using structures or tables without DDIC binding, you have to declare the respective types
* by yourself. Insert the according data type at the respective source code line.
****************************************************************************************************
    GET TIME STAMP FIELD lv_timestamp.
****************************************************************************************************
* Process a function without recording trace data, passing context data objects via a name/value table.
****************************************************************************************************
* Prepare function processing:
****************************************************************************************************
    ls_name_value-name = 'ZMDG_CR_TYPE'.
    ASSIGN me->mr_data->* TO FIELD-SYMBOL(<ls_my_data>).
    IF <ls_my_data> IS ASSIGNED.
      ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_my_data> TO FIELD-SYMBOL(<lv_cr_type>).
      IF me->mv_cr_type = <lv_cr_type>.
        RETURN.
      ELSE.
        CLEAR: mt_filtered_editions.
      ENDIF.
      me->mv_cr_type = <lv_cr_type>.
    ENDIF.
    la_ZMDG_CR_TYPE = <lv_cr_type>.
    GET REFERENCE OF la_ZMDG_CR_TYPE INTO lr_data.
    ls_name_value-value = lr_data.
    INSERT ls_name_value INTO TABLE lt_name_value.
    CLEAR ls_name_value.
****************************************************************************************************
    ls_name_value-name = 'IS_ACTIVE'.
    la_IS_ACTIVE = abap_true.
    GET REFERENCE OF la_IS_ACTIVE INTO lr_data.
    ls_name_value-value = lr_data.
    INSERT ls_name_value INTO TABLE lt_name_value.
    CLEAR ls_name_value.
****************************************************************************************************
* Create the data to store the result value after processing the function
* You can skip the following call, if you already have
* a variable for the result. Please replace also the parameter
* EA_RESULT in the method call CL_FDT_FUNCTION_PROCESS=>PROCESS
* with the desired variable.
****************************************************************************************************
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_function_id
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = lv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lr_data ).
    ASSIGN lr_data->* TO <la_any>.
    TRY.
        cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_function_id
                                                    iv_timestamp   = lv_timestamp
                                          IMPORTING ea_result      = <la_any>
                                          CHANGING  ct_name_value  = lt_name_value ).
      CATCH cx_fdt INTO lx_fdt.
****************************************************************************************************
* You can check CX_FDT->MT_MESSAGE for error handling.
****************************************************************************************************
    ENDTRY.
    IF <la_any> IS NOT INITIAL.
      mt_filtered_editions = CORRESPONDING #( <la_any> ).
    ENDIF.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~get_definition.
    CALL METHOD super->if_fpm_guibb_form~get_definition
      IMPORTING
        eo_field_catalog         = eo_field_catalog
        et_field_description     = et_field_description
        et_action_definition     = et_action_definition
        et_special_groups        = et_special_groups
        et_dnd_definition        = et_dnd_definition
        es_options               = es_options
        es_message               = es_message
        ev_additional_error_info = ev_additional_error_info.
    READ TABLE et_field_description ASSIGNING FIELD-SYMBOL(<ls_field_description>) WITH TABLE KEY name = 'EDITION'.
    IF  <ls_field_description> IS ASSIGNED.
      <ls_field_description>-ovs_name = 'ZCL_USMD_CR_TYPE_EDITION_POPUP'.
    ENDIF.
*    READ TABLE et_field_description ASSIGNING <ls_field_description> WITH TABLE KEY name = 'TYPE'.
*    IF  <ls_field_description> IS ASSIGNED.
*      <ls_field_description>-ovs_name = 'ZCL_USMD_CR_TYPE_EDITION_POPUP'.
*    ENDIF.
  ENDMETHOD.


  METHOD if_fpm_guibb_ovs~handle_phase_2.
    CALL METHOD super->if_fpm_guibb_ovs~handle_phase_2
      EXPORTING
        iv_field_name   = iv_field_name
        io_ovs_callback = io_ovs_callback
        iv_index        = iv_index.
  ENDMETHOD.


  METHOD ovs_handle_phase_2.
    DATA: ls_pair              TYPE crmt_text_value_pair,
          lt_filtered_editions TYPE me->ty_t_edition,
          lt_editions          TYPE ty_ts_ovs_out_edition,
          lv_index             TYPE i VALUE 1.

    CASE iv_field_name.
      WHEN  'TYPE'.
        CREATE DATA er_output TYPE crmt_text_value_pair_tab.
        ASSIGN er_output->* TO FIELD-SYMBOL(<lt_cr_types>).

        <lt_cr_types> = VALUE crmt_text_value_pair_tab( FOR <ls_cr_type> IN me->mt_cr_types
                                     ( key = <ls_cr_type>-type text = <ls_cr_type>-type_descr ) ).
      WHEN 'EDITION'.
        CALL METHOD super->ovs_handle_phase_2
          EXPORTING
            iv_field_name      = iv_field_name
            ir_query_parameter = ir_query_parameter
            io_access          = io_access
          IMPORTING
            er_output          = er_output
            ev_table_header    = ev_table_header
            et_column_texts    = et_column_texts
            et_messages        = et_messages.

        me->get_editions( ).
        IF me->mt_filtered_editions IS NOT INITIAL.
          lt_filtered_editions = me->mt_filtered_editions.
          ASSIGN er_output->* TO FIELD-SYMBOL(<lt_output>).
          IF <lt_output> IS ASSIGNED.
            lt_editions = CORRESPONDING #( <lt_output> ).
            LOOP AT lt_editions ASSIGNING FIELD-SYMBOL(<ls_output>).
              IF <ls_output> IS ASSIGNED.
                READ TABLE lt_filtered_editions TRANSPORTING NO FIELDS WITH TABLE KEY edition = <ls_output>-edition.
                IF sy-subrc <> 0.
                  DELETE lt_editions INDEX lv_index.
                ELSE.
                  lv_index = lv_index + 1.
                ENDIF.
              ENDIF.
            ENDLOOP.
            <lt_output> = CORRESPONDING #( lt_editions ).
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
