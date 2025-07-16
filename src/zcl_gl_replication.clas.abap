CLASS zcl_gl_replication DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_usmd_glam_rplctn .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA it_mapping TYPE mdg_t_add_matching_easy_bs .
    DATA wa_mapping TYPE mdg_s_add_matching_easy_bs .
    DATA ri_model TYPE REF TO if_mdg_id_matching_api_bs .
    CONSTANTS c_sysid TYPE sld_bskey VALUE 'MESCLNT100' ##NO_TEXT.
    CONSTANTS c_ids_type_907 TYPE mdg_ids_type_code_bs VALUE '907' ##NO_TEXT.
    CONSTANTS c_obj_type_892 TYPE mdg_object_type_code_bs VALUE '892' ##NO_TEXT.
    CONSTANTS c_ids_type_909 TYPE mdg_ids_type_code_bs VALUE '909' ##NO_TEXT.
    CONSTANTS c_obj_type_899 TYPE mdg_object_type_code_bs VALUE '899' ##NO_TEXT.

    METHODS read_key_mapping
      IMPORTING
        !pfd_i_object_type        TYPE mdg_object_type_code_bs
        !pfd_i_ids_type           TYPE mdg_ids_type_code_bs
        !pfd_i_system_id          TYPE sld_bskey DEFAULT 'MERCLNT100'
        !pfd_i_id_value           TYPE mdg_object_id_bs
      EXPORTING
        !pit_e_matching_objects   TYPE mdg_t_matching_obj_data_bs
        !pfd_e_value              TYPE mdg_object_id_bs
        !pfd_e_does_mapping_exist TYPE abap_boolean .
    METHODS update_source_obj
      IMPORTING
        !pfd_i_obj_type   TYPE mdg_object_type_code_bs
        !pwa_i_identifier TYPE mdg_t_identifier_bs
        !pfd_i_group_ref  TYPE boole_d
        !pfd_i_system_ref TYPE boole_d .
    METHODS update_target_obj
      IMPORTING
        !pfd_i_obj_type   TYPE mdg_object_type_code_bs
        !pwa_i_identifier TYPE mdg_t_identifier_bs
        !pfd_i_group_ref  TYPE boole_d
        !pfd_i_system_ref TYPE boole_d .
    METHODS update_mapping_table .
    METHODS write_key_mapping
      RETURNING
        VALUE(pfd_r_is_succesful) TYPE abap_boolean .
ENDCLASS.



CLASS ZCL_GL_REPLICATION IMPLEMENTATION.


  METHOD if_ex_usmd_glam_rplctn~inbound_processing.
  ENDMETHOD.


  METHOD if_ex_usmd_glam_rplctn~outbound_processing.
    DATA:
      lfd_id_value     TYPE mdg_object_id_bs,
      lfd_id_value1    TYPE mdg_object_id_bs,
      lfd_id_value2    TYPE mdg_object_id_bs,
      lfd_id_value3    TYPE mdg_object_id_bs,
      lfd_id_check     TYPE mdg_object_id_bs,
      lwa_strc         TYPE mdg_s_get_matching_easy_bs,
      lfd_does_exist   TYPE abap_boolean,
      lfd_coa_holder   TYPE ktopl,
      lfd_comp_holder  TYPE bukrs,
      lfd_acc_holder   TYPE saknr,
      lfd_is_succesful TYPE abap_boolean,
      lri_conv_api     TYPE REF TO if_usmd_conv_som_gov_api,
      lc_acc_length    TYPE i VALUE 10,
      lfd_string       TYPE string,
      lfd_char         TYPE char3,
      lfd_space        TYPE string.


*    lfd_char = ' % '.
*
    CHECK i_dis_system = 'MERCLNT100'.
*
*
*    lri_conv_api = cl_usmd_conv_som_gov_api=>get_instance(
*                     iv_model_name = '0G'                           " Data Model
**                       iv_classname  = 'CL_USMD_CONV_SOM_GOV_API' " Object Type Name
*                   ).
*
*
*
*    CALL METHOD cl_mdg_id_matching_api_bs=>get_instance
*      EXPORTING
*        iv_direct_db_insert       = abap_false       " 'X' Chg. are written directly to the DB ==> Save not needed
*        iv_set_lcl_system_by_api  = abap_true       " Set local System automatically by API
*      IMPORTING
*        er_if_mdg_id_matching_api = ri_model.                 " Interface for ID Mapping
*
*    ASSIGN cs_proxy_data-general_ledger_account_master-general_ledger_account_master[ 1 ]-general_ledger_account_master TO FIELD-SYMBOL(<lwa_gl_account>).
*    IF <lwa_gl_account> IS ASSIGNED.
*
*      lfd_coa_holder = <lwa_gl_account>-chart_of_accounts_code.
*      lfd_acc_holder = <lwa_gl_account>-chart_of_accounts_item_code-content.
*
*
*      DATA(lfd_length) = strlen( lfd_coa_holder ).
*      IF lfd_length = 3.
*        CONCATENATE lfd_coa_holder lfd_acc_holder INTO lfd_id_value1 SEPARATED BY space.
*        CONCATENATE lfd_coa_holder lfd_acc_holder INTO lfd_id_value SEPARATED BY space.
*      ELSEIF lfd_length = 4.
*        CONCATENATE lfd_coa_holder  lfd_acc_holder INTO lfd_id_value1.
*        CONCATENATE lfd_coa_holder  lfd_acc_holder INTO lfd_id_value.
*      ENDIF.
*
*      lfd_id_value2 = lfd_id_value1.
*
*      "Mapping
*
*
*      LOOP AT <lwa_gl_account>-company_details ASSIGNING FIELD-SYMBOL(<lwa_company>).
*        IF <lwa_company> IS ASSIGNED.
*          CLEAR: lfd_id_value .
*          lfd_comp_holder = <lwa_company>-company_id.
*          "If we will not map company id, this holder is unnecessary.
*          CONCATENATE lfd_id_value1 lfd_comp_holder INTO lfd_id_value.
*          me->read_key_mapping(
*            EXPORTING
*              pfd_i_object_type        = c_obj_type_892
*              pfd_i_ids_type           = c_ids_type_907
*              pfd_i_system_id          = i_dis_system
*              pfd_i_id_value           = lfd_id_value
*            IMPORTING
*              pfd_e_value              = lfd_id_check
*              pit_e_matching_objects =  DATA(lit_matching_obj)
*              pfd_e_does_mapping_exist = lfd_does_exist ).
*
**
*          IF lfd_does_exist = abap_true AND lfd_id_check <> lfd_id_value.
*            "Write suitable messages to slg1.
*            RETURN. "If mapping
*
*          ELSEIF lfd_does_exist = abap_false. "If this is the first send of an account, we create key mapping first.
*
*            me->read_key_mapping(
*                    EXPORTING
*                      pfd_i_object_type        = c_obj_type_899
*                      pfd_i_ids_type           = c_ids_type_909
*                      pfd_i_system_id          = i_dis_system
*                      pfd_i_id_value           = lfd_id_value1
*                    IMPORTING
*                      pfd_e_value              = lfd_id_check
*                      pit_e_matching_objects   = lit_matching_obj
*                      pfd_e_does_mapping_exist = lfd_does_exist ).
*
*            IF lfd_does_exist EQ abap_false.
*
*              me->update_source_obj(
*              pfd_i_obj_type   = c_obj_type_899
*              pwa_i_identifier = VALUE mdg_t_identifier_bs( ( ident_defining_scheme_code = c_ids_type_909 business_system_id = c_sysid id_value = lfd_id_value1 ) )
*              pfd_i_group_ref  = abap_true
*              pfd_i_system_ref = abap_true  ).
*
*              CLEAR: lfd_id_value1.
*
**              <lwa_gl_account>-chart_of_accounts_code = '0MB2'.
*
*              CONCATENATE 'J' <lwa_gl_account>-chart_of_accounts_item_code-content INTO <lwa_gl_account>-chart_of_accounts_item_code-content. "Is E a business constant? If so, it should be contained in BRF+.
*              CONCATENATE 'J' <lwa_gl_account>-id-content INTO <lwa_gl_account>-id-content.
*
*              lfd_length = strlen( <lwa_gl_account>-chart_of_accounts_code ).
*
*              IF lfd_length = 3.
*                CONCATENATE <lwa_gl_account>-chart_of_accounts_code <lwa_gl_account>-chart_of_accounts_item_code-content INTO lfd_id_value1 SEPARATED BY space.
*              ELSEIF lfd_length = 4.
*                CONCATENATE <lwa_gl_account>-chart_of_accounts_code <lwa_gl_account>-chart_of_accounts_item_code-content INTO lfd_id_value1.
*              ENDIF.
*
*              me->update_target_obj(
*                pfd_i_obj_type   = c_obj_type_899
*                pwa_i_identifier = VALUE mdg_t_identifier_bs( ( ident_defining_scheme_code = c_ids_type_909 business_system_id = i_dis_system id_value = lfd_id_value1 ) )
*                pfd_i_group_ref  = abap_true
*                pfd_i_system_ref = abap_true
*              ).
*
*              me->update_mapping_table( ).
*
*              me->write_key_mapping(
*                RECEIVING
*                  pfd_r_is_succesful = lfd_is_succesful ). "Suitable error handling can be implemented.
*
*            ELSE. "CoA and Account are mapped already.
*              "Map company code if necessary.
*            ENDIF.
*            CLEAR: lfd_id_value.
*
**            READ TABLE lit_matching_obj INTO DATA(lwa_mathing_obj) WITH KEY business_system_id = c_sysid.
**            READ TABLE lwa_mathing_obj-object_identifier INTO DATA(lwa_identifier) WITH KEY ident_defining_scheme_code = c_ids_type_909.
**            CONCATENATE lwa_identifier-id_value  lfd_comp_holder INTO lfd_id_value .
*
*            "CONCATENATE lfd_id_value2 lfd_comp_holder INTO lfd_id_value.
*
*            LOOP AT it_objkey ASSIGNING FIELD-SYMBOL(<lwa_objkey>).
*              IF <lwa_objkey> IS ASSIGNED.
*                ASSIGN COMPONENT if_usmdz_cons_attributes=>gc_attr_coa OF STRUCTURE <lwa_objkey> TO FIELD-SYMBOL(<lfd_any>). "CoA
*                IF <lfd_any> IS ASSIGNED.
*                  "CLEAR: lfd_comp_holder.
*                  lfd_coa_holder = <lfd_any>.
*                  "lfd_id_value = lfd_coa_holder.
*                ENDIF.
*                UNASSIGN <lfd_any>.
*                ASSIGN COMPONENT if_usmdz_cons_attributes=>gc_attr_account OF STRUCTURE <lwa_objkey> TO <lfd_any>. "Account
*                IF <lfd_any> IS ASSIGNED.
*                  CLEAR: lfd_acc_holder.
*                  lfd_acc_holder = |{ <lfd_any> ALPHA = OUT }|.
*
**                  lfd_length = strlen( lfd_id_value ).
**                  IF lfd_length = 3.
**                    CONCATENATE lfd_id_value lfd_acc_holder INTO lfd_id_value SEPARATED BY space.
**                  ELSEIF lfd_length = 4.
**                    CONCATENATE lfd_id_value lfd_acc_holder INTO lfd_id_value.
**                  ENDIF.
**                  "CONCATENATE lfd_id_value <lfd_any> INTO lfd_id_value.
*                ENDIF.
*                UNASSIGN <lfd_any>.
*                ASSIGN COMPONENT if_usmdz_cons_attributes=>gc_attr_compcode OF STRUCTURE <lwa_objkey> TO <lfd_any>.
*                IF <lfd_any> IS ASSIGNED.
*                  lfd_comp_holder = <lfd_any>.
**                  CLEAR: lfd_length.
**                  lfd_length = strlen( lfd_acc_holder ).
**                  lfd_length = lc_acc_length - lfd_length.
**
**                  DO lfd_length TIMES.
**                    CONCATENATE lfd_space ' ' INTO lfd_space.
**                    "REPLACE ALL OCCURENCES OF lfd_char IN lfd_id_value WITH space.
**                  ENDDO.
**                  CONCATENATE lfd_id_value <lfd_any> INTO lfd_id_value SEPARATED BY lfd_space.
*                ENDIF.
*              ENDIF.
*            ENDLOOP.
*
*            cl_mdg_id_matching_tools_ext=>create_id_value_from_comp(
*                                          EXPORTING
*                                            it_id_value_data       =  VALUE mdg_t_km_field_value( ( comp_name = 'COA' value_ext_format = lfd_coa_holder )
*                                                                      ( comp_name = 'ACCOUNT' value_ext_format = lfd_acc_holder )
*                                                                      ( comp_name = 'COMPCODE' value_ext_format = lfd_comp_holder ) )                " Field/Value pairs of structured ID values
*                                            iv_oitc                = '907'                 " Object Identifier Type
**                iv_no_case_translation =                  " Supress Upper Case Translation
*                                          IMPORTING
*                                            ev_id_value            = lfd_id_value3                 " Key Mapping: Object ID Value
*            ).
*
*            "lfd_id_value3 = lfd_id_value.
*            "lfd_id_value = lfd_string.
*            me->update_source_obj(
*              pfd_i_obj_type   = c_obj_type_892
*              pwa_i_identifier = VALUE mdg_t_identifier_bs( ( ident_defining_scheme_code = c_ids_type_907 business_system_id = c_sysid id_value = lfd_id_value3 ) )
*              pfd_i_group_ref  = abap_true
*              pfd_i_system_ref = abap_true  ).
*
**            CLEAR: lfd_id_value, lfd_string.
*
**            lfd_length = strlen( <lwa_gl_account>-chart_of_accounts_code ).
**
**            IF lfd_length = 3.
**              CONCATENATE <lwa_gl_account>-chart_of_accounts_code <lwa_gl_account>-chart_of_accounts_item_code-content INTO lfd_id_value SEPARATED BY space.
**            ELSEIF lfd_length = 4.
**              CONCATENATE <lwa_gl_account>-chart_of_accounts_code <lwa_gl_account>-chart_of_accounts_item_code-content INTO lfd_id_value.
**            ENDIF.
*
**            CLEAR: lfd_length.
**            lfd_length = strlen( <lwa_gl_account>-chart_of_accounts_item_code-content ).
**            lfd_length = lc_acc_length - lfd_length.
**            CLEAR: lfd_space.
**            DO lfd_length TIMES.
**              CONCATENATE lfd_space ' ' INTO lfd_space.
**            ENDDO.
**            CONCATENATE lfd_id_value lfd_comp_holder INTO lfd_id_value SEPARATED BY lfd_space.
*
*            cl_mdg_id_matching_tools_ext=>create_id_value_from_comp(
*                                            EXPORTING
*                                              it_id_value_data       =  VALUE mdg_t_km_field_value( ( comp_name = 'COA' value_ext_format = <lwa_gl_account>-chart_of_accounts_code )
*                                                                                                    ( comp_name = 'ACCOUNT' value_ext_format = <lwa_gl_account>-chart_of_accounts_item_code-content )
*                                                                                                    ( comp_name = 'COMPCODE' value_ext_format = lfd_comp_holder ) )                " Field/Value pairs of structured ID values
*                                              iv_oitc                = '907'                 " Object Identifier Type
**                iv_no_case_translation =                  " Supress Upper Case Translation
*                                            IMPORTING
*                                              ev_id_value            = lfd_id_value3                 " Key Mapping: Object ID Value
*).
*            "CLEAR lfd_id_value3.
*            "lfd_id_value3 = lfd_id_value.
*            me->update_target_obj(
*              pfd_i_obj_type   = c_obj_type_892
*              pwa_i_identifier = VALUE mdg_t_identifier_bs( ( ident_defining_scheme_code = c_ids_type_907 business_system_id = i_dis_system id_value = lfd_id_value3 ) )
*              pfd_i_group_ref  = abap_true
*              pfd_i_system_ref = abap_false
*            ).
*
*            me->update_mapping_table( ).
*
*            me->write_key_mapping(
*              RECEIVING
*                pfd_r_is_succesful = lfd_is_succesful ). "Suitable error handling can be implemented.
*
*
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.

  ENDMETHOD.


  METHOD read_key_mapping.
    DATA:
      ls_search TYPE mdg_s_object_key_bs,
      ls_strc   TYPE mdg_s_get_matching_easy_bs.


    ls_search-object_type_code = pfd_i_object_type. "'899'.
    ls_search-identifier_key-ident_defining_scheme_code = pfd_i_ids_type.
    ls_search-identifier_key-business_system_id = pfd_i_system_id.
    ls_search-identifier_key-id_value = pfd_i_id_value.


    ri_model->get_matching(
      EXPORTING
        is_search_key               =  ls_search                " Object Key inlcuding Object Type Code
        iv_access_ctrl_reg_directly = abap_false                " 'X' Get KM from ctrl. reg. directly
        iv_do_not_access_ctrl_reg   = abap_false             " 'X' Ignore customizing & do not access ctrl. reg.
      IMPORTING
        es_matching_objects_easy    =  ls_strc                 " Identifier Set data of mapped objects
    ).
    IF ls_strc-no_matching_objects_found = abap_true.
      pfd_e_does_mapping_exist = abap_false.
      RETURN.
    ELSE.
      pfd_e_does_mapping_exist = abap_true.
    ENDIF.
    pit_e_matching_objects = ls_strc-matching_objects.
    LOOP AT ls_strc-matching_objects INTO DATA(ls_objects) WHERE object_type_code = c_obj_type_892 AND business_system_id = pfd_i_system_id.
      READ TABLE ls_objects-object_identifier INTO DATA(ls_identifier) WITH KEY ident_defining_scheme_code = c_ids_type_907.
      IF sy-subrc EQ 0.
        pfd_e_value = ls_identifier-id_value.

        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD update_mapping_table.
    APPEND wa_mapping TO it_mapping.
  ENDMETHOD.


  METHOD update_source_obj.
    DATA: lwa_matching   TYPE mdg_s_matching_obj_data_inp_bs.

    lwa_matching-object_type_code   = pfd_i_obj_type. "899
    lwa_matching-object_identifier  = pwa_i_identifier.
    lwa_matching-group_reference    = pfd_i_group_ref.
    lwa_matching-system_reference   = pfd_i_system_ref.

    wa_mapping-source_object = lwa_matching.
    CLEAR: lwa_matching.

  ENDMETHOD.


  METHOD update_target_obj.
    DATA: lwa_matching   TYPE mdg_s_matching_obj_data_inp_bs.

    lwa_matching-object_type_code   = pfd_i_obj_type. "899
    lwa_matching-object_identifier  = pwa_i_identifier.
    lwa_matching-group_reference    = pfd_i_group_ref.
    lwa_matching-system_reference   = pfd_i_system_ref.

    wa_mapping-target_object = lwa_matching.
    CLEAR: lwa_matching.

  ENDMETHOD.


  METHOD write_key_mapping.
    TRY.
        ri_model->add_matching(
                              it_matching_simple         = it_mapping                 " Identifier data of multiple pairs of  two obj. to be mapped
                              iv_update_central_registry = abap_false   ).     " Update Central Registry with Key Mapping Content

      CATCH cx_mdg_missing_input_parameter INTO DATA(c1). " Missing Input parameter in a method
      CATCH cx_mdg_missing_id_data INTO DATA(c2).         " One or more ID data are missing
      CATCH cx_mdg_otc_idm_error INTO DATA(c3).           " ID matching related OTC error
      CATCH cx_mdg_idsc_invalid INTO DATA(c4).            " IDS code does not exist
      CATCH cx_mdg_id_matching_bs INTO DATA(c5).          " General ID matching messages
      CATCH cx_mdg_km_same_identifier INTO DATA(c6).      " Identifier for Object still exist
      CATCH cx_mdg_no_ctrl_reg_defined INTO DATA(c7).     " For OTC no central registry defined in KM
      CATCH cx_mdg_different_bus_systems INTO DATA(c8).   " Different Business Systems for one Object
      CATCH cx_mdg_km_invalid_id_value INTO DATA(c9).     " Invalid ID value
    ENDTRY.
    TRY.
        ri_model->save(
          IMPORTING
            ev_save_successful     = pfd_r_is_succesful                  " Boolean Variable (X=True, -=False, Space=Unknown)
        ).
      CATCH cx_mdg_id_matching_bs INTO c5. " General ID matching messages
    ENDTRY.
    COMMIT WORK AND WAIT.
  ENDMETHOD.
ENDCLASS.
