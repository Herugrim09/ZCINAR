CLASS zcl_med_coa DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_usmd_coa_rplctn .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      it_mapping TYPE mdg_t_add_matching_easy_bs,
      wa_mapping TYPE mdg_s_add_matching_easy_bs,
      ri_model   TYPE REF TO if_mdg_id_matching_api_bs.
    CONSTANTS c_sysid TYPE sld_bskey VALUE 'MESCLNT100'.
    CONSTANTS c_ids_type_909 TYPE mdg_ids_type_code_bs VALUE '909'.
    CONSTANTS c_obj_type_899 TYPE mdg_object_type_code_bs VALUE '899'.
    METHODS read_key_mapping
      IMPORTING
        !pfd_i_object_type        TYPE mdg_object_type_code_bs
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
        !pfd_i_system_ref TYPE boole_d.
    METHODS update_target_obj
      IMPORTING
        !pfd_i_obj_type   TYPE mdg_object_type_code_bs
        !pwa_i_identifier TYPE mdg_t_identifier_bs
        !pfd_i_group_ref  TYPE boole_d
        !pfd_i_system_ref TYPE boole_d.
    METHODS update_mapping_table.
    METHODS write_key_mapping
      RETURNING
        VALUE(pfd_r_is_succesful) TYPE abap_boolean.



ENDCLASS.



CLASS ZCL_MED_COA IMPLEMENTATION.


  METHOD if_ex_usmd_coa_rplctn~inbound_processing.

    CHECK cs_data IS NOT INITIAL.

  ENDMETHOD.


  METHOD if_ex_usmd_coa_rplctn~outbound_processing.




  ENDMETHOD.


  METHOD if_ex_usmd_coa_rplctn~outbound_processing_v1.

    DATA:
      lfd_id_value     TYPE mdg_object_id_bs,
      lfd_id_check     TYPE mdg_object_id_bs,
      lwa_strc         TYPE mdg_s_get_matching_easy_bs,
      lfd_does_exist   TYPE abap_boolean,
      lfd_coa_holder   TYPE ktopl,
      lfd_acc_holder   TYPE saknr,
      lfd_is_succesful TYPE abap_boolean,
      lit_matching_obj TYPE mdg_t_matching_obj_data_bs,
      lwa_item_copy    TYPE mdgf_cht_accts_rplctn_req_itm.

    FIELD-SYMBOLS:
                <lit_data> TYPE table.
*    DO.ENDDO.
*    DO.ENDDO.
    CHECK i_dis_system = 'MERCLNT100'.
    "double replication test
    lwa_item_copy = cs_proxy_data-chart_of_accounts_replication-chart_of_accounts-item[ 1 ].
    CONCATENATE lwa_item_copy-code-content '7' INTO lwa_item_copy-code-content.
    lwa_item_copy-posting_blocked_indicator = abap_true.
    lwa_item_copy-additional_chart_of_accounts_i = 'C'.
    lwa_item_copy-additional_chart_of_accounts_1 = 'P'.
    APPEND lwa_item_copy TO cs_proxy_data-chart_of_accounts_replication-chart_of_accounts-item.
    CALL METHOD cl_mdg_id_matching_api_bs=>get_instance
      EXPORTING
        iv_direct_db_insert       = abap_false       " 'X' Chg. are written directly to the DB ==> Save not needed
        iv_set_lcl_system_by_api  = abap_true       " Set local System automatically by API
      IMPORTING
        er_if_mdg_id_matching_api = ri_model.                 " Interface for ID Mapping
    ASSIGN it_data TO <lit_data>.
*    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<lwa_data>).
*      IF <lwa_data> IS ASSIGNED.
*        ASSIGN COMPONENT 'MAPPING' OF STRUCTURE <lwa_data> TO FIELD-SYMBOL(<lfd_smt_mapping>).
*        IF <lfd_smt_mapping> = 'USMDZ6_0G_ACCOUNT_V1'.
*          ASSIGN <lwa_data>- TO FIELD-SYMBOL(<lit_account_data>).
*          EXIT.
*        ELSE.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

*    ASSIGN cs_proxy_data-chart_of_accounts_replication-chart_of_accounts-code TO FIELD-SYMBOL(<lfd_coa>).
*    IF <lfd_coa> IS ASSIGNED.
*      lfd_coa_holder = <lfd_coa>. "Should we write this to slg1?
*      LOOP AT cs_proxy_data-chart_of_accounts_replication-chart_of_accounts-item ASSIGNING FIELD-SYMBOL(<lwa_account>).
*        IF <lwa_account> IS ASSIGNED. "This also?
*          <lwa_account>-code-content = |{ <lwa_account>-code-content ALPHA = OUT }|.
*          lfd_acc_holder = <lwa_account>-code-content.
*          DATA(lfd_length) = strlen( lfd_coa_holder ).
*          IF lfd_length = 3.
*            CONCATENATE lfd_coa_holder lfd_acc_holder INTO lfd_id_value SEPARATED BY space.
*          ELSEIF lfd_length = 4.
*            CONCATENATE lfd_coa_holder  lfd_acc_holder INTO lfd_id_value.
*          ENDIF.
*          me->read_key_mapping(
*            EXPORTING
*              pfd_i_object_type      = c_obj_type_899
*              pfd_i_system_id        = c_sysid "i_dis_system
*              pfd_i_id_value         = lfd_id_value
*            IMPORTING
*              pfd_e_value            = lfd_id_check
*              pit_e_matching_objects = lit_matching_obj
*          pfd_e_does_mapping_exist = lfd_does_exist
*      ).
*
*          IF lfd_does_exist = abap_true AND lfd_id_check <> lfd_id_value.
*            "Write suitable messages to slg1.
*            RETURN.
*
*          ELSEIF lfd_does_exist = abap_false. "If this is the first send of an account, we create key mapping first.
*
*
*
*
*            me->update_source_obj(
*              pfd_i_obj_type   = c_obj_type_899
*              pwa_i_identifier = VALUE mdg_t_identifier_bs( ( ident_defining_scheme_code = c_ids_type_909 business_system_id = c_sysid id_value = lfd_id_value ) )
*              pfd_i_group_ref  = abap_true
*              pfd_i_system_ref = abap_true  ).
*
*            CLEAR: lfd_id_value.
*
*            "Mapping
**            <lfd_coa> = '0MB2'. "Consider move this constant to brf+. This should be a business constant.
*            CONCATENATE 'J' <lwa_account>-code-content INTO <lwa_account>-code-content. "Is E a business constant? If so, it should be contained in BRF+.
*
*            CLEAR: lfd_length.
*            lfd_length = strlen( <lfd_coa> ).
*
*            IF lfd_length = 3.
*              CONCATENATE <lfd_coa> <lwa_account>-code-content INTO lfd_id_value SEPARATED BY space.
*            ELSEIF lfd_length = 4.
*              CONCATENATE  <lfd_coa> <lwa_account>-code-content INTO lfd_id_value.
*            ENDIF.
*
*            me->update_target_obj(
*              pfd_i_obj_type   = c_obj_type_899
*              pwa_i_identifier = VALUE mdg_t_identifier_bs( ( ident_defining_scheme_code = c_ids_type_909 business_system_id = i_dis_system id_value = lfd_id_value ) )
*              pfd_i_group_ref  = abap_true
*              pfd_i_system_ref = abap_false
*            ).
*
*            me->update_mapping_table( ).
*
*            me->write_key_mapping(
*              RECEIVING
*                pfd_r_is_succesful = lfd_is_succesful ). "Suitable error handling can be implemented.
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
    ls_search-identifier_key-ident_defining_scheme_code = c_ids_type_909.
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
    LOOP AT ls_strc-matching_objects INTO DATA(ls_objects) WHERE object_type_code = c_obj_type_899 AND business_system_id = pfd_i_system_id.
      READ TABLE ls_objects-object_identifier INTO DATA(ls_identifier) WITH KEY ident_defining_scheme_code = c_ids_type_909.
      IF sy-subrc EQ 0.
        pfd_e_value = ls_identifier-id_value.
        pit_e_matching_objects = ls_strc-matching_objects.
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
  ENDMETHOD.
ENDCLASS.
