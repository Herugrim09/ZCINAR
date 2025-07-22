CLASS zcl_med_mdg_km_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " Constants for G/L Account key mapping
    CONSTANTS: c_obj_type_glaccount TYPE mdg_object_type_code_bs VALUE '892',
               c_ids_type_glaccount TYPE mdg_ids_type_code_bs VALUE '907'.

    " Singleton instance method
    CLASS-METHODS: get_instance
      IMPORTING
        iv_direct_db_insert      TYPE abap_bool DEFAULT abap_false
        iv_set_lcl_system_by_api TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_instance)       TYPE REF TO zcl_med_mdg_km_utility.

    " Public methods
    METHODS: constructor
      IMPORTING
        iv_direct_db_insert      TYPE abap_bool DEFAULT abap_false
        iv_set_lcl_system_by_api TYPE abap_bool DEFAULT abap_true,

      " Method to create ID from field components
      create_id_from_components
        IMPORTING
          it_field_data TYPE mdg_t_km_field_value
          iv_ids_type   TYPE mdg_ids_type_code_bs
        RETURNING
          VALUE(rv_id)  TYPE mdg_object_id_bs,

      " Check if mapping exists
      check_mapping_exists
        IMPORTING
          iv_object_type   TYPE mdg_object_type_code_bs
          iv_ids_type      TYPE mdg_ids_type_code_bs
          iv_system_id     TYPE sld_bskey
          iv_id_value      TYPE mdg_object_id_bs
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      " Create key mapping
      create_key_mapping
        IMPORTING
          iv_object_type     TYPE mdg_object_type_code_bs
          iv_ids_type        TYPE mdg_ids_type_code_bs
          iv_source_system   TYPE sld_bskey
          iv_target_system   TYPE sld_bskey
          iv_source_id_value TYPE mdg_object_id_bs
          iv_target_id_value TYPE mdg_object_id_bs
        RETURNING
          VALUE(rv_exist)    TYPE abap_bool,

      " Get mapped value for given ID
      get_mapped_value
        IMPORTING
          iv_object_type      TYPE mdg_object_type_code_bs
          iv_ids_type         TYPE mdg_ids_type_code_bs
          iv_source_system    TYPE sld_bskey
          iv_target_system    TYPE sld_bskey
          iv_source_id_value  TYPE mdg_object_id_bs
        RETURNING
          VALUE(rv_mapped_id) TYPE mdg_object_id_bs,

      " Save all mappings
      save_mappings
        EXPORTING
          et_messages       TYPE usmd_t_message
        RETURNING
          VALUE(rv_success) TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA: go_instance TYPE REF TO zcl_med_mdg_km_utility.

    " Instance variables
    DATA: mo_km_api  TYPE REF TO if_mdg_id_matching_api_bs,
          mt_mapping TYPE mdg_t_add_matching_easy_bs.

    " Private helper methods
    METHODS: read_key_mapping
      IMPORTING
        iv_object_type      TYPE mdg_object_type_code_bs
        iv_ids_type         TYPE mdg_ids_type_code_bs
        iv_system_id        TYPE sld_bskey
        iv_id_value         TYPE mdg_object_id_bs
      EXPORTING
        et_matching_objects TYPE mdg_t_matching_obj_data_bs
        ev_mapped_value     TYPE mdg_object_id_bs
        ev_mapping_exists   TYPE abap_bool,

      create_matching_object
        IMPORTING
          iv_obj_type            TYPE mdg_object_type_code_bs
          iv_ids_type            TYPE mdg_ids_type_code_bs
          iv_system_id           TYPE sld_bskey
          iv_id_value            TYPE mdg_object_id_bs
          iv_group_ref           TYPE boole_d
          iv_system_ref          TYPE boole_d
        RETURNING
          VALUE(rs_matching_obj) TYPE mdg_s_matching_obj_data_inp_bs.

ENDCLASS.



CLASS ZCL_MED_MDG_KM_UTILITY IMPLEMENTATION.


  METHOD check_mapping_exists.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Check if a mapping have already been done
************************************************************************
    CLEAR: rv_exists.
    read_key_mapping(
      EXPORTING
        iv_object_type    = iv_object_type
        iv_ids_type       = iv_ids_type
        iv_system_id      = iv_system_id
        iv_id_value       = iv_id_value
      IMPORTING
        ev_mapping_exists = rv_exists
    ).
  ENDMETHOD.


  METHOD constructor.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Constructor
************************************************************************
    " Initialize the key mapping API
    CALL METHOD cl_mdg_id_matching_api_bs=>get_instance
      EXPORTING
        iv_direct_db_insert       = iv_direct_db_insert
        iv_set_lcl_system_by_api  = iv_set_lcl_system_by_api
      IMPORTING
        er_if_mdg_id_matching_api = mo_km_api.
  ENDMETHOD.


  METHOD create_id_from_components.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Create ID
************************************************************************
    " Create ID value from field components using standard API
    cl_mdg_id_matching_tools_ext=>create_id_value_from_comp(
      EXPORTING
        it_id_value_data = it_field_data
        iv_oitc          = iv_ids_type
      IMPORTING
        ev_id_value      = rv_id
    ).
  ENDMETHOD.


  METHOD create_key_mapping.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Create Key Mapping
************************************************************************
    DATA: ls_mapping TYPE mdg_s_add_matching_easy_bs.

    CLEAR: rv_exist.

    " Check if mapping already exists
    IF check_mapping_exists(
         iv_object_type = iv_object_type
         iv_ids_type    = iv_ids_type
         iv_system_id   = iv_target_system
         iv_id_value    = iv_target_id_value ) = abap_true.
      rv_exist = abap_true.
      RETURN.
    ENDIF.

    " Create source mapping entry
    ls_mapping-source_object = create_matching_object(
      iv_obj_type   = iv_object_type
      iv_ids_type   = iv_ids_type
      iv_system_id  = iv_source_system
      iv_id_value   = iv_source_id_value
      iv_group_ref  = abap_true
      iv_system_ref = abap_true
    ).

    " Create target mapping entry
    ls_mapping-target_object = create_matching_object(
      iv_obj_type   = iv_object_type
      iv_ids_type   = iv_ids_type
      iv_system_id  = iv_target_system
      iv_id_value   = iv_target_id_value
      iv_group_ref  = abap_true
      iv_system_ref = abap_false
    ).

    " Add to mapping table
    APPEND ls_mapping TO mt_mapping.
  ENDMETHOD.


  METHOD create_matching_object.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Create Matching Object
************************************************************************
    rs_matching_obj-object_type_code  = iv_obj_type.
    rs_matching_obj-object_identifier = VALUE #( ( ident_defining_scheme_code = iv_ids_type
                                                   business_system_id = iv_system_id
                                                   id_value = iv_id_value ) ).
    rs_matching_obj-group_reference   = iv_group_ref.
    rs_matching_obj-system_reference  = iv_system_ref.
  ENDMETHOD.


  METHOD get_instance.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Get Instance
************************************************************************
    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance
        EXPORTING
          iv_direct_db_insert      = iv_direct_db_insert
          iv_set_lcl_system_by_api = iv_set_lcl_system_by_api.
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD get_mapped_value.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Get mapped values
************************************************************************
    DATA: lv_mapping_exists TYPE abap_bool,
          lt_matching       TYPE mdg_t_matching_obj_data_bs.

    " Read key mapping
    read_key_mapping(
      EXPORTING
        iv_object_type      = iv_object_type
        iv_ids_type         = iv_ids_type
        iv_system_id        = iv_target_system
        iv_id_value         = iv_source_id_value
      IMPORTING
        et_matching_objects = lt_matching
        ev_mapped_value     = rv_mapped_id
        ev_mapping_exists   = lv_mapping_exists
    ).

    IF lv_mapping_exists = abap_false.
      CLEAR rv_mapped_id.
    ENDIF.
  ENDMETHOD.


  METHOD read_key_mapping.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Read Key Mapping
************************************************************************
    DATA: ls_search TYPE mdg_s_object_key_bs,
          ls_strc   TYPE mdg_s_get_matching_easy_bs.

    CLEAR: et_matching_objects, ev_mapped_value, ev_mapping_exists.

    " Prepare search key
    ls_search-object_type_code = iv_object_type.
    ls_search-identifier_key-ident_defining_scheme_code = iv_ids_type.
    ls_search-identifier_key-business_system_id = cl_mdg_ca_get_own_bus_sys=>get_local_business_system( ).
    ls_search-identifier_key-id_value = iv_id_value.

    " Get matching objects
    mo_km_api->get_matching(
      EXPORTING
        is_search_key               = ls_search
        iv_access_ctrl_reg_directly = abap_false
        iv_do_not_access_ctrl_reg   = abap_false
      IMPORTING
        es_matching_objects_easy    = ls_strc
    ).

    IF ls_strc-no_matching_objects_found = abap_true.
      ev_mapping_exists = abap_false.
      RETURN.
    ELSE.
      ev_mapping_exists = abap_true.
    ENDIF.

    " Extract mapped value
    READ TABLE ls_strc-matching_objects ASSIGNING FIELD-SYMBOL(<ls_objects>)
         WITH KEY object_type_code = iv_object_type
                  business_system_id = iv_system_id.
    IF sy-subrc = 0.
      READ TABLE <ls_objects>-object_identifier INTO DATA(ls_identifier)
           WITH KEY ident_defining_scheme_code = iv_ids_type.
      IF sy-subrc = 0.
        ev_mapped_value = ls_identifier-id_value.
        et_matching_objects = ls_strc-matching_objects.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD save_mappings.
************************************************************************
* Extension ID  : E00607
* Project ID    : S4E
* Purpose       : Save Mapping from Buffer
************************************************************************
    CLEAR et_messages.

    IF mt_mapping IS NOT INITIAL.
      TRY.
          " Add mappings to the API
          mo_km_api->add_matching(
            it_matching_simple         = mt_mapping
            iv_update_central_registry = abap_false
          ).

          " Save the mappings
          mo_km_api->save(
            IMPORTING
              ev_save_successful = rv_success
          ).

          " Clear the mapping table after successful save
          IF rv_success = abap_true.
            CLEAR mt_mapping.
          ENDIF.

        CATCH cx_mdg_id_matching_bs INTO DATA(lo_mdg_error).
          rv_success = abap_false.

          " Convert exception messages to return format
          IF lo_mdg_error->dt_msg_bapiret IS NOT INITIAL.
            LOOP AT lo_mdg_error->dt_msg_bapiret INTO DATA(ls_error_msg).
              APPEND VALUE usmd_s_message(
                msgid = ls_error_msg-id
                msgno = ls_error_msg-number
                msgty = ls_error_msg-type
                msgv1 = ls_error_msg-message_v1
                msgv2 = ls_error_msg-message_v2
                msgv3 = ls_error_msg-message_v3
                msgv4 = ls_error_msg-message_v4
              ) TO et_messages.
            ENDLOOP.
          ENDIF.
      ENDTRY.
    ELSE.
      rv_success = abap_true. " No mappings to save
    ENDIF.

  ENDMETHOD.
ENDCLASS.
