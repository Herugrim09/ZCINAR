*&---------------------------------------------------------------------*
*& Report ZPROG_PLAYGROUND
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprog_playground.

DATA: lo_data_descr TYPE REF TO cl_abap_typedescr,
      lv_type_name  TYPE string,
      your_variable TYPE prctr.
your_variable = 'PX101104'.

lo_data_descr = cl_abap_typedescr=>describe_by_data( your_variable ).
lv_type_name = lo_data_descr->get_relative_name( ).
TRY.
    cl_mdg_id_matching_tools=>process_rtti_key_struc(
      EXPORTING
        iv_oitc         = '907'                 " Object Identifier Type
      IMPORTING
        ev_multi_key    = DATA(lv_multi_key)                 " 'X': Structure does have multiple key fields
        et_key_fields   = DATA(lt_key_fields)                 " Key Fields of the Key structure
        ev_no_key_struc = DATA(lv_no_key_struct)                 " 'X': No Key structure defined in customizing
        es_oitc_data    = DATA(ls_oitc_Data)                 " OITC data
    ).
  CATCH cx_mdg_idsc_invalid INTO DATA(lx_mdg_idsc_invalid).           " Key Mapping related OITC code error
  CATCH cx_mdg_km_invalid_id_datatype INTO DATA(lx_mdg_km_invalid_id_datatype). " Key Structure does not exist
  CATCH cx_mdg_obj_id_struc_not_exist INTO DATA(lrcx_mdg_obj_id_struc_no_exist). " Key Structure does not exist
ENDTRY.
BREAK-POINT.
" If it's based on a data element, lv_type_name will contain the data element name
*DATA: ls_attributes TYPE zmed_t_tx_attr.
*ls_attributes-attr_id = '0000000005'.
*ls_attributes-attr_name = 'AGIRLIK'.
*ls_attributes-data_type = 'INT2'.
*ls_attributes-status = 'A'.
*
*MODIFY zmed_t_tx_attr FROM ls_attributes.
*
*
*DATA: lt_input_list TYPE TABLE OF swwwimulti.
*DATA: lt_header_list TYPE TABLE OF swwwihead.
*DATA: lt_simple_container TYPE TABLE OF swr_cont.
*DATA: lt_message_lines TYPE TABLE OF swr_messag.
*DATA: lt_message_struct TYPE TABLE OF swr_mstruc.
*DATA: lt_subcontainer_bor_objects TYPE TABLE OF swr_cont.
*DATA: lt_subcontainer_all_objects TYPE TABLE OF swr_cont.
*DATA: lt_key_name TYPE if_salv_service_types=>yt_field_path.
*DATA: lt_local_data TYPE REF TO data.
*DATA: lo_itab_services TYPE REF TO if_salv_itab_services.
*DATA: lo_struct TYPE REF TO cl_abap_structdescr.
*FIELD-SYMBOLS: <ls_first_row> TYPE any.
*FIELD-SYMBOLS: <ls_holder> TYPE any.
*
*DATA: lo_app_context TYPE REF TO if_usmd_app_context.
*
*lo_app_context = cl_usmd_app_context=>get_context( ).
*
*IF lo_app_context IS INITIAL.
*  cl_usmd_app_context=>init_context(
*        EXPORTING
*          iv_model                 =                  " Data Model
*          it_parameter             =                  " Context Parameter
*          iv_otc                   =                  " Business Object Type
*          iv_wi_id                 =                  " Work Item ID
*      iv_crequest_id           = '4290'                 " Change Request
*          iv_crequest_type         =                  " Type of Change Request
*          iv_edition               =                  " Edition
*          iv_process               =                  " Business Activity
*          iv_skip_wi_determination = abap_false       " Suppress WorkItem determination for Change Request
*          iv_skip_wf_cont          = abap_false       " No Data Transfer from Workflow Container
*        IMPORTING
*          et_wf_item               =                  " List of All Suitable Workflow Items for Change Request
*          et_messages              =                  " Messages
*  ).
*      CATCH cx_usmd_app_context_cons_error. " Exception: Consistency Error in Design of Appl. Context
*ENDIF.
*lo_app_context = cl_usmd_app_context=>get_context( ).
*
*APPEND lo_app_context->mv_wi_id TO lt_input_list.
*CALL FUNCTION 'SWW_WIS_HEADER_READ'
*  EXPORTING
*    use_instance_manager = 'X'              " Use Instance Management
*  TABLES
*    wi_input_list        = lt_input_list                 " List of IDs of WIs To Be Read
*    wi_header_list       = lt_header_list.                 " List of WI Header Data Read
*
*IF lt_header_list IS NOT INITIAL.
*  DATA(ls_header) = lt_header_list[ 1 ].
*ENDIF.
*
*CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
*  EXPORTING
*    workitem_id              = ls_header-parent_wi
*  TABLES
*    simple_container         = lt_simple_container
*    message_lines            = lt_message_lines
*    message_struct           = lt_message_struct
*    subcontainer_bor_objects = lt_subcontainer_bor_objects
*    subcontainer_all_objects = lt_subcontainer_all_objects.
*DATA: lt_components TYPE cl_abap_structdescr=>component_table,
*      ls_component  TYPE cl_abap_structdescr=>component.
*
*DATA: ls_dd03p TYPE dd03p,
*      lt_dd03p TYPE TABLE OF dd03p,
*      ls_dd02v TYPE dd02v.
*
*CONSTANTS: lc_structure_name TYPE ddobjname VALUE 'ZMY_STRUCTURE'.
*
*" Prepare the structure header (DD02V) with corrected TABCLASS
*CLEAR ls_dd02v.
*ls_dd02v-tabname    = lc_structure_name.
*ls_dd02v-tabclass   = 'INTTAB'.        " Correct class for a structure
*ls_dd02v-ddlanguage = sy-langu.
*ls_dd02v-contflag   = 'A'.             " Active state
*
*" Define the first component FIELD1 (Character)
*CLEAR ls_dd03p.
*ls_dd03p-tabname   = lc_structure_name.
*ls_dd03p-fieldname = 'FIELD1'.
*ls_dd03p-datatype  = 'CHAR'.
*ls_dd03p-rollname  = 'FIELD1'.         " Rollname same as fieldname
*ls_dd03p-leng      = 20.
*ls_dd03p-position  = 1.
*APPEND ls_dd03p TO lt_dd03p.
*
*" Define the second component FIELD2 (Integer)
*CLEAR ls_dd03p.
*ls_dd03p-tabname   = lc_structure_name.
*ls_dd03p-fieldname = 'FIELD2'.
*ls_dd03p-datatype  = 'INT4'.
*ls_dd03p-rollname  = 'FIELD2'.         " Rollname same as fieldname
*ls_dd03p-leng      = 10.
*ls_dd03p-position  = 2.
*APPEND ls_dd03p TO lt_dd03p.
*
*" Ensure the component list is properly populated
*IF lt_dd03p[] IS INITIAL.
*  WRITE: 'Error: No components defined for the structure.'.
*  EXIT.
*ENDIF.
*
*" Create the structure definition using DDIF_TABL_PUT
*CALL FUNCTION 'DDIF_TABL_PUT'
*  EXPORTING
*    name              = lc_structure_name
*    dd02v_wa          = ls_dd02v
*  TABLES
*    dd03p_tab         = lt_dd03p
*  EXCEPTIONS
*    tabl_not_found    = 1
*    name_inconsistent = 2
*    tabl_inconsistent = 3
*    put_failure       = 4
*    put_refused       = 5
*    OTHERS            = 6.
*
*IF sy-subrc = 0.
*  WRITE: 'Structure definition created successfully. Activating...'.
*
*  " Activate the structure in the ABAP Dictionary
*  CALL FUNCTION 'DDIF_TABL_ACTIVATE'
*    EXPORTING
*      name              = lc_structure_name
*    EXCEPTIONS
*      tabl_not_found    = 1
*      activation_failed = 2
*      OTHERS            = 3.
*
*  IF sy-subrc = 0.
*    WRITE: 'Structure activated successfully!'.
*  ELSE.
*    WRITE: 'Error during structure activation!'.
*  ENDIF.
*ELSE.
*  WRITE: 'Error occurred during structure creation!'.
*  CASE sy-subrc.
*    WHEN 3. WRITE: 'Reason: TABL_INCONSISTENT. Ensure TABCLASS is set to INTTAB.'.
*    WHEN 2. WRITE: 'Reason: NAME_INCONSISTENT. Ensure TABNAME matches in all fields.'.
*    WHEN OTHERS. WRITE: 'An unknown error occurred. Please check field definitions.'.
*  ENDCASE.
*ENDIF.
