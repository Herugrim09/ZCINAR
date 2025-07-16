class ZCL_MDGF_GUIBB_RESULT definition
  public
  inheriting from CL_MDGF_GUIBB_RESULT
  final
  create public .

public section.

  methods GET_ACTION_CONTROL
    importing
      !IV_ACTION_ID type FPM_EVENT_ID
    exporting
      !ET_RESULT type ANY TABLE .

  methods IF_FPM_GUIBB_LIST~GET_DATA
    redefinition .
  methods IF_FPM_GUIBB_LIST~PROCESS_EVENT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MDGF_GUIBB_RESULT IMPLEMENTATION.


  METHOD if_fpm_guibb_list~get_data.
    DATA: lit_return TYPE STANDARD TABLE OF bapiret2.
    DATA: lit_activitygrps TYPE STANDARD TABLE OF bapiagr.

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

    CASE sy-uname.
      WHEN 'BGOZUMOGULLA'.
        TYPES: BEGIN OF ty_action_control_result,
                 agr_name TYPE agr_name,
               END OF ty_action_control_result.

        DATA:
          lv_enable_action TYPE boolean,
          lrt_result       TYPE REF TO data.

        FIELD-SYMBOLS:
          <lt_result> TYPE ANY TABLE.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username       = sy-uname                 " User Name
          TABLES
            activitygroups = lit_activitygrps                 " Activity Groups
            return         = lit_return.                  " Return Structure

        CREATE DATA lrt_result TYPE TABLE OF ty_action_control_result.
        ASSIGN lrt_result->* TO <lt_result>.

        LOOP AT ct_action_usage ASSIGNING FIELD-SYMBOL(<ls_action_usage>).
          CLEAR lv_enable_action.

          " If the button is already not enabled then it is considered as normal behaviour
          ASSIGN COMPONENT 'ENABLED' OF STRUCTURE <ls_action_usage> TO FIELD-SYMBOL(<lfd_enabled>).
          IF <lfd_enabled> IS ASSIGNED
            AND <lfd_enabled> IS INITIAL.
            CONTINUE.
          ENDIF.

          " Get role limitations from BRF for this action - Enable/Disable the button
          me->get_action_control(
            EXPORTING
              iv_action_id = <ls_action_usage>-id                 " ID of the FPM Event
            IMPORTING
              et_result    = <lt_result>
          ).

          " There is no specific role limitation for this button, continue
          IF     <lt_result> IS NOT ASSIGNED
              OR <lt_result> IS INITIAL.
            CONTINUE.
          ENDIF.

          " Check related role assigned to the user
          LOOP AT <lt_result> ASSIGNING FIELD-SYMBOL(<lv_result>).
            IF line_exists( lit_activitygrps[ agr_name = <lv_result> ] ).
              lv_enable_action = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.

          " Enable/Disable button according to outcome of BRF+ DT
          IF <lfd_enabled> IS ASSIGNED.
            <lfd_enabled> = lv_enable_action.
            ev_action_usage_changed = abap_true.
          ENDIF.
        ENDLOOP.

      WHEN OTHERS.

*        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*          EXPORTING
*            username       = sy-uname                 " User Name
**           cache_results  = 'X'              " Temporarily buffer results in work process
**           extuid_get     =                  " Read Flags for External User ID
**      IMPORTING
**           logondata      =                  " Structure with Logon Data
**           defaults       =                  " Structure with User Defaults
**           address        =                  " Address Data
**           company        =                  " Company for Company Address
**           snc            =                  " Secure Network Communication Data
**           ref_user       =                  " User Name of the Reference User
**           alias          =                  " User Name Alias
**           uclass         =                  " License-Related User Classification
**           lastmodified   =                  " User: Last Change (Date and Time)
**           islocked       =                  " User Lock
**           identity       =                  " Person Assignment of an Identity
**           admindata      =                  " User: Administration Data
**           description    =                  " Description
**           tech_user      =                  " Technical User
**           sapuser_uuid   =                  " Global User ID
*          TABLES
**           parameter      =                  " Table with User Parameters
**           profiles       =                  " Profiles
*            activitygroups = lit_activitygrps                 " Activity Groups
*            return         = lit_return                  " Return Structure
**           addtel         =                  " BAPI Structure Telephone Numbers
**           addfax         =                  " BAPI Structure Fax Numbers
**           addttx         =                  " BAPI Structure Teletex Numbers
**           addtlx         =                  " BAPI Structure Telex Numbers
**           addsmtp        =                  " E-Mail Addresses BAPI Structure
**           addrml         =                  " Inhouse Mail BAPI Structure
**           addx400        =                  " BAPI Structure X400 Addresses
**           addrfc         =                  " BAPI Structure RFC Addresses
**           addprt         =                  " BAPI Structure Printer Addresses
**           addssf         =                  " BAPI Structure SSF Addresses
**           adduri         =                  " BAPI Structure: URL, FTP, and so on
**           addpag         =                  " BAPI Structure Pager Numbers
**           addcomrem      =                  " BAPI Structure Communication Comments
**           parameter1     =                  " Replaces Parameter (Length 18 -> 40)
**           groups         =                  " Transfer Structure for a List of User Groups
**           uclasssys      =                  " System-Specific License-Related User Classification
**           extidhead      =                  " Header Data for External ID of a User
**           extidpart      =                  " Part of a Long Field for the External ID of a User
**           systems        =                  " BAPI Structure for CUA Target Systems
**           extuid         =                  " User: Transfer Structure External User ID
**           sapuser_uuid_hist =                  " Global User ID History (Not Supported)
**           usattribute    =                  " User: Transfer Structure for User Attributes
*          .
*
*
*        READ TABLE lit_activitygrps WITH KEY agr_name = 'TEST_ROLE' TRANSPORTING NO FIELDS.
*        IF sy-subrc NE 0.
*          READ TABLE ct_action_usage ASSIGNING FIELD-SYMBOL(<lwa_actions>) WITH KEY id = 'REPLICATE'.
*          IF <lwa_actions> IS ASSIGNED.
*            <lwa_actions>-visible = if_fpm_constants=>gc_visibility-not_visible.
*            ev_action_usage_changed = abap_true.
*          ENDIF.
*        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD get_action_control.
    CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '000C292C36471FE09482E2BCD147F145'.
    DATA:lv_timestamp  TYPE timestamp,
         lt_name_value TYPE abap_parmbind_tab,
         ls_name_value TYPE abap_parmbind,
         lr_data       TYPE REF TO data,
         lx_fdt        TYPE REF TO cx_fdt,
         la_is_active  TYPE if_fdt_types=>element_text,
         la_action_id  TYPE if_fdt_types=>element_text.
    FIELD-SYMBOLS <la_any> TYPE any.

    CLEAR et_result.
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
    ls_name_value-name = 'IS_ACTIVE'.
    la_IS_ACTIVE = abap_true.
    GET REFERENCE OF la_IS_ACTIVE INTO lr_data.
    ls_name_value-value = lr_data.
    INSERT ls_name_value INTO TABLE lt_name_value.
    CLEAR ls_name_value.
****************************************************************************************************
    ls_name_value-name = 'ACTION_ID'.
    la_ACTION_ID = iv_action_id.
    GET REFERENCE OF la_ACTION_ID INTO lr_data.
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
        IF      <la_any> IS ASSIGNED
            AND <la_any> IS NOT INITIAL.
          MOVE-CORRESPONDING <la_any> TO et_result.
        ENDIF.
      CATCH cx_fdt INTO lx_fdt.
****************************************************************************************************
* You can check CX_FDT->MT_MESSAGE for error handling.
****************************************************************************************************
    ENDTRY.
  ENDMETHOD.


  METHOD if_fpm_guibb_list~process_event.
    DATA: lo_fpm TYPE REF TO if_fpm.
    lo_fpm = cl_fpm_factory=>get_instance( ).

    IF io_event->mv_event_id = 'NEW'.
      io_event->mo_event_data->set_value(
      iv_key   = 'ZMY_KEY'
      iv_value = '1234'
*  ir_value =
    ).
      me->mo_app_parameter->set_value(
        iv_key   = 'ZMY_KEY'
        iv_value = '1234'
*  ir_value =
      ).
    ENDIF.
    CALL METHOD super->if_fpm_guibb_list~process_event
      EXPORTING
        io_event            = io_event
        iv_raised_by_own_ui = iv_raised_by_own_ui
        iv_lead_index       = iv_lead_index
        iv_event_index      = iv_event_index
        it_selected_lines   = it_selected_lines
        io_ui_info          = io_ui_info
      IMPORTING
        ev_result           = ev_result
        et_messages         = et_messages.

    DATA(lt_key_values) = me->mt_key_comp_values.
    lo_fpm->get_runtime_info(
      RECEIVING
        rs_runtime_info = DATA(ls_runtime_info)                 " Runtime Information
    ).

    lo_fpm->get_service(
      EXPORTING
        iv_service_key       = 'CNR_OVP'                 " Service key
      RECEIVING
        rr_service_interface = DATA(lr_service_interface)                 " Reference to service interface
    ).
  ENDMETHOD.
ENDCLASS.
