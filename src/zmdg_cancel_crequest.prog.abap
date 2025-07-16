*&---------------------------------------------------------------------*
*& Report ZMDG_CANCEL_CREQUEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmdg_cancel_crequest.
DATA: gs_dummy_crequest    TYPE usmd120c,
      it_creqs_2bewithdraw TYPE usmd_ts_change_request,
      it_crequests         TYPE TABLE OF usmd_crequest.

TYPES: BEGIN OF ty_deleted_cr,
         crequest TYPE usmd_crequest,     " Change Request ID
         cr_type  TYPE usmd_crequest_type, " Change Request Type
       END OF ty_deleted_cr.

TYPES: ty_deleted_crs TYPE TABLE OF ty_deleted_cr WITH EMPTY KEY. " Table type for ALV
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  PARAMETERS model TYPE usmd_model MATCHCODE OBJECT usmd_shlp_rt_model1.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS cr_id FOR gs_dummy_crequest-usmd_crequest.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS creat_by FOR gs_dummy_crequest-usmd_created_by. "OBLIGATORY.
  SELECT-OPTIONS crtype FOR gs_dummy_crequest-usmd_creq_type. "OBLIGATORY.
  PARAMETERS period TYPE i DEFAULT 1.
SELECTION-SCREEN END OF BLOCK b03.

AT SELECTION-SCREEN.
  "Initial checks.
  DATA: lfd_usmd_model TYPE usmd_model.

  IF cr_id IS INITIAL AND creat_by[] IS INITIAL AND crtype[] IS INITIAL.

    IF lfd_usmd_model IS INITIAL.

      MESSAGE 'The Case that all parameters are empty is under development' TYPE 'I'.

      cl_usmd_crequest_util=>get_crequest_list(
        EXPORTING
          iv_model_low             = 'BP'
          iv_model_high            = 'ZT'
*        iv_creq_type             =
*        iv_datapacket_count      =
*        iv_maxsize               =
*        iv_cr_created_start_date =
*        iv_cr_created_end_date   =
*        iv_cr_priority           =
       IMPORTING
          et_crequest              = DATA(lit_crequest)
*        ev_done                  =
      ).
    ENDIF.
  ENDIF.

  IF cr_id IS NOT INITIAL AND ( creat_by[] IS NOT INITIAL OR crtype[] IS NOT INITIAL ).
    MESSAGE ID 'ZMDG_DQM_MESSAGES' TYPE 'E' NUMBER '002'.
    LEAVE PROGRAM.
  ENDIF.

  IF creat_by[] IS NOT INITIAL AND model IS INITIAL.
    MESSAGE ID 'ZMDG_DQM_MESSAGES' TYPE 'E' NUMBER '003'.
  ENDIF.
*  IF cr_id IS NOT INITIAL AND creat_by[] IS NOT INITIAL.
*    MESSAGE ID 'ZMDG_DQM_MESSAGES' TYPE 'S' NUMBER '001'.
*    LEAVE PROGRAM.
*  ENDIF.
*  IF crtype[] IS NOT INITIAL AND model IS NOT INITIAL.
*    LOOP AT crtype[] ASSIGNING FIELD-SYMBOL(<lwa_crtype>).
*
*    ENDLOOP.
*  ENDIF.


START-OF-SELECTION.
  TYPES creat_at_type TYPE RANGE OF timestamp.
  DATA creat_at TYPE creat_at_type.

  TYPES crequest_type TYPE RANGE OF usmd_crequest.
  DATA crequest TYPE crequest_type.

  TYPES crtype_tab_type TYPE RANGE OF usmd_crequest_type.
  DATA: crtype_tab TYPE crtype_tab_type.
  DATA: lfd_usmd_model TYPE usmd_model.

  DATA: lfd_period TYPE i.
  DATA:
    w_end_date       TYPE sy-datum,
    w_end_time       TYPE sy-uzeit,
    w_start_date     LIKE sy-datum,
    w_start_time     LIKE sy-uzeit,
    start_tms        TYPE timestamp,
    end_tms          TYPE timestamp,
    w_unit           LIKE t006-msehi VALUE 'H',
    w_duration       TYPE i,
    lri_creq_api     TYPE REF TO if_usmd_crequest_api,
    lit_messages     TYPE usmd_t_message,
    lfd_crequest2api TYPE usmd_crequest,
    lrc_log_service  TYPE REF TO zcl_med_mdg_msg_log_service.


  DATA       lv_withdrawn       TYPE abap_bool.             " Flag to indicate if a CR was withdrawn

  DATA: it_withdrawn_creqs TYPE ty_deleted_crs, " Table to store withdrawn CRs
        lv_cr_status       TYPE usmd_crequest_status, " Variable to store the CR status
        ls_deleted_cr      TYPE ty_deleted_cr.       " Structure for deleted CR


  w_end_date  = sy-datum.
  w_end_time  = sy-uzeit.

  w_duration = period * 24.

* calculate
  CALL FUNCTION 'START_TIME_DETERMINE'
    EXPORTING
      duration   = w_duration
      unit       = w_unit
    IMPORTING
      start_date = w_start_date
      start_time = w_start_time
    CHANGING
      end_date   = w_end_date
      end_time   = w_end_time.

  CALL FUNCTION 'IB_CONVERT_INTO_TIMESTAMP'
    EXPORTING
      i_datlo     = w_start_date
      i_timlo     = w_start_time
    IMPORTING
      e_timestamp = start_tms.

  end_tms = '20221001000000'.

  creat_at = VALUE creat_at_type( ( sign = 'I'
                                    option = 'BT'
                                    low = end_tms
                                    high = start_tms  ) ).

*  crequest = VALUE crequest_type( ( sign = 'I'
*                                    option = 'BT'
*                                    low = '1'
*                                    high = '100000' ) ).

  IF creat_by[] IS NOT INITIAL AND crtype[] IS INITIAL.
    LOOP AT creat_by ASSIGNING FIELD-SYMBOL(<lfd_created_by>).
      cl_usmd_crequest_util=>get_crequest_drafts(
        EXPORTING
          iv_model       = model                 " Data Model
          iv_uname       = <lfd_created_by>-low                 " User Name in User Master Record
          iv_single_obj  = abap_true                 " Change Request Contains One Entity Only
        RECEIVING
          rt_crequest_id = DATA(lit_crequest_drafts)                 " List of Change Requests
      ).
      DATA(lit_crequest2gt) = VALUE crequest_type( FOR <lfd_creq_id> IN lit_crequest_drafts ( sign = 'I'
                                option = 'EQ'
                                low = <lfd_creq_id>
                                high = ''
                                 ) ).

      APPEND LINES OF lit_crequest2gt TO crequest[].
    ENDLOOP.



    IF crequest[] IS NOT INITIAL.
      SELECT usmd_crequest FROM usmd120c INTO TABLE @DATA(lit_drafts_2be_deleted)
                                              WHERE usmd_crequest IN @crequest
                                              AND usmd_created_at IN @creat_at.
      INSERT LINES OF lit_drafts_2be_deleted INTO TABLE it_creqs_2bewithdraw.
    ENDIF.
    " lfd_period = period.
  ENDIF.

  IF creat_by[] IS INITIAL AND crtype[] IS NOT INITIAL.
    PERFORM get_cr_in_draft_by_crtype USING period creat_at crtype[].
  ENDIF.

  IF creat_by[] IS NOT INITIAL AND crtype[] IS NOT INITIAL.
    CLEAR lit_drafts_2be_deleted.
    SELECT usmd_crequest FROM usmd120c INTO TABLE @lit_drafts_2be_deleted
                                              WHERE usmd_created_by IN @creat_by[]
                                              AND usmd_created_at IN @creat_at
                                              AND usmd_creq_type IN @crtype[]
                                              AND usmd_draft_step NE ''.
    INSERT LINES OF lit_drafts_2be_deleted INTO TABLE it_creqs_2bewithdraw.
  ENDIF.

  IF cr_id[] IS NOT INITIAL.
    it_creqs_2bewithdraw = VALUE #( FOR <lfd_creq_id2> IN cr_id[] ( <lfd_creq_id2>-low ) ).
  ENDIF.


  DELETE ADJACENT DUPLICATES FROM it_creqs_2bewithdraw.


  DATA: lfd_note TYPE usmd_note.

  CONCATENATE 'This CR has been cancelled due to long draft time at' space w_end_date space w_end_time INTO lfd_note RESPECTING BLANKS.
  IF model IS NOT INITIAL.
    lfd_usmd_model = model.
  ENDIF.



  LOOP AT it_creqs_2bewithdraw ASSIGNING FIELD-SYMBOL(<lfd_crequest>).
    IF <lfd_crequest> IS ASSIGNED.

      lfd_crequest2api = |{ <lfd_crequest> ALPHA = OUT }|.

      " Retrieve the model if not already set
      IF lfd_usmd_model IS INITIAL.
        cl_usmd_crequest_util=>get_model_by_cr(
          EXPORTING
            i_crequest = <lfd_crequest>
          IMPORTING
            e_model    = lfd_usmd_model
        ).
      ENDIF.

      " Get the Change Request API instance
      cl_usmd_crequest_api=>get_instance(
        EXPORTING
          iv_crequest          = lfd_crequest2api
          iv_model_name        = lfd_usmd_model                " Data Model
        IMPORTING
          re_inst_crequest_api = lri_creq_api                  " Change Request API Interface
      ).

      " Enqueue the CR for processing
      lri_creq_api->enqueue_crequest(
        IMPORTING
          et_message = lit_messages                            " Log Interface Messages
      ).

      " Add a note to the CR
      lri_creq_api->add_note(
        EXPORTING
          iv_note    = lfd_note
        IMPORTING
          et_message = lit_messages                            " Messages
      ).

      " Withdraw the CR
      lri_creq_api->withdraw_crequest(
        IMPORTING
          et_message = lit_messages                            " Messages
      ).

      " Save the CR
      lri_creq_api->save_crequest(
        IMPORTING
          et_message = lit_messages                            " Messages
      ).

    ENDIF.

    " Free the API instance after processing
    FREE: lri_creq_api.
  ENDLOOP.

  " Commit the changes to the database
  COMMIT WORK AND WAIT.


  " Check the status of each CR after commit
  LOOP AT it_creqs_2bewithdraw ASSIGNING FIELD-SYMBOL(<lfd_cr_id>).
    " Check the status of the CR
    lv_cr_status = cl_usmd_crequest_util=>get_cr_status_by_cr(
      i_crequest = <lfd_cr_id>  " Change Request ID
    ).

    " If the CR status is '06' (Withdrawn), add it to the withdrawn CR table
    IF lv_cr_status = '06'. " '06' means Withdrawn
      ls_deleted_cr-crequest    = <lfd_cr_id>.
      ls_deleted_cr-cr_type  = cl_usmd_crequest_util=>get_cr_type_by_cr( i_crequest = <lfd_cr_id> ).
      APPEND ls_deleted_cr TO it_withdrawn_creqs.
    ENDIF.
  ENDLOOP.



  PERFORM show_deleted_crs
    USING
      it_withdrawn_creqs
    .

END-OF-SELECTION.


FORM get_cr_in_draft_by_crtype USING p_period TYPE i
                                p_creat_at TYPE creat_at_type
                                p_tab_crtype TYPE crtype_tab_type.




  SELECT usmd_crequest FROM usmd120c WHERE usmd_creq_type IN @p_tab_crtype
                                       AND usmd_draft_step NE ''
                                       AND usmd_created_at IN @p_creat_at
  INTO TABLE @DATA(lit_crequests).
  LOOP AT lit_crequests ASSIGNING FIELD-SYMBOL(<lfd_crequest>).
    IF line_exists( it_creqs_2bewithdraw[ <lfd_crequest>-usmd_crequest ] ).
      CONTINUE.
    ELSE.
      cl_usmd_crequest_util=>get_model_by_cr(
        EXPORTING
          i_crequest = <lfd_crequest>-usmd_crequest                  " Change Request
        IMPORTING
          e_model    = DATA(lfd_model_local)                 " Data Model of Change Request
      ).
      IF model IS NOT INITIAL AND lfd_model_local NE model.
        DATA(lfd_cr_type) = cl_usmd_crequest_util=>get_cr_type_by_cr(
                              i_crequest           = <lfd_crequest>-usmd_crequest                  " Change Request                " MDG Data Model for Access from SAP Standard Delivery
*                      iv_skip_read_archive =
                            ).
        MESSAGE ID 'ZMDG_DQM_MESSAGES' TYPE 'W' NUMBER '004' WITH lfd_cr_type.
        CONTINUE.
      ENDIF.
      INSERT <lfd_crequest>-usmd_crequest INTO TABLE it_creqs_2bewithdraw.
    ENDIF.
    " read table it_creqs_2bewithdraw WITH TABLE KEY <lfd_crequest> TRANSPORTING NO FIELDS.
  ENDLOOP.
  "INSERT LINES OF lit_crequests INTO table it_creqs_2bewithdraw.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SHOW_DELETED_CRS
*&---------------------------------------------------------------------*
*& This form displays deleted CRs and their descriptions using ALV.
*&---------------------------------------------------------------------*
FORM show_deleted_crs USING it_deleted_crs TYPE ty_deleted_crs.
  DATA: lo_alv     TYPE REF TO cl_salv_table,
        lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column. " Corrected type for lo_column

  " Check if there are deleted CRs
  IF it_deleted_crs IS INITIAL.
    MESSAGE 'No deleted CRs to display.' TYPE 'I'.
    RETURN.
  ENDIF.

  " Create the ALV object
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = it_deleted_crs
      ).
    CATCH cx_salv_msg INTO DATA(lo_error).
      MESSAGE lo_error->get_text( ) TYPE 'E'.
      RETURN.
  ENDTRY.

  " Set column settings
  lo_columns = lo_alv->get_columns( ).

  " Adjust CR_ID column
  TRY.
      lo_column = lo_columns->get_column( 'CREQUEST' ). " Adjusted to match ty_deleted_crs field name
      lo_column->set_short_text( 'CR ID' ).
      lo_column->set_medium_text( 'Change Request ID' ).
      lo_column->set_long_text( 'Change Request ID' ).
      lo_column->set_output_length( 20 ). " Set the width of the column
    CATCH cx_salv_not_found.
      MESSAGE 'Column CREQUEST not found.' TYPE 'E'.
  ENDTRY.
  " Adjust CR Type column
  TRY.
      lo_column = lo_columns->get_column( 'CR_TYPE' ).
      lo_column->set_short_text( 'CR Type' ).
      lo_column->set_medium_text( 'Change Request Type' ).
      lo_column->set_long_text( 'Change Request Type' ).
      lo_column->set_output_length( 30 ). " Set the width of the column
    CATCH cx_salv_not_found.
      MESSAGE 'Column CR_TYPE not found.' TYPE 'E'.
  ENDTRY.

  " Display ALV
  lo_alv->display( ).



ENDFORM.
