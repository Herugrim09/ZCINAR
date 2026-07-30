*&---------------------------------------------------------------------*
*& Include Z_BG_SAP_RECOMMENDED_DELETION_LOGIC
*&---------------------------------------------------------------------*
*& LCL_OBSOLETE_DELETION - the main deletion process - plus the
*& START-OF-SELECTION trigger. DEFINITION and IMPLEMENTATION live
*& together here, no split: nothing before this include needs to call
*& this class (F4 help is handled by the separate LCL_F4_HELPER in
*& _LC instead, precisely so that this class doesn't have to be), and
*& this class's own RUN_PRODUCTIVE_MODE needs LCL_OBSOLETE_ALV_FC
*& (_ALV include, included just before this one) - so _LOGIC is the
*& first point in the program where this class can be both declared
*& and used.
*&---------------------------------------------------------------------*
CLASS lcl_obsolete_deletion DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING iv_model  TYPE usmd_model
                iv_entity TYPE usmd_entity
                iv_edtn   TYPE usmd_edition
                iv_delall TYPE abap_bool
                iv_kattr1 TYPE name_feld
                iv_kval1  TYPE string
                iv_kattr2 TYPE name_feld
                iv_kval2  TYPE string
                iv_kattr3 TYPE name_feld
                iv_kval3  TYPE string
                iv_test   TYPE abap_bool.

  PRIVATE SECTION.
    DATA mv_model        TYPE usmd_model.
    DATA mv_entity       TYPE usmd_entity.
    DATA mv_edtn         TYPE usmd_edition.
    DATA mv_delall       TYPE abap_bool.
    DATA mv_kattr1       TYPE name_feld.
    DATA mv_kval1        TYPE string.
    DATA mv_kattr2       TYPE name_feld.
    DATA mv_kval2        TYPE string.
    DATA mv_kattr3       TYPE name_feld.
    DATA mv_kval3        TYPE string.
    DATA mv_test         TYPE abap_bool.
    DATA mv_edtn_number  TYPE usmd020c-usmd_edtn_number.
    DATA mt_tables       TYPE STANDARD TABLE OF tabname16 WITH EMPTY KEY.
    DATA mt_tck_tables   TYPE STANDARD TABLE OF tabname16 WITH EMPTY KEY.
    DATA mt_txt_tables   TYPE STANDARD TABLE OF tabname16 WITH EMPTY KEY.
    DATA mv_entity_field TYPE string.
    DATA mv_where        TYPE string.
    DATA mt_obsolete_ids TYPE string_table.
    DATA mv_id_in_clause TYPE string.

    METHODS resolve_edition_number.
    METHODS resolve_physical_tables.
    METHODS build_where_clause.
    METHODS resolve_obsolete_ids
      RETURNING VALUE(rv_ok) TYPE abap_bool.
    METHODS run_test_mode.
    METHODS run_productive_mode.
    METHODS build_table_where
      IMPORTING iv_tabname                     TYPE tabname16
                iv_restrict_check_table_by_ids TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_where)                TYPE string.
ENDCLASS.

CLASS lcl_obsolete_deletion IMPLEMENTATION.

  METHOD run.
    mv_model  = iv_model.
    mv_entity = iv_entity.
    mv_edtn   = iv_edtn.
    mv_delall = iv_delall.
    mv_kattr1 = iv_kattr1.
    mv_kval1  = iv_kval1.
    mv_kattr2 = iv_kattr2.
    mv_kval2  = iv_kval2.
    mv_kattr3 = iv_kattr3.
    mv_kval3  = iv_kval3.
    mv_test   = iv_test.

    resolve_edition_number( ).
    resolve_physical_tables( ).
    build_where_clause( ).

    IF resolve_obsolete_ids( ) = abap_false.
      RETURN.
    ENDIF.

    IF mv_test = abap_true.
      run_test_mode( ).
    ELSE.
      run_productive_mode( ).
    ENDIF.
  ENDMETHOD.

  METHOD resolve_edition_number.
*----------------------------------------------------------------------*
* Read edition number – physical tables use USMD_EDTN_NUMBER (NUMC),
* not USMD_EDITION. Do NOT put quotes around it in a dynamic WHERE.
*----------------------------------------------------------------------*
    SELECT SINGLE usmd_edtn_number
      FROM usmd020c
      INTO @mv_edtn_number
      WHERE usmd_edition = @mv_edtn.
  ENDMETHOD.

  METHOD resolve_physical_tables.
*----------------------------------------------------------------------*
* Resolve physical table names
*----------------------------------------------------------------------*
    " Check table and text table via MDG_GN_TGOBJ
    " Logical name pattern: <KIND>_<MODEL>_<ENTITY>
    "   TCK = check table (entity master data)
    "   TXT = text table  (entity descriptions)
    DATA lt_log_names TYPE string_table.
    APPEND |TCK_{ mv_model }_{ mv_entity }| TO lt_log_names.
    APPEND |TXT_{ mv_model }_{ mv_entity }| TO lt_log_names.

    LOOP AT lt_log_names INTO DATA(lv_log).
      SELECT physical_name
        FROM mdg_gn_tgobj
        WHERE logical_name = @lv_log
        INTO TABLE @DATA(lt_phys).

      LOOP AT lt_phys INTO DATA(lv_phys).
        APPEND lv_phys TO mt_tables.
      ENDLOOP.
    ENDLOOP.

* Track the check table's physical name(s) separately: the
* USMD_OBS_TCK obsolete-flag field only exists on the check table
* (TCK_<model>_<entity>), not on the text/hierarchy tables. This
* lets BUILD_TABLE_WHERE add the obsolete-flag condition only for
* that specific table.
    DATA(lv_log_tck) = |TCK_{ mv_model }_{ mv_entity }|.

    SELECT physical_name
      FROM mdg_gn_tgobj
      WHERE logical_name = @lv_log_tck
      INTO TABLE @mt_tck_tables.

* Track the text table's physical name too - needed to look up each
* obsolete object's description (field TXTSH) for the review list.
    DATA(lv_log_txt) = |TXT_{ mv_model }_{ mv_entity }|.

    SELECT physical_name
      FROM mdg_gn_tgobj
      WHERE logical_name = @lv_log_txt
      INTO TABLE @mt_txt_tables.

    " Hierarchy assignment table via MDG_MDF2011
    " TABLE_USAGE = '3' identifies hierarchy assignment tables.
    " Only group entities (e.g. CCTRG, PCTRG, FSIH) have one.
    " FIELDNAME = namespaced entity field: /1MD/<model><entity>
    mv_entity_field = |/1MD/{ mv_model }{ mv_entity }|.

    SELECT tabname
      FROM mdg_mdf2011
      WHERE fieldname   = @mv_entity_field
        AND table_usage = '3'
      INTO TABLE @DATA(lt_hry).

    LOOP AT lt_hry INTO DATA(lv_hry_tab).
      APPEND lv_hry_tab TO mt_tables.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_where_clause.
    mv_where = |USMD_EDTN_NUMBER eq { mv_edtn_number }|.
    IF mv_delall NE abap_true.
      IF mv_kattr1 IS NOT INITIAL AND mv_kval1 IS NOT INITIAL.
        mv_where = |{ mv_where } and { mv_kattr1 } eq '{ mv_kval1 }'|.
      ENDIF.
      IF mv_kattr2 IS NOT INITIAL AND mv_kval2 IS NOT INITIAL.
        mv_where = |{ mv_where } and { mv_kattr2 } eq '{ mv_kval2 }'|.
      ENDIF.
      IF mv_kattr3 IS NOT INITIAL AND mv_kval3 IS NOT INITIAL.
        mv_where = |{ mv_where } and { mv_kattr3 } eq '{ mv_kval3 }'|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD resolve_obsolete_ids.
* Determine which object IDs are actually flagged obsolete in the
* check table (USMD_OBS_TCK = 'X'). This is the ONLY table that
* carries the flag - text/hierarchy tables don't - so every other
* table's COUNT/DELETE must be restricted to these specific IDs via
* MV_ID_IN_CLAUSE, not just the edition number.
    rv_ok = abap_false.

    IF mt_tck_tables IS NOT INITIAL.
      READ TABLE mt_tck_tables INDEX 1 INTO DATA(lv_tck_tabname).
      DATA(lv_tck_obsolete_where) = |{ mv_where } and USMD_OBS_TCK eq 'X'|.

      TRY.
          SELECT (mv_entity_field) FROM (lv_tck_tabname) WHERE (lv_tck_obsolete_where)
            INTO TABLE @mt_obsolete_ids.
        CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax INTO DATA(lx_tck_select).
          WRITE: |Could not read obsolete object IDs ({ lx_tck_select->get_text( ) }) - aborting, nothing deleted.|, /.
          RETURN.
      ENDTRY.

      LOOP AT mt_obsolete_ids INTO DATA(lv_id_for_in).
        IF mv_id_in_clause IS INITIAL.
          mv_id_in_clause = |'{ lv_id_for_in }'|.
        ELSE.
          mv_id_in_clause = |{ mv_id_in_clause }, '{ lv_id_for_in }'|.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF mt_obsolete_ids IS INITIAL.
      WRITE: |No obsolete (USMD_OBS_TCK = 'X') records found for entity { mv_entity }, edition { mv_edtn }.|, /.
      RETURN.
    ENDIF.

    rv_ok = abap_true.
  ENDMETHOD.

  METHOD build_table_where.
* Only the check table carries USMD_OBS_TCK - restrict it to
* obsolete-flagged rows; every other table is restricted to
* MV_ID_IN_CLAUSE, not just the edition number. IV_RESTRICT_CHECK_TABLE_BY_IDS
* additionally restricts the check table's rows to MV_ID_IN_CLAUSE too -
* test mode passes ABAP_FALSE here (no review/selection step happens in
* test mode, so the check table's WHERE stays flag-only, matching the
* original behaviour), productive mode's preview/DELETE use the default
* ABAP_TRUE (after the review list has narrowed MV_ID_IN_CLAUSE down to
* the user-selected subset).
    READ TABLE mt_tck_tables TRANSPORTING NO FIELDS WITH KEY table_line = iv_tabname.
    IF sy-subrc = 0.
      rv_where = |{ mv_where } and USMD_OBS_TCK eq 'X'|.
      IF iv_restrict_check_table_by_ids = abap_true.
        rv_where = |{ rv_where } and { mv_entity_field } in ( { mv_id_in_clause } )|.
      ENDIF.
    ELSE.
      rv_where = |{ mv_where } and { mv_entity_field } in ( { mv_id_in_clause } )|.
    ENDIF.
  ENDMETHOD.

  METHOD run_test_mode.
* Example usage – test mode (count only):
    LOOP AT mt_tables INTO DATA(lv_tabname).
      DATA(lv_table_where) = build_table_where(
        iv_tabname                     = lv_tabname
        iv_restrict_check_table_by_ids = abap_false ).

      TRY.
          SELECT COUNT(*) FROM (lv_tabname) WHERE (lv_table_where)
            INTO @DATA(lv_cnt).
          WRITE: |{ lv_cnt } entries found in table { lv_tabname } and will be deleted | , /.
        CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax.
          RETURN.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_productive_mode.
* Example usage – productive mode (delete):
    DATA lv_txt_tabname TYPE tabname16.
    READ TABLE mt_txt_tables INDEX 1 INTO lv_txt_tabname.

    mv_id_in_clause = NEW lcl_obsolete_alv_fc( )->show_and_get_selected_ids(
      iv_model        = mv_model
      iv_entity       = mv_entity
      it_obsolete_ids = mt_obsolete_ids
      iv_txt_tabname  = lv_txt_tabname
      iv_entity_field = mv_entity_field
      iv_edtn_number  = mv_edtn_number ).

    IF mv_id_in_clause IS INITIAL.
      WRITE: |No objects were selected for deletion - aborting, nothing deleted.|, /.
      RETURN.
    ENDIF.

* Do a dry-run COUNT per table (same per-table WHERE as the actual
* DELETE below - check table restricted by the obsolete flag AND the
* user-selected ID list, every other table restricted to the same
* selected IDs via MV_ID_IN_CLAUSE) and show the result to the user in
* a confirmation dialog. Deletion only proceeds if the user explicitly
* confirms.
    DATA lv_confirm_text TYPE string.
    DATA lv_total_cnt    TYPE i.
    DATA lv_answer       TYPE c LENGTH 1.
    DATA(lv_nl)          = cl_abap_char_utilities=>newline.

    LOOP AT mt_tables INTO DATA(lv_preview_tabname).
      DATA(lv_preview_where) = build_table_where( iv_tabname = lv_preview_tabname ).

      TRY.
          SELECT COUNT(*) FROM (lv_preview_tabname) WHERE (lv_preview_where)
            INTO @DATA(lv_preview_cnt).
        CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax.
          CONTINUE.
      ENDTRY.

      IF lv_preview_cnt > 0.
        lv_total_cnt    = lv_total_cnt + lv_preview_cnt.
        lv_confirm_text = |{ lv_confirm_text }{ lv_preview_tabname }: { lv_preview_cnt } record(s){ lv_nl }|.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar               = 'Confirm Deletion'
        text_question          = |The following { lv_total_cnt } obsolete record(s) will be permanently deleted:{ lv_nl }{ lv_confirm_text }{ lv_nl }Do you want to continue?|
        text_button_1          = 'Yes, Delete'
        text_button_2          = 'No, Cancel'
        default_button         = '2'
        display_cancel_button  = abap_false
      IMPORTING
        answer                 = lv_answer.

    IF lv_answer <> '1'.
      WRITE: |Deletion cancelled by user.|, /.
      RETURN.
    ENDIF.

* Same restriction as the preview COUNT above: the check table is
* restricted to USMD_OBS_TCK = 'X' AND the user-selected ID list,
* every other table (text, hierarchy) is restricted to the same
* selected IDs via MV_ID_IN_CLAUSE - NOT just the edition number - so
* productive mode only ever removes rows tied to an object that was
* both already marked obsolete AND left checked in the review list.
    LOOP AT mt_tables INTO DATA(lv_tabname).
      DATA(lv_table_where) = build_table_where( iv_tabname = lv_tabname ).

      TRY.
          DELETE FROM (lv_tabname) WHERE (lv_table_where).
          COMMIT WORK AND WAIT.
          WRITE: |{ lv_table_where } has been deleted from table { lv_tabname }  |, /.
        CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
*--------------------------------------------------------------------*
* Input parameters - gathered from the selection screen (_DISPL
* include) and handed to a throwaway LCL_OBSOLETE_DELETION instance.
*--------------------------------------------------------------------*
  NEW lcl_obsolete_deletion( )->run(
    iv_model  = p_model
    iv_entity = p_entity
    iv_edtn   = p_edtn
    iv_delall = p_delall
    iv_kattr1 = p_kattr1
    iv_kval1  = p_kval1
    iv_kattr2 = p_kattr2
    iv_kval2  = p_kval2
    iv_kattr3 = p_kattr3
    iv_kval3  = p_kval3
    iv_test   = p_test ).
