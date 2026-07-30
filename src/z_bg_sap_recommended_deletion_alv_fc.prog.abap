*&---------------------------------------------------------------------*
*& Include Z_BG_SAP_RECOMMENDED_DELETION_ALV_FC
*&---------------------------------------------------------------------*
*& Obsolete-object review list. Self-contained (DEFINITION and
*& IMPLEMENTATION together) since nothing calls this class before this
*& include is reached (only _LOGIC does, which comes after _ALV_FC).
*&
*& Displayed as a full-screen ALV (SET_SCREEN_STATUS with PF-STATUS
*& ZOBSDEL_SCR9000, then DISPLAY( ) with no SET_SCREEN_POPUP call) -
*& not a small modal dialog box. Requires PF-STATUS ZOBSDEL_SCR9000 to
*& exist in this program (Menu Painter design-time object - created
*& manually, cannot be created by editing this source), containing
*& standard functions BACK/EXIT/CANC plus custom pushbuttons SELALL
*& ("Select All"), DESELALL ("Deselect All") and CONF ("Continue") -
*& all handled by LCL_OBSOLETE_HANDLER (_LC include).
*&---------------------------------------------------------------------*
CLASS lcl_obsolete_alv_fc DEFINITION.
  PUBLIC SECTION.
    METHODS show_and_get_selected_ids
      IMPORTING iv_model               TYPE usmd_model
                iv_entity              TYPE usmd_entity
                it_obsolete_ids        TYPE string_table
                iv_txt_tabname         TYPE tabname16
                iv_entity_field        TYPE string
                iv_edtn_number         TYPE usmd020c-usmd_edtn_number
      RETURNING VALUE(rv_id_in_clause) TYPE string.

  PRIVATE SECTION.
    METHODS build_rows
      IMPORTING iv_model        TYPE usmd_model
                iv_entity       TYPE usmd_entity
                it_obsolete_ids TYPE string_table
                iv_txt_tabname  TYPE tabname16
                iv_entity_field TYPE string
                iv_edtn_number  TYPE usmd020c-usmd_edtn_number
      RETURNING VALUE(rt_rows)  TYPE ty_obsolete_row_tab.
ENDCLASS.

CLASS lcl_obsolete_alv_fc IMPLEMENTATION.
  METHOD build_rows.
* Description comes from the text table's TXTSH field (same field
* name on every entity's text table), matched by the same dynamic ID
* field + edition (the text table does not carry USMD_OBS_TCK, so it
* cannot be filtered by obsolete status itself - only used to look up
* the description for IDs already known to be obsolete).
    DATA lv_description     TYPE string.
    DATA lv_txt_error_shown TYPE abap_bool.

    IF iv_txt_tabname IS INITIAL.
      WRITE: |No physical text table found for TXT_{ iv_model }_{ iv_entity } - descriptions will be blank in the review list.|, /.
    ENDIF.

    LOOP AT it_obsolete_ids INTO DATA(lv_obsolete_id).
      CLEAR lv_description.

      IF iv_txt_tabname IS NOT INITIAL.
        DATA(lv_txt_where) = |{ iv_entity_field } eq '{ lv_obsolete_id }' and USMD_EDTN_NUMBER eq { iv_edtn_number }|.
        TRY.
            SELECT SINGLE txtsh FROM (iv_txt_tabname) WHERE (lv_txt_where)
              INTO @lv_description.
* SELECT SINGLE succeeding but leaving lv_description blank is normal
* when no row matches the filter (e.g. edition/language mismatch) -
* not an error, so it is not written here. Only a genuine dynamic-SQL
* failure (wrong field name on this table) is reported, and only once
* (not per obsolete ID) to avoid spamming the list.
          CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax INTO DATA(lx_txt_select).
            IF lv_txt_error_shown = abap_false.
              WRITE: |Could not read description from table { iv_txt_tabname } ({ lx_txt_select->get_text( ) }) - descriptions will be blank.|, /.
              lv_txt_error_shown = abap_true.
            ENDIF.
            CLEAR lv_description.
        ENDTRY.
      ENDIF.

      APPEND VALUE ty_obsolete_row( chk = abap_true id = lv_obsolete_id description = lv_description ) TO rt_rows.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_and_get_selected_ids.
* The row type (TY_OBSOLETE_ROW/_TAB, _TOP include) is a fixed, generic
* (chk, id, description) structure - not dynamically built per entity -
* since the object's own key field name (iv_entity_field, e.g.
* /1MD/0GCCTRG) changes per entity/model and building a dynamic ALV
* structure per entity is unnecessary extra complexity here; we just
* move the dynamically-read ID value into the generic "id" component
* instead.
    DATA(lt_obsolete_row) = build_rows(
      iv_model        = iv_model
      iv_entity       = iv_entity
      it_obsolete_ids = it_obsolete_ids
      iv_txt_tabname  = iv_txt_tabname
      iv_entity_field = iv_entity_field
      iv_edtn_number  = iv_edtn_number ).

* Default: treat all obsolete objects as selected, so if the list
* cannot be shown at all we still fall back to the pre-selection
* behaviour (delete everything obsolete) instead of silently deleting
* nothing.
    CLEAR rv_id_in_clause.
    LOOP AT it_obsolete_ids INTO DATA(lv_obsolete_id).
      IF rv_id_in_clause IS INITIAL.
        rv_id_in_clause = |'{ lv_obsolete_id }'|.
      ELSE.
        rv_id_in_clause = |{ rv_id_in_clause }, '{ lv_obsolete_id }'|.
      ENDIF.
    ENDLOOP.

    IF lt_obsolete_row IS NOT INITIAL.
      TRY.
          cl_salv_table=>factory(
            IMPORTING r_salv_table = DATA(lo_obsolete_alv)
            CHANGING  t_table      = lt_obsolete_row ).
        CATCH cx_salv_msg INTO DATA(lx_salv_msg).
          WRITE: |Could not display the detailed review list ({ lx_salv_msg->get_text( ) }) - proceeding with all obsolete records selected.|, /.
      ENDTRY.

      IF lo_obsolete_alv IS BOUND.
        lo_obsolete_alv->get_columns( )->set_optimize( abap_true ).

        DATA(lo_chk_column) = lo_obsolete_alv->get_columns( )->get_column( 'CHK' ).
        lo_chk_column->set_short_text( 'Del?' ).
        lo_chk_column->set_medium_text( 'Delete?' ).
        lo_chk_column->set_long_text( 'Delete this object?' ).
        CAST cl_salv_column_list( lo_chk_column )->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).

        DATA(lo_id_column) = lo_obsolete_alv->get_columns( )->get_column( 'ID' ).
        lo_id_column->set_short_text( 'ID' ).
        lo_id_column->set_medium_text( 'Identifier' ).
        lo_id_column->set_long_text( 'Identifier' ).

        DATA(lo_description_column) = lo_obsolete_alv->get_columns( )->get_column( 'DESCRIPTION' ).
        lo_description_column->set_short_text( 'Descr.' ).
        lo_description_column->set_medium_text( 'Description' ).
        lo_description_column->set_long_text( 'Description' ).

        DATA(lo_obsolete_handler) = NEW lcl_obsolete_handler( ).
        GET REFERENCE OF lt_obsolete_row INTO lo_obsolete_handler->mr_rows.
        lo_obsolete_handler->mo_alv = lo_obsolete_alv.
        SET HANDLER lo_obsolete_handler->on_link_click FOR lo_obsolete_alv->get_event( ).
        SET HANDLER lo_obsolete_handler->on_added_function FOR lo_obsolete_alv->get_event( ).

* Requires a PF-STATUS named ZOBSDEL_SCR9000 to exist in THIS program
* (created manually via SE38 -> Goto -> PF-Status, since GUI statuses
* are Menu Painter design-time objects and cannot be created by editing
* this source file). It must contain standard functions BACK/EXIT/CANC
* plus application toolbar function codes SELALL ("Select All"),
* DESELALL ("Deselect All") and CONF ("Continue"). Without it,
* SET_SCREEN_STATUS raises a screen-status-not-found runtime error, so
* it is wrapped in TRY/CATCH to fail gracefully (individual row
* checkboxes still work either way).
        TRY.
            lo_obsolete_alv->set_screen_status(
              pfstatus      = 'ZOBSDEL_SCR9000'
              report        = sy-repid
              set_functions = lo_obsolete_alv->c_functions_all ).
          CATCH cx_root INTO DATA(lx_salv_status).
            WRITE: |Could not load PF-STATUS ZOBSDEL_SCR9000 ({ lx_salv_status->get_text( ) }) - Back/Select All/Deselect All buttons unavailable; individual row checkboxes still work.|, /.
        ENDTRY.

* No SET_SCREEN_POPUP call here on purpose: that is what would turn
* this into a small modal dialog box. Leaving it out makes DISPLAY( )
* fill the whole screen instead, like a normal full-screen ALV list.
        lo_obsolete_alv->display( ).

        " display( ) only returns once the user leaves this screen
        " (BACK/EXIT/CANC/CONF, or any other way of ending it). The
        " handler toggled CHK directly on lt_obsolete_row's memory via
        " the GET REFERENCE OF binding above, so it already reflects
        " every click made while the list was open.
        CLEAR rv_id_in_clause.
        LOOP AT lt_obsolete_row INTO DATA(lv_selected_row) WHERE chk = abap_true.
          IF rv_id_in_clause IS INITIAL.
            rv_id_in_clause = |'{ lv_selected_row-id }'|.
          ELSE.
            rv_id_in_clause = |{ rv_id_in_clause }, '{ lv_selected_row-id }'|.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      WRITE: |No detailed obsolete-object list available for review; proceeding with all obsolete records selected.|, /.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
