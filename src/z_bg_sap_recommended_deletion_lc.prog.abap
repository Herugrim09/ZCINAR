*&---------------------------------------------------------------------*
*& Include Z_BG_SAP_RECOMMENDED_DELETION_LC
*&---------------------------------------------------------------------*
*& Local classes.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Obsolete-object review click handler for the CL_SALV_TABLE
*& full-screen list built by LCL_OBSOLETE_ALV_FC (_ALV include).
*& Uses CL_SALV_TABLE's real checkbox mechanism: the CHK column is set
*& to cell type CHECKBOX_HOTSPOT, which fires the LINK_CLICK event on
*& click; the handler toggles CHK on the clicked row (via a data
*& reference to the caller's table, bound with GET REFERENCE OF right
*& before DISPLAY) and calls REFRESH so the toggle is visible
*& immediately. This replaced two non-working attempts: (1)
*& CL_SALV_COLUMN_TABLE has no accessible SET_EDITABLE, and (2)
*& GET_SELECTIONS( )->SET_SELECTION_MODE only enables click-to-highlight
*& row selection, not a visible checkbox (confirmed by testing - no
*& checkbox column appeared at all).
*&---------------------------------------------------------------------*
CLASS lcl_obsolete_handler DEFINITION.
  PUBLIC SECTION.
    DATA mr_rows TYPE REF TO ty_obsolete_row_tab.
    DATA mo_alv  TYPE REF TO cl_salv_table.
    METHODS on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
    METHODS on_added_function FOR EVENT added_function OF cl_salv_events_table
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_obsolete_handler IMPLEMENTATION.
  METHOD on_link_click.
    CHECK column = 'CHK'.
    FIELD-SYMBOLS <row> TYPE ty_obsolete_row.
    READ TABLE mr_rows->* ASSIGNING <row> INDEX row.
    CHECK sy-subrc = 0.
    IF <row>-chk = abap_true.
      CLEAR <row>-chk.
    ELSE.
      <row>-chk = abap_true.
    ENDIF.
    mo_alv->refresh( ).
  ENDMETHOD.

  METHOD on_added_function.
* Custom toolbar buttons "Select All" / "Deselect All", wired to our
* own CHK column via the ZOBSDEL_SCR9000 PF-STATUS (see the setup
* comment in LCL_OBSOLETE_ALV_FC, _ALV include) - the ALV's native
* Select All/Deselect All toolbar functions only affect its own
* internal row-selection state, never a custom checkbox column, so
* this is the only way to make bulk-check/uncheck buttons actually
* flip CHK.
    CASE e_salv_function.
      WHEN 'SELALL'.
        FIELD-SYMBOLS <row_all> TYPE ty_obsolete_row.
        LOOP AT mr_rows->* ASSIGNING <row_all>.
          <row_all>-chk = abap_true.
        ENDLOOP.
        mo_alv->refresh( ).
      WHEN 'DESELALL'.
        FIELD-SYMBOLS <row_none> TYPE ty_obsolete_row.
        LOOP AT mr_rows->* ASSIGNING <row_none>.
          CLEAR <row_none>-chk.
        ENDLOOP.
        mo_alv->refresh( ).
      WHEN 'CONF'.
* CONF ("Continue") is a custom function, unlike BACK/EXIT/CANC which
* CL_SALV_TABLE's own framework recognizes and closes automatically -
* for any other function code it just fires this event and otherwise
* does nothing, so without this the screen never closed and control
* never returned to the code after DISPLAY( ). LEAVE TO SCREEN 0 ends
* the ALV's own internal dynpro the same way BACK/EXIT/CANC do,
* handing control back right after the DISPLAY( ) call.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& F4 value-help helper - a small, fully self-contained class (no
*& DEFINITION/IMPLEMENTATION split needed) with no dependency on
*& anything from _ALV, unlike the main LCL_OBSOLETE_DELETION class
*& (_LOGIC include) - that is precisely why F4 help lives on its own
*& class instead of on LCL_OBSOLETE_DELETION: keeping it separate means
*& _DISPL only ever needs to know about this tiny class, so
*& LCL_OBSOLETE_DELETION itself (whose RUN method depends on
*& LCL_OBSOLETE_ALV_FC, defined later in _ALV) never has to be visible
*& before _DISPL, and can live entirely in _LOGIC - DEFINITION and
*& IMPLEMENTATION together, no split.
*&
*& F4_ENTITY/F4_EDITION are plain instance methods (no static/
*& CLASS-METHODS anywhere in this program), called via a throwaway NEW
*& instance from _DISPL, and read SY-REPID/SY-DYNNR directly same as
*& the FORMs they replace. F4_EDITION takes IV_MODEL as a parameter
*& (rather than reading PARAMETER P_MODEL directly) purely because
*& this class lives in _LC, before _DISPL declares P_MODEL - passing it
*& in from the _DISPL call site avoids that forward reference.
*&---------------------------------------------------------------------*
CLASS lcl_f4_helper DEFINITION.
  PUBLIC SECTION.
    METHODS f4_entity.
    METHODS f4_edition
      IMPORTING iv_model TYPE usmd_model.
ENDCLASS.

CLASS lcl_f4_helper IMPLEMENTATION.
  METHOD f4_entity.
    TYPES: BEGIN OF ty_val,
             usmd_entity TYPE usmd_entity,
           END OF ty_val.
    DATA lt_vals TYPE STANDARD TABLE OF ty_val WITH EMPTY KEY.

    SELECT usmd_entity
      FROM usmd0020
      WHERE usmd_model   = '0G'
        AND usmd_objstat = 'A'
      ORDER BY usmd_entity
      INTO CORRESPONDING FIELDS OF TABLE @lt_vals.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'USMD_ENTITY'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'P_ENTITY'
        value_org   = 'S'
      TABLES
        value_tab   = lt_vals
      EXCEPTIONS
        OTHERS      = 0.
  ENDMETHOD.

  METHOD f4_edition.
    TYPES: BEGIN OF ty_val,
             usmd_edition TYPE usmd_edition,
           END OF ty_val.
    DATA lt_vals  TYPE STANDARD TABLE OF ty_val WITH EMPTY KEY.
    DATA lv_etype TYPE usmd020c-usmd_edtn_type.

    CONCATENATE iv_model '_ALL' INTO lv_etype.

    SELECT usmd_edition
      FROM usmd020c
      WHERE usmd_edtn_type = @lv_etype
      ORDER BY usmd_edition
      INTO CORRESPONDING FIELDS OF TABLE @lt_vals.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'USMD_EDITION'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'P_EDTN'
        value_org   = 'S'
      TABLES
        value_tab   = lt_vals
      EXCEPTIONS
        OTHERS      = 0.
  ENDMETHOD.
ENDCLASS.
