*&---------------------------------------------------------------------*
*& Include Z_BG_SAP_RECOMMENDED_DELETION_LC
*&---------------------------------------------------------------------*
*& Local classes.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Obsolete-object review click handler for the CL_SALV_TABLE
*& full-screen list built by LCL_OBSOLETE_ALV_FC (_ALV_FC include).
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
* comment in LCL_OBSOLETE_ALV_FC, _ALV_FC include) - the ALV's native
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
*& CLASS DEFINITION only (no IMPLEMENTATION) for the main deletion
*& process. The IMPLEMENTATION is in the _LOGIC include, further down
*& the INCLUDE chain - but the DEFINITION has to live here, before
*& _DISPL, because _DISPL's AT SELECTION-SCREEN ON VALUE-REQUEST blocks
*& instantiate this class to call its F4 methods, and ABAP resolves
*& class definitions top-down within a program just like it does DATA
*& (a class used before its DEFINITION has been read is unknown to the
*& compiler). Splitting DEFINITION and IMPLEMENTATION across includes
*& is normal, ordinary ABAP - only the DEFINITION needs to precede use.
*&
*& No static/CLASS-METHODS are used anywhere: F4_ENTITY/F4_EDITION are
*& plain instance methods (called via a throwaway NEW instance from
*& _DISPL, since they take no parameters and read the PARAMETERS/
*& SY-REPID/SY-DYNNR directly, same as the FORMs they replace), and RUN
*& is likewise a plain instance method (called via a throwaway NEW
*& instance from _LOGIC's START-OF-SELECTION, with the gathered
*& selection-screen values passed in as IMPORTING parameters and stored
*& into instance attributes for the private helper methods to share).
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

    METHODS f4_entity.
    METHODS f4_edition.

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
