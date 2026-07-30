*&---------------------------------------------------------------------*
*& Report Z_BG_SAP_RECOMMENDED_DELETION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_bg_sap_recommended_deletion.

*&---------------------------------------------------------------------*
*& Obsolete-object review screen (dynpro 9000). Declared at report
*& (global) level - not inside START-OF-SELECTION - because MODULE/
*& ENDMODULE cannot be nested inside an event block, and the PBO/PAI
*& modules need to reach GT_OBSOLETE_ROW independent of where the
*& productive-mode logic that fills it lives.
*&
*& Screen 9000 shows every obsolete object with a real, editable
*& checkbox column (CHK), backed by CL_GUI_ALV_GRID's native EDIT +
*& CHECKBOX fieldcatalog attributes - no hotspot/event-handler hack
*& needed (unlike the CL_SALV_TABLE popup this replaced, which had no
*& accessible SET_EDITABLE and had to fake a checkbox via
*& CHECKBOX_HOTSPOT cell type + a LINK_CLICK event handler).
*&
*& REQUIRES MANUAL SETUP IN THE SAP SYSTEM (Screen Painter / Menu
*& Painter design-time objects - cannot be created by editing this
*& source file):
*&   1. Screen 9000, program Z_BG_SAP_RECOMMENDED_DELETION:
*&        - Screen type: Normal screen (NOT Modal dialog box) - this is
*&          what makes it a real screen instead of a popup.
*&        - One Custom Control element named CUSTOM_CTRL, sized to
*&          fill most of the screen (hosts the ALV Grid).
*&        - Flow logic:
*&            PROCESS BEFORE OUTPUT.
*&              MODULE status_9000.
*&            PROCESS AFTER INPUT.
*&              MODULE user_command_9000.
*&   2. PF-STATUS ZOBSDEL_SCR9000, same program:
*&        - Standard functions BACK, EXIT, CANC (the status's own
*&          Back/Exit/Cancel buttons).
*&        - Custom pushbuttons CONF ("Continue"), SELALL ("Select
*&          All"), DESELALL ("Deselect All").
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_obsolete_row,
         chk         TYPE c LENGTH 1,
         id          TYPE string,
         description TYPE string,
       END OF ty_obsolete_row.
TYPES ty_obsolete_row_tab TYPE STANDARD TABLE OF ty_obsolete_row WITH EMPTY KEY.

DATA go_container    TYPE REF TO cl_gui_custom_container.
DATA go_grid         TYPE REF TO cl_gui_alv_grid.
DATA gt_obsolete_row TYPE ty_obsolete_row_tab.

*&---------------------------------------------------------------------*
*& Code snippet: Resolve physical staging table names for a given
*& MDG entity and edition.
*&
*& HOW TO USE THIS SNIPPET
*& -----------------------
*& 1. Set the input variables lv_model, lv_edtn and lv_entity.
*&
*& 2. The snippet reads USMD_EDTN_NUMBER from USMD020C.
*&    The physical staging tables filter by USMD_EDTN_NUMBER (NUMC 6),
*&    NOT by USMD_EDITION (CHAR 10). USMD_EDITION only exists in the
*&    generated views, not in the underlying physical tables.
*&
*& 3. lt_tables is populated with the physical DDIC table names.
*&    Use these as targets for your DELETE statement:
*&      DELETE FROM (<lv_tabname>) WHERE (<lv_where>).
*&      COMMIT WORK AND WAIT.
*&    Always run a COUNT query first to verify record counts before
*&    executing the DELETE.
*&
*& 4. Wrap each SELECT/DELETE in TRY...CATCH for:
*&      CX_SY_DYNAMIC_OSQL_SEMANTICS and CX_SY_DYNAMIC_OSQL_SYNTAX
*&    to gracefully handle tables that do not carry USMD_EDTN_NUMBER.
*&
*& DEPENDENT ENTITIES
*& ------------------
*& Run this snippet separately for each related entity. Example:
*&   CCTR  -> also process CCTRG (group), CCTRH (hierarchy)
*&   PCTR  -> also process PCTRG, PCTRH
*&   FSI   -> also process FSI, FSIH
*& Find dependents via USMD0021:
*&   SELECT usmd_entity FROM usmd0021
*&     WHERE usmd_model = lv_model AND usmd_objstat = 'A'
*&       AND entity_foreign = lv_entity.
*& The USMD_ENTITY values (e.g. CCTRG) are the group entities.
*& Their ENTITY_FOREIGN entries give further dependents (e.g. CCTRH).
*&
*& AUTHORISATION
*& -------------
*& S_TABU_DIS or S_TABU_NAM with activity 06 (Delete) is required
*& for the resolved /1MD/MD* and /SMD/MD* target tables.
*&---------------------------------------------------------------------*
*& CHANGE LOG
*& ----------
*& [2026-07-29] Added USMD_OBS_TCK (obsolete flag) guard.
*&   The check table (TCK_<model>_<entity>) carries a USMD_OBS_TCK
*&   flag. Both test-mode counting and the productive DELETE now add
*&   "AND USMD_OBS_TCK eq 'X'" to the WHERE clause, but only for the
*&   check table's physical table (tracked via lt_tck_tables) - rows
*&   matching the filter that are NOT flagged obsolete are silently
*&   left alone; only already-obsoleted rows get deleted/counted.
*&   TXT/hierarchy tables are unaffected since they don't carry this
*&   field. See lt_tck_tables and the "obsolete-flag restriction"
*&   comment blocks below.
*&   (A hierarchy-node protection guard was added and then reworked
*&   on this date too, but was decided to be unnecessary - the
*&   obsolete-flag guard above was judged sufficient - and has been
*&   removed again.)
*& [2026-07-29] Added a confirmation dialog before productive delete.
*&   Right before the DELETE loop (productive mode only, not test
*&   mode), the program now does a dry-run COUNT per table (with the
*&   same obsolete-flag-restricted WHERE used for the real DELETE),
*&   shows the resulting per-table/total record counts to the user
*&   via POPUP_TO_CONFIRM, and only proceeds with the actual DELETE
*&   loop if the user clicks "Yes, Delete". Cancelling, clicking
*&   "No", or there being nothing obsolete to delete all abort with
*&   a message and no data is touched.
*& [2026-07-29] Added an ID+Description review popup before the
*&   Yes/No confirmation. Reads the obsolete object IDs from the
*&   check table (dynamic key field lv_entity_field) and looks up
*&   each one's description (field TXTSH) from the text table (new
*&   lt_txt_tables), then displays them in a scrollable CL_SALV_TABLE
*&   popup using a fixed generic (ID, Description) row type - not a
*&   dynamically-built structure per entity - before the existing
*&   POPUP_TO_CONFIRM Yes/No dialog runs.
*& [2026-07-29] Fixed two rounds of syntax/runtime issues in the
*&   above: (1) "SELECT (dyn_field) ... INTO TABLE @DATA(...)" does
*&   not compile when both the column list and FROM are dynamic -
*&   fixed by pre-declaring lt_obsolete_ids with a concrete type
*&   instead of inline. (2) The follow-up attempt to use
*&   "SELECT * ... INTO TABLE @DATA(...)" with a dynamic FROM hit the
*&   exact same inline-declaration restriction (not exempt just
*&   because the projection is "*") and had to be reverted. The
*&   working version selects the single dynamic column
*&   (lv_entity_field) directly into the pre-declared lt_obsolete_ids.
*&   Every failure/empty-result path here now WRITEs the real error
*&   text instead of silently doing nothing, so future issues are
*&   visible instead of just making the review popup disappear.
*& [2026-07-29] Fixed a count/delete mismatch: the confirmation
*&   popup's total (and the productive DELETE) restricted only the
*&   check table's rows to USMD_OBS_TCK = 'X'; the text and hierarchy
*&   tables were filtered by edition number alone, since they don't
*&   carry the obsolete flag themselves. That let an edition with
*&   zero obsolete records still show a nonzero total (e.g. "44") and
*&   delete unrelated text/hierarchy rows that merely shared the
*&   edition, while the ID review popup (built only from the check
*&   table) correctly showed nothing. The obsolete object IDs are now
*&   resolved once up front (lt_obsolete_ids / lv_id_in_clause) and
*&   every non-check table's COUNT and DELETE is restricted to those
*&   IDs too. If no IDs are found, the report now stops immediately
*&   (before test-mode counting or the productive popup) with
*&   "No obsolete (USMD_OBS_TCK = 'X') records found ..." - previously
*&   test mode had this same mismatch and would show misleading counts
*&   even with nothing obsolete.
*& [2026-07-30] Made the review ALV interactive: every row now shows a
*&   real, clickable "Delete?" checkbox (CHK column, pre-checked for
*&   all rows), so the user can uncheck any obsolete object they do
*&   NOT want deleted. Implemented with CL_SALV_TABLE's cell type
*&   CHECKBOX_HOTSPOT plus a LINK_CLICK event handler
*&   (LCL_OBSOLETE_HANDLER, declared at report level together with
*&   TY_OBSOLETE_ROW/_TAB) that toggles CHK on the clicked row and
*&   calls REFRESH( ). This took three attempts: (1) a custom "selected"
*&   column via CL_SALV_COLUMN_TABLE=>SET_CELL_TYPE/SET_EDITABLE -
*&   those methods don't exist/aren't accessible there and the system
*&   rejected the syntax; (2) GET_SELECTIONS( )->SET_SELECTION_MODE( ...
*&   =>multiple ), which only enables click-to-highlight row selection,
*&   not a visible checkbox (confirmed by testing - no checkbox column
*&   appeared at all); (3) the classic FM REUSE_ALV_POPUP_TO_SELECT,
*&   which does render a real checkbox but was dropped since it isn't
*&   part of the modern/cloud-compatible ALV API surface. The ALV popup
*&   is shown before the count preview/confirmation instead of after,
*&   so lv_id_in_clause is rebuilt from only the still-checked rows
*&   once the popup closes (the handler mutates lt_obsolete_row's own
*&   memory via a GET REFERENCE OF binding, so no read-back call is
*&   needed), and everything downstream (preview COUNT, the Yes/No
*&   total, and the productive DELETE) is restricted to that
*&   user-selected subset. If nothing is left checked, the report stops
*&   with "No objects were selected for deletion". This also fixed a
*&   related gap: the check table's WHERE only ever filtered on
*&   USMD_OBS_TCK = 'X', never on the ID list, so it would have deleted
*&   every obsolete row in the check table regardless of what the user
*&   unchecked while other tables respected the selection - the check
*&   table's WHERE now additionally requires the entity field to be IN
*&   the selected-ID list.
*& [2026-07-30] Added ID/Description column headers (were blank) and
*&   real "Select All"/"Deselect All" toolbar buttons for the CHK
*&   column. The ALV's OWN built-in Select All/Deselect All buttons
*&   only ever affect its internal row-selection state, never a custom
*&   checkbox column - clicking them visibly did nothing to CHK.
*&   CL_SALV_TABLE has no equivalent of the classic ALV Grid's
*&   LAYOUT-BOX_FIELDNAME (which natively wires a checkbox field to
*&   those buttons - the reason REUSE_ALV_POPUP_TO_SELECT/
*&   REUSE_ALV_GRID_DISPLAY get this for free), so two custom toolbar
*&   functions (SELALL/DESELALL) were added instead, requiring a
*&   PF-STATUS named ZOBSDEL_REVIEW in this program (a Menu Painter
*&   design-time object - created manually via SE38 -> Goto ->
*&   PF-Status, not by editing this source) wired via
*&   SET_SCREEN_STATUS and handled by the new ON_ADDED_FUNCTION method
*&   on LCL_OBSOLETE_HANDLER, which sets/clears CHK for every row and
*&   refreshes. If the PF-STATUS doesn't exist yet, SET_SCREEN_STATUS
*&   fails gracefully (caught, message written) and individual row
*&   checkboxes keep working regardless.
*& [2026-07-30] Replaced the CL_SALV_TABLE modal popup review with a
*&   real screen (dynpro 9000, normal screen type - not a dialog box),
*&   because the popup had no working Back/Cancel or OK/Continue
*&   function: closing it any way (including the window's own X)
*&   always fell through to "proceed with whatever's checked", with no
*&   way to abort the whole run. LCL_OBSOLETE_HANDLER and all
*&   CL_SALV_TABLE-specific popup code were removed; CL_GUI_ALV_GRID
*&   hosts the review list instead, in a Custom Control named
*&   CUSTOM_CTRL, with CHK as a real editable checkbox column
*&   (fieldcat EDIT + CHECKBOX) - no hotspot/event-handler hack needed.
*&   New PF-STATUS ZOBSDEL_SCR9000 (replaces ZOBSDEL_REVIEW) adds
*&   standard BACK/EXIT/CANC functions alongside the existing SELALL/
*&   DESELALL buttons and a new CONF ("Continue") button. BACK/EXIT/
*&   CANC do LEAVE TO SCREEN 1000 (back to the selection screen,
*&   nothing deleted); CONF does LEAVE SCREEN so control resumes right
*&   after CALL SCREEN 9000, where lv_id_in_clause is rebuilt from the
*&   checked rows exactly as before - the existing count-preview,
*&   POPUP_TO_CONFIRM Yes/No dialog, and DELETE loop are unchanged.
*&   See the setup comment above TY_OBSOLETE_ROW (report level) for
*&   the manual Screen Painter/Menu Painter steps this requires.
*& [2026-07-30] Fixed a runtime dump on BACK/EXIT/CANC: "Selection
*&   screen Z_BG_SAP_RECOMMENDED_DELETION 1000 was not called using
*&   CALL SELECTION-SCREEN". LEAVE TO SCREEN 1000 only works if the
*&   selection screen was entered via an explicit CALL SELECTION-SCREEN
*&   statement, which is not the case here - the runtime opens it
*&   automatically on execute (SE38/SA38). Replaced with
*&   SUBMIT z_bg_sap_recommended_deletion VIA SELECTION-SCREEN, passing
*&   every current parameter value via WITH so the selection screen
*&   reappears pre-filled instead of blank.
*& [2026-07-30] Fixed an activation syntax error: "Field P_MODEL is
*&   unknown" (and the same for the other PARAMETERS) at the SUBMIT ...
*&   WITH lines. MODULE status_9000/user_command_9000 were originally
*&   declared at the very top of the program, alongside TY_OBSOLETE_ROW
*&   - but ABAP resolves global names top-down within one program, so a
*&   MODULE positioned before PARAMETERS p_model etc. are declared
*&   cannot reference them yet. Moved both MODULEs down to just after
*&   the end of START-OF-SELECTION (before FORM f4_entity), i.e. after
*&   every PARAMETERS statement in the SELECTION-SCREEN block. The
*&   TY_OBSOLETE_ROW/_TAB types and GO_CONTAINER/GO_GRID/GT_OBSOLETE_ROW
*&   data stay at the top, since nothing there depends on the
*&   selection-screen parameters.
*&---------------------------------------------------------------------*

" -----------------------------------------------------------------------
" Selection screen
" -----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS p_model  TYPE usmd_model DEFAULT '0G' OBLIGATORY.
  PARAMETERS p_entity TYPE usmd_entity OBLIGATORY.
  PARAMETERS p_edtn   TYPE usmd_edition OBLIGATORY.
*--------------------------------------------------------------------*  START
*--------------------------------------------------------------------*
  PARAMETERS p_delall AS CHECKBOX.
  SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME.
    PARAMETERS p_kattr1   TYPE name_feld.
    PARAMETERS p_kval1   TYPE string.
  SELECTION-SCREEN END OF BLOCK bk1.
  SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME.
    PARAMETERS p_kattr2   TYPE name_feld.
    PARAMETERS p_kval2   TYPE string.
  SELECTION-SCREEN END OF BLOCK bk2.
  SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME.
    PARAMETERS p_kattr3   TYPE name_feld.
    PARAMETERS p_kval3   TYPE string.
  SELECTION-SCREEN END OF BLOCK bk3.
  PARAMETERS p_test AS CHECKBOX.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  %_p_model_%_app_%-text  = 'Data Model'.
  %_p_edtn_%_app_%-text   = 'Edition'.
  %_p_entity_%_app_%-text = 'Entity Type'.
*--------------------------------------------------------------------*  START
*--------------------------------------------------------------------*
  %_p_delall_%_app_%-text = 'Deletion without key input'.
  %_p_kattr1_%_app_%-text = 'Key Field Name 1'.
  %_p_kval1_%_app_%-text = 'Key Field Value 1'.
  %_p_kattr2_%_app_%-text = 'Key Field Name 2'.
  %_p_kval2_%_app_%-text = 'Key Field Value 2'.
  %_p_kattr3_%_app_%-text = 'Key Field Name 3'.
  %_p_kval3_%_app_%-text = 'Key Field Value 3'.
  %_p_test_%_app_%-text = 'Test Run'.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_edtn.
  PERFORM f4_edition.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_entity.
  PERFORM f4_entity.


START-OF-SELECTION.
*----------------------------------------------------------------------*
* Input variables – set these before running
*----------------------------------------------------------------------*
  DATA lv_model  TYPE usmd_model.                     " e.g. '0G'
  DATA lv_edtn   TYPE usmd_edition.       " e.g. 'UNITTEST'
  DATA lv_entity TYPE usmd_entity.                 " e.g. 'CCTR', 'CCTRG'
*--------------------------------------------------------------------*  START
*--------------------------------------------------------------------*
  DATA:
    lv_delall TYPE boolean,
    lv_kattr1 TYPE name_feld,
    lv_kval1  TYPE string,
    lv_kattr2 TYPE name_feld,
    lv_kval2  TYPE string,
    lv_kattr3 TYPE name_feld,
    lv_kval3  TYPE string.

  lv_delall = p_delall.
  lv_kattr1 = p_kattr1.
  lv_kval1  = p_kval1.
  lv_kattr2 = p_kattr2.
  lv_kval2  = p_kval2.
  lv_kattr3 = p_kattr3.
  lv_kval3  = p_kval3.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*

  lv_model = p_model.
  lv_entity = p_entity.
  lv_edtn = p_edtn.

*----------------------------------------------------------------------*
* Read edition number – physical tables use USMD_EDTN_NUMBER (NUMC),
* not USMD_EDITION. Do NOT put quotes around it in a dynamic WHERE.
*----------------------------------------------------------------------*
  DATA lv_edtn_number TYPE usmd020c-usmd_edtn_number.


  SELECT SINGLE usmd_edtn_number
    FROM usmd020c
    INTO @lv_edtn_number
    WHERE usmd_edition   = @lv_edtn.

*----------------------------------------------------------------------*
* Resolve physical table names
*----------------------------------------------------------------------*
  DATA lt_tables TYPE STANDARD TABLE OF tabname16 WITH EMPTY KEY.

  " Check table and text table via MDG_GN_TGOBJ
  " Logical name pattern: <KIND>_<MODEL>_<ENTITY>
  "   TCK = check table (entity master data)
  "   TXT = text table  (entity descriptions)
  DATA lt_log_names TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  APPEND |TCK_{ lv_model }_{ lv_entity }| TO lt_log_names.
  APPEND |TXT_{ lv_model }_{ lv_entity }| TO lt_log_names.


  LOOP AT lt_log_names INTO DATA(lv_log).
    SELECT physical_name
      FROM mdg_gn_tgobj
      WHERE logical_name = @lv_log
      INTO TABLE @DATA(lt_phys).

    LOOP AT lt_phys INTO DATA(lv_phys).
      APPEND lv_phys TO lt_tables.
    ENDLOOP.
  ENDLOOP.

*--------------------------------------------------------------------*  START [2026-07-29]
*--------------------------------------------------------------------*
* Track the check table's physical name(s) separately: the
* USMD_OBS_TCK obsolete-flag field only exists on the check table
* (TCK_<model>_<entity>), not on the text/hierarchy tables. This
* lets the WHERE-clause builder below add the obsolete-flag
* condition only for that specific table.
  DATA lt_tck_tables TYPE STANDARD TABLE OF tabname16 WITH EMPTY KEY.
  DATA(lv_log_tck) = |TCK_{ lv_model }_{ lv_entity }|.

  SELECT physical_name
    FROM mdg_gn_tgobj
    WHERE logical_name = @lv_log_tck
    INTO TABLE @lt_tck_tables.

* Track the text table's physical name too - needed to look up each
* obsolete object's description (field TXTSH) for the review popup.
  DATA lt_txt_tables TYPE STANDARD TABLE OF tabname16 WITH EMPTY KEY.
  DATA(lv_log_txt) = |TXT_{ lv_model }_{ lv_entity }|.

  SELECT physical_name
    FROM mdg_gn_tgobj
    WHERE logical_name = @lv_log_txt
    INTO TABLE @lt_txt_tables.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*

  " Hierarchy assignment table via MDG_MDF2011
  " TABLE_USAGE = '3' identifies hierarchy assignment tables.
  " Only group entities (e.g. CCTRG, PCTRG, FSIH) have one.
  " FIELDNAME = namespaced entity field: /1MD/<model><entity>
  DATA(lv_entity_field) = |/1MD/{ lv_model }{ lv_entity }|.

  SELECT tabname
    FROM mdg_mdf2011
    WHERE fieldname   = @lv_entity_field
      AND table_usage = '3'
    INTO TABLE @DATA(lt_hry).

  LOOP AT lt_hry INTO DATA(lv_hry_tab).
    APPEND lv_hry_tab TO lt_tables.
  ENDLOOP.

  "BREAK-POINT.

*--------------------------------------------------------------------*  START
*--------------------------------------------------------------------*
  DATA(lv_where) = |USMD_EDTN_NUMBER eq { lv_edtn_number }|.
  IF lv_delall NE abap_true.
    IF lv_kattr1 IS NOT INITIAL AND lv_kval1 IS NOT INITIAL.
      lv_where = |{ lv_where } and { lv_kattr1 } eq '{ lv_kval1 }'|.
    ENDIF.
    IF lv_kattr2 IS NOT INITIAL AND lv_kval2 IS NOT INITIAL.
      lv_where = |{ lv_where } and { lv_kattr2 } eq '{ lv_kval2 }'|.
    ENDIF.
    IF lv_kattr3 IS NOT INITIAL AND lv_kval3 IS NOT INITIAL.
      lv_where = |{ lv_where } and { lv_kattr3 } eq '{ lv_kval3 }'|.
    ENDIF.
  ENDIF.
*  DATA(lv_where) = |USMD_EDTN_NUMBER eq { lv_edtn_number } and /1MD/0GCCTR eq 'TESTA2'|.  " CCTR & DUMMY (999999)
*  DATA(lv_where) = |USMD_EDTN_NUMBER eq { lv_edtn_number }|.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*

*----------------------------------------------------------------------*
* lt_tables now contains all physical staging table names.
* lv_where contains the edition filter ready for use.
*--------------------------------------------------------------------*  START [2026-07-29]
*--------------------------------------------------------------------*
* Determine which object IDs are actually flagged obsolete in the
* check table (USMD_OBS_TCK = 'X'). This is the ONLY table that
* carries the flag - text/hierarchy tables don't - so every other
* table's COUNT/DELETE must be restricted to these specific IDs via
* lv_id_in_clause, not just the edition number. Previously text/
* hierarchy tables were filtered by edition alone, so an edition with
* zero obsolete records could still show a nonzero total and delete
* unrelated rows that merely shared the edition.
  DATA lt_obsolete_ids TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lv_id_in_clause TYPE string.

  IF lt_tck_tables IS NOT INITIAL.
    READ TABLE lt_tck_tables INDEX 1 INTO DATA(lv_tck_tabname).
    DATA(lv_tck_obsolete_where) = |{ lv_where } and USMD_OBS_TCK eq 'X'|.

    TRY.
        SELECT (lv_entity_field) FROM (lv_tck_tabname) WHERE (lv_tck_obsolete_where)
          INTO TABLE @lt_obsolete_ids.
      CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax INTO DATA(lx_tck_select).
        WRITE: |Could not read obsolete object IDs ({ lx_tck_select->get_text( ) }) - aborting, nothing deleted.|, /.
        RETURN.
    ENDTRY.

    LOOP AT lt_obsolete_ids INTO DATA(lv_id_for_in).
      IF lv_id_in_clause IS INITIAL.
        lv_id_in_clause = |'{ lv_id_for_in }'|.
      ELSE.
        lv_id_in_clause = |{ lv_id_in_clause }, '{ lv_id_for_in }'|.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lt_obsolete_ids IS INITIAL.
    WRITE: |No obsolete (USMD_OBS_TCK = 'X') records found for entity { lv_entity }, edition { lv_edtn }.|, /.
    RETURN.
  ENDIF.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*
*
* Example usage – test mode (count only):
  IF p_test = abap_true. "START - IF Statement added
    LOOP AT lt_tables INTO DATA(lv_tabname).
*--------------------------------------------------------------------*  START [2026-07-29]
*--------------------------------------------------------------------*
* Only the check table carries USMD_OBS_TCK - restrict counting there
* to obsolete-flagged rows; every other table is restricted to the
* same obsolete IDs via lv_id_in_clause (see above), not just edition.
      DATA(lv_table_where) = lv_where.
      READ TABLE lt_tck_tables TRANSPORTING NO FIELDS WITH KEY table_line = lv_tabname.
      IF sy-subrc = 0.
        lv_table_where = |{ lv_where } and USMD_OBS_TCK eq 'X'|.
      ELSE.
        lv_table_where = |{ lv_where } and { lv_entity_field } in ( { lv_id_in_clause } )|.
      ENDIF.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*
      TRY.
          SELECT COUNT(*) FROM (lv_tabname) WHERE (lv_table_where)
            INTO @DATA(lv_cnt).
*--------------------------------------------------------------------*  START
*--------------------------------------------------------------------*
          WRITE: |{ lv_cnt } entries found in table { lv_tabname } and will be deleted | , /.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*
        CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax.
*--------------------------------------------------------------------*  START
*--------------------------------------------------------------------*
          RETURN.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*
      ENDTRY.
    ENDLOOP.
  ELSE.
* Example usage – productive mode (delete):
*--------------------------------------------------------------------*  START [2026-07-30]
*--------------------------------------------------------------------*
* Show the actual obsolete objects (chk, ID, Description) on screen
* 9000 (see the module/setup comment at report level, above) instead
* of a modal popup - CHK is a real, editable checkbox column
* (CL_GUI_ALV_GRID fieldcat EDIT + CHECKBOX), pre-set to 'X' for every
* row before display, so the default is "delete everything obsolete"
* unless the user unchecks something.
* The row type (TY_OBSOLETE_ROW/_TAB, declared at report level) is a
* fixed, generic (chk, id, description) structure - not dynamically
* built per entity - since the object's own key field name
* (lv_entity_field, e.g. /1MD/0GCCTRG) changes per entity/model and
* building a dynamic ALV structure per entity is unnecessary extra
* complexity here; we just move the dynamically-read ID value into the
* generic "id" component instead.
* Description comes from the text table's TXTSH field (same field
* name on every entity's text table), matched by the same dynamic ID
* field + edition (the text table does not carry USMD_OBS_TCK, so it
* cannot be filtered by obsolete status itself - only used to look up
* the description for IDs already known to be obsolete).
* lt_obsolete_ids was already resolved above (needed there to build
* lv_id_in_clause) - reused here as-is instead of re-reading the check
* table a second time.
    CLEAR gt_obsolete_row.
    DATA lv_description TYPE string.

    READ TABLE lt_txt_tables INDEX 1 INTO DATA(lv_txt_tabname).

    LOOP AT lt_obsolete_ids INTO DATA(lv_obsolete_id).
      CLEAR lv_description.

      IF lv_txt_tabname IS NOT INITIAL.
        DATA(lv_txt_where) = |{ lv_entity_field } eq '{ lv_obsolete_id }' and USMD_EDTN_NUMBER eq { lv_edtn_number }|.
        TRY.
            SELECT SINGLE txtsh FROM (lv_txt_tabname) WHERE (lv_txt_where)
              INTO @lv_description.
          CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax.
            CLEAR lv_description.
        ENDTRY.
      ENDIF.

      APPEND VALUE ty_obsolete_row( chk = abap_true id = lv_obsolete_id description = lv_description ) TO gt_obsolete_row.
    ENDLOOP.

    CALL SCREEN 9000.

    " Reached only via the CONF ("Continue") button on screen 9000 -
    " BACK/EXIT/CANC jump straight back to the selection screen
    " (LEAVE TO SCREEN 1000 in MODULE user_command_9000) before this
    " line, so nothing below runs and no data is touched.
    CLEAR lv_id_in_clause.
    LOOP AT gt_obsolete_row INTO DATA(lv_selected_row) WHERE chk = abap_true.
      IF lv_id_in_clause IS INITIAL.
        lv_id_in_clause = |'{ lv_selected_row-id }'|.
      ELSE.
        lv_id_in_clause = |{ lv_id_in_clause }, '{ lv_selected_row-id }'|.
      ENDIF.
    ENDLOOP.

    IF lv_id_in_clause IS INITIAL.
      WRITE: |No objects were selected for deletion - aborting, nothing deleted.|, /.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*  START [2026-07-29]
*--------------------------------------------------------------------*
* Do a dry-run COUNT per table (same per-table WHERE as the actual
* DELETE below - check table restricted by the obsolete flag AND the
* user-selected ID list, every other table restricted to the same
* selected IDs via lv_id_in_clause) and show the result to the user in
* a confirmation dialog. Deletion only proceeds if the user explicitly
* confirms.
    DATA lv_confirm_text TYPE string.
    DATA lv_total_cnt    TYPE i.
    DATA lv_answer       TYPE c LENGTH 1.
    DATA(lv_nl)          = cl_abap_char_utilities=>newline.

    LOOP AT lt_tables INTO DATA(lv_preview_tabname).
      DATA(lv_preview_where) = lv_where.
      READ TABLE lt_tck_tables TRANSPORTING NO FIELDS WITH KEY table_line = lv_preview_tabname.
      IF sy-subrc = 0.
        lv_preview_where = |{ lv_where } and USMD_OBS_TCK eq 'X' and { lv_entity_field } in ( { lv_id_in_clause } )|.
      ELSE.
        lv_preview_where = |{ lv_where } and { lv_entity_field } in ( { lv_id_in_clause } )|.
      ENDIF.

      TRY.
          SELECT COUNT(*) FROM (lv_preview_tabname) WHERE (lv_preview_where)
            INTO @DATA(lv_preview_cnt).
        CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax.
          CONTINUE.
      ENDTRY.

      IF lv_preview_cnt > 0.
        lv_total_cnt     = lv_total_cnt + lv_preview_cnt.
        lv_confirm_text  = |{ lv_confirm_text }{ lv_preview_tabname }: { lv_preview_cnt } record(s){ lv_nl }|.
      ENDIF.
    ENDLOOP.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar             = 'Confirm Deletion'
        text_question        = |The following { lv_total_cnt } obsolete record(s) will be permanently deleted:{ lv_nl }{ lv_confirm_text }{ lv_nl }Do you want to continue?|
        text_button_1        = 'Yes, Delete'
        text_button_2        = 'No, Cancel'
        default_button       = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer               = lv_answer.

    IF lv_answer <> '1'.
      WRITE: |Deletion cancelled by user.|, /.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*
    LOOP AT lt_tables INTO lv_tabname.
*  LOOP AT lt_tables INTO DATA(lv_tabname).
*--------------------------------------------------------------------*  START [2026-07-29]
*--------------------------------------------------------------------*
* Same restriction as the preview COUNT above: the check table is
* restricted to USMD_OBS_TCK = 'X' AND the user-selected ID list,
* every other table (text, hierarchy) is restricted to the same
* selected IDs via lv_id_in_clause - NOT just the edition number - so
* productive mode only ever removes rows tied to an object that was
* both already marked obsolete AND left checked in the review popup.
      lv_table_where = lv_where.
      READ TABLE lt_tck_tables TRANSPORTING NO FIELDS WITH KEY table_line = lv_tabname.
      IF sy-subrc = 0.
        lv_table_where = |{ lv_where } and USMD_OBS_TCK eq 'X' and { lv_entity_field } in ( { lv_id_in_clause } )|.
      ELSE.
        lv_table_where = |{ lv_where } and { lv_entity_field } in ( { lv_id_in_clause } )|.
      ENDIF.
*--------------------------------------------------------------------*  FINISH
*--------------------------------------------------------------------*
      TRY.
          DELETE FROM (lv_tabname) WHERE (lv_table_where).
          COMMIT WORK AND WAIT.
          WRITE: |{ lv_table_where } has been deleted from table { lv_tabname }  |, /.
        CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax.
      ENDTRY.
    ENDLOOP.
  ENDIF.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& PBO/PAI modules for the obsolete-object review screen (dynpro 9000,
*& see the setup comment above TY_OBSOLETE_ROW near the top of this
*& program). Placed here, after the SELECTION-SCREEN block, because
*& ABAP resolves global names top-down within a single program - a
*& MODULE textually positioned before PARAMETERS p_model/p_entity/...
*& are declared cannot see them ("Field P_MODEL is unknown"), so these
*& could not stay at the very top of the source together with
*& TY_OBSOLETE_ROW/GT_OBSOLETE_ROW as originally placed.
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZOBSDEL_SCR9000'.

  IF go_container IS NOT BOUND.
    CREATE OBJECT go_container
      EXPORTING container_name = 'CUSTOM_CTRL'.
    CREATE OBJECT go_grid
      EXPORTING i_parent = go_container.

    DATA lt_fcat TYPE lvc_t_fcat.
    APPEND VALUE #( fieldname = 'CHK' checkbox = abap_true edit = abap_true outputlen = 4
                     scrtext_s = 'Del?' scrtext_m = 'Delete?' scrtext_l = 'Delete this object?' ) TO lt_fcat.
    APPEND VALUE #( fieldname = 'ID' outputlen = 20
                     scrtext_s = 'ID' scrtext_m = 'Identifier' scrtext_l = 'Identifier' ) TO lt_fcat.
    APPEND VALUE #( fieldname = 'DESCRIPTION' outputlen = 40
                     scrtext_s = 'Descr.' scrtext_m = 'Description' scrtext_l = 'Description' ) TO lt_fcat.

    go_grid->set_table_for_first_display(
      EXPORTING is_layout       = VALUE lvc_s_layo( cwidth_opt = abap_true )
      CHANGING  it_outtab       = gt_obsolete_row
                it_fieldcatalog = lt_fcat ).
  ELSE.
    go_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.

MODULE user_command_9000 INPUT.
  go_grid->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
* LEAVE TO SCREEN 1000 does NOT work here: that raises "Selection
* screen ... 1000 was not called using CALL SELECTION-SCREEN" at
* runtime, because the report's own selection screen is opened
* automatically by the ABAP runtime (via SE38/SA38 execute), not via
* an explicit CALL SELECTION-SCREEN statement in this program - LEAVE
* TO SCREEN can only jump to a screen that was entered that way.
* SUBMIT ... VIA SELECTION-SCREEN restarts the report and forces the
* selection screen to display again instead of auto-executing, with
* WITH carrying the user's current parameter values forward so it
* feels like "going back" rather than starting over blank.
      SUBMIT z_bg_sap_recommended_deletion VIA SELECTION-SCREEN
        WITH p_model  = p_model
        WITH p_entity = p_entity
        WITH p_edtn   = p_edtn
        WITH p_delall = p_delall
        WITH p_kattr1 = p_kattr1
        WITH p_kval1  = p_kval1
        WITH p_kattr2 = p_kattr2
        WITH p_kval2  = p_kval2
        WITH p_kattr3 = p_kattr3
        WITH p_kval3  = p_kval3
        WITH p_test   = p_test.
    WHEN 'CONF'.
      LEAVE SCREEN.
    WHEN 'SELALL'.
      LOOP AT gt_obsolete_row ASSIGNING FIELD-SYMBOL(<row_all>).
        <row_all>-chk = abap_true.
      ENDLOOP.
      go_grid->refresh_table_display( ).
    WHEN 'DESELALL'.
      LOOP AT gt_obsolete_row ASSIGNING FIELD-SYMBOL(<row_none>).
        CLEAR <row_none>-chk.
      ENDLOOP.
      go_grid->refresh_table_display( ).
  ENDCASE.

  CLEAR sy-ucomm.
ENDMODULE.

*&---------------------------------------------------------------------*
*& FORM f4_entity
*&---------------------------------------------------------------------*
FORM f4_entity.

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

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM f4_edition
*&---------------------------------------------------------------------*
FORM f4_edition.

  TYPES: BEGIN OF ty_val,
           usmd_edition TYPE usmd_edition,
         END OF ty_val.
  DATA lt_vals  TYPE STANDARD TABLE OF ty_val WITH EMPTY KEY.
  DATA lv_etype TYPE usmd020c-usmd_edtn_type.

  CONCATENATE p_model '_ALL' INTO lv_etype.

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

ENDFORM.

*----------------------------------------------------------------------*
