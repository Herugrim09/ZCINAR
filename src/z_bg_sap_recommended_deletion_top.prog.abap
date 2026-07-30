*&---------------------------------------------------------------------*
*& Include Z_BG_SAP_RECOMMENDED_DELETION_TOP
*&---------------------------------------------------------------------*
*& Global data and types shared across the other includes of
*& Z_BG_SAP_RECOMMENDED_DELETION.
*&---------------------------------------------------------------------*

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
*& [2026-07-30] Reverted the dynpro-9000/CL_GUI_ALV_GRID approach after
*&   the Custom Control kept causing problems (activation errors, then
*&   a blank screen because the element defaulted to 1 line tall).
*&   Turns out CL_SALV_TABLE never needed a custom dynpro to stop being
*&   a "dialog box": SET_SCREEN_POPUP is what makes it a small modal
*&   window - simply not calling it and calling DISPLAY( ) directly
*&   makes CL_SALV_TABLE fill the whole screen like a real screen, with
*&   BACK/EXIT/CANC in the PF-STATUS handled entirely by the SALV
*&   framework (no custom PBO/PAI, no SUBMIT gymnastics needed). Restored
*&   LCL_OBSOLETE_HANDLER and the CL_SALV_TABLE popup-building code from
*&   before the dynpro rewrite, removed screen 9000's MODULEs/
*&   GO_CONTAINER/GO_GRID/GT_OBSOLETE_ROW, and reused the already-created
*&   PF-STATUS ZOBSDEL_SCR9000 (its BACK/EXIT/CANC/SELALL/DESELALL
*&   functions all still apply here - screen 9000 and its Custom Control
*&   are simply no longer called and can be left unused or deleted).
*& [2026-07-30] Fixed two follow-up issues found after the revert above:
*&   (1) Descriptions showed blank with no visible cause - the TRY/CATCH
*&   around the TXTSH SELECT silently cleared lv_description on any
*&   dynamic-SQL failure. Now writes the real exception text (once, not
*&   per row) and flags when no physical text table was found at all.
*&   (2) The custom "Continue" (CONF) toolbar button did nothing -
*&   unlike BACK/EXIT/CANC, which CL_SALV_TABLE's own framework
*&   recognizes and closes automatically, a custom function code only
*&   fires ADDED_FUNCTION and otherwise leaves the screen open. Added
*&   WHEN 'CONF'. LEAVE TO SCREEN 0. to ON_ADDED_FUNCTION so Continue
*&   closes the review list the same way BACK/EXIT/CANC do, handing
*&   control back to the code after DISPLAY( ) so the count-preview/
*&   POPUP_TO_CONFIRM step actually runs.
*& [2026-07-30] Restructured the whole program from one file into the
*&   standard TOP/LC/DISPL/ALV/LOGIC include layout (matching the
*&   team's zmdg_r_key_map_util pattern) and replaced the two FORMs
*&   (f4_entity, f4_edition) and the inline START-OF-SELECTION
*&   procedural code with local classes - no static/CLASS-METHODS
*&   anywhere, every call site uses a throwaway instance instead
*&   (e.g. NEW lcl_f4_helper( )->f4_entity( )):
*&   - LCL_OBSOLETE_HANDLER (unchanged) lives in _LC, alongside a new
*&     small class LCL_F4_HELPER that replaces both FORMs. F4 help was
*&     deliberately NOT put on the main LCL_OBSOLETE_DELETION class: that
*&     class's RUN_PRODUCTIVE_MODE needs LCL_OBSOLETE_ALV_FC (_ALV
*&     include, included after _DISPL), so if LCL_OBSOLETE_DELETION had
*&     to be visible before _DISPL too (for F4), its DEFINITION and
*&     IMPLEMENTATION would have had to live in different includes - ABAP
*&     resolves class definitions top-down just like it does DATA (same
*&     rule that caused the P_MODEL/MODULE issue above). Giving F4 help
*&     its own tiny, dependency-free class in _LC instead means
*&     LCL_OBSOLETE_DELETION never has to be visible before _DISPL, so it
*&     lives entirely in _LOGIC - DEFINITION and IMPLEMENTATION together,
*&     no split. F4_ENTITY takes no parameters (reads SY-REPID/SY-DYNNR
*&     directly); F4_EDITION takes IV_MODEL as a parameter instead of
*&     reading PARAMETER P_MODEL directly, since _LC (where this class
*&     lives) is included before _DISPL declares P_MODEL - the _DISPL
*&     call site passes iv_model = p_model in.
*&   - The former inline CL_SALV_TABLE review-list code (factory,
*&     column setup, handler wiring, SET_SCREEN_STATUS, DISPLAY( ),
*&     selected-rows readback) is now an instance method on
*&     LCL_OBSOLETE_ALV_FC, in _ALV, called as
*&     NEW lcl_obsolete_alv_fc( )->show_and_get_selected_ids( ... ).
*&   - The three near-duplicate per-table WHERE-clause blocks (test
*&     mode's count loop, productive mode's preview-count loop, and the
*&     final DELETE loop) are consolidated into one private method,
*&     BUILD_TABLE_WHERE, parameterized by IV_RESTRICT_CHECK_TABLE_BY_IDS
*&     - false for test mode (the check table's WHERE has no ID
*&     restriction there, matching the original behaviour), true
*&     (default) for productive mode's preview and DELETE, where the
*&     check table's WHERE is additionally restricted to the
*&     user-selected ID list.
*&   No behavioural change was intended anywhere in this split.
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_obsolete_row,
         chk         TYPE c LENGTH 1,
         id          TYPE string,
         description TYPE string,
       END OF ty_obsolete_row.
TYPES ty_obsolete_row_tab TYPE STANDARD TABLE OF ty_obsolete_row WITH EMPTY KEY.
