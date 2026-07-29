*&---------------------------------------------------------------------*
*& Report Z_BG_SAP_RECOMMENDED_DELETION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_bg_sap_recommended_deletion.

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
*
* Example usage – test mode (count only):
  IF p_test = abap_true. "START - IF Statement added
    LOOP AT lt_tables INTO DATA(lv_tabname).
      TRY.
          SELECT COUNT(*) FROM (lv_tabname) WHERE (lv_where)
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
    LOOP AT lt_tables INTO lv_tabname.
*  LOOP AT lt_tables INTO DATA(lv_tabname).
      TRY.
          DELETE FROM (lv_tabname) WHERE (lv_where).
          COMMIT WORK AND WAIT.
          WRITE: |{ lv_where } has been deleted from table { lv_tabname }  |, /.
        CATCH cx_sy_dynamic_osql_semantics cx_sy_dynamic_osql_syntax.
      ENDTRY.
    ENDLOOP.
  ENDIF.
*----------------------------------------------------------------------*

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
