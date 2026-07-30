*&---------------------------------------------------------------------*
*& Include Z_BG_SAP_RECOMMENDED_DELETION_DISPL
*&---------------------------------------------------------------------*
*& Selection screen.
*&---------------------------------------------------------------------*

" -----------------------------------------------------------------------
" Selection screen
" -----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS p_model  TYPE usmd_model DEFAULT '0G' OBLIGATORY.
  PARAMETERS p_entity TYPE usmd_entity OBLIGATORY.
  PARAMETERS p_edtn   TYPE usmd_edition OBLIGATORY.
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
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  %_p_model_%_app_%-text  = 'Data Model'.
  %_p_edtn_%_app_%-text   = 'Edition'.
  %_p_entity_%_app_%-text = 'Entity Type'.
  %_p_delall_%_app_%-text = 'Deletion without key input'.
  %_p_kattr1_%_app_%-text = 'Key Field Name 1'.
  %_p_kval1_%_app_%-text = 'Key Field Value 1'.
  %_p_kattr2_%_app_%-text = 'Key Field Name 2'.
  %_p_kval2_%_app_%-text = 'Key Field Value 2'.
  %_p_kattr3_%_app_%-text = 'Key Field Name 3'.
  %_p_kval3_%_app_%-text = 'Key Field Value 3'.
  %_p_test_%_app_%-text = 'Test Run'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_edtn.
  NEW lcl_f4_helper( )->f4_edition( iv_model = p_model ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_entity.
  NEW lcl_f4_helper( )->f4_entity( ).
