INTERFACE zif_p40_mdg_0g_hierarchy
  PUBLIC .


  METHODS get_parent_nodes
    IMPORTING
      !iv_hierarchy_name   TYPE usmd_entity
      !is_hierarchy_key    TYPE any
      !iv_entity           TYPE usmd_entity
      !is_key              TYPE any OPTIONAL
      !iv_parent           TYPE usmd_entity
      !is_parent_key       TYPE any OPTIONAL
      !iv_edition          TYPE usmd_edition OPTIONAL
      !iv_index            TYPE i
    RETURNING
      VALUE(rr_parent_key) TYPE REF TO data
    RAISING
      cx_usmd_gov_api_core_error .
  METHODS assign_hierarchy
    IMPORTING
      !iv_hierarchy_name TYPE usmd_entity
      !is_hierarchy_key  TYPE any
      !iv_entity         TYPE usmd_entity
      !is_key            TYPE any
      !iv_parent         TYPE usmd_entity
      !is_parent_key     TYPE any
      !iv_edition        TYPE usmd_edition OPTIONAL
      !iv_previous       TYPE usmd_entity OPTIONAL
      !is_previous_key   TYPE any OPTIONAL
    EXPORTING
      !et_message_info   TYPE usmd_t_message .
  METHODS get_children
    IMPORTING
      !iv_lead_entity TYPE usmd_entity
      !iv_edition     TYPE usmd_edition OPTIONAL
      !iv_top_entity  TYPE usmd_entity
      !ir_top_key     TYPE REF TO data
      !ir_parent_key  TYPE REF TO data
    EXPORTING
      !et_children    TYPE usmd_t_hry_edge_rkey .
  METHODS assign_hierarchy_with_badi
    IMPORTING
      !iv_hierarchy_name   TYPE usmd_entity OPTIONAL
      !is_hierarchy_key    TYPE any OPTIONAL
      !iv_entity           TYPE usmd_entity
      !is_key              TYPE any
      !iv_parent           TYPE usmd_entity OPTIONAL
      !is_parent_key       TYPE any OPTIONAL
      !iv_edition          TYPE usmd_edition
      !iv_previous         TYPE usmd_entity OPTIONAL
      !is_previous_key     TYPE any OPTIONAL
      !iv_project_name     TYPE char5
      !iv_force_assignment TYPE abap_boolean OPTIONAL
    EXPORTING
      !et_message_info     TYPE usmd_t_message .
  METHODS create_data_reference
    IMPORTING
      !iv_entity          TYPE usmd_entity
    RETURNING
      VALUE(rr_structure) TYPE REF TO data .
ENDINTERFACE.
