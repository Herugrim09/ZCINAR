INTERFACE zif_p40_mdg_hierarchy_badi
  PUBLIC .


  INTERFACES if_badi_interface .

  METHODS determine_parent
    IMPORTING
      iv_entity_type TYPE usmd_entity
      iv_entity_key  TYPE usmd_value
    CHANGING
      cv_parent_type TYPE usmd_entity
      cv_parent_key  TYPE usmd_value.

  METHODS determine_previous
    IMPORTING
      iv_entity_type   TYPE usmd_entity
      iv_entity_key    TYPE usmd_value
    CHANGING
      cv_previous_type TYPE usmd_entity
      cv_previous_key  TYPE usmd_value.


  METHODS determine_hierarchy
    CHANGING
      cv_hierarchy_type TYPE usmd_entity
      cv_hierarchy_key  TYPE usmd_value.
ENDINTERFACE.
