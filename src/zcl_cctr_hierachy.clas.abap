class ZCL_CCTR_HIERACHY definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces ZIF_P40_MDG_HIERARCHY_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CCTR_HIERACHY IMPLEMENTATION.


  method ZIF_P40_MDG_HIERARCHY_BADI~DETERMINE_HIERARCHY.
    cv_hierarchy_type = 'CCTRH'.
    cv_hierarchy_key = 'CCGH'.
  endmethod.


  METHOD zif_p40_mdg_hierarchy_badi~determine_parent.
    cv_parent_type = 'CCTRG'.
    cv_parent_key = 'CCG'.
  ENDMETHOD.


  method ZIF_P40_MDG_HIERARCHY_BADI~DETERMINE_PREVIOUS.
  endmethod.
ENDCLASS.
