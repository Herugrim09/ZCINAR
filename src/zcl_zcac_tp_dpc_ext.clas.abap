class ZCL_ZCAC_TP_DPC_EXT definition
  public
  inheriting from ZCL_ZCAC_TP_DPC
  create public .

public section.
protected section.

  methods ZTPHEADERSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZCAC_TP_DPC_EXT IMPLEMENTATION.


  METHOD ztpheaderset_get_entityset.
    SELECT * FROM zvg_mdgx_matdesc INTO CORRESPONDING FIELDS OF TABLE et_entityset UP TO 30 ROWS.
  ENDMETHOD.
ENDCLASS.
