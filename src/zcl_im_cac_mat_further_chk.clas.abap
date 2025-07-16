class ZCL_IM_CAC_MAT_FURTHER_CHK definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_MATERIAL_CHECK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_CAC_MAT_FURTHER_CHK IMPLEMENTATION.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_MARA_MEINS.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_PMATA.
  endmethod.


  METHOD if_ex_badi_material_check~check_data.
    DATA: lfd_username TYPE string.
    lfd_username = sy-uname.
    IF lfd_username NE 'MKAYA'.
      MESSAGE 'You have not authorization to save material data' TYPE 'I'.
      RAISE application_error.
    ENDIF.
  ENDMETHOD.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_DATA_RETAIL.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_MASS_MARC_DATA.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~FRE_SUPPRESS_MARC_CHECK.
  endmethod.
ENDCLASS.
