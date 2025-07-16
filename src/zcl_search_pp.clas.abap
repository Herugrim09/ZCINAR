class ZCL_SEARCH_PP definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_BADI_SDQ_PP_SEARCH .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SEARCH_PP IMPLEMENTATION.


  METHOD if_badi_sdq_pp_search~search_pp.
    CHECK 1 <> 2.
  ENDMETHOD.
ENDCLASS.
