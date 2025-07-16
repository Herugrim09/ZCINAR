class ZCL_COA_INBOUND_V1 definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CHTACCTSRPLCTNRQ_V1 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_COA_INBOUND_V1 IMPLEMENTATION.


  METHOD if_ex_chtacctsrplctnrq_v1~inbound_processing.
    CHECK 1 = 1.
  ENDMETHOD.


  METHOD if_ex_chtacctsrplctnrq_v1~outbound_processing.
    CHECK 1 = 1.
  ENDMETHOD.
ENDCLASS.
