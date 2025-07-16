class ZCL_FRS_OUTBOUND definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_FRS_RPLCTN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FRS_OUTBOUND IMPLEMENTATION.


  method IF_EX_USMD_FRS_RPLCTN~INBOUND_PROCESSING.
  endmethod.


  method IF_EX_USMD_FRS_RPLCTN~OUTBOUND_PROCESSING.
    check 1 = 1.
  endmethod.
ENDCLASS.
