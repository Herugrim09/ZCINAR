class ZCL_FCE_OUTBOUND definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_FCE_RPLCTN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FCE_OUTBOUND IMPLEMENTATION.


  method IF_EX_USMD_FCE_RPLCTN~INBOUND_PROCESSING.
  endmethod.


  method IF_EX_USMD_FCE_RPLCTN~OUTBOUND_PROCESSING.
    CHECK 1 = 1.
  endmethod.
ENDCLASS.
