class ZCL_MED_COA_INBOUND definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CHTACCTSRPLCTNRQ .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MED_COA_INBOUND IMPLEMENTATION.


  METHOD if_ex_chtacctsrplctnrq~inbound_processing.
    LOOP AT is_input-chart_of_accounts_replication-chart_of_accounts-items ASSIGNING FIELD-SYMBOL(<lwa_acc>).
      IF <lwa_acc> IS ASSIGNED.
        ASSIGN <lwa_acc>-account-content TO FIELD-SYMBOL(<lfd_account>).
        CONCATENATE 'Y' <lfd_account> INTO <lfd_account>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_ex_chtacctsrplctnrq~outbound_processing.
    CHECK 1 = 1.
  ENDMETHOD.
ENDCLASS.
