class ZCL_MDG_0G_FIELD_PROPS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_ACC_FLD_PROP_CDS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MDG_0G_FIELD_PROPS IMPLEMENTATION.


  method IF_EX_USMD_ACC_FLD_PROP_CDS~IS_FIELD_PROP_HIDDEN_SUPPORTED.
  endmethod.


  method IF_EX_USMD_ACC_FLD_PROP_CDS~MODIFY_ENTITY_PROPERTIES.
  endmethod.


  METHOD if_ex_usmd_acc_fld_prop_cds~modify_fld_prop_attr.
    IF iv_entity = if_usmdz_cons_entitytypes=>gc_entity_account.
*      LOOP AT ct_fld_prop ASSIGNING FIELD-SYMBOL(<lwa_outer>).
*        IF  <lwa_outer> IS ASSIGNED.
*          ASSIGN COMPONENT 'USMD_FP' OF STRUCTURE <lwa_outer> TO FIELD-SYMBOL(<lwa_field_prop>).
*          IF  <lwa_field_prop> IS ASSIGNED.
*            ASSIGN COMPONENT 'TXTSH' OF STRUCTURE <lwa_field_prop> TO FIELD-SYMBOL(<lfd_any>).
*            <lfd_any> = 'R'.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
