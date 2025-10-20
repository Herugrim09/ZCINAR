CLASS zcl_mdgf_guibb_fi_accccdet_dq DEFINITION
  PUBLIC
  INHERITING FROM cl_mdgf_guibb_fi_accccdet_dq
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_fpm_guibb_search~process_event
        REDEFINITION .
protected section.

  methods MAP_SEARCH_OPERATOR
    importing
      !PFD_I_OPERATOR type FPMGB_SEARCH_OPERATOR
    returning
      value(PFD_R_OPTION) type DDOPTION .
  methods CONVERT_SEARCH_CRITERIA
    importing
      !PWA_I_SEARCH_CRITERIA type FPMGB_S_SEARCH_CRITERIA
    exporting
      !PFD_E_SEARCH_STRING type STRING
      !PFD_E_HAS_WILDCARD type ABAP_BOOLEAN .

  methods CREATE_STRUCT_RTTI
    redefinition .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MDGF_GUIBB_FI_ACCCCDET_DQ IMPLEMENTATION.


METHOD convert_search_criteria.

  DATA: lfd_low_value TYPE string.

  " Initialize output parameters
  CLEAR: pfd_e_search_string, pfd_e_has_wildcard.

  " Get the search value
  lfd_low_value = pwa_i_search_criteria-low.

  " Check if low value contains wildcards
  IF lfd_low_value CA '*+'.
    pfd_e_has_wildcard = abap_true.
  ELSE.
    pfd_e_has_wildcard = abap_false.
  ENDIF.

  CASE pwa_i_search_criteria-operator.
    WHEN '01'.  " equal to
      IF pfd_e_has_wildcard = abap_true.
        " Convert all * and + to % for LIKE operation (case-insensitive)
        pfd_e_search_string = to_upper( lfd_low_value ).
        pfd_e_search_string = replace( val = pfd_e_search_string sub = '*' with = '%' occ = 0 ).
        pfd_e_search_string = replace( val = pfd_e_search_string sub = '+' with = '%' occ = 0 ).
      ELSE.
        " Exact match (case-insensitive)
        pfd_e_search_string = to_upper( lfd_low_value ).
      ENDIF.

    WHEN '02'.  " not equal to
      IF pfd_e_has_wildcard = abap_true.
        " Convert all wildcards for NOT LIKE operation (case-insensitive)
        pfd_e_search_string = to_upper( lfd_low_value ).
        pfd_e_search_string = replace( val = pfd_e_search_string sub = '*' with = '%' occ = 0 ).
        pfd_e_search_string = replace( val = pfd_e_search_string sub = '+' with = '%' occ = 0 ).
      ELSE.
        " Not equal to exact value (case-insensitive)
        pfd_e_search_string = to_upper( lfd_low_value ).
      ENDIF.

    WHEN '03'.  " empty
      " For empty check
      pfd_e_search_string = ''.
      pfd_e_has_wildcard = abap_false.  " Override wildcard flag for empty check

    WHEN '23'.  " not empty
      " For not empty check
      pfd_e_search_string = ''.
      pfd_e_has_wildcard = abap_false.  " Override wildcard flag for not empty check

    WHEN '04'.  " starts with
      " Always treat as wildcard pattern (case-insensitive)
      pfd_e_has_wildcard = abap_true.
      IF lfd_low_value IS NOT INITIAL.
        " Convert to uppercase and replace all wildcards with %
        pfd_e_search_string = to_upper( lfd_low_value ).
        pfd_e_search_string = replace( val = pfd_e_search_string sub = '*' with = '%' occ = 0 ).
        pfd_e_search_string = replace( val = pfd_e_search_string sub = '+' with = '%' occ = 0 ).
        " If doesn't end with %, add it
        IF substring( val = pfd_e_search_string len = 1 off = strlen( pfd_e_search_string ) - 1 ) <> '%'.
          pfd_e_search_string = |{ pfd_e_search_string }%|.
        ENDIF.
      ELSE.
        pfd_e_search_string = '%'.  " Match anything if empty input
      ENDIF.

    WHEN '05'.  " contains
      " Always treat as wildcard pattern (case-insensitive)
      pfd_e_has_wildcard = abap_true.
      IF lfd_low_value IS NOT INITIAL.
        " Convert to uppercase and then to %word% pattern
        lfd_low_value = to_upper( lfd_low_value ).
        " First remove existing wildcards
        lfd_low_value = replace( val = lfd_low_value sub = '*' with = '' occ = 0 ).
        lfd_low_value = replace( val = lfd_low_value sub = '+' with = '' occ = 0 ).
        " Then add % on both sides
        pfd_e_search_string = |%{ lfd_low_value }%|.
      ELSE.
        pfd_e_search_string = '%'.  " Match anything if empty input
      ENDIF.

    WHEN OTHERS.
      " Unknown operator - return as is (case-insensitive)
      pfd_e_search_string = to_upper( lfd_low_value ).

  ENDCASE.

ENDMETHOD.


  METHOD create_struct_rtti.
    DATA:
    lit_component        TYPE cl_abap_structdescr=>component_table.

    CALL METHOD super->create_struct_rtti.

    mo_struct_rtti->get_components(
      RECEIVING
        p_result = lit_component                  " Component Description Table
    ).

    APPEND INITIAL LINE TO lit_component ASSIGNING FIELD-SYMBOL(<lwa_component>).
    IF <lwa_component> IS ASSIGNED.
      <lwa_component>-name = 'COMPCODE_TXT'.
      <lwa_component>-type ?= cl_abap_elemdescr=>describe_by_name( p_name = 'BUTXT' ).
    ENDIF.

    mo_struct_rtti = cl_abap_structdescr=>create( lit_component ).
  ENDMETHOD.


METHOD if_fpm_guibb_search~process_event.

  IF io_event->mv_event_id = 'FPM_EXECUTE_SEARCH'.

    DATA: lfd_butxt           TYPE text25,
          lfd_search_string   TYPE string,
          lfd_has_wildcard    TYPE abap_bool,
          lit_bukrs           TYPE STANDARD TABLE OF bukrs,
          lit_search_criteria TYPE fpmgb_t_search_criteria.

    lit_search_criteria = it_fpm_search_criteria.

    READ TABLE lit_search_criteria ASSIGNING FIELD-SYMBOL(<lwa_fpm_search_criteria>)
         WITH KEY search_attribute = 'COMPCODE_TXT'.

    IF <lwa_fpm_search_criteria> IS ASSIGNED.

      " Convert search criteria to string and wildcard flag
      me->convert_search_criteria(
        EXPORTING pwa_i_search_criteria = <lwa_fpm_search_criteria>
        IMPORTING pfd_e_search_string   = lfd_search_string
                  pfd_e_has_wildcard    = lfd_has_wildcard ).

      " Execute different queries based on wildcard flag
      IF lfd_has_wildcard = abap_true.
        " Use LIKE for wildcard patterns
        CASE <lwa_fpm_search_criteria>-operator.
          WHEN '01' OR '04' OR '05'.  " equal to, starts with, contains
            SELECT bukrs FROM t001
              WHERE upper( butxt ) LIKE @lfd_search_string
              INTO TABLE @lit_bukrs.
          WHEN '02'.  " not equal to with wildcards
            SELECT bukrs FROM t001
              WHERE upper( butxt ) NOT LIKE @lfd_search_string
              INTO TABLE @lit_bukrs.
        ENDCASE.
      ELSE.
        " Use exact matching
        CASE <lwa_fpm_search_criteria>-operator.
          WHEN '01'.  " equal to
            SELECT bukrs FROM t001
              WHERE upper( butxt ) = @lfd_search_string
              INTO TABLE @lit_bukrs.
          WHEN '02'.  " not equal to
            SELECT bukrs FROM t001
              WHERE upper( butxt ) <> @lfd_search_string
              INTO TABLE @lit_bukrs.
          WHEN '03'.  " empty
            SELECT bukrs FROM t001
              WHERE butxt = ''
              INTO TABLE @lit_bukrs.
          WHEN '23'.  " not empty
            SELECT bukrs FROM t001
              WHERE butxt <> ''
              INTO TABLE @lit_bukrs.
        ENDCASE.
      ENDIF.

      " Loop through found company codes and update search criteria
      LOOP AT lit_bukrs INTO DATA(lfd_bukrs).
        " For first entry, update existing search criteria
*        IF sy-tabix = 1.
        <lwa_fpm_search_criteria>-search_attribute = if_usmdz_cons_entitytypes=>gc_entity_company_code.
        <lwa_fpm_search_criteria>-operator = '01'.
        <lwa_fpm_search_criteria>-low = lfd_bukrs.
        "<lwa_fpm_search_criteria>-sign = COND char1( WHEN <lwa_fpm_search_criteria>-operator = '02' THEN 'E'  " Exclude for 'not equal to'
        "ELSE 'I' ).
        INSERT <lwa_fpm_search_criteria> INTO TABLE lit_search_criteria.
*        ELSE.
*          " For additional entries, append new search criteria
*          APPEND VALUE #( search_attribute = if_usmdz_cons_entitytypes=>gc_entity_company_code
*                         operator = '01'  " equal to
*                         sign = 'I'       " include
*                         low = lfd_bukrs ) TO lit_search_criteria.
*        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDIF.

  " Call the super method with updated search criteria
  CALL METHOD super->if_fpm_guibb_search~process_event
    EXPORTING
      io_event               = io_event
      it_fpm_search_criteria = lit_search_criteria
      iv_raised_by_own_ui    = iv_raised_by_own_ui
      iv_max_num_results     = iv_max_num_results
      io_search_conversion   = io_search_conversion
    IMPORTING
      et_messages            = et_messages
      ev_result              = ev_result.

ENDMETHOD.


  METHOD map_search_operator.

    CLEAR pfd_r_option.
    CASE pfd_i_operator.
      WHEN '01'.  " equal to
        pfd_r_option = usmd0_cs_ra-option_eq.
      WHEN '02'.  " not equal to
        pfd_r_option = usmd0_cs_ra-option_ne.
      WHEN '03'.  " empty
        " No direct mapping available - leave blank
        CLEAR pfd_r_option.
      WHEN '04'.  " starts with
        pfd_r_option = usmd0_cs_ra-option_cp.
      WHEN '05'.  " contains
        pfd_r_option = usmd0_cs_ra-option_cp.
      WHEN '06'.  " equal to
        pfd_r_option = usmd0_cs_ra-option_eq.
      WHEN '07'.  " not equal to
        pfd_r_option = usmd0_cs_ra-option_ne.
      WHEN '08'.  " greater than
        pfd_r_option = usmd0_cs_ra-option_gt.
      WHEN '09'.  " less than
        pfd_r_option = usmd0_cs_ra-option_lt.
      WHEN '10'.  " between
        pfd_r_option = usmd0_cs_ra-option_bt.
      WHEN '11'.  " earlier than
        pfd_r_option = usmd0_cs_ra-option_lt.
      WHEN '12'.  " later than
        pfd_r_option = usmd0_cs_ra-option_gt.
      WHEN '13'.  " within
        pfd_r_option = usmd0_cs_ra-option_bt.
      WHEN '14'.  " not within
        pfd_r_option = usmd0_cs_ra-option_nb.
      WHEN '15'.  " contains all texts
        pfd_r_option = usmd0_cs_ra-option_cp.
      WHEN '16'.  " contains one of the texts
        pfd_r_option = usmd0_cs_ra-option_cp.
      WHEN '17'.  " exactly
        pfd_r_option = usmd0_cs_ra-option_eq.
      WHEN '18'.  " contains none of the texts
        pfd_r_option = usmd0_cs_ra-option_np.
      WHEN '19'.  " greater than or equal to
        pfd_r_option = usmd0_cs_ra-option_ge.
      WHEN '20'.  " less than or equal to
        pfd_r_option = usmd0_cs_ra-option_le.
      WHEN '21'.  " earlier than or on
        pfd_r_option = usmd0_cs_ra-option_le.
      WHEN '22'.  " later than or on
        pfd_r_option = usmd0_cs_ra-option_ge.
      WHEN '23'.  " not empty
        " No direct mapping available - leave blank
        CLEAR pfd_r_option.
      WHEN '24'.  " similar to
        pfd_r_option = usmd0_cs_ra-option_cp.
      WHEN OTHERS.
        " Unknown operator - leave blank
        CLEAR pfd_r_option.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
