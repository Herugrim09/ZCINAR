class ZCL_CAC_DATA_TRANSFER definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_DATA_TRANSFER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CAC_DATA_TRANSFER IMPLEMENTATION.


METHOD if_ex_usmd_data_transfer~process_download.
  " Table ct_data_ext already contains the data rows
  " We need to insert header lines at the beginning

  DATA: lt_header     TYPE usmd_t_charline,
        ls_charline   TYPE usmd_charline,
        lv_field_line TYPE string,
        lv_field_name TYPE string,
        lv_date       TYPE string,
        lv_time       TYPE string,
        lv_sel_line   TYPE string.

  " Variables for CEPC table data
  DATA: lt_cepc      TYPE TABLE OF cepc,
        ls_cepc      TYPE cepc,
        lv_cepc_line TYPE usmd_charline,
        lv_value     TYPE string,
        lv_fieldname TYPE usmd_fieldname,
        lv_duplicate TYPE abap_bool.

  FIELD-SYMBOLS: <lt_data_int> TYPE ANY TABLE,
                 <ls_data_int> TYPE any.

  " Format the current date and time
  CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO lv_date.
  CONCATENATE sy-uzeit(2) sy-uzeit+2(2) sy-uzeit+4(2) INTO lv_time.

  " Create header lines
  CLEAR ls_charline.
  ls_charline = '* Download'.
  APPEND ls_charline TO lt_header.

  CLEAR ls_charline.
  CONCATENATE '* Data Model: ' i_model INTO ls_charline.
  APPEND ls_charline TO lt_header.

  CLEAR ls_charline.
  CONCATENATE '* Entity Type: ' i_entity INTO ls_charline.
  APPEND ls_charline TO lt_header.

  CLEAR ls_charline.
  CONCATENATE '* Variant: ' i_trvari INTO ls_charline.
  APPEND ls_charline TO lt_header.

  CLEAR ls_charline.
  CONCATENATE '* User: ' sy-uname INTO ls_charline.
  APPEND ls_charline TO lt_header.

  CLEAR ls_charline.
  CONCATENATE '* Date: ' lv_date ' / Time: ' lv_time INTO ls_charline.
  APPEND ls_charline TO lt_header.

  CLEAR ls_charline.
  ls_charline = '* Selection:'.
  APPEND ls_charline TO lt_header.

  " Add the selection criteria from IT_SEL
  LOOP AT it_sel ASSIGNING FIELD-SYMBOL(<ls_sel>).
    CLEAR lv_sel_line.
    CLEAR ls_charline.

    " Format the selection criteria using the field's actual name
    " but following the same template format
    CONCATENATE '* ' <ls_sel>-fieldname ' EQ | ' <ls_sel>-low ' |' INTO ls_charline.
    APPEND ls_charline TO lt_header.
  ENDLOOP.

  " If no selection criteria, add the default USMD_EDITION line
  IF lines( it_sel ) = 0.
    CLEAR ls_charline.
    CONCATENATE '* USMD_EDITION EQ | ' i_edition ' |' INTO ls_charline.
    APPEND ls_charline TO lt_header.
  ENDIF.

  CLEAR ls_charline.
  CONCATENATE '* Edition: ' i_edition INTO ls_charline.
  APPEND ls_charline TO lt_header.

  " Add the column header line with field names
  " Initialize with the asterisk
  lv_field_line = '*'.

  " Loop through the field sequence table to build the header line
  LOOP AT its_field ASSIGNING FIELD-SYMBOL(<ls_field>).
    " Get the field name
    lv_field_name = <ls_field>-fieldname.

    " For the first field, add to the initial string with asterisk
    " For subsequent fields, add with separator
    IF sy-tabix = 1.
      CONCATENATE lv_field_line lv_field_name INTO lv_field_line.
    ELSE.
      CONCATENATE lv_field_line ';' lv_field_name INTO lv_field_line.
    ENDIF.
  ENDLOOP.

  " Add the constructed field line to the header table
  CLEAR ls_charline.
  ls_charline = lv_field_line.
  APPEND ls_charline TO lt_header.

  " Insert header lines at the beginning of ct_data_ext
  INSERT LINES OF lt_header INTO ct_data_ext INDEX 1.

  " Assign internal table reference to field symbol
  ASSIGN it_data_int TO <lt_data_int>.

  CASE i_entity.
    WHEN 'PCTR'.
      " Now read data from CEPC table and append to ct_data_ext
      SELECT * FROM cepc INTO TABLE lt_cepc
        ORDER BY prctr.

      " Check if we got data
      IF sy-subrc = 0.
        " Process each CEPC record and append to ct_data_ext
        LOOP AT lt_cepc INTO ls_cepc.
          " Check if this record already exists in IT_DATA_INT using READ TABLE
          READ TABLE <lt_data_int> ASSIGNING <ls_data_int>
            WITH KEY (if_usmdz_cons_entitytypes=>gc_entity_coarea) = ls_cepc-kokrs
                    (if_usmdz_cons_entitytypes=>gc_entity_pctr) = ls_cepc-prctr.

          IF sy-subrc = 0.
            " Record exists - it's a duplicate, skip it
            CONTINUE.
          ENDIF.

          CLEAR lv_cepc_line.

          " Loop through all positions in the field mapping to build the line
          LOOP AT its_field ASSIGNING <ls_field>.
            " Map CEPC fields to output fields based on fieldname
            CLEAR lv_value.
            CASE <ls_field>-fieldname.
              WHEN 'USMD_ACTIONCODE'.
                lv_value = 'M'.
              WHEN 'USMD_ENT_CRTD_BY'.
                lv_value = ls_cepc-usnam.
              WHEN 'USMD_ENT_CRTD_AT'.
                " Format date/time as needed
                lv_value = ls_cepc-ersda.
              WHEN 'COAREA'.
                lv_value = ls_cepc-kokrs.
              WHEN 'PC_LAND1'.
                lv_value = ls_cepc-land1.
              WHEN 'PCTRDEPT'.
                lv_value = ls_cepc-abtei.
              WHEN 'TXTMI'.
                lv_value = ls_cepc-name1.
              WHEN 'TXTSH'.
                lv_value = ls_cepc-prctr.
              WHEN 'PC_ORT02'.
                lv_value = ls_cepc-ort01.
              WHEN 'PC_TELFX'.
                lv_value = ls_cepc-telfx.
              WHEN 'PC_SPRAS'.
                lv_value = ls_cepc-spras.
              WHEN 'PC_NAME1'.
                lv_value = ls_cepc-name1.
              WHEN 'PC_NAME2'.
                lv_value = ls_cepc-name2.
              WHEN 'PC_NAME3'.
                lv_value = ls_cepc-name3.
              WHEN 'PC_NAME4'.
                lv_value = ls_cepc-name4.
              WHEN 'PC_PSTL2'.
                lv_value = ls_cepc-pstl2.
              WHEN 'PC_PFACH'.
                lv_value = ls_cepc-pfach.
              WHEN 'PCTRRESPP'.
                lv_value = ls_cepc-verak. " User Responsible (12 chars)
              WHEN 'PCTRRESPU'.
                lv_value = ls_cepc-verak_user.     " Person Responsible (20 chars)
              WHEN 'PC_PSTLZ'.
                lv_value = ls_cepc-pstlz.
              WHEN 'PC_DRNAM'.
                lv_value = ls_cepc-drnam.
              WHEN 'PCTR'.
                lv_value = ls_cepc-prctr.
              WHEN 'PC_REGION'.
                lv_value = ls_cepc-regio.
              WHEN 'PCTRSEG'.
                lv_value = ls_cepc-segment.
              WHEN 'PC_STRAS'.
                lv_value = ls_cepc-stras.
              WHEN 'PCTRTXJCD'.
                lv_value = ls_cepc-txjcd.
              WHEN 'PC_TELBX'.
                lv_value = ls_cepc-telbx.
              WHEN 'PC_TELF1'.
                lv_value = ls_cepc-telf1.
              WHEN 'PC_TELF2'.
                lv_value = ls_cepc-telf2.
              WHEN 'PC_TELTX'.
                lv_value = ls_cepc-teltx.
              WHEN 'PC_TELX1'.
                lv_value = ls_cepc-telx1.
              WHEN 'PC_ANRED'.
                lv_value = ls_cepc-anred.
              WHEN OTHERS.
                " Fields not specifically mapped remain empty
                lv_value = ''.
            ENDCASE.
            " Add the value to the line
            IF <ls_field>-seqnr = 1.
              " First field - no leading semicolon
              lv_cepc_line = lv_value.
            ELSE.
              " Add with a semicolon
              CONCATENATE lv_cepc_line ';' lv_value INTO lv_cepc_line.
            ENDIF.
          ENDLOOP.
          " Append to the target table
          APPEND lv_cepc_line TO ct_data_ext.
        ENDLOOP.
      ENDIF.
    WHEN 'PCTRG'.
      " Logic for PCTRG entity type
      " This is a placeholder - implement the specific logic for PCTRG as needed
      CLEAR lv_cepc_line.

      " Loop through all positions in the field mapping to build the line
      LOOP AT its_field ASSIGNING <ls_field>.
        " Map fields based on fieldname for PCTRG entity
        CLEAR lv_value.
        CASE <ls_field>-fieldname.
          WHEN 'USMD_ACTIONCODE'.
            lv_value = 'M'.
          WHEN 'COAREA'.
            lv_value = '0001'.
          WHEN 'TXTMI'.
            lv_value = 'name'.
          WHEN 'PCTRG'.
            lv_value = 'PCG2'.
          WHEN OTHERS.
            " Fields not specifically mapped remain empty
            lv_value = ''.
        ENDCASE.

        " Add the value to the line
        IF <ls_field>-seqnr = 1.
          " First field - no leading semicolon
          lv_cepc_line = lv_value.
        ELSE.
          " Add with a semicolon
          CONCATENATE lv_cepc_line ';' lv_value INTO lv_cepc_line.
        ENDIF.
      ENDLOOP.

      " Append the example row to the target table
      APPEND lv_cepc_line TO ct_data_ext.
    WHEN OTHERS.
      " Default handling for other entity types
      " No additional data rows added
  ENDCASE.

  CLEAR: lt_header, lt_cepc.
ENDMETHOD.


  method IF_EX_USMD_DATA_TRANSFER~PROCESS_DOWNLOAD_HRY.
  endmethod.
ENDCLASS.
