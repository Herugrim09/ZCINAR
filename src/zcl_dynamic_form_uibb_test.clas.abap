CLASS zcl_dynamic_form_uibb_test DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_fpm_guibb_dynamic_config .
    INTERFACES if_fpm_guibb .
    INTERFACES if_fpm_guibb_form .

    DATA: gt_components  TYPE cl_abap_structdescr=>component_table,

          gr_structdescr TYPE REF TO cl_abap_structdescr.

    TYPES:
      BEGIN OF ty_mara,
        matnr TYPE matnr,
        mtart TYPE mtart,
        meins TYPE meins,
      END OF ty_mara .

    METHODS construct_structure .
    DATA: gt_attribute_data TYPE STANDARD TABLE OF zmdg_tax_text_vw.
    DATA: gs_user_lock TYPE zmdg_user_lock.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dynamic_form_uibb_test IMPLEMENTATION.


  METHOD if_fpm_guibb_dynamic_config~has_dynamic_configuration.
    rv_has_dynamic_configuration = abap_true.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~check_config.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~flush.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~get_data.
    DATA : ls_mara TYPE ty_mara.
    IF cs_data IS NOT INITIAL.
    ENDIF.

    LOOP AT ct_field_usage ASSIGNING FIELD-SYMBOL(<lwa_field_usage>).
      <lwa_field_usage>-visibility = '02'.
    ENDLOOP.
*    IF io_event->mv_event_id = 'FPM_START'.
*      SELECT single matnr, mtart, meins FROM mara INTO @ls_mara where matkl = '01'.
*
*      cs_data = ls_mara.
*      ev_data_changed = abap_true.
*    ENDIF.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~get_default_config.


    DATA: lr_structdescr TYPE REF TO  cl_abap_structdescr,
          lt_field_list  TYPE         ddfields,
          ls_field_list  TYPE LINE OF ddfields,
          lv_id          TYPE fpm_element_id,
          ls_component   TYPE cl_abap_structdescr=>component,
          lt_components  TYPE cl_abap_structdescr=>component_table,
          lv_index       TYPE         i,
          lv_name_comp   TYPE name_komp.

    lv_index = 1.

    "lr_structdescr ?= cl_abap_typedescr=>describe_by_name( 'ZMK_DYNAMIC_MARA' ).
*    me->construct_structure( ).
    "lt_field_list = gr_structdescr->get_ddic_field_list( ).
    me->construct_structure( ).
    CHECK gt_attribute_data IS NOT INITIAL.

    SELECT last_node_name FROM zmdg_TAX_LSTNDE INTO @DATA(lfd_child_name) WHERE
                                                                         top_node = @gs_user_lock-main
                                                                         AND sub_node1 = @gs_user_lock-sub1
                                                                         AND sub_node2 = @gs_user_lock-sub2.
    ENDSELECT.
    io_layout_config->add_group(
      EXPORTING
        iv_title          = lfd_child_name                 " Web Dynpro: Configuration: Translatable Text
*       iv_index          = 0                " WDA Konfiguration: Index-Attribut
        iv_group_type     = 'F2'
*       iv_ctxt_menu_id   =                  " Context Menu ID
*       iv_instance_style =                  " UI Element specific Style
*       iv_id             =                  " FPM: Element ID
      RECEIVING
        rv_group_id       =   lv_id               " FPM: Element ID
    ).

    "lt_components = gr_structdescr->get_components( ).

    LOOP AT gt_attribute_data ASSIGNING FIELD-SYMBOL(<lwa_attr_data>).

      TRY.
          io_layout_config->add_element(
            EXPORTING
              iv_group_id             = lv_id              " FPM: Element ID
              iv_name                 = <lwa_attr_data>-ui_name                 " Component name
              iv_display_type         = <lwa_attr_data>-display_type             " Display Type
              iv_row                  = lv_index                " Row in form
              iv_index                = lv_index
              iv_visibility_label     = '02'             " Web Dynpro: Visibility
              iv_column               =  1
              iv_text                 = <lwa_attr_data>-ui_label               " Web Dynpro: Configuration: Translatable Text
*              it_element_action       =                  " Actions in Configuration
*              is_special_properties   =                  " Special Properties for UI Elements
*              iv_width                =                  " Width of a UI Element
*              iv_tag_active           =                  " Tag is Active
*              iv_tooltip              =                  " Web Dynpro: Configuration: Translatable Text
*              iv_explanation          =                  " Web Dynpro: Configuration: Translatable Text
*              iv_ctxt_menu_id         =                  " Context Menu ID
*              iv_instance_style       =                  " UI Element specific Style
*              iv_label_instance_style =                  " Label Style
*              iv_id                   =                 " FPM: Element ID
*            RECEIVING
*              rv_element_id           =   lv_id               " FPM: Element ID
          ).

          lv_index = lv_index + 2.
        CATCH cx_fpm_configuration INTO DATA(lcx_fpm_config). " Configuration exceptions
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_fpm_guibb_form~get_definition.

    DATA: ls_field_desc  TYPE fpmgb_s_formfield_descr.
    DATA: ls_action_definition TYPE fpmgb_s_actiondef.

*    CLEAR ls_action_definition.
*    ls_action_definition-id       = 'ZGET_ATTRIBUTES2'.
*    ls_action_definition-text     = 'Retreive Attributes'.
*    ls_action_definition-tooltip  = 'Click to get taxnomoy attributes'.
*    ls_action_definition-enabled  = abap_true.
*    ls_action_definition-visible  = '02'.
*    ls_action_definition-exposable = 'X'.
*
*    " Append the action to the list of actions
*    APPEND ls_action_definition TO et_action_definition.

    IF gr_structdescr IS NOT INITIAL. "IF zcl_tax_category_uibb=>gs_tax_attributes IS NOT INITIAL.
      eo_field_catalog = gr_structdescr.
    ENDIF.

*    " --- Populate Field Descriptions for UI (Ensure visibility) ---
*    CLEAR ls_field_desc.
*    ls_field_desc-name       = 'FIELD1'.
*    ls_field_desc-label_text = 'Field 1 (Text)'.
*    ls_field_desc-read_only  = abap_false.
*    ls_field_desc-mandatory  = abap_false.
*    ls_field_desc-visibility = '02'.  " Visible
*    APPEND ls_field_desc TO et_field_description.
*
*    CLEAR ls_field_desc.
*    ls_field_desc-name       = 'FIELD2'.
*    ls_field_desc-label_text = 'Field 2 (Number)'.
*    ls_field_desc-read_only  = abap_false.
*    ls_field_desc-mandatory  = abap_false.
*    ls_field_desc-visibility = '02'.  " Visible
*    APPEND ls_field_desc TO et_field_description.




    "eo_field_catalog ?= cl_abap_structdescr=>describe_by_name( p_name = 'ZMK_DYNAMIC_MARA' ).
*    eo_field_catalog ?= cl_abap_structdescr=>create(
*                          p_components =  gt_components                " Component Table
**                          p_strict     = true             " Type Creation According to ABAP-OO Rules?
*                        ).


  ENDMETHOD.


  METHOD if_fpm_guibb_form~process_event.
*    IF io_event->mv_event_id = 'FPM_START'.
*      me->construct_structure( ).
*    ENDIF.
  ENDMETHOD.


  METHOD if_fpm_guibb~get_parameter_list.
  ENDMETHOD.


  METHOD if_fpm_guibb~initialize.
  ENDMETHOD.


  METHOD construct_structure.
    DATA: ls_field_desc TYPE fpmgb_s_formfield_descr,
          ls_component  TYPE cl_abap_structdescr=>component,
          lo_type_descr TYPE REF TO cl_abap_typedescr.



    SELECT SINGLE * FROM zmdg_user_lock INTO @gs_user_lock WHERE username = @sy-uname.

    SELECT * FROM zmdg_tax_text_vw INTO TABLE @gt_attribute_data
    WHERE top_node = @gs_user_lock-main
    AND sub_node1 = @gs_user_lock-sub1
    AND sub_node2 = @gs_user_lock-sub2 .




*    LOOP AT lit_attribute_data ASSIGNING FIELD-SYMBOL(<lwa_attribute_data>).
*      CLEAR ls_field_desc.
*      ls_field_desc-name       = <lwa_attribute_data>-attribute.
*      ls_field_desc-label_text = <lwa_attribute_data>-ui_label.
*      ls_field_desc-read_only  = <lwa_attribute_data>-ui_read_only.
*      ls_field_desc-mandatory  = <lwa_attribute_data>-ui_mandatory.
*      ls_field_desc-visibility = '02'.  " Visible
*      APPEND ls_field_desc TO et_field_description.
*    ENDLOOP.
    FREE gr_structdescr.
    IF gr_structdescr IS NOT BOUND.
      LOOP AT gt_attribute_data ASSIGNING FIELD-SYMBOL(<lwa_attr_data>).
        IF <lwa_attr_data> IS ASSIGNED.
          ls_component-name = <lwa_attr_data>-attribute.
          ls_component-type ?= cl_abap_elemdescr=>describe_by_name( p_name = <lwa_attr_data>-attr_type ).
          APPEND ls_component TO gt_components.
        ENDIF.
      ENDLOOP.
      " Define FIELD1 as an integer field
*      ls_component-name = 'FIELD2'.
*      lo_type_descr = cl_abap_elemdescr=>describe_by_name( 'INT4' ).
*      ls_component-type ?= lo_type_descr.
*      APPEND ls_component TO gt_components.
*
*      " Define FIELD2 as a CHAR field
*      ls_component-name = 'FIELD1'.
*      lo_type_descr = cl_abap_elemdescr=>describe_by_name( 'CHAR10' ).
*      ls_component-type ?= lo_type_descr.
*      APPEND ls_component TO gt_components.

      " Create the structure descriptor dynamically
      gr_structdescr ?= cl_abap_structdescr=>create( p_components = gt_components ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
