*&---------------------------------------------------------------------*
*& Include          ZMDG_I_KM_UNIVERSAL_TOP
*&---------------------------------------------------------------------*
*& Universal Key Mapping Program - Global Data Declarations
*&---------------------------------------------------------------------*

" Type pools
TYPE-POOLS: icon, slis.

"----------------------------------------------------------------------
" Global Types
"----------------------------------------------------------------------
TYPES: BEGIN OF gty_object_type_list,
         object_type_code TYPE mdg_object_type_code_bs,
         description      TYPE mdg_object_type_code_desc_bs,
       END OF gty_object_type_list,

       BEGIN OF gty_ids_type_list,
         ids_type_code    TYPE mdg_ids_type_code_bs,
         object_type_code TYPE mdg_object_type_code_bs,
         description      TYPE mdg_ids_type_code_desc_bs,
       END OF gty_ids_type_list,

       BEGIN OF gty_business_system,
         business_system_id TYPE sld_bskey,
         description        TYPE string,
       END OF gty_business_system,

       BEGIN OF gty_key_mapping_display,
         object_type_code TYPE mdg_object_type_code_bs,
         object_type_desc TYPE mdg_object_type_code_desc_bs,
         ids_type_code    TYPE mdg_ids_type_code_bs,
         ids_type_desc    TYPE mdg_ids_type_code_desc_bs,
         source_system    TYPE sld_bskey,
         source_id_value  TYPE mdg_object_id_bs,
         target_system    TYPE sld_bskey,
         target_id_value  TYPE mdg_object_id_bs,
         mapping_status   TYPE char10,
         created_by       TYPE syuname,
         created_on       TYPE sydatum,
         changed_by       TYPE syuname,
         changed_on       TYPE sydatum,
         message          TYPE string,
         row_color        TYPE char4,
       END OF gty_key_mapping_display,

       BEGIN OF gty_key_mapping_input,
         object_type_code TYPE mdg_object_type_code_bs,
         ids_type_code    TYPE mdg_ids_type_code_bs,
         source_system    TYPE sld_bskey,
         source_id_value  TYPE mdg_object_id_bs,
         target_system    TYPE sld_bskey,
         target_id_value  TYPE mdg_object_id_bs,
         operation        TYPE char1,  " C=Create, U=Update, D=Delete
         status           TYPE char1,  " S=Success, E=Error, W=Warning
         message          TYPE string,
       END OF gty_key_mapping_input.

" Table types
TYPES: gtty_object_type_list     TYPE TABLE OF gty_object_type_list,
       gtty_ids_type_list        TYPE TABLE OF gty_ids_type_list,
       gtty_business_system_list TYPE TABLE OF gty_business_system,
       gtty_key_mapping_display  TYPE TABLE OF gty_key_mapping_display,
       gtty_key_mapping_input    TYPE TABLE OF gty_key_mapping_input.

"----------------------------------------------------------------------
" Global Variables - Selection Screen
"----------------------------------------------------------------------
" Object Type Selection
DATA: gv_object_type_code TYPE mdg_object_type_code_bs,
      gv_ids_type_code    TYPE mdg_ids_type_code_bs.

" Operation Selection
DATA: gv_operation_mode TYPE char1 VALUE 'D'. " D=Display, C=Create, U=Update, X=Delete

" Filter Ranges
DATA: gr_source_system LIKE RANGE OF gv_object_type_code,
      gr_target_system LIKE RANGE OF gv_object_type_code,
      gr_source_id     LIKE RANGE OF gv_object_type_code,
      gr_target_id     LIKE RANGE OF gv_object_type_code.

" File Upload
DATA: gv_file_path   TYPE localfile,
      gv_has_header  TYPE abap_bool VALUE abap_true,
      gv_file_format TYPE char4 VALUE 'XLSX'. " XLSX, CSV, TXT

" Processing Options
DATA: gv_test_mode     TYPE abap_bool,
      gv_batch_size    TYPE i VALUE 1000,
      gv_validate_only TYPE abap_bool.

"----------------------------------------------------------------------
" Global Variables - Data Processing
"----------------------------------------------------------------------
" Master Data Tables
DATA: gt_object_types     TYPE gtty_object_type_list,
      gt_ids_types        TYPE gtty_ids_type_list,
      gt_business_systems TYPE gtty_business_system_list.

" Working Tables
DATA: gt_key_mappings_display TYPE gtty_key_mapping_display,
      gt_key_mappings_input   TYPE gtty_key_mapping_input,
      gt_selected_rows        TYPE gtty_key_mapping_display.

" Utility References
DATA: go_km_utility TYPE REF TO zcl_med_mdg_km_utility,
      go_tools_ext  TYPE REF TO cl_mdg_id_matching_tools_ext.

" ALV References
DATA: go_alv_grid   TYPE REF TO cl_salv_table,
      go_alv_events TYPE REF TO cl_salv_events_table,
      go_selections TYPE REF TO cl_salv_selections.

" Status and Message Handling
DATA: gt_messages TYPE usmd_t_message,
      gv_subrc    TYPE sysubrc,
      gv_message  TYPE string.

"----------------------------------------------------------------------
" Global Constants
"----------------------------------------------------------------------
CONSTANTS: " Operation modes
  gc_mode_display    TYPE char1 VALUE 'D',
  gc_mode_create     TYPE char1 VALUE 'C',
  gc_mode_update     TYPE char1 VALUE 'U',
  gc_mode_delete     TYPE char1 VALUE 'X',

  " Status indicators
  gc_status_active   TYPE char10 VALUE 'ACTIVE',
  gc_status_inactive TYPE char10 VALUE 'INACTIVE',
  gc_status_error    TYPE char10 VALUE 'ERROR',
  gc_status_pending  TYPE char10 VALUE 'PENDING',

  " Row colors for ALV
  gc_color_normal    TYPE char4 VALUE '',
  gc_color_success   TYPE char4 VALUE 'C5',  " Green
  gc_color_warning   TYPE char4 VALUE 'C3',  " Yellow
  gc_color_error     TYPE char4 VALUE 'C6',  " Red

  " File formats
  gc_format_excel    TYPE char4 VALUE 'XLSX',
  gc_format_csv      TYPE char4 VALUE 'CSV',
  gc_format_txt      TYPE char4 VALUE 'TXT',

  " ALV function codes
  gc_fc_create       TYPE syucomm VALUE 'CREATE',
  gc_fc_update       TYPE syucomm VALUE 'UPDATE',
  gc_fc_delete       TYPE syucomm VALUE 'DELETE',
  gc_fc_refresh      TYPE syucomm VALUE 'REFRESH',
  gc_fc_export       TYPE syucomm VALUE 'EXPORT',
  gc_fc_import       TYPE syucomm VALUE 'IMPORT',
  gc_fc_template     TYPE syucomm VALUE 'TEMPLATE',
  gc_fc_validate     TYPE syucomm VALUE 'VALIDATE'.

"----------------------------------------------------------------------
" Global Work Areas
"----------------------------------------------------------------------
DATA: gwa_key_mapping_display TYPE gty_key_mapping_display,
      gwa_key_mapping_input   TYPE gty_key_mapping_input,
      gwa_object_type         TYPE gty_object_type_list,
      gwa_ids_type            TYPE gty_ids_type_list,
      gwa_business_system     TYPE gty_business_system.

"----------------------------------------------------------------------
" Selection Screen Text Elements
"----------------------------------------------------------------------
" These will be defined in the text elements of the program
" TEXT-001: Object Type and Scheme Selection
" TEXT-002: Operation Mode Selection
" TEXT-003: Filter Criteria
" TEXT-004: File Upload Options
" TEXT-005: Processing Options
" TEXT-010: Display Key Mappings
" TEXT-011: Create Key Mappings
" TEXT-012: Update Key Mappings
" TEXT-013: Delete Key Mappings
" TEXT-020: Excel File (*.xlsx)
" TEXT-021: CSV File (*.csv)
" TEXT-022: Text File (*.txt)
