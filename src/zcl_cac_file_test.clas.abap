class ZCL_CAC_FILE_TEST definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_MODIFY_FILE_CONTENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CAC_FILE_TEST IMPLEMENTATION.


  METHOD if_modify_file_content~modify_file_content.
    CHECK 1 = 1.
  ENDMETHOD.
ENDCLASS.
