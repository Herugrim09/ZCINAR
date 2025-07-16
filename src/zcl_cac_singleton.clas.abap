class ZCL_CAC_SINGLETON definition
  public
  final
  create private .

public section.

  class-data GR_MY_INSTANCE type ref to ZCL_CAC_SINGLETON .
  data GV_CATEGORY type STRING .

  class-methods GET_INSTANCE
    importing
      !I_CATEGORY type STRING
    returning
      value(RO_INSTANCE) type ref to ZCL_CAC_SINGLETON .
protected section.
private section.

  methods CONSTRUCTOR
    importing
      !I_CATEGORY type STRING .
ENDCLASS.



CLASS ZCL_CAC_SINGLETON IMPLEMENTATION.


  METHOD constructor.
    gv_category = i_category.
  ENDMETHOD.


  METHOD get_instance.
    IF gr_my_instance IS INITIAL.
      CREATE OBJECT gr_my_instance
        EXPORTING
          i_category = i_category.
    ENDIF.
    RETURN gr_my_instance.
  ENDMETHOD.
ENDCLASS.
