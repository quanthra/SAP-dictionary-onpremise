class ZQUANTHRACL_SDO_HANDLER definition
  public
  inheriting from CL_REST_HTTP_HANDLER
  final
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
protected section.

  methods HANDLE_CSRF_TOKEN
    redefinition .
private section.
ENDCLASS.



CLASS ZQUANTHRACL_SDO_HANDLER IMPLEMENTATION.


  method HANDLE_CSRF_TOKEN.
*CALL METHOD SUPER->HANDLE_CSRF_TOKEN
*  EXPORTING
*    IO_CSRF_HANDLER =
*    IO_REQUEST      =
*    IO_RESPONSE     =
*    .
  endmethod.


  METHOD if_rest_application~get_root_handler.

    DATA(lo_router) = NEW cl_rest_router( ).

    " ADD Routes
    lo_router->attach( iv_template = |/quanthra/create_package|       iv_handler_class = 'ZQUANTHRACL_SDO' ).
    lo_router->attach( iv_template = |/quanthra/create_table|         iv_handler_class = 'ZQUANTHRACL_SDO' ).
    lo_router->attach( iv_template = |/quanthra/create_domain|        iv_handler_class = 'ZQUANTHRACL_SDO' ).
    lo_router->attach( iv_template = |/quanthra/create_data_element|  iv_handler_class = 'ZQUANTHRACL_SDO' ).
    lo_router->attach( iv_template = |/quanthra/create_class|         iv_handler_class = 'ZQUANTHRACL_SDO' ).
    lo_router->attach( iv_template = |/quanthra/create_function|      iv_handler_class = 'ZQUANTHRACL_SDO' ).
    lo_router->attach( iv_template = |/quanthra/create_request|       iv_handler_class = 'ZQUANTHRACL_SDO' ).
    lo_router->attach( iv_template = |/quanthra/create_program|       iv_handler_class = 'ZQUANTHRACL_SDO' ).
    lo_router->attach( iv_template = |/quanthra/get_program_source|   iv_handler_class = 'ZQUANTHRACL_SDO' ).

    ro_root_handler = lo_router.

  ENDMETHOD.
ENDCLASS.
