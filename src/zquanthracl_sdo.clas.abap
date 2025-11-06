class ZQUANTHRACL_SDO definition
  public
  inheriting from CL_REST_RESOURCE
  final
  create public .

public section.

  methods CREATE_PACKAGE .
  methods CREATE_TABLE .
  methods CREATE_DOMAIN .
  methods CREATE_DATA_ELEMENT .
  methods CREATE_CLASS .
  methods CREATE_FUNCTION .
  methods CREATE_REQUEST .

  methods IF_REST_RESOURCE~POST
    redefinition .
protected section.
private section.

  methods VALIDATE_FIELDS
    importing
      !IS_FIELDS type DATA
      !IO_REQUEST type ref to IF_REST_RESPONSE
    returning
      value(RV_ERROR) type STRING .
ENDCLASS.



CLASS ZQUANTHRACL_SDO IMPLEMENTATION.


  method CREATE_CLASS.
  endmethod.


  method CREATE_DATA_ELEMENT.
  endmethod.


  method CREATE_DOMAIN.
  endmethod.


  method CREATE_FUNCTION.
  endmethod.


  METHOD create_package.

    " GET REQ Data
    DATA(lo_entity) = mo_response->create_entity( ).





    " Create Package


  ENDMETHOD.


  METHOD create_request.

    "<<<<< GET REQUEST DATA
    DATA: BEGIN OF ls_json_input,
            r_type   TYPE string,
            r_text   TYPE string,
            r_owner  TYPE string,
            r_target TYPE string,
          END OF ls_json_input.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_json_input ).
    "<<<<<<<<<


    " Validate Fields
    CHECK validate_fields( EXPORTING is_fields = ls_json_input io_request = mo_response ) IS INITIAL.


    "<<<<< Create Request
    DATA: ls_header TYPE trwbo_request_header.

    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_type           = CONV trfunction( ls_json_input-r_type )
        iv_text           = CONV as4text( ls_json_input-r_text )
        iv_owner          = CONV as4user( ls_json_input-r_owner )
        iv_target         = CONV tr_target( ls_json_input-r_target )
      IMPORTING
        es_request_header = ls_header
      EXCEPTIONS
        insert_failed     = 1
        enqueue_failed    = 2
        OTHERS            = 3.

    lo_entity->set_string_data( |\{ "success":"Request created", "data": \{ "request": "{ ls_header-trkorr }" \} \}| ).
    lo_entity->set_content_type( `application/json; charset=UTF-8` ) ##NO_TEXT.
    mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).

  ENDMETHOD.


  method CREATE_TABLE.
  endmethod.


  METHOD if_rest_resource~post.

    " GET URL Route
    DATA(lv_route) = mo_request->get_uri_path( ).

    " Call Method
    TRY.
        " Extract Method Name
        SPLIT lv_route AT '/' INTO TABLE DATA(lt_route_decomposition).
        DATA(lv_method) = lt_route_decomposition[ 3 ].
        TRANSLATE lv_method TO UPPER CASE.

        CALL METHOD (lv_method).
      CATCH cx_root INTO DATA(lo_cxroot).
        "#TODO
    ENDTRY.

  ENDMETHOD.


  METHOD validate_fields.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lt_components  TYPE cl_abap_structdescr=>component_table,
          ls_comp        LIKE LINE OF lt_components.

    FIELD-SYMBOLS: <fs_field> TYPE any.
    DATA(lo_entity) = mo_response->create_entity( ).

    " Obter descrição da estrutura
    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( is_fields ).

    " Obter lista de componentes (campos)
    lt_components = lo_structdescr->get_components( ).

    LOOP AT lt_components INTO ls_comp.
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_fields TO <fs_field>.
      IF <fs_field> IS ASSIGNED AND ls_comp-name(1) = 'R' AND <fs_field> IS INITIAL.
        rv_error = |Field { ls_comp-name } is required|.

        lo_entity->set_string_data(
          |\{ "error":"{ rv_error }" \}| ).
        lo_entity->set_content_type( 'application/json; charset=UTF-8' ).
        mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
