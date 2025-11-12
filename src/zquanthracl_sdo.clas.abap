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
  methods CREATE_PROGRAM .
  methods GET_PROGRAM_SOURCE .
  methods GET_CLASS_SOURCE .
  methods GET_FUNCTION_SOURCE .
  methods GET_CDS_SOURCE .

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

    "<<<<< GET REQUEST DATA
    DATA: BEGIN OF ls_json_input,
            r_name               TYPE string,
            r_text               TYPE string,
            r_transport_layer    TYPE string,
            r_software_component TYPE string,
            r_request            TYPE string,
            o_super_package      TYPE string,
          END OF ls_json_input.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_json_input ).
    "<<<<<<<<<


    " Validate Fields
    CHECK validate_fields( EXPORTING is_fields = ls_json_input io_request = mo_response ) IS INITIAL.


    " Create Package
    DATA(lo_package_factory) = NEW cl_package_factory( ).
    DATA lo_package         TYPE REF TO if_package.

    DATA(ls_package_data) = VALUE scompkdtln(
      devclass  = ls_json_input-r_name
      ctext     = ls_json_input-r_text
      pdevclass = ls_json_input-r_transport_layer
      dlvunit   = ls_json_input-r_software_component
      parentcl  = ls_json_input-o_super_package
      as4user   = sy-uname
      language  = sy-langu
    ).

    TRY.
        lo_package_factory->create_new_package(
          EXPORTING  i_suppress_dialog  = abap_true
          IMPORTING  e_package          = lo_package
          CHANGING   c_package_data     = ls_package_data
          EXCEPTIONS OTHERS = 1
        ).

        " Save Package
        lo_package->save( i_transport_request = |{ ls_json_input-r_request }|
                          i_suppress_dialog   = abap_true ).

        lo_entity->set_string_data( |\{ "success":"Request created", "data": \{ "package": "{ ls_package_data-devclass }"  \} \}| ).
        lo_entity->set_content_type( `application/json; charset=UTF-8` ) ##NO_TEXT.
        mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).
      CATCH cx_root INTO DATA(lx_error).
        lo_entity->set_string_data(
            |\{ "error":"{ lx_error->get_text( ) }." \}| ).
        lo_entity->set_content_type( 'application/json; charset=UTF-8' ).
        mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
    ENDTRY.

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

    IF ls_header IS NOT INITIAL.
      DATA: lv_task TYPE trkorr.

      CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
        EXPORTING
          wi_kurztext   = CONV e07t-as4text( ls_json_input-r_text )
          wi_trfunction = 'S'
          wi_strkorr    = ls_header-trkorr
        EXCEPTIONS
          OTHERS        = 9.

    ELSE.
      lo_entity->set_string_data(
            |\{ "error":"Error to create request." \}| ).
      lo_entity->set_content_type( 'application/json; charset=UTF-8' ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

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
        DATA(lo_entity) = mo_response->create_entity( ).
        lo_entity->set_string_data(
            |\{ "error":"{ lo_cxroot->get_text( ) }." \}| ).
        lo_entity->set_content_type( 'application/json; charset=UTF-8' ).
        mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
        RETURN.
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


  METHOD create_program.

    "<<<<< GET REQUEST DATA
    DATA: BEGIN OF ls_json_input,
            r_name   TYPE string,
            r_text   TYPE string,
            r_source TYPE string,
          END OF ls_json_input.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_json_input ).
    "<<<<<<<<<

    " Validate Fields
    CHECK validate_fields( EXPORTING is_fields = ls_json_input io_request = mo_response ) IS INITIAL.

    SPLIT ls_json_input-r_source AT '\n' INTO TABLE DATA(lt_source).

    DATA(lv_name) = CONV sy-repid( ls_json_input-r_name ).
    TRANSLATE lv_name TO UPPER CASE.
    INSERT REPORT lv_name FROM lt_source.
    GENERATE REPORT lv_name.

    lo_entity->set_string_data( |\{ "success":"Program created", "data": \{ "program": "{ lv_name }" \} \}| ).
    lo_entity->set_content_type( `application/json; charset=UTF-8` ) ##NO_TEXT.
    mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).

  ENDMETHOD.


  METHOD get_program_source.

    "<<<<< GET REQUEST DATA
    DATA: BEGIN OF ls_json_input,
            r_obj_name TYPE string,
            o_obj_type TYPE string,
          END OF ls_json_input.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_json_input ).
    "<<<<<<<<<

    " Validate Fields
    CHECK validate_fields( EXPORTING is_fields = ls_json_input io_request = mo_response ) IS INITIAL.

    DATA: ls_response TYPE zquanthraes_sdo_001.
    ls_response-object_name = ls_json_input-r_obj_name.
    ls_response-object_type = 'PROGRAM'.

    DATA(lv_name) = CONV progname( ls_json_input-r_obj_name ).
    READ REPORT lv_name INTO ls_response-source.

    IF ls_response-source IS INITIAL.
      " Buscamos pela Transação
      SELECT SINGLE *
        FROM tstc
        WHERE tcode = @lv_name
        INTO @DATA(ls_tcode).

      IF ls_tcode IS NOT INITIAL.
        ls_response-object_name = ls_tcode-pgmna.
        lv_name = CONV progname( ls_response-object_name ).
        READ REPORT lv_name INTO ls_response-source.
      ENDIF.
    ELSE.
      ls_response-object_name = lv_name.
    ENDIF.

    IF ls_response-source IS INITIAL.
      DATA(lv_json_partial) = /ui2/cl_json=>serialize( data = ls_response ).
      lo_entity->set_string_data(
            |\{ "message":"Not found.", "data": { lv_json_partial } \}| ).
      lo_entity->set_content_type( 'application/json; charset=UTF-8' ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    SELECT SINGLE text
      FROM trdirt
      WHERE name = @ls_response-object_name
      INTO @ls_response-object_description.

    DELETE ls_response-source WHERE table_line(1) = '*' OR table_line(1) = |"|.
    DELETE ls_response-source WHERE table_line = ''.

    DATA(lv_json) = /ui2/cl_json=>serialize( data = ls_response  ).

    lo_entity->set_string_data( |\{ "message":"Found", "data": { lv_json } \}| ).
    lo_entity->set_content_type( `application/json; charset=UTF-8` ) ##NO_TEXT.
    mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).

  ENDMETHOD.


  METHOD get_cds_source.

    "<<<<< GET REQUEST DATA
    DATA: BEGIN OF ls_json_input,
            r_obj_name TYPE string,
            o_obj_type TYPE string,
          END OF ls_json_input.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_json_input ).
    "<<<<<<<<<

    " Validate Fields
    CHECK validate_fields( EXPORTING is_fields = ls_json_input io_request = mo_response ) IS INITIAL.

    DATA: ls_response TYPE zquanthraes_sdo_001.
    ls_response-object_name = ls_json_input-r_obj_name.
    ls_response-object_type = ''.

    SELECT SINGLE *
      FROM tadir
      WHERE obj_name = @ls_json_input-r_obj_name
        AND object   = 'STOB'
      INTO @DATA(ls_tadir_cds).

    IF ls_tadir_cds IS NOT INITIAL.
      SELECT SINGLE ddddlsrc~ddlname,
           ddddlsrc~source,
           ddddlsrct~ddtext
      FROM ddddlsrc
      JOIN ddddlsrct ON ddddlsrc~ddlname     = ddddlsrct~ddlname
                    AND ddddlsrct~ddlanguage = @sy-langu
      WHERE ddddlsrc~ddlname = @ls_response-object_name
      INTO @DATA(ls_cds).

      ls_response-object_description = ls_cds-ddtext.
      ls_response-object_type        = 'CDS'.

      SPLIT ls_cds-source AT cl_abap_char_utilities=>cr_lf INTO TABLE ls_response-source.
      DELETE ls_response-source WHERE table_line CS '/*'.

      SELECT SINGLE *
        FROM ddlxsrc_src
        WHERE ddlxname = @ls_response-object_name
        INTO @DATA(ls_metadata_extension).
      SPLIT ls_metadata_extension-source AT cl_abap_char_utilities=>cr_lf INTO TABLE ls_response-source_extension.

    ELSE.
      " Se não é CDS é Tabela
      DATA lt_table_fields TYPE TABLE OF dd03p.
      CALL FUNCTION 'DDIF_TABL_GET'
        EXPORTING
          name          = CONV ddobjname( ls_response-object_name )
        TABLES
          dd03p_tab     = lt_table_fields
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      ls_response-source = VALUE w3_htmltab( FOR ls IN lt_table_fields ( |CAMPO: { ls-fieldname }; ELEMENTO_DADOS: { ls-rollname }; TIPO: { ls-inttype }| ) ).
      ls_response-object_type = 'TABLE'.
    ENDIF.

    DATA(lv_json) = /ui2/cl_json=>serialize( data = ls_response  ).

    lo_entity->set_string_data( |\{ "message":"Found", "data": { lv_json } \}| ).
    lo_entity->set_content_type( `application/json; charset=UTF-8` ) ##NO_TEXT.
    mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).

  ENDMETHOD.


  METHOD get_class_source.

    "<<<<< GET REQUEST DATA
    DATA: BEGIN OF ls_json_input,
            r_obj_name TYPE string,
            o_obj_type TYPE string,
          END OF ls_json_input.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_json_input ).
    "<<<<<<<<<

    " Validate Fields
    CHECK validate_fields( EXPORTING is_fields = ls_json_input io_request = mo_response ) IS INITIAL.

    DATA: ls_response TYPE zquanthraes_sdo_001.
    ls_response-object_name = ls_json_input-r_obj_name.
    ls_response-object_type = 'CLASS'.

    DATA: lo_cifref       TYPE REF TO if_oo_clif_incl_naming.
    DATA: lo_clsref       TYPE REF TO if_oo_class_incl_naming.
    DATA: lt_source_pool  TYPE seop_source_string.
    DATA: lt_source       TYPE seop_source_string.
    DATA: lv_tabix        TYPE sy-tabix.

    CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
      EXPORTING
        cifkey = CONV seoclskey( ls_response-object_name )
      RECEIVING
        cifref = lo_cifref
      EXCEPTIONS
        OTHERS = 1.

    lo_clsref ?= lo_cifref.

    IF lo_clsref IS NOT BOUND.
      DATA(lv_json_partial) = /ui2/cl_json=>serialize( data = ls_response  ).
      lo_entity->set_string_data( |\{ "message":"Not Found", "data": { lv_json_partial } \}| ).
      lo_entity->set_content_type( `application/json; charset=UTF-8` ) ##NO_TEXT.
      mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).
      RETURN.
    ENDIF.

    READ REPORT lo_clsref->class_pool
      INTO lt_source_pool.
    LOOP AT lt_source_pool INTO DATA(source_line).
      IF   source_line CP 'CLASS-POOL'
        OR source_line CP 'class-pool'.
        lv_tabix = sy-tabix.
        APPEND source_line TO ls_response-source.
        EXIT.
      ENDIF.
    ENDLOOP.

    READ REPORT lo_clsref->locals_old
      INTO lt_source.
    LOOP AT lt_source
      INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO ls_response-source.
      ENDIF.
    ENDLOOP.

    READ REPORT lo_clsref->locals_def
      INTO lt_source.
    LOOP AT lt_source
      INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO ls_response-source.
      ENDIF.
    ENDLOOP.

    READ REPORT lo_clsref->locals_imp
      INTO lt_source.
    LOOP AT lt_source
      INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO ls_response-source.
      ENDIF.
    ENDLOOP.

    READ REPORT lo_clsref->macros
      INTO lt_source.
    LOOP AT lt_source
      INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO ls_response-source.
      ENDIF.
    ENDLOOP.

    READ REPORT lo_clsref->public_section
      INTO lt_source.
    LOOP AT lt_source
      INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO ls_response-source.
      ENDIF.
    ENDLOOP.

    READ REPORT lo_clsref->protected_section
      INTO lt_source.
    LOOP AT lt_source
      INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO ls_response-source.
      ENDIF.
    ENDLOOP.

    READ REPORT lo_clsref->private_section
      INTO lt_source.
    LOOP AT lt_source
      INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO ls_response-source.
      ENDIF.
    ENDLOOP.

    CONCATENATE 'CLASS' ls_response-object_name 'IMPLEMENTATION' INTO DATA(lv_string) SEPARATED BY space.
    LOOP AT lt_source_pool
      FROM lv_tabix
      INTO source_line.
      IF source_line CS 'ENDCLASS'.
        APPEND source_line TO ls_response-source.
      ENDIF.
      IF source_line CS lv_string.
        SKIP.
        APPEND source_line TO ls_response-source.
        lv_tabix = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.

    DATA(lt_includes) = lo_clsref->get_all_method_includes( ).
    LOOP AT lt_includes
      INTO DATA(ls_include).
      READ REPORT ls_include-incname INTO lt_source.
      SKIP.
      LOOP AT lt_source
        INTO source_line.
        APPEND source_line TO ls_response-source.
      ENDLOOP.
    ENDLOOP.
    LOOP AT lt_source_pool
      FROM lv_tabix
      INTO source_line.
      IF source_line CS 'ENDCLASS'.
        APPEND source_line TO ls_response-source.
      ENDIF.
    ENDLOOP.

    DELETE ls_response-source WHERE table_line(1) = '*' OR table_line(1) = |"|.
    DELETE ls_response-source WHERE table_line = ''.

    DATA(ls_clskey) = VALUE seoclskey( clsname = ls_response-object_name ).
    DATA ls_class TYPE seoc_class_r.
    CALL FUNCTION 'SEO_CLASS_READ'
      EXPORTING
        clskey          = ls_clskey
        master_language = sy-langu
      IMPORTING
        class           = ls_class.

    ls_response-object_description = ls_class-descript.

    DATA(lv_json) = /ui2/cl_json=>serialize( data = ls_response  ).

    lo_entity->set_string_data( |\{ "message":"Found", "data": { lv_json } \}| ).
    lo_entity->set_content_type( `application/json; charset=UTF-8` ) ##NO_TEXT.
    mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).

  ENDMETHOD.


  METHOD get_function_source.

    "<<<<< GET REQUEST DATA
    DATA: BEGIN OF ls_json_input,
            r_obj_name TYPE string,
            o_obj_type TYPE string,
          END OF ls_json_input.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA(lv_request_body) = mo_request->get_entity( )->get_string_data( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_json_input ).
    "<<<<<<<<<

    " Validate Fields
    CHECK validate_fields( EXPORTING is_fields = ls_json_input io_request = mo_response ) IS INITIAL.

    DATA: ls_response TYPE zquanthraes_sdo_001.
    ls_response-object_name = ls_json_input-r_obj_name.
    ls_response-object_type = 'FUNCTION'.

    " Get Source Code Function
    SELECT SINGLE *
      FROM tfdir
      WHERE funcname = @ls_json_input-r_obj_name
      INTO @DATA(ls_include).

    DATA: lt_source TYPE TABLE OF rssource.
    DATA: lt_source_new TYPE rsfb_source.
    DATA: lt_import_parameter TYPE TABLE OF rsimp.
    DATA: lt_changing_parameter TYPE TABLE OF rscha.
    DATA: lt_export_parameter TYPE TABLE OF rsexp.
    DATA: lt_tables_parameter TYPE TABLE OF rstbl.
    DATA: lt_exception_list TYPE TABLE OF rsexc.
    DATA: lt_documentation TYPE TABLE OF rsfdo.
    DATA: lt_include_fm      TYPE w3_htmltab.
    DATA: lt_include_source  TYPE w3_htmltab.
    DATA: lt_include_sources TYPE w3_htmltab.
    DATA: lt_include_name    TYPE TABLE OF string.
    DATA: lv_include_name    TYPE w3_html.
    READ REPORT ls_include-pname INTO lt_include_fm.
    DELETE lt_include_fm WHERE table_line NS 'INCLUDE z'.

    LOOP AT lt_include_fm INTO DATA(ls_include_fm).
      CONDENSE ls_include_fm.
      SPLIT ls_include_fm AT ' ' INTO TABLE lt_include_name.
      lv_include_name = CONV string( lt_include_name[ 2 ] ).
      REPLACE ALL OCCURRENCES OF '.' IN lv_include_name WITH ''.
      READ REPORT lv_include_name INTO lt_include_source.
      DELETE lt_include_source WHERE table_line(1) = '*' OR table_line(1) = |"|.

      APPEND LINES OF lt_include_source TO lt_include_sources.

      CLEAR: lt_include_source[], lt_include_name[], lv_include_name, ls_include_fm.
    ENDLOOP.

    CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
      EXPORTING
        functionname       = CONV rs38l-name( ls_json_input-r_obj_name )
      TABLES
        source             = lt_source
        import_parameter   = lt_import_parameter
        changing_parameter = lt_changing_parameter
        export_parameter   = lt_export_parameter
        tables_parameter   = lt_tables_parameter
        exception_list     = lt_exception_list
        documentation      = lt_documentation
      CHANGING
        new_source         = lt_source_new
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.

    APPEND LINES OF lt_include_sources TO ls_response-source.
    APPEND '    ' TO ls_response-source.
    APPEND LINES OF lt_source_new TO ls_response-source.
    APPEND LINES OF lt_source TO ls_response-source.

    IF ls_response-source IS INITIAL.
      DATA(lv_json_partial) = /ui2/cl_json=>serialize( data = ls_response  ).
      lo_entity->set_string_data(
            |\{ "message":"Not found.", "data": { lv_json_partial } \}| ).
      lo_entity->set_content_type( 'application/json; charset=UTF-8' ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    SELECT SINGLE stext
      FROM tftit
      WHERE funcname = @ls_json_input-r_obj_name
      INTO @ls_response-object_description.

    DELETE ls_response-source WHERE table_line(1) = '*' OR table_line(1) = |"|.
    DELETE ls_response-source WHERE table_line = ''.

    DATA(lv_json) = /ui2/cl_json=>serialize( data = ls_response  ).

    lo_entity->set_string_data( |\{ "message":"Found", "data": { lv_json } \}| ).
    lo_entity->set_content_type( `application/json; charset=UTF-8` ) ##NO_TEXT.
    mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).

  ENDMETHOD.
ENDCLASS.
