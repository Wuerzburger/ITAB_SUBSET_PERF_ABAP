CLASS zcl_filter_perf_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
    DATA: out TYPE REF TO if_oo_adt_classrun_out.
  PRIVATE SECTION.
    DATA: mt_small TYPE tt_small.
    METHODS: init_data IMPORTING iv_entries TYPE i

           ,  run IMPORTING iv_entries TYPE i
           , measure_with_loop EXPORTING ev_start TYPE timestampl
                                         ev_end   TYPE timestampl
                               RETURNING VALUE(rv_runtime) TYPE decfloat16
                                , measure_with_loop_fs EXPORTING ev_start TYPE timestampl
                                         ev_end   TYPE timestampl
                               RETURNING VALUE(rv_runtime) TYPE decfloat16
           , measure_with_filter EXPORTING ev_start TYPE timestampl
                                           ev_end   TYPE timestampl
            , measure_with_for EXPORTING ev_start TYPE timestampl
                                       ev_end   TYPE timestampl
                               RETURNING VALUE(rv_runtime) TYPE decfloat16
           , measure_with_cond EXPORTING ev_start TYPE timestampl
                                        ev_end   TYPE timestampl
                               RETURNING VALUE(rv_runtime) TYPE decfloat16
           , measure_with_reduce EXPORTING ev_start TYPE timestampl
                                          ev_end   TYPE timestampl
                               RETURNING VALUE(rv_runtime) TYPE decfloat16
           .
ENDCLASS.



CLASS ZCL_FILTER_PERF_TEST IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    me->out = out.
    " Hier kannst du den Aufruf der Methode init_data implementieren, um die Daten zu initialisieren
    run( 10 ).
    run( 100 ).
    run( 1000 ).
    run( 10000 ).
    run( 100000 ).
    run( 1000000 ).

  ENDMETHOD.


  METHOD init_data.
    DATA: lv_counter    TYPE i,
          lv_random_str TYPE string,
          lv_mod_value  TYPE char1.


    CLEAR: mt_small.

    DO iv_entries TIMES.
      lv_counter = sy-index.

      " Zufällige Zeichenfolge erzeugen (kürzer und eleganter)
      lv_random_str = ''.
      DATA: lv_chars      TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
            lv_char_count TYPE i VALUE 62.
      DO 40 TIMES.
        DATA(lv_index) = cl_abap_random_int=>create( seed = lv_counter * sy-uzeit(4) )->get_next( ) MOD lv_char_count.
        lv_random_str = lv_random_str && lv_chars+lv_index(1).
      ENDDO.

      " Modulo-Operation zur Berechnung von element3
      CASE lv_counter MOD 3.
        WHEN 0.
          lv_mod_value = 'A'.
        WHEN 1.
          lv_mod_value = 'B'.
        WHEN 2.
          lv_mod_value = 'C'.
      ENDCASE.

      " Eintrag zur Tabelle hinzufügen
      INSERT VALUE #(
        element1 = lv_counter
        element2 = lv_random_str
        element3 = lv_mod_value
        element4 = cl_abap_random_int=>create( seed = lv_counter * 100 + sy-index )->get_next( ) MOD 99999 + 1
      ) INTO TABLE mt_small.
    ENDDO.


  ENDMETHOD.


  METHOD measure_with_cond.
    GET TIME STAMP FIELD ev_start.
    "DATA(lt_target) = COND tt_small( WHEN element3 = 'A' THEN lt_source ).
    "DATA(lt_target) = VALUE tt_small( FOR row IN mt_small ( COND #( WHEN row-element3 = 'A' THEN row ) ) ).

    DATA(lt_subset) = COND tt_small(
  LET lt_filtered = FILTER #( mt_small WHERE element3 = 'A' )
  IN
    WHEN lines( lt_filtered ) > 0 THEN lt_filtered
    ELSE VALUE tt_small( ) ).


    GET TIME STAMP FIELD ev_end.

    rv_runtime = ev_end - ev_start.
  ENDMETHOD.


  METHOD measure_with_filter.
    GET TIME STAMP FIELD ev_start.

    DATA(lt_target) = FILTER #( mt_small WHERE element3 = 'A' ).
    GET TIME STAMP FIELD ev_end.
  ENDMETHOD.


  METHOD measure_with_for.
    GET TIME STAMP FIELD ev_start.

    DATA(lt_target) = VALUE tt_small(
  FOR ls_source IN mt_small WHERE ( element3 = 'A' )
  ( ls_source )
).
    GET TIME STAMP FIELD ev_end.

    rv_runtime = ev_end - ev_start.
  ENDMETHOD.


  METHOD measure_with_loop.
    DATA: lt_target TYPE tt_small
        .
    GET TIME STAMP FIELD ev_start.

    "lv_start = cl_abap_context_info=>get_system_time( ).
    LOOP AT mt_small INTO DATA(ls_row) WHERE element3 = 'A'.
      APPEND ls_row TO lt_target.
    ENDLOOP.
    GET TIME STAMP FIELD ev_end.



    rv_runtime = ev_end - ev_start.
  ENDMETHOD.


   METHOD measure_with_loop_fs.
   DATA: lt_target TYPE tt_small
        .
    GET TIME STAMP FIELD ev_start.

    "lv_start = cl_abap_context_info=>get_system_time( ).
    LOOP AT mt_small ASSIGNING FIELD-SYMBOL(<ls_row>) WHERE element3 = 'A'.
      APPEND <ls_row> TO lt_target.
    ENDLOOP.
    GET TIME STAMP FIELD ev_end.



    rv_runtime = ev_end - ev_start.
  ENDMETHOD.


  METHOD measure_with_reduce.
    GET TIME STAMP FIELD ev_start.

DATA(lt_subset) = REDUCE tt_small(
  INIT lt_acc = VALUE tt_small( )
  FOR wa IN mt_small WHERE ( element3 = 'A' )
  NEXT lt_acc = VALUE tt_small( BASE lt_acc ( wa ) )
).
    GET TIME STAMP FIELD ev_end.

    rv_runtime = ev_end - ev_start.

  ENDMETHOD.


  METHOD run.
    data lv_diff type i.
    init_data( iv_entries = iv_entries ). " Beispielhafte Anzahl der Einträge
    measure_with_loop(
     IMPORTING
        ev_start = DATA(lv_start)
        ev_end   = DATA(lv_end)
    ).

    out->write( |LOOP INTO { lines( mt_small ) }| ).
    lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_end
                                   tstmp2 = lv_start ) / 1000000.

    out->write( | Laufzeit: { lv_diff } |  ).

     measure_with_loop_fs(
     IMPORTING
        ev_start = lv_start
        ev_end   = lv_end
    ).

    out->write( |LOOP ASSIGNING { lines( mt_small ) }| ).
    lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_end
                                   tstmp2 = lv_start ) / 1000000.

    out->write( | Laufzeit: { lv_diff } |  ).

    measure_with_filter(
 IMPORTING
    ev_start = lv_start
    ev_end   = lv_end
).

    out->write( |FILTER { lines( mt_small ) }| ).
   lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_end
                                   tstmp2 = lv_start ) .
    out->write( | Laufzeit:  { lv_diff } |  ).

    measure_with_for( IMPORTING ev_start = lv_start ev_end = lv_end ).
    out->write( |FOR { lines( mt_small ) }| ).
    lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_end
                                   tstmp2 = lv_start ) .
    out->write( | Laufzeit:  { lv_diff } |  ).

    measure_with_cond( IMPORTING ev_start = lv_start ev_end = lv_end ).
    out->write( |COND { lines( mt_small ) }| ).
lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_end
                                   tstmp2 = lv_start ) .
    out->write( | Laufzeit:  { lv_diff } |  ).

    measure_with_reduce( IMPORTING ev_start = lv_start ev_end = lv_end ).
    out->write( |REDUCE { lines( mt_small ) }| ).
lv_diff = cl_abap_tstmp=>subtract( tstmp1 = lv_end
                                   tstmp2 = lv_start ) .
    out->write( | Laufzeit: { lv_diff } |  ).
  ENDMETHOD.
ENDCLASS.
