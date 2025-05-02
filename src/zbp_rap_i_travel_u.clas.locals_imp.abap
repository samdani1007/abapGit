CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE Travel.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Travel.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Travel.

    METHODS read FOR READ
      IMPORTING keys FOR READ Travel RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Travel.

    METHODS rba_Booking FOR READ
      IMPORTING keys_rba FOR READ Travel\Booking FULL result_requested RESULT result LINK association_links.

    METHODS cba_Booking FOR MODIFY
      IMPORTING entities_cba FOR CREATE Travel\Booking.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.

  DATA messages   TYPE /dmo/t_message.
    DATA legacy_entity_in  TYPE /dmo/travel.
    DATA legacy_entity_out TYPE /dmo/travel.
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

      legacy_entity_in = CORRESPONDING #( <entity> MAPPING FROM ENTITY USING CONTROL ).


      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_CREATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/s_travel_in( legacy_entity_in )
        IMPORTING
          es_travel   =  legacy_entity_out
          et_messages = messages.

      IF messages IS INITIAL.
        APPEND VALUE #( %cid = <entity>-%cid
                        travelid = legacy_entity_out-travel_id ) TO mapped-travel.
      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = legacy_entity_in-travel_id ) TO failed-travel.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( travelid = legacy_entity_in-travel_id
                        %msg = new_message( id = messages[ 1 ]-msgid
                                            number = messages[ 1 ]-msgno
                                            v1 = messages[ 1 ]-msgv1
                                            v2 = messages[ 1 ]-msgv2
                                            v3 = messages[ 1 ]-msgv3
                                            v4 = messages[ 1 ]-msgv4
                                            severity = CONV #( messages[ 1 ]-msgty ) )
       ) TO reported-travel.


      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.

  DATA: legacy_entity_out TYPE /dmo/travel,
          messages          TYPE /dmo/t_message.

    LOOP AT keys INTO DATA(key) GROUP BY key-TravelId.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = key-travelid
        IMPORTING
          es_travel    = legacy_entity_out
          et_messages  = messages.

      IF messages IS INITIAL.
        "fill result parameter with flagged fields

        INSERT CORRESPONDING #( legacy_entity_out MAPPING TO ENTITY ) INTO TABLE result.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = key-travelid ) TO failed-travel.

        LOOP AT messages INTO DATA(message).

          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = key-travelid
                          %msg = new_message( id = message-msgid
                                              number = message-msgno
                                              v1 = message-msgv1
                                              v2 = message-msgv2
                                              v3 = message-msgv3
                                              v4 = message-msgv4
                                              severity = CONV #( message-msgty ) )


         ) TO reported-travel.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD rba_Booking.
  ENDMETHOD.

  METHOD cba_Booking.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_Booking DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Booking.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Booking.

    METHODS read FOR READ
      IMPORTING keys FOR READ Booking RESULT result.

    METHODS rba_Travel FOR READ
      IMPORTING keys_rba FOR READ Booking\Travel FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_Booking IMPLEMENTATION.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD rba_Travel.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZRAP_I_TRAVEL_U DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZRAP_I_TRAVEL_U IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

   CALL FUNCTION '/DMO/FLIGHT_TRAVEL_SAVE'.

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
