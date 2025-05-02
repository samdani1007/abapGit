CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS earlynumbering_cba_Booksupplem FOR NUMBERING
      IMPORTING entities FOR CREATE Booking\Booksupplement.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~calculateTotalPrice.

ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

  METHOD earlynumbering_cba_Booksupplem.

    DATA: max_booking_suppl_id TYPE /dmo/booking_supplement_id.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
                    ENTITY booking BY \Booksupplement
                    FROM CORRESPONDING #( entities )
                    LINK DATA(booking_supplements).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<booking>) GROUP BY <booking>-%tky.

      " Get highest bookingsupplement_id from bookings belonging to booking
*      max_booking_suppl_id = REDUCE #( INIT max = CONV /dmo/booking_supplement_id( '0' )
*                                       FOR  booksuppl IN booking_supplements USING KEY entity
*                                                                             WHERE (     source-TravelId  = <booking>-TravelId
*                                                                                     AND source-BookingId = <booking>-BookingId )
*                                       NEXT max = COND /dmo/booking_supplement_id( WHEN   booksuppl-target-BookingSupplementId > max
*                                                                          THEN booksuppl-target-BookingSupplementId
*                                                                          ELSE max )
*                                      ).


      LOOP AT booking_supplements ASSIGNING FIELD-SYMBOL(<booksuppl>) WHERE source-BookingId = <booking>-BookingId .

        IF max_booking_suppl_id < <booksuppl>-target-BookingSupplementId.       "-target-BookingId.
          max_booking_suppl_id = <booksuppl>-target-BookingSupplementId.
        ENDIF.

      ENDLOOP.

*      " Get highest assigned bookingsupplement_id from incoming entities
*      max_booking_suppl_id = REDUCE #( INIT max = max_booking_suppl_id
*                                       FOR  entity IN entities USING KEY entity
*                                                               WHERE (     TravelId  = <booking>-TravelId
*                                                                       AND BookingId = <booking>-BookingId )
*                                       FOR  target IN entity-%target
*                                      NEXT max = COND /dmo/booking_supplement_id( WHEN   target-BookingSupplementId > max
*                                                                                 THEN target-BookingSupplementId
*                                                                                  ELSE max )
*
*                                     ).

      LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_entities>) "USING key entity
                                                        WHERE BookingId = <booking>-BookingId .
        LOOP AT <booking>-%target  INTO DATA(ls_target).
          IF  max_booking_suppl_id < ls_target-BookingSupplementId.
            max_booking_suppl_id = ls_target-BookingSupplementId.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      LOOP AT <booking>-%target ASSIGNING FIELD-SYMBOL(<booksuppl_wo_numbers>).
        APPEND CORRESPONDING #( <booksuppl_wo_numbers> ) TO mapped-booksuppl ASSIGNING FIELD-SYMBOL(<mapped_booksuppl>).
        IF <booksuppl_wo_numbers>-BookingSupplementId IS INITIAL.
          max_booking_suppl_id += 1 .
          <mapped_booksuppl>-BookingSupplementId = max_booking_suppl_id .
        ENDIF.
      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.

  METHOD calculateTotalPrice.



  MODIFY ENTITIES OF zvi_i_travel01 IN LOCAL MODE
             ENTITY Travel
             EXECUTE recalculcTotalPrice
             FROM CORRESPONDING #( keys )
              REPORTED DATA(execute_reported).

  ENDMETHOD.

ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
