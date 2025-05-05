CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1 VALUE 'O', "Open
        accepted TYPE c LENGTH 1 VALUE 'A', "Accepted
        rejected TYPE c LENGTH 1 VALUE 'X', "Rejected
      END OF travel_status,

      BEGIN OF customer_message_blnk,
        msgid TYPE symsgid VALUE 'ZVI_RAP_MSG_CLAS',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF customer_message_blnk,

      BEGIN OF customer_message_invalid,
        msgid TYPE symsgid VALUE 'ZVI_RAP_MSG_CLAS',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF customer_message_invalid,

      BEGIN OF status_message,
        msgid TYPE symsgid VALUE 'ZVI_RAP_MSG_CLAS',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF status_message.




    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.

    METHODS copytravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~copytravel.


    METHODS validatecustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatecustomer.
    METHODS validatestatus FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatestatus.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR travel RESULT result.

    METHODS setstatustoopen FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~setstatustoopen.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~calculatetotalprice.
    METHODS recalculctotalprice FOR MODIFY
      IMPORTING keys FOR ACTION travel~recalculctotalprice.

    METHODS approve FOR MODIFY
      IMPORTING keys FOR ACTION travel~approve RESULT result.

    METHODS reject FOR MODIFY
      IMPORTING keys FOR ACTION travel~reject RESULT result.

    METHODS earlynumbering_cba_booking FOR NUMBERING
      IMPORTING entities FOR CREATE travel\booking.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE travel.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD earlynumbering_create.

    DATA: entity        TYPE STRUCTURE FOR CREATE zvi_i_travel01,
          travel_id_max TYPE /dmo/travel_id.

    LOOP AT entities INTO entity WHERE TravelId IS NOT INITIAL.

      APPEND CORRESPONDING #( entity ) TO mapped-travel.

    ENDLOOP.

    DATA(entities_wo_travelid) = entities.
    DELETE entities_wo_travelid WHERE TravelId IS NOT INITIAL.

    TRY.

        cl_numberrange_runtime=>number_get(
                                   EXPORTING
                                    nr_range_nr = '01'
                                    object = '/DMO/TRV_M'
                                    quantity = CONV #( lines( entities_wo_travelid ) )
                                    IMPORTING
                                    number = DATA(number_range_key)
                                    returncode = DATA(number_range_return_code)
                                    returned_quantity = DATA(number_range_returned_quantity)
                                    ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).

        LOOP AT entities_wo_travelid INTO entity.

          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %msg = lx_number_ranges ) TO reported-travel.

          APPEND VALUE #( %cid = entity-%cid
                           %key = entity-%key
                           ) TO Failed-travel.

        ENDLOOP.

    ENDTRY.

    CASE number_range_return_code.
      WHEN '1'. " returned number is in the critical range
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %msg = NEW /dmo/cm_flight_messages(
                                                     textid = /dmo/cm_flight_messages=>number_range_depleted
                                                     severity = if_abap_behv_message=>severity-warning )
                           ) TO reported-travel.

        ENDLOOP.

      WHEN '2' OR '3'.
        "2 - the last number of the interval was returned
        "3 - if fewer numbers are available than requested
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                           %key = entity-%key
                           %msg = NEW /dmo/cm_flight_messages(
                                                      textid = /dmo/cm_flight_messages=>number_range_depleted
                                                      severity = if_abap_behv_message=>severity-warning )
                            ) TO reported-travel.
          APPEND VALUE #( %cid = entity-%cid
                         %key = entity-%key
                         %fail-cause = if_abap_behv=>cause-conflict
                          ) TO failed-travel.

        ENDLOOP.

    ENDCASE.

    " final check for all numbers
    ASSERT number_range_returned_quantity = lines( entities_wo_travelid ) .
    travel_id_max = number_range_key - number_range_returned_quantity.

    LOOP AT entities_wo_travelid INTO entity.

      travel_id_max += 1.

      entity-TravelId = travel_id_max.

      APPEND VALUE #(  %cid = entity-%cid
                       %key = entity-%key ) TO mapped-travel.

    ENDLOOP.





  ENDMETHOD.

  METHOD copyTravel.

    DATA: travels       TYPE TABLE FOR CREATE zvi_i_travel01\\Travel,
          bookings_cba  TYPE TABLE FOR CREATE zvi_i_travel01\\Travel\Booking,
          booksuppl_cba TYPE TABLE FOR CREATE zvi_i_travel01\\Booking\Booksupplement.


    "step 1 remove the travel instances with initial %cid
    READ TABLE keys WITH KEY %cid = '' INTO DATA(key_with_initial_cid).

    ASSERT key_with_initial_cid IS INITIAL.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
               ENTITY Travel
               ALL FIELDS WITH CORRESPONDING #( keys )
       RESULT DATA(travel_read_result)
       FAILED failed.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
       ENTITY Travel BY \Booking
       ALL FIELDS WITH CORRESPONDING #( travel_read_result )
       RESULT DATA(book_read_result)
       FAILED failed.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
            ENTITY Booking BY \Booksupplement
            ALL FIELDS WITH CORRESPONDING #( book_read_result )
    RESULT DATA(booksuppl_read_result)
    FAILED failed.

    LOOP AT travel_read_result ASSIGNING FIELD-SYMBOL(<travel>).

      " Fill travel internal table for travel data creation -%cid
      APPEND VALUE #( %cid = keys[ KEY entity %tky = <travel>-%tky ]-%cid
                      %data = CORRESPONDING #( <travel> EXCEPT travelid ) )
                      TO travels ASSIGNING FIELD-SYMBOL(<new_travel>).

      <new_travel>-BeginDate = cl_abap_context_info=>get_system_date(  ).
      <new_travel>-EndDate = cl_abap_context_info=>get_system_date(  ).
      <new_travel>-OverallStatus = 'O'.

      " Fill booking internal table for booking data creation - %cid_ref
      APPEND VALUE #( %cid_ref = keys[ KEY entity %tky = <travel>-%tky ]-%cid )
           TO Bookings_cba ASSIGNING FIELD-SYMBOL(<bookings_cba>).

      LOOP AT book_read_result ASSIGNING FIELD-SYMBOL(<bookings>) USING KEY
                                        entity WHERE TravelId = <travel>-TravelId.


        APPEND VALUE #( %cid     = keys[ KEY entity %tky = <travel>-%tky ]-%cid && <bookings>-BookingId
                       %data    = CORRESPONDING #(  book_read_result[ KEY entity %tky = <bookings>-%tky ] EXCEPT TravelId ) )
                       TO <bookings_cba>-%target ASSIGNING FIELD-SYMBOL(<new_booking>).

        <new_booking>-BookingStatus = 'N'.

        "Fill %cid_ref of booking as instance identifier for cba booksuppl
        APPEND VALUE #( %cid_ref = keys[ KEY entity %tky = <travel>-%tky ]-%cid && <bookings>-BookingId )
                        TO booksuppl_cba ASSIGNING FIELD-SYMBOL(<booksuppl_cba>).

        LOOP AT booksuppl_read_result ASSIGNING FIELD-SYMBOL(<booksuppl>) USING KEY
                                                       entity WHERE TravelId = <bookings>-TravelId AND
                                                                    BookingId = <bookings>-BookingId.

          "Fill booksuppl container for creating supplement with cba
          APPEND VALUE #( %cid  = keys[ KEY entity %tky = <travel>-%tky ]-%cid  && <bookings>-BookingId && <booksuppl>-BookingSupplementId
                          %data = CORRESPONDING #( <booksuppl> EXCEPT TravelId BookingId ) )
                          TO <booksuppl_cba>-%target.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    "Create ne BO instance
    MODIFY ENTITIES OF zvi_i_travel01 IN LOCAL MODE
                           ENTITY Travel
                             CREATE FIELDS ( AgencyID CustomerID BeginDate EndDate BookingFee TotalPrice CurrencyCode )
                                WITH Travels
                             CREATE BY \Booking FIELDS ( BookingId BookingDate CustomerId CarrierId ConnectionId )
                                WITH bookings_cba
                           ENTITY Booking
                             CREATE BY \Booksupplement FIELDS ( BookingSupplementId SupplementId Price CurrencyCode )
                                WITH booksuppl_cba
                           MAPPED DATA(mapped_create)
                           FAILED DATA(lv_failed)
                           REPORTED DATA(lv_reported).

    mapped-travel =   mapped_create-travel.



  ENDMETHOD.

  METHOD earlynumbering_cba_Booking.


    DATA: max_booking_id TYPE /dmo/booking_id.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
                          ENTITY Travel BY \Booking
                          FROM CORRESPONDING #( entities )
                          LINK DATA(bookings).
    IF sy-subrc EQ 0.
      LOOP AT entities ASSIGNING FIELD-SYMBOL(<Travel>) GROUP BY <Travel>-TravelId.

*        max_booking_id = REDUCE #( INIT max = CONV /dmo/booking_id( '0' )
*                                   FOR Booking IN Bookings USING KEY entity WHERE ( source-TravelId = <Travel>-TravelId )
*                                   NEXT max = COND /dmo/booking_id( WHEN Booking-target-BookingId > Max
*                                                                      THEN Booking-target-BookingId
*                                                                      ELSE Max )
*                                   ).
        LOOP AT Bookings ASSIGNING FIELD-SYMBOL(<Travel_Booking>) USING KEY entity WHERE source-TravelId = <Travel>-TravelId .

          IF  max_booking_id < <Travel_Booking>-target-BookingId.
            max_booking_id = <Travel_Booking>-target-BookingId.
          ENDIF.

        ENDLOOP.

*        max_booking_id = REDUCE #(  INIT max = max_booking_id
*                                    FOR entity IN entities USING KEY entity WHERE ( TravelId = <Travel>-TravelId )
*                                    FOR target IN entity-%target
*                                    NEXT max = COND /dmo/booking_id( WHEN target-BookingId > max
*                                                                       THEN target-BookingId
*                                                                       ELSE max )
*                                    ).

        LOOP AT entities ASSIGNING FIELD-SYMBOL(<ls_entities>) USING KEY entity
                                                    WHERE TravelId = <Travel>-TravelId.
          LOOP AT <ls_entities>-%target INTO DATA(ls_target).
            IF  max_booking_id < ls_target-BookingId.
              max_booking_id = ls_target-BookingId.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

        LOOP AT <Travel>-%target ASSIGNING FIELD-SYMBOL(<booking_wo_numbers>).
          APPEND CORRESPONDING #( <booking_wo_numbers> ) TO mapped-booking ASSIGNING FIELD-SYMBOL(<mapped_booking>).
          IF <booking_wo_numbers>-BookingId IS INITIAL.
            max_booking_id += 10.
            <mapped_booking>-BookingId = max_booking_id.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.

  METHOD validatecustomer.



    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
     ENTITY Travel
     FIELDS ( CustomerID )
       WITH CORRESPONDING #( keys )
       RESULT DATA(travels).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).

    DELETE customers WHERE customer_id IS INITIAL.

    IF customers IS NOT INITIAL.

      SELECT FROM /dmo/customer FIELDS customer_id FOR ALL ENTRIES IN @customers
                                                          WHERE customer_id = @customers-customer_id
                             INTO TABLE @DATA(valid_customers).
    ENDIF.

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #(  %tky = travel-%tky
                        %state_area = 'VALIDATE_CUSTOMER' ) TO reported-travel.

      IF travel-CustomerID IS INITIAL.

        APPEND VALUE #( %tky = travel-%tky  ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'VALIDATE_CUSTOMER'
                        %msg =  NEW /dmo/cm_flight_messages(
                                        textid = customer_message_blnk                 "/dmo/cm_flight_messages=>enter_customer_id
                                        severity = if_abap_behv_message=>severity-error )
                       %element-CustomerID = if_abap_behv=>mk-on        ) TO reported-travel.

      ELSEIF travel-CustomerID IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = travel-CustomerID ] ).


        APPEND VALUE #( %tky = travel-%tky  ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'VALIDATE_CUSTOMER'
                        %msg = NEW /dmo/cm_flight_messages(
                                        textid = customer_message_invalid
                                        severity = if_abap_behv_message=>severity-error )
                       %element-CustomerID = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.



    ENDLOOP.

  ENDMETHOD.

  METHOD validatestatus.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
    ENTITY Travel
    FIELDS ( OverallStatus )
      WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).

      CASE travel-OverallStatus.
        WHEN 'O'. "open

        WHEN 'X'. "Cancelled

        WHEN 'A'.  "Approved

        WHEN OTHERS.

          APPEND VALUE #( %tky = travel-%tky )  TO failed-travel.

          APPEND VALUE #(  %tky = travel-%tky
                          %state_area = 'VALIDATE_CUSTOMER'
                           %msg = NEW /dmo/cm_flight_messages(
                                    textid = status_message
                                  severity = if_abap_behv_message=>severity-error
                                    status = travel-OverallStatus )
                          %element-OverallStatus = if_abap_behv=>mk-on  ) TO reported-travel.


      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
      ENTITY Travel
      FIELDS ( TravelId OverallStatus )
        WITH CORRESPONDING #( keys )
        RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels
                   ( %tky = travel-%tky
                     %features-%action-Reject = COND #( WHEN travel-OverallStatus = 'X'
                                                      THEN IF_ABAP_BEHV=>fc-o-disabled
                                                       ELSE IF_ABAP_BEHV=>fc-o-enabled )
                     %features-%action-Approve = COND #( WHEN travel-OverallStatus = 'A'
                                                        THEN IF_ABAP_BEHV=>fc-o-disabled
                                                       ELSE IF_ABAP_BEHV=>fc-o-enabled )
                    %assoc-Booking =  COND #( WHEN travel-OverallStatus = 'X'
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled ) )
                    ).

  ENDMETHOD.

  METHOD setStatusToOpen.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
      ENTITY Travel
      FIELDS ( OverallStatus )
      WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DELETE travels WHERE overallstatus IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    MODIFY ENTITIES OF zvi_i_travel01 IN LOCAL MODE
      ENTITY Travel
      UPDATE FIELDS ( OverallStatus )
      WITH VALUE #( FOR travel IN travels
                    ( %tky = travel-%tky
                      OverallStatus = travel_status-open ) )
      FAILED DATA(lv_failed)
      REPORTED DATA(lv_reported).




  ENDMETHOD.

  METHOD calculateTotalPrice.

    MODIFY ENTITIES OF zvi_i_travel01 IN LOCAL MODE
      ENTITY Travel
      EXECUTE recalculcTotalPrice
      FROM CORRESPONDING #( keys )
      REPORTED DATA(lv_reported).

  ENDMETHOD.

  METHOD recalculcTotalPrice.

    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: amount_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
      ENTITY Travel
      FIELDS ( BookingFee CurrencyCode )
      WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DELETE travels WHERE CurrencyCode IS INITIAL.

    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
               ENTITY Travel BY \Booking
               FIELDS ( FlightPrice CurrencyCode )
               WITH CORRESPONDING #( travels )
               RESULT DATA(bookings).


    READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
               ENTITY Booking BY \Booksupplement
               FIELDS ( Price CurrencyCode )
               WITH CORRESPONDING #( bookings )
               RESULT DATA(booksuppls).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).

      amount_per_currencycode = VALUE #( ( amount = <travel>-BookingFee
                                         currency_code = <travel>-CurrencyCode ) ).

      LOOP AT bookings INTO DATA(booking) USING KEY id WHERE TravelId = <travel>-TravelId
                                                          AND CurrencyCode IS NOT INITIAL.

        COLLECT VALUE ty_amount_per_currencycode(  amount = booking-FlightPrice
                                                  currency_code = booking-CurrencyCode ) INTO amount_per_currencycode.

      ENDLOOP.

      LOOP AT booksuppls INTO DATA(booksuppl) USING KEY id WHERE TravelId =  <travel>-TravelId
                                                              AND CurrencyCode IS NOT INITIAL.

        COLLECT VALUE ty_amount_per_currencycode(  amount = booksuppl-Price
                                          currency_code = booksuppl-CurrencyCode ) INTO amount_per_currencycode.
      ENDLOOP.


      DELETE amount_per_currencycode WHERE currency_code IS INITIAL.

      CLEAR <travel>-TotalPrice.

      LOOP AT amount_per_currencycode INTO DATA(single_amount_per_currencycode).

        IF single_amount_per_currencycode-currency_code = <travel>-CurrencyCode.

          <travel>-TotalPrice += single_amount_per_currencycode-amount.

        ELSE.

          /dmo/cl_flight_amdp=>convert_currency(
                    EXPORTING
                       iv_amount = single_amount_per_currencycode-amount
                       iv_currency_code_source = single_amount_per_currencycode-currency_code
                       iv_currency_code_target = <travel>-CurrencyCode
                       iv_exchange_rate_date = cl_abap_context_info=>get_system_date( )
                     IMPORTING
                      ev_amount   = DATA(total_booking_price_per_curr)
                      ).

          <travel>-TotalPrice +=   total_booking_price_per_curr.

        ENDIF.


      ENDLOOP.

    ENDLOOP.

    MODIFY ENTITIES OF zvi_i_travel01 IN LOCAL MODE
            ENTITY Travel
            UPDATE FIELDS ( TotalPrice )
            WITH CORRESPONDING #( travels ).





  ENDMETHOD.

  METHOD Approve.
  MODIFY ENTITIES OF zvi_i_travel01 IN LOCAL MODE
          ENTITY Travel
           UPDATE FIELDS ( OverallStatus )
           WITH VALUE #(  for key IN keys (  %tky = key-%tky
                                              OverallStatus = 'A' ) ).

  READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
             ENTITY Travel
             ALL FIELDS WITH
             CORRESPONDING #( keys )
             RESULT data(travels).
 result =  value #(  for travel IN travels  (  %tky = travel-%tky
                                         %param = travel ) ).

  ENDMETHOD.

  METHOD Reject.

  MODIFY ENTITIES OF zvi_i_travel01 IN LOCAL MODE
          ENTITY Travel
           UPDATE FIELDS ( OverallStatus )
           WITH VALUE #(  for key IN keys (  %tky = key-%tky
                                              OverallStatus = 'X' ) ).

  READ ENTITIES OF zvi_i_travel01 IN LOCAL MODE
             ENTITY Travel
             ALL FIELDS WITH
             CORRESPONDING #( keys )
             RESULT data(travels).
 result =  value #(  for travel IN travels  (  %tky = travel-%tky
                                         %param = travel ) ).

  ENDMETHOD.

ENDCLASS.
