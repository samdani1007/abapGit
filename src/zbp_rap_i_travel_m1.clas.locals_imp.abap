CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1 VALUE 'O', "Open
        accepted TYPE c LENGTH 1 VALUE 'A', "Accepted
        rejected TYPE c LENGTH 1 VALUE 'X', "Rejected
      END OF travel_status.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR travel RESULT result.

    METHODS accepttravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~accepttravel RESULT result.

    METHODS rejecttravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~rejecttravel RESULT result.
    METHODS copytravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~copytravel.
    METHODS setstatustoopen FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~setstatustoopen.
    METHODS recalctotalprice FOR MODIFY
      IMPORTING keys FOR ACTION travel~recalctotalprice.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~calculatetotalprice.

    METHODS validatecustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatecustomer.

    METHODS validatedates FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatedates.

*    METHODS calculatetravelid FOR DETERMINE ON SAVE
*      IMPORTING keys FOR travel~calculatetravelid.
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE travel.

    METHODS earlynumbering_cba_booking FOR NUMBERING
      IMPORTING entities FOR CREATE travel\booking.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD get_instance_authorizations.


  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
             ENTITY Travel
             FIELDS ( TravelId OverallStatus )
              WITH CORRESPONDING #( keys )
     RESULT DATA(travel_result)
     FAILED failed.

*    READ TABLE travel_result INTO DATA(ls_travel) INDEX 1.
*    IF ls_travel-OverallStatus = 'X'.
*      DATA(lv_allow) = if_abap_behv=>fc-o-disabled.
*    ELSE.
*      lv_allow = if_abap_behv=>fc-o-enabled.
*    ENDIF.

    result = VALUE #( FOR Travel IN travel_result
                        ( %tky = travel-%tky
                        %features-%action-rejectTravel = COND #( WHEN travel-OverallStatus = 'X'
                                                               THEN if_abap_behv=>fc-o-disabled
                                                               ELSE if_abap_behv=>fc-o-enabled )
                        %features-%action-acceptTravel = COND #( WHEN travel-OverallStatus = 'A'
                                                               THEN if_abap_behv=>fc-o-disabled
                                                               ELSE if_abap_behv=>fc-o-enabled )
                          %assoc-Booking               = COND #( WHEN travel-OverallStatus = 'X'
                                                                 THEN if_abap_behv=>fc-o-disabled
                                                               ELSE if_abap_behv=>fc-o-enabled  )  )
                      ).



  ENDMETHOD.

  METHOD acceptTravel.

*  *************************************************************************************
* Instance-bound non-factory action: Set the overall travel status to 'A' (accepted)
*************************************************************************************

    " modify travel instance
    MODIFY ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-accepted ) )  " 'A'
    FAILED failed
    REPORTED reported.

    " read changed data for action result
    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(travels).

    " set the action result parameter
    result = VALUE #( FOR travel IN travels ( %tky   = travel-%tky
                                              %param = travel ) ).

  ENDMETHOD.

  METHOD rejectTravel.
*  *************************************************************************************
* Instance-bound non-factory action: Set the overall travel status to 'X' (Rejected)
*************************************************************************************

    " modify travel instance
    MODIFY ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-rejected ) )  " 'X'
    FAILED failed
    REPORTED reported.

    " read changed data for action result
    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(travels).

    " set the action result parameter
    result = VALUE #( FOR travel IN travels ( %tky   = travel-%tky
                                              %param = travel ) ).

  ENDMETHOD.

  METHOD copyTravel.

    DATA: travels       TYPE TABLE FOR CREATE zrap_i_travel_m1\\Travel,
          bookings_cba  TYPE TABLE FOR CREATE zrap_i_travel_m1\\Travel\Booking,
          booksuppl_cba TYPE TABLE FOR CREATE zrap_i_travel_m1\\Booking\Booksupplement.

    "step 1 remove the travel instances with initial %cid
    READ TABLE keys WITH KEY %cid = '' INTO DATA(key_with_initial_cid).

    ASSERT key_with_initial_cid IS INITIAL.

    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
               ENTITY Travel
               ALL FIELDS WITH CORRESPONDING #( keys )
       RESULT DATA(travel_read_result)
       FAILED failed.

    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
       ENTITY Travel BY \Booking
       ALL FIELDS WITH CORRESPONDING #( travel_read_result )
       RESULT DATA(book_read_result)
       FAILED failed.

    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
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
    MODIFY ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
                           ENTITY Travel
                             CREATE FIELDS ( AgencyID CustomerID BeginDate EndDate BookingFee TotalPrice CurrencyCode )
                                WITH Travels
                             CREATE BY \Booking FIELDS ( BookingId BookingDate CustomerId CarrierId ConnectionId )
                                WITH bookings_cba
                           ENTITY Booking
                             CREATE BY \Booksupplement FIELDS ( BookingSupplementId SupplementId Price CurrencyCode )
                                WITH booksuppl_cba
                           MAPPED DATA(mapped_create).

    mapped-travel =   mapped_create-travel.

  ENDMETHOD.

  METHOD earlynumbering_create.

    DATA: entity        TYPE STRUCTURE FOR CREATE zrap_i_travel_m1,
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

  METHOD earlynumbering_cba_Booking.

    DATA: max_booking_id TYPE /dmo/booking_id.

    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
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

  METHOD setStatusToOpen.


    "Read travel instances of the transferred keys
    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
     ENTITY Travel
       FIELDS ( OverallStatus )
       WITH CORRESPONDING #( keys )
     RESULT DATA(travels)
     FAILED DATA(read_failed).

    "If overall travel status is already set, do nothing, i.e. remove such instances
    DELETE travels WHERE OverallStatus IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    "else set overall travel status to open ('O')
    MODIFY ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR travel IN travels ( %tky          = travel-%tky
                                              OverallStatus = travel_status-open ) )
    REPORTED DATA(update_reported).

    "Set the changing parameter
    reported = CORRESPONDING #( DEEP update_reported ).

  ENDMETHOD.

  METHOD recalcTotalPrice.

    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: amount_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
          ENTITY Travel
             FIELDS ( BookingFee CurrencyCode )
             WITH CORRESPONDING #( keys )
          RESULT DATA(travels).

 DELETE travels WHERE CurrencyCode IS INITIAL.

      " Read all associated bookings and add them to the total price.
      READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
         ENTITY Travel BY \Booking
            FIELDS ( FlightPrice CurrencyCode )
          WITH CORRESPONDING #( travels )
          RESULT DATA(bookings).

read ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
           ENTITY Booking BY \Booksupplement
           FIELDS (  Price CurrencyCode )
           WITH CORRESPONDING #( bookings )
           RESULT DATA(booksuppls).



    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      " Set the start for the calculation by adding the booking fee.
      amount_per_currencycode = VALUE #( ( amount        = <travel>-BookingFee
                                           currency_code = <travel>-CurrencyCode ) ).

      LOOP AT bookings INTO DATA(booking) USING KEY id WHERE TravelId = <travel>-TravelId and
                                                           CurrencyCode is not INITIAL.

        COLLECT VALUE ty_amount_per_currencycode( amount        = booking-FlightPrice
                                                  currency_code = booking-CurrencyCode ) INTO amount_per_currencycode.
      ENDLOOP.

      LOOP AT booksuppls into data(booksuppl) USING KEY id WHERE TravelId = <travel>-TravelId and
                                                           CurrencyCode is not INITIAL.

      COLLECT VALUE ty_amount_per_currencycode( amount = booksuppl-Price
                                                currency_code = booksuppl-CurrencyCode ) INTO amount_per_currencycode.

      ENDLOOP.

      DELETE amount_per_currencycode WHERE currency_code is INITIAL.
      CLEAR <travel>-TotalPrice.
      LOOP AT amount_per_currencycode INTO DATA(single_amount_per_currencycode).
        " If needed do a Currency Conversion
        IF single_amount_per_currencycode-currency_code = <travel>-CurrencyCode.
          <travel>-TotalPrice += single_amount_per_currencycode-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  single_amount_per_currencycode-amount
               iv_currency_code_source     =  single_amount_per_currencycode-currency_code
               iv_currency_code_target     =  <travel>-CurrencyCode
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_booking_price_per_curr)
            ).
          <travel>-TotalPrice += total_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of travels
    MODIFY ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( TotalPrice )
        WITH CORRESPONDING #( travels ).

  ENDMETHOD.

  METHOD calculateTotalPrice.
    MODIFY ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
     ENTITY travel
       EXECUTE recalcTotalPrice
       FROM CORRESPONDING #( keys )
     REPORTED DATA(execute_reported).

    reported = CORRESPONDING #( DEEP execute_reported ).

  ENDMETHOD.

*  METHOD calculateTravelID.
*  ENDMETHOD.

  METHOD validatecustomer.

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.
    "read relevant travel instance data
    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
    ENTITY Travel
     FIELDS ( CustomerID )
     WITH CORRESPONDING #( keys )
    RESULT DATA(travels).
    "optimization of DB select: extract distinct non-initial customer IDs
    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = customerID EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.

    IF customers IS NOT INITIAL.
      "check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
                                FOR ALL ENTRIES IN @customers
                                WHERE customer_id = @customers-customer_id
        INTO TABLE @DATA(valid_customers).
    ENDIF.


    "raise msg for non existing and initial customer id
    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #(  %tky                 = travel-%tky
                       %state_area          = 'VALIDATE_CUSTOMER'
                     ) TO reported-travel.

      IF travel-CustomerID IS  INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky                = travel-%tky
                        %state_area         = 'VALIDATE_CUSTOMER'
                        %msg                = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_customer_id
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-CustomerID = if_abap_behv=>mk-on
                      ) TO reported-travel.

      ELSEIF travel-CustomerID IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = travel-CustomerID ] ).
        APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky                = travel-%tky
                         %state_area         = 'VALIDATE_CUSTOMER'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                customer_id = travel-customerid
                                                                textid      = /dmo/cm_flight_messages=>customer_unkown
                                                                severity    = if_abap_behv_message=>severity-error )
                         %element-CustomerID = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.

    ENDLOOP.



  ENDMETHOD.

  METHOD validatedates.

    READ ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
          ENTITY Travel
            FIELDS (  BeginDate EndDate TravelID )
            WITH CORRESPONDING #( keys )
          RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #(  %tky               = travel-%tky
                             %state_area        = 'VALIDATE_DATES' ) TO reported-travel.

      IF travel-BeginDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg              = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_begin_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.

      ENDIF.

      IF travel-BeginDate < cl_abap_context_info=>get_system_date( ) AND travel-BeginDate IS NOT INITIAL.
        APPEND VALUE #( %tky               = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                   %state_area        = 'VALIDATE_DATES'
                    %msg              = NEW /dmo/cm_flight_messages(
                                                           begin_date = travel-BeginDate
                                                           textid     = /dmo/cm_flight_messages=>begin_date_on_or_bef_sysdate
                                                           severity   = if_abap_behv_message=>severity-error )
                   %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.


      ENDIF.


      IF travel-EndDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_end_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.


      IF travel-EndDate < travel-BeginDate AND travel-BeginDate IS NOT INITIAL
                                      AND travel-EndDate IS NOT INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW /dmo/cm_flight_messages(
                                                                textid     = /dmo/cm_flight_messages=>begin_date_bef_end_date
                                                                begin_date = travel-BeginDate
                                                                end_date   = travel-EndDate
                                                                severity   = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.


    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
