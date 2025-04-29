CLASS lhc_booksuppl DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booksuppl~calculateTotalPrice.

ENDCLASS.

CLASS lhc_booksuppl IMPLEMENTATION.

  METHOD calculateTotalPrice.

   DATA: TRAVEL_IDS TYPE STANDARD TABLE OF zrap_i_travel_m1 WITH UNIQUE HASHED KEY KEY COMPONENTS TravelId.

  TRAVEL_IDS = CORRESPONDING #( keys discarding duplicates mapping TravelId = TravelId ).

      MODIFY ENTITIES OF zrap_i_travel_m1 IN LOCAL MODE
     ENTITY travel
       EXECUTE recalcTotalPrice
       FROM CORRESPONDING #( TRAVEL_IDS ).
*     REPORTED DATA(execute_reported).

*    reported = CORRESPONDING #( DEEP execute_reported ).

  ENDMETHOD.

ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
