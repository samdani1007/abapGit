CLASS lhc_ZI_RAP_TRAVEL_S2 DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_rap_travel_s2 RESULT result.

ENDCLASS.

CLASS lhc_ZI_RAP_TRAVEL_S2 IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
