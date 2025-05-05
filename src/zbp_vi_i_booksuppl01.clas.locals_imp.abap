CLASS lhc_booksuppl DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booksuppl~calculateTotalPrice.

ENDCLASS.

CLASS lhc_booksuppl IMPLEMENTATION.

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
