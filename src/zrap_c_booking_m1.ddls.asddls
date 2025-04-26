@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection view for Booking'
@Metadata.allowExtensions: true
define view entity ZRAP_C_BOOKING_M1
  as projection on ZRAP_I_BOOKING_M1
{
  key TravelId,
  key BookingId,
      BookingDate,
      CustomerId,

      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Carrier',
                                   element: 'AirlineID' } } ]
      CarrierId,

      @Consumption.valueHelpDefinition: [{ entity: { name:'/DMO/I_Connection',
                                            element: 'ConnectionID' },
                                     additionalBinding: [ { localElement:'CarrierId',
                                                             element: 'AirlineID' } ]

                                   } ]
      ConnectionId,
      FlightDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      FlightPrice,
      CurrencyCode,
      BookingStatus,
      LastChangedAt,
      /* Associations */
      Booksupplement : redirected to composition child ZRAP_C_BOOKSUPPL_M1,
      carrier,
      connection,
      customer,
      status,
      Travel         : redirected to parent ZRAP_C_TRAVEL_M1
}
