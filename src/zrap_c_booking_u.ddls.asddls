@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection view for Booking'
@Metadata.allowExtensions: true
define view entity ZRAP_C_BOOKING_U
  as projection on ZRAP_I_BOOKING_U
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
      //      BookingStatus,
      //      LastChangedAt,
      /* Associations */
      carrier,
      connection,
      customer,
      //      status,
      Travel : redirected to parent ZRAP_C_TRAVEL_U
}
