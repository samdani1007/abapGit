@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bookind details for the approver'
@Metadata.allowExtensions: true
define view entity ZVI_C_BOOKING01_APPROVER
  as projection on ZVI_I_BOOKING01
{
  key TravelId,
  key BookingId,
      BookingDate,
      CustomerId,
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Carrier',
                                               element: 'AirlineID' } } ]
      CarrierId,
      CarrierName,
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
      //      Booksupplement,
      carrier,
      connection,
      customer,
      Travel : redirected to parent ZVI_C_TRAVEL01_APPROVER
}
