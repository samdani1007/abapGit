@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking BO projection View'
@Metadata.allowExtensions: true
@Search.searchable: true
define view entity ZC_RAP_BOOK_S2
  as projection on ZI_RAP_BOOK_S2 as Booking
{
  key BookingUUID,
      TravelUUID,
      @Search.defaultSearchElement: true
      BookingID,
      
      BookingDate,
      @Consumption.valueHelpDefinition: [{ entity : {name: '/DMO/I_Customer', element: 'CustomerID'  } }]
      @ObjectModel.text.element: ['CustomerName']
      @Search.defaultSearchElement: true
      CustomerID,
      _Customer.LastName as CustomerName,
      Travel.TravelID as TravelID,
      CarrierID,
      _Carrier.Name      as CarrierName,
      @Consumption.valueHelpDefinition: [ {entity: {name: '/DMO/I_Flight', element: 'ConnectionID'},
                                          additionalBinding: [ { localElement: 'CarrierID',    element: 'AirlineID' },
                                                               { localElement: 'FlightDate',   element: 'FlightDate',   usage: #RESULT},
                                                               { localElement: 'FlightPrice',  element: 'Price',        usage: #RESULT },
                                                               { localElement: 'CurrencyCode', element: 'CurrencyCode', usage: #RESULT } ] } ]
      ConnectionID,
      FlightDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      FlightPrice,
      @Consumption.valueHelpDefinition: [{entity: {name: 'I_Currency', element: 'Currency' }}]
      CurrencyCode,
      CreatedBy,
      LastChangedBy,
      LocalLastChangedAt,
      /* Associations */
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _Flight,
      Travel : redirected to parent ZC_RAP_TRAVEL_S2
      
      
}
