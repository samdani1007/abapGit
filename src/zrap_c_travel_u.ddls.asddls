@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection Travel view'
@Metadata.allowExtensions: true
define root view entity ZRAP_C_TRAVEL_U
  provider contract transactional_query
  as projection on ZRAP_I_TRAVEL_U
{
      @EndUserText.quickInfo: 'Travel request'
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZRAP_I_TRAVEL_U',
                                                    element: 'TravelId' } }]

  key TravelId,

      @ObjectModel.text.element: ['AgencyName']
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Agency_StdVH',
                                                     element: 'AgencyID' }, useForValidation: true  }]
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
      AgencyID,
      agency.Name       as AgencyName,

      @ObjectModel.text.element: [ 'CustomerName' ]
      @Consumption.valueHelpDefinition: [{ entity: {name: '/DMO/I_Customer_StdVH',
                                                   element: 'CustomerID' }
                                                   } ]

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
      CustomerID,
      @Semantics.text: true
      @ObjectModel.text.element: [ 'CustomerName' ]
      @EndUserText.label: 'Customer Name'
      customer.LastName as CustomerName,
      BeginDate,
      EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      TotalPrice,
      CurrencyCode,
      Description,
      //      @ObjectModel.text.element: [ 'StatusText' ]
      @Consumption.valueHelpDefinition: [{ entity: {name: '/DMO/I_Travel_Status_VH',
                                                  element: 'TravelStatus' }
                                                  } ]
      Status,
      @Semantics.text: true
      //      StatusText,
      //      Criticality,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      /* Associations */
      agency,
      Booking : redirected to composition child ZRAP_C_BOOKING_U,
      currency,
      customer
      //      status
}
