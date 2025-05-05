@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel projection approver'
@Metadata.allowExtensions: true
define root view entity ZVI_C_TRAVEL01_APPROVER
  provider contract transactional_query
  as projection on ZVI_I_TRAVEL01
{
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZVI_I_TRAVEL',
                                                     element: 'TravelId' } }]
  key TravelId,

      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Agency_StdVH',
                                                     element: 'AgencyID'  } } ]

      AgencyID,
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Customer_StdVH',
                                                     element: 'CustomerID' } } ]
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @ObjectModel.text.element: [ 'CustomerName' ]
      CustomerID,
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
      @ObjectModel.text.element: ['StatusText']
      @Consumption.valueHelpDefinition: [{ entity: { name: '/DMO/I_Overall_Status_VH',
                                                     element: 'OverallStatus' } } ]

      OverallStatus,
      StatusText,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      /* Associations */
      agency,
      Booking : redirected to composition child ZVI_C_BOOKING01_APPROVER,
      currency,
      //      customer,
      status
}
