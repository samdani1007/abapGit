@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel details'

define root view entity ZRAP_I_TRAVEL_U
  as select from /dmo/travel as Travel

  composition [0..*] of ZRAP_I_BOOKING_U        as Booking
  association [0..1] to /DMO/I_Agency           as agency       on $projection.AgencyID = agency.AgencyID
  association [0..1] to /DMO/I_Customer         as customer     on $projection.CustomerID = customer.CustomerID
  association [0..1] to I_Currency              as currency     on $projection.CurrencyCode = currency.Currency
  association [1..1] to /DMO/I_Travel_Status_VH as Travelstatus on $projection.Status = Travelstatus.TravelStatus

{

  key travel_id     as TravelId,

      agency_id     as AgencyID,
      //      agency.Name       as AgencyName,

      customer_id   as CustomerID,
      //      @Semantics.text: true
      //      customer.LastName as CustomerName,
      begin_date    as BeginDate,
      end_date      as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      booking_fee   as BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price   as TotalPrice,
      currency_code as CurrencyCode,
      description   as Description,
      //      @Consumption.valueHelpDefinition: [{
      //      entity.name: '/DMO/I_Overall_Status_VH',
      //      entity.element: 'OverallStatus'
      //      }]


      status        as Status,
      //      case overall_status
      //            when 'O' then 'Open'
      //            when 'A' then 'Approved'
      //            when 'R' then 'Rejected'
      //            when 'X' then 'Cancelled'
      //            else ''
      //            end       as StatusText,
      //      case overall_status
      //            when 'O' then '1'
      //            when 'A' then '3'
      //            when 'R' then '2'
      //            when 'X' then '4'
      //            else ''
      //            end       as Criticality,
      @Semantics.user.createdBy: true
      createdby     as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      createdat     as CreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      lastchangedby as LastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      lastchangedat as LastChangedAt,

      // Associations
      Booking,
      agency,
      customer,
      currency,
      Travelstatus
}
