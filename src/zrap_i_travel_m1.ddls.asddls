@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel details'

define root view entity ZRAP_I_TRAVEL_M1
  as select from /dmo/travel_m as Travel

  composition [0..*] of ZRAP_I_BOOKING_M1        as Booking // on $projection.TravelId = Booking.TravelId
  association [0..1] to /DMO/I_Agency            as agency   on $projection.AgencyID = agency.AgencyID
  association [0..1] to /DMO/I_Customer          as customer on $projection.CustomerID = customer.CustomerID
  association [0..1] to I_Currency               as currency on $projection.CurrencyCode = currency.Currency
  association [1..1] to /DMO/I_Overall_Status_VH as status   on $projection.OverallStatus = status.OverallStatus

{

  key travel_id       as TravelId,

      agency_id       as AgencyID,
      //      agency.Name       as AgencyName,

      customer_id     as CustomerID,
      //      @Semantics.text: true
      //      customer.LastName as CustomerName,
      begin_date      as BeginDate,
      end_date        as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      booking_fee     as BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price     as TotalPrice,
      currency_code   as CurrencyCode,
      description     as Description,
      //      @Consumption.valueHelpDefinition: [{
      //      entity.name: '/DMO/I_Overall_Status_VH',
      //      entity.element: 'OverallStatus'
      //      }]

      overall_status  as OverallStatus,
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
      created_by      as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at      as CreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      last_changed_by as LastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at as LastChangedAt,

      // Associations
      Booking,
      agency,
      customer,
      currency,
      status
}
