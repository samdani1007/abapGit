@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel details'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZVI_I_TRAVEL01
  as select from /dmo/travel_m

  composition [0..*] of ZVI_I_BOOKING01            as Booking // on $projection.TravelId = Booking.TravelId

  association [0..1] to /DMO/I_Agency            as agency   on $projection.AgencyID = agency.AgencyID
  association [0..1] to /DMO/I_Customer          as customer on $projection.CustomerID = customer.CustomerID
  association [0..1] to I_Currency               as currency on $projection.CurrencyCode = currency.Currency
  association [1..1] to /DMO/I_Overall_Status_VH as status   on $projection.OverallStatus = status.OverallStatus
{
  key travel_id       as TravelId,
      agency_id       as AgencyID,
      customer_id     as CustomerID,
      begin_date      as BeginDate,
      end_date        as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      booking_fee     as BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price     as TotalPrice,
      currency_code   as CurrencyCode,
      description     as Description,
      overall_status  as OverallStatus,

      case overall_status
      when 'O' then 'Open'
      when 'A' then 'Approved'
      when 'R' then 'Rejected'
      when 'X' then 'Cancelled'
      else  ''
      end             as StatusText,

      created_by      as CreatedBy,
      created_at      as CreatedAt,
      last_changed_by as LastChangedBy,
      last_changed_at as LastChangedAt,

      // Associations
      Booking,
      agency,
      customer,
      currency,
      status
}
