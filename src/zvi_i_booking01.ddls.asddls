@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking details'
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZVI_I_BOOKING01
  as select from /dmo/booking_m

  association        to parent ZVI_I_TRAVEL01 as Travel     on $projection.TravelId = Travel.TravelId

  composition [0..*] of ZVI_I_SUPPLEMENT01    as Booksupplement //on $projection.BookingId = Booksupplement.BookingId
  association [1..1] to /DMO/I_Customer     as customer   on $projection.CustomerId = customer.CustomerID
  association [1..1] to /DMO/I_Carrier      as carrier    on $projection.CarrierId = carrier.AirlineID
  association [1..*] to /DMO/I_Connection   as connection on $projection.CarrierId = connection.AirlineID
{

  key travel_id       as TravelId,
  key booking_id      as BookingId,
      booking_date    as BookingDate,
      customer_id     as CustomerId,
      carrier_id      as CarrierId,
      carrier.Name    as CarrierName,
      connection_id   as ConnectionId,
      flight_date     as FlightDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      flight_price    as FlightPrice,
      currency_code   as CurrencyCode,
      booking_status  as BookingStatus,
      last_changed_at as LastChangedAt,

      // Associations
      Travel,
      Booksupplement,
      customer,
      carrier,
      connection
}
