@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection view for Booking'
@Metadata.allowExtensions: true
define view entity ZRAP_C_BOOKING_APPROVER_M1
  as projection on ZRAP_I_BOOKING_M1
{
  key TravelId,
  key BookingId,
      BookingDate,
      CustomerId,
      CarrierId,
      ConnectionId,
      FlightDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      FlightPrice,
      CurrencyCode,
      BookingStatus,
      LastChangedAt,
      /* Associations */
      Booksupplement,
      carrier,
      connection,
      customer,
      status,
      Travel         : redirected to parent ZRAP_C_TRAVEL_APPROVER_M1
}
