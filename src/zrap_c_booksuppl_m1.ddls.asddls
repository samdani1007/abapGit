@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection view for Booking supplement'
@Metadata.allowExtensions: true
define view entity ZRAP_C_BOOKSUPPL_M1
  as projection on ZRAP_I_BOOKSUPPL_M1
{
  key TravelId,
  key BookingId,
  key BookingSupplementId,
      SupplementId,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      Price,
      CurrencyCode,
      LastChangedAt,
      /* Associations */
      Booking : redirected to parent ZRAP_C_BOOKING_M1,
      Product,
      SupplementText,
      Travel  : redirected to ZRAP_C_TRAVEL_M1
}
