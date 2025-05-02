@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection view for Booking supplement'
@Metadata.allowExtensions: true
define view entity ZVI_C_SUPPLEMENT01

  as projection on ZVI_I_SUPPLEMENT01
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
      booking : redirected to parent ZVI_C_BOOKING01,
      product,
      Travel  : redirected to ZVI_C_TRAVEL01
}
