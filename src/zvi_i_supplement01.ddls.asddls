@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Supplement details'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZVI_I_SUPPLEMENT01
  as select from /dmo/booksuppl_m

  association        to parent ZVI_I_BOOKING01 as booking on  $projection.BookingId = booking.BookingId
                                                          and $projection.TravelId  = booking.TravelId

  association [1..1] to ZVI_I_TRAVEL01         as Travel  on  $projection.TravelId = Travel.TravelId
  association [1..1] to /DMO/I_Supplement      as product on  $projection.SupplementId = product.SupplementID
{
  key travel_id             as TravelId,
  key booking_id            as BookingId,
  key booking_supplement_id as BookingSupplementId,
      supplement_id         as SupplementId,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      price                 as Price,
      currency_code         as CurrencyCode,
      last_changed_at       as LastChangedAt,

      // Associations
      booking,
      Travel,
      product
}
