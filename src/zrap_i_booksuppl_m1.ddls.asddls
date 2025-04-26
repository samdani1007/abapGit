@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking supplements'

define view entity ZRAP_I_BOOKSUPPL_M1
  as select from /dmo/booksuppl_m as BookingSuppl
  association        to parent ZRAP_I_BOOKING_M1 as Booking        on  $projection.TravelId  = Booking.TravelId
                                                                   and $projection.BookingId = Booking.BookingId
  association [1..1] to ZRAP_I_TRAVEL_M1         as Travel         on  $projection.TravelId = Travel.TravelId
  association [1..1] to /DMO/I_Supplement        as Product        on  $projection.SupplementId = Product.SupplementID
  association [1..*] to /DMO/I_SupplementText    as SupplementText on  $projection.SupplementId = SupplementText.SupplementID

{
  key travel_id             as TravelId,
  key booking_id            as BookingId,
  key booking_supplement_id as BookingSupplementId,
      supplement_id         as SupplementId,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      price                 as Price,
      currency_code         as CurrencyCode,
      last_changed_at       as LastChangedAt,

      //Associations
      Booking,
      Travel,
      Product,
      SupplementText
}
