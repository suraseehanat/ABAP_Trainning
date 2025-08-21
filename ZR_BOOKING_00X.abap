@AbapCatalog.sqlViewName: 'ZV_BOOKING_003'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Management'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view ZR_BOOKING_003 as select from ztb_booking_003 as Booking
association[0..1] to I_Country as _Country on $projection.Country = _Country.Country
association[0..1] to I_Currency as _Currency on $projection.CurrencyCode = _Currency.Currency
{
  key booking   as Booking,
  @Search.defaultSearchElement: true
  customername as CustomerName,
  numberofpassengers as NumberOfPassengers,
  emailaddress as EmailAddress,
  country as Country,
  dateofbooking as DateOfBooking,
  dateoftravel as DateOfTravel,
  @Semantics.amount.currencyCode: 'CurrencyCode'
  cost,
  currencycode as CurrencyCode,
  lastchangedat as LastChangedAt,
  //public associations
  _Country,
  _Currency
}
