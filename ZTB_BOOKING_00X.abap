@EndUserText.label : 'Table Booking'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztb_booking_003 {

  key client         : abap.clnt not null;
  key booking        : abap.int4 not null;
  customername       : abap.char(50);
  numberofpassengers : abap.int2;
  emailaddress       : abap.char(50);
  country            : abap.char(50);
  dateofbooking      : timestampl;
  dateoftravel       : timestampl;
  @Semantics.amount.currencyCode : 'ztbooking_002.currencycode'
  cost               : abap.curr(15,2);
  currencycode       : abap.cuky;
  lastchangedat      : timestampl;

}
