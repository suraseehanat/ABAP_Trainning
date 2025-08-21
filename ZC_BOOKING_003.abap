@Metadata.layer: #CUSTOMER
annotate view ZR_BOOKING_003 with
{
  @UI.facet: [
    {
      id: 'GeneralInfo',
      label: 'General Information',
      type: #IDENTIFICATION_REFERENCE,
      position: 10
    },
    {
      id: 'TravelInfo',
      label: 'Travel Details',
      type: #FIELDGROUP_REFERENCE,
      targetQualifier: 'Travel',
      position: 20
    }
  ]

  // ============================================
  // FIELDS
  // ============================================
  
  // Booking Number
  @UI: {
    lineItem: [{ position: 10 ,importance: #HIGH }],
    identification: [{ position: 10 }],
    selectionField: [{ position: 10 }]
  }
  Booking;

  // Customer Name
  @UI: {
    lineItem: [{ position: 20 }],
    identification: [{ position: 20 }],
    selectionField: [{ position: 20 }]
  }
  CustomerName;

  // Number of Passengers
  @UI: {
    lineItem: [{ position: 30,importance: #HIGH}],
    identification: [{ position: 30 }],
    fieldGroup: [{ qualifier: 'Travel', position: 10 }]
  }
  NumberOfPassengers;

  // Email
  @UI: {
    lineItem: [{ position: 40 }],
    identification: [{ position: 40 }]
  }
  EmailAddress;

  // Country
  @UI: {
    lineItem: [{ position: 50 }],
    identification: [{ position: 50 }],
    fieldGroup: [{ qualifier: 'Travel', position: 20 }],
    selectionField: [{ position: 30 }]
  }
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'I_Country', element: 'Country' }
  }]
  Country;

  // Date of Booking
  @UI: {
    lineItem: [{ position: 60 }],
    identification: [{ position: 60 }],
    fieldGroup: [{ qualifier: 'Travel', position: 30 }]
  }
  DateOfBooking;

  // Date of Travel
  @UI: {
    lineItem: [{ position: 70 }],
    identification: [{ position: 70 }],
    fieldGroup: [{ qualifier: 'Travel', position: 40 }],
    selectionField: [{ position: 40 }]
  }
  DateOfTravel;

  // Cost
  @UI: {
    lineItem: [{ position: 80 }],
    identification: [{ position: 80 }]
  }
  cost;

  // Currency
  @UI: {
    lineItem: [{ position: 90 }],
    identification: [{ position: 90 }]
  }
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'I_Currency', element: 'Currency' }
  }]
  CurrencyCode;

  // Last Changed
  @UI: {
    lineItem: [{ position: 100 }],
    identification: [{ position: 100 }]
  }
  LastChangedAt;
}
