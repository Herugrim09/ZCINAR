@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS View for Taxonomy Text'
@OData.publish: true
@Search.searchable: true
@Metadata.allowExtensions: true

                   // Defining as Root Entity

@ObjectModel.representativeKey: 'top_node' // Key for the entity


define root view entity ZMDG_TAX_TEXT_VW
  as select from zmdg_tax_text

  // Association to the child view

{

        @Search.defaultSearchElement: true
        @UI.lineItem: [{ position: 10, importance: #HIGH }]
        @UI.selectionField: [{ position: 1 }]
        @UI.identification: [{ position: 1, label: 'Top Node' }]
  key   top_node,

        @Search.defaultSearchElement: true
        @UI.lineItem: [{ position: 20 }]
        @UI.selectionField: [{ position: 2 }]
        @UI.identification: [{ position: 2, label: 'Sub Node 1' }]
  key   sub_node1,

        @Search.defaultSearchElement: true
        @UI.lineItem: [{ position: 30 }]
        @UI.selectionField: [{ position: 3 }]
        @UI.identification: [{ position: 3, label: 'Sub Node 2' }]
  key   sub_node2,
        @UI.selectionField: [{ position: 6 }]
        @UI.identification: [{ position: 6, label: 'UI Name' }]
        @UI.lineItem: [{ position: 60 }]
  key   ui_name,

        @Search.defaultSearchElement: true
        @UI.lineItem: [{ position: 40 }]
        @UI.selectionField: [{ position: 4 }]
        @UI.identification: [{ position: 4, label: 'Attribute' }]
        attribute,

        @UI.lineItem: [{ position: 50 }]
        @UI.selectionField: [{ position: 5 }]
        @UI.identification: [{ position: 5, label: 'Attribute Type' }]
        attr_type,



        @UI.lineItem: [{ position: 70 }]
        ui_label,

        @UI.lineItem: [{ position: 80 }]
        ui_read_only,

        @UI.lineItem: [{ position: 90 }]
        ui_mandatory,

        @UI.lineItem: [{ position: 100 }]
        display_type

}
