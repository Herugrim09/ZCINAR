@AbapCatalog.sqlViewName: 'ZMDG_TAX_SQL'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View for Taxonomy Text'
@OData.publish: true
@Search.searchable: true
// Key for the entity

define view ZMDG_TAX_TEXT_VW2
  as select from zmdg_tax_text

  // Association to the child view
  association [1..1] to ZMDG_TAX_LSTND_VW as _LastNode on  $projection.top_node  = _LastNode.top_node
                                                       and $projection.sub_node1 = _LastNode.sub_node1
                                                       and $projection.sub_node2 = _LastNode.sub_node2

{

      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
      @UI.selectionField: [{ position: 1 }]
      @UI.identification: [{ position: 1, label: 'Top Node' }]
  key top_node,

      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 20 }]
      @UI.selectionField: [{ position: 2 }]
      @UI.identification: [{ position: 2, label: 'Sub Node 1' }]
  key sub_node1,

      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 30 }]
      @UI.selectionField: [{ position: 3 }]
      @UI.identification: [{ position: 3, label: 'Sub Node 2' }]
  key sub_node2,

      @Search.defaultSearchElement: true
      @UI.lineItem: [{ position: 40 }]
      @UI.selectionField: [{ position: 4 }]
      @UI.identification: [{ position: 4, label: 'Attribute' }]
      attribute,

      @UI.lineItem: [{ position: 50 }]
      @UI.selectionField: [{ position: 5 }]
      @UI.identification: [{ position: 5, label: 'Attribute Type' }]
      attr_type,

      @UI.lineItem: [{ position: 60 }]
      ui_name,

      @UI.lineItem: [{ position: 70 }]
      ui_label,

      @UI.lineItem: [{ position: 80 }]
      ui_read_only,

      @UI.lineItem: [{ position: 90 }]
      ui_mandatory,

      @UI.lineItem: [{ position: 100 }]
      display_type,

      @UI.lineItem: [{ position: 110, label: 'Last Node Name' }]
      _LastNode.last_node_name



}
