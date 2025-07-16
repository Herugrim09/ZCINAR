@AbapCatalog.sqlViewName: 'ZMDG_TAX_LSTSQL'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View for Taxonomy Last Node Name'
@OData.publish: true
@ObjectModel:{ createEnabled: true, updateEnabled: true, deleteEnabled: true }
@Metadata.allowExtensions: true



@ObjectModel.transactionalProcessingEnabled: true


@ObjectModel.writeActivePersistence: 'ZMDG_TAX_TEXT'
define view ZMDG_TAX_LSTND_VW
  as select from zmdg_tax_lstnde
{


      @UI.selectionField: [{ position: 1 }]
      @UI.lineItem: [{ position: 10, importance: #HIGH }]
      @UI.identification: [{ position: 1, label: 'Top Node' }]
  key top_node,

      @UI.selectionField: [{ position: 2 }]
      @UI.lineItem: [{ position: 20 }]
      @UI.identification: [{ position: 2, label: 'Sub Node 1' }]
  key sub_node1,

      @UI.selectionField: [{ position: 3 }]
      @UI.lineItem: [{ position: 30 }]
      @UI.identification: [{ position: 3, label: 'Sub Node 2' }]
  key sub_node2,

      @UI.lineItem: [{ position: 40 }]
      @UI.identification: [{ position: 4, label: 'Last Node Name' }]
      last_node_name
}
