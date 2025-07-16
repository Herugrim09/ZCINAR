@AbapCatalog.sqlViewName: 'ZVH_DISPTYPE'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help for Display Type'
define view ZMDG_VALUE_HELP
  as select from dd07v
{
  @UI.hidden
  key domname    as domain_name, // Hidden key field

  @UI.hidden
  key valpos     as value_key,  // Hidden key field

  @UI.lineItem: [ { position: 10 } ]
  @UI.selectionField: [ { position: 10 } ]
  domvalue_l as display_type,   // Actual value of the domain

  @UI.lineItem: [ { position: 20 } ]
  @UI.selectionField: [ { position: 20 } ]
  ddtext as display_type_text   // Text for the fixed value

}
where
      domname    = 'FPMGB_DISPLAY_TYPE' // Replace with your domain name
  and ddlanguage = $session.system_language
