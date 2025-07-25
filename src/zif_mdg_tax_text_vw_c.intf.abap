interface ZIF_MDG_TAX_TEXT_VW_C
  public .


  interfaces /BOBF/IF_LIB_CONSTANTS .

  constants:
    BEGIN OF SC_ACTION,
      BEGIN OF ZMDG_TAX_TEXT_VW,
 CREATE_ZMDG_TAX_TEXT_VW        TYPE /BOBF/ACT_KEY VALUE '000C292C36471EEFB4DA1FC33D9D7145',
 DELETE_ZMDG_TAX_TEXT_VW        TYPE /BOBF/ACT_KEY VALUE '000C292C36471EEFB4DA1FC33D9DF145',
 LOCK_ZMDG_TAX_TEXT_VW          TYPE /BOBF/ACT_KEY VALUE '000C292C36471EEFB4DA1FC33D9CB145',
 SAVE_ZMDG_TAX_TEXT_VW          TYPE /BOBF/ACT_KEY VALUE '000C292C36471EEFB4DA1FC33D9E7145',
 UNLOCK_ZMDG_TAX_TEXT_VW        TYPE /BOBF/ACT_KEY VALUE '000C292C36471EEFB4DA1FC33D9CF145',
 UPDATE_ZMDG_TAX_TEXT_VW        TYPE /BOBF/ACT_KEY VALUE '000C292C36471EEFB4DA1FC33D9DB145',
 VALIDATE_ZMDG_TAX_TEXT_VW      TYPE /BOBF/ACT_KEY VALUE '000C292C36471EEFB4DA1FC33D9E3145',
      END OF ZMDG_TAX_TEXT_VW,
    END OF SC_ACTION .
  constants:
    BEGIN OF SC_ACTION_ATTRIBUTE,
        BEGIN OF ZMDG_TAX_TEXT_VW,
        BEGIN OF LOCK_ZMDG_TAX_TEXT_VW,
 GENERIC                        TYPE STRING VALUE 'GENERIC',
 EDIT_MODE                      TYPE STRING VALUE 'EDIT_MODE',
 ALL_NONE                       TYPE STRING VALUE 'ALL_NONE',
 SCOPE                          TYPE STRING VALUE 'SCOPE',
 FORCE_INVALIDATION             TYPE STRING VALUE 'FORCE_INVALIDATION',
 LOCK_PARAMETER_BUFFER          TYPE STRING VALUE 'LOCK_PARAMETER_BUFFER',
 LEGACY_DAC_KEY                 TYPE STRING VALUE 'LEGACY_DAC_KEY',
        END OF LOCK_ZMDG_TAX_TEXT_VW,
        BEGIN OF UNLOCK_ZMDG_TAX_TEXT_VW,
 GENERIC                        TYPE STRING VALUE 'GENERIC',
 EDIT_MODE                      TYPE STRING VALUE 'EDIT_MODE',
 ALL_NONE                       TYPE STRING VALUE 'ALL_NONE',
 SCOPE                          TYPE STRING VALUE 'SCOPE',
 FORCE_INVALIDATION             TYPE STRING VALUE 'FORCE_INVALIDATION',
 LOCK_PARAMETER_BUFFER          TYPE STRING VALUE 'LOCK_PARAMETER_BUFFER',
 LEGACY_DAC_KEY                 TYPE STRING VALUE 'LEGACY_DAC_KEY',
        END OF UNLOCK_ZMDG_TAX_TEXT_VW,
      END OF ZMDG_TAX_TEXT_VW,
    END OF SC_ACTION_ATTRIBUTE .
  constants:
    BEGIN OF SC_ALTERNATIVE_KEY,
      BEGIN OF ZMDG_TAX_TEXT_VW,
 DB_KEY                         TYPE /BOBF/OBM_ALTKEY_KEY VALUE '000C292C36471EEFB4DA1FC33D9FB145',
      END OF ZMDG_TAX_TEXT_VW,
    END OF SC_ALTERNATIVE_KEY .
  constants:
    BEGIN OF SC_ASSOCIATION,
      BEGIN OF ZMDG_TAX_TEXT_VW,
 LOCK                           TYPE /BOBF/OBM_ASSOC_KEY VALUE '000C292C36471EEFB4DA1FC33D9C9145',
 MESSAGE                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '000C292C36471EEFB4DA1FC33D9C5145',
 PROPERTY                       TYPE /BOBF/OBM_ASSOC_KEY VALUE '000C292C36471EEFB4DA1FC33D9D5145',
      END OF ZMDG_TAX_TEXT_VW,
      BEGIN OF ZMDG_TAX_TEXT_VW_LOCK,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '000C292C36471EEFB4DA1FC33D9ED145',
      END OF ZMDG_TAX_TEXT_VW_LOCK,
      BEGIN OF ZMDG_TAX_TEXT_VW_MESSAGE,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '000C292C36471EEFB4DA1FC33D9EB145',
      END OF ZMDG_TAX_TEXT_VW_MESSAGE,
      BEGIN OF ZMDG_TAX_TEXT_VW_PROPERTY,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '000C292C36471EEFB4DA1FC33D9EF145',
      END OF ZMDG_TAX_TEXT_VW_PROPERTY,
    END OF SC_ASSOCIATION .
  constants:
    BEGIN OF SC_ASSOCIATION_ATTRIBUTE,
      BEGIN OF ZMDG_TAX_TEXT_VW,
        BEGIN OF PROPERTY,
 ALL_NODE_PROPERTY              TYPE STRING VALUE 'ALL_NODE_PROPERTY',
 ALL_NODE_ATTRIBUTE_PROPERTY    TYPE STRING VALUE 'ALL_NODE_ATTRIBUTE_PROPERTY',
 ALL_ASSOCIATION_PROPERTY       TYPE STRING VALUE 'ALL_ASSOCIATION_PROPERTY',
 ALL_ASSOCIATION_ATTRIBUTE_PROP TYPE STRING VALUE 'ALL_ASSOCIATION_ATTRIBUTE_PROP',
 ALL_ACTION_PROPERTY            TYPE STRING VALUE 'ALL_ACTION_PROPERTY',
 ALL_ACTION_ATTRIBUTE_PROPERTY  TYPE STRING VALUE 'ALL_ACTION_ATTRIBUTE_PROPERTY',
 ALL_QUERY_PROPERTY             TYPE STRING VALUE 'ALL_QUERY_PROPERTY',
 ALL_QUERY_ATTRIBUTE_PROPERTY   TYPE STRING VALUE 'ALL_QUERY_ATTRIBUTE_PROPERTY',
 ALL_SUBTREE_PROPERTY           TYPE STRING VALUE 'ALL_SUBTREE_PROPERTY',
        END OF PROPERTY,
      END OF ZMDG_TAX_TEXT_VW,
    END OF SC_ASSOCIATION_ATTRIBUTE .
  constants:
    SC_BO_KEY  TYPE /BOBF/OBM_BO_KEY VALUE '000C292C36471EEFB4DA1FC33D9BB145' .
  constants:
    SC_BO_NAME TYPE /BOBF/OBM_NAME VALUE 'ZMDG_TAX_TEXT_VW' .
  constants:
    BEGIN OF SC_DETERMINATION,
      BEGIN OF ZMDG_TAX_TEXT_VW,
 ACTION_AND_FIELD_CONTROL       TYPE /BOBF/DET_KEY VALUE '000C292C36471EEFB4DA1FC33D9FD145',
      END OF ZMDG_TAX_TEXT_VW,
    END OF SC_DETERMINATION .
  constants:
    BEGIN OF SC_GROUP,
 ZMDG_TAX_TEXT_VW               TYPE /BOBF/OBM_GROUP_KEY VALUE '000C292C36471EEFB4DA1FC33DA69145',
    END OF SC_GROUP .
  constants:
    SC_MODEL_VERSION TYPE /BOBF/CONF_VERSION VALUE '00000' .
  constants:
    BEGIN OF SC_NODE,
 ZMDG_TAX_TEXT_VW               TYPE /BOBF/OBM_NODE_KEY VALUE '000C292C36471EEFB4DA1FC33D9BF145',
 ZMDG_TAX_TEXT_VW_LOCK          TYPE /BOBF/OBM_NODE_KEY VALUE '000C292C36471EEFB4DA1FC33D9C7145',
 ZMDG_TAX_TEXT_VW_MESSAGE       TYPE /BOBF/OBM_NODE_KEY VALUE '000C292C36471EEFB4DA1FC33D9C3145',
 ZMDG_TAX_TEXT_VW_PROPERTY      TYPE /BOBF/OBM_NODE_KEY VALUE '000C292C36471EEFB4DA1FC33D9D3145',
    END OF SC_NODE .
  constants:
    BEGIN OF SC_NODE_ATTRIBUTE,
      BEGIN OF ZMDG_TAX_TEXT_VW,
  NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
  TOP_NODE                       TYPE STRING VALUE 'TOP_NODE',
  SUB_NODE1                      TYPE STRING VALUE 'SUB_NODE1',
  SUB_NODE2                      TYPE STRING VALUE 'SUB_NODE2',
  ATTRIBUTE                      TYPE STRING VALUE 'ATTRIBUTE',
  ATTR_TYPE                      TYPE STRING VALUE 'ATTR_TYPE',
  UI_NAME                        TYPE STRING VALUE 'UI_NAME',
  UI_LABEL                       TYPE STRING VALUE 'UI_LABEL',
  UI_READ_ONLY                   TYPE STRING VALUE 'UI_READ_ONLY',
  UI_MANDATORY                   TYPE STRING VALUE 'UI_MANDATORY',
  DISPLAY_TYPE                   TYPE STRING VALUE 'DISPLAY_TYPE',
  LAST_NODE_NAME                 TYPE STRING VALUE 'LAST_NODE_NAME',
      END OF ZMDG_TAX_TEXT_VW,
    END OF SC_NODE_ATTRIBUTE .
  constants:
    BEGIN OF SC_NODE_CATEGORY,
      BEGIN OF ZMDG_TAX_TEXT_VW,
 ROOT                           TYPE /BOBF/OBM_NODE_CAT_KEY VALUE '000C292C36471EEFB4DA1FC33D9C1145',
      END OF ZMDG_TAX_TEXT_VW,
    END OF SC_NODE_CATEGORY .
  constants:
    BEGIN OF SC_VALIDATION,
      BEGIN OF ZMDG_TAX_TEXT_VW,
 ALTKEY_UNIQUENESS_CHECK        TYPE /BOBF/VAL_KEY VALUE '000C292C36471EEFB4DA1FC33DA05145',
      END OF ZMDG_TAX_TEXT_VW,
    END OF SC_VALIDATION .
endinterface.
