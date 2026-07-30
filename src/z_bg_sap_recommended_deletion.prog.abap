*&---------------------------------------------------------------------*
*& Report Z_BG_SAP_RECOMMENDED_DELETION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_bg_sap_recommended_deletion.

INCLUDE: z_bg_sap_recommended_deletion_top,    " global data
         z_bg_sap_recommended_deletion_lc,     " local classes
         z_bg_sap_recommended_deletion_displ,  " selection screen / display
         z_bg_sap_recommended_deletion_alv,    " ALV review-list class
         z_bg_sap_recommended_deletion_logic.  " business logic + START-OF-SELECTION
