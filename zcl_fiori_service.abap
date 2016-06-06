class ZCL_FIORI_SERVICE definition
  public
  final
  create public .

public section.

  class-methods DISABLE_BROWSER_CACHE
    importing
      !IO_CALLER type ref to /IWBEP/IF_MGW_CONV_SRV_RUNTIME .
  class-methods APPLY_PAGING
    importing
      !IS_PAGING type /IWBEP/S_MGW_PAGING
    changing
      !CT_ENTITYSET type INDEX TABLE
      !CV_INLINECOUNT type I optional .
  class-methods RAISE_TEXT_MESSAGE
    importing
      !IV_MESSAGE type CSEQUENCE
      !IO_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  class-methods RAISE_BAPI_MESSAGES
    importing
      !IT_RETURN type BAPIRET2_T
      !IO_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  class-methods RAISE_SYST_MESSAGE
    importing
      !IS_SYST type SYST
      !IO_CONTEXT type ref to /IWBEP/IF_MGW_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  class-methods GET_FILTER
    importing
      !IT_FILTER type /IWBEP/T_MGW_SELECT_OPTION
      !IV_PROPERTY type STRING
    exporting
      !ETR_FILTER type INDEX TABLE .
  class-methods ORDER_BY
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    changing
      !CT_ENTITYSET type INDEX TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FIORI_SERVICE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FIORI_SERVICE=>APPLY_PAGING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [<-->] CT_ENTITYSET                   TYPE        INDEX TABLE
* | [<-->] CV_INLINECOUNT                 TYPE        I(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD APPLY_PAGING.
    cv_inlinecount = lines( ct_entityset ).
*--------------------------------------------------------------------*
    DATA: lv_tabix  TYPE sy-tabix.
* Remove all entries that are not part of the requested page
    IF is_paging-top > 0.
*   For performance reason cut the trailing lines first
      lv_tabix = is_paging-skip + is_paging-top + 1.
      DELETE ct_entityset FROM lv_tabix.
    ENDIF.
    IF is_paging-skip > 0.
      DELETE ct_entityset TO is_paging-skip.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FIORI_SERVICE=>DISABLE_BROWSER_CACHE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CALLER                      TYPE REF TO /IWBEP/IF_MGW_CONV_SRV_RUNTIME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD DISABLE_BROWSER_CACHE.
    DATA header TYPE ihttpnvp.
    header-name = 'Cache-Control'        ##NO_TEXT.
    header-value = 'no-cache, no-store'  ##NO_TEXT.
    io_caller->set_header( EXPORTING is_header = header ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FIORI_SERVICE=>GET_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_FILTER                      TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IV_PROPERTY                    TYPE        STRING
* | [<---] ETR_FILTER                     TYPE        INDEX TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_filter.
    READ TABLE it_filter ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY property = iv_property.
    CHECK sy-subrc IS INITIAL.
    MOVE-CORRESPONDING <fs>-select_options TO etr_filter.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FIORI_SERVICE=>ORDER_BY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET
* | [<-->] CT_ENTITYSET                   TYPE        INDEX TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD order_by.
    DATA: lt_sortby TYPE abap_sortorder_tab.
    DATA(lt_orderby) = io_tech_request_context->get_orderby( ).
    LOOP AT lt_orderby ASSIGNING FIELD-SYMBOL(<fs_orderby>).
      APPEND INITIAL LINE TO lt_sortby ASSIGNING FIELD-SYMBOL(<fs_sortby>).
      <fs_sortby>-name = <fs_orderby>-property.
      IF <fs_orderby>-order = 'asc'.
        CLEAR <fs_sortby>-descending.
      ELSE.
        <fs_sortby>-descending = abap_true.
      ENDIF.
    ENDLOOP.

    CHECK lt_sortby IS NOT INITIAL.
    TRY.
        SORT ct_entityset BY (lt_sortby).
      CATCH cx_sy_dyn_table_ill_comp_val.
        " raise exception if you want
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FIORI_SERVICE=>RAISE_BAPI_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_RETURN                      TYPE        BAPIRET2_T
* | [--->] IO_CONTEXT                     TYPE REF TO /IWBEP/IF_MGW_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD raise_bapi_messages.

    DATA: lo_message_container TYPE REF TO /iwbep/if_message_container.
    lo_message_container = io_context->get_message_container( ).

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      lo_message_container->add_message(
          iv_msg_type       = <fs_return>-type
          iv_msg_id         = <fs_return>-id
          iv_msg_number     = <fs_return>-number
          iv_msg_text       = <fs_return>-message
          iv_msg_v1         = <fs_return>-message_v1
          iv_msg_v2         = <fs_return>-message_v2
          iv_msg_v3         = <fs_return>-message_v3
          iv_msg_v4         = <fs_return>-message_v4
      ).
    ENDLOOP.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message_container.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FIORI_SERVICE=>RAISE_SYST_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SYST                        TYPE        SYST
* | [--->] IO_CONTEXT                     TYPE REF TO /IWBEP/IF_MGW_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD raise_syst_message.

    DATA: lo_message_container TYPE REF TO /iwbep/if_message_container.
    lo_message_container = io_context->get_message_container( ).

    DATA: lv_txt TYPE bapi_msg.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_txt.

    lo_message_container->add_message(
        iv_msg_type       = is_syst-msgty
        iv_msg_id         = is_syst-msgid
        iv_msg_number     = is_syst-msgno
        iv_msg_text       = lv_txt "is_syst-msgli
        iv_msg_v1         = is_syst-msgv1
        iv_msg_v2         = is_syst-msgv2
        iv_msg_v3         = is_syst-msgv3
        iv_msg_v4         = is_syst-msgv4
    ).

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message_container.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FIORI_SERVICE=>RAISE_TEXT_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MESSAGE                     TYPE        CSEQUENCE
* | [--->] IO_CONTEXT                     TYPE REF TO /IWBEP/IF_MGW_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD RAISE_TEXT_MESSAGE.
    DATA: lo_message_container TYPE REF TO /iwbep/if_message_container.
    DATA: lv_msg_text TYPE bapi_msg.
    lv_msg_text = iv_message.

    IF iv_message IS NOT INITIAL.
      lo_message_container = io_context->get_message_container( ).
      CALL METHOD lo_message_container->add_message_text_only
        EXPORTING
          iv_msg_type           = /iwbep/if_message_container=>gcs_message_type-error
          iv_msg_text           = lv_msg_text
          iv_error_category     = /iwbep/if_message_container=>gcs_error_category-processing
          iv_is_leading_message = abap_true. "Show this message to consumer
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.
  ENDMETHOD.
ENDCLASS.