*&---------------------------------------------------------------------*
*& Report  ZCASHJOURNAL
*& Catalyst 07066112584 08109255546
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZCASHJOURNAL.


*data for the function module to be used for uploading cash journal
data:
      header like BAPI_CJ_HEADER,
      items type table of  BAPI_CJ_ITEMS WITH HEADER LINE,
      items_line type TABLE OF BAPI_CJ_ITEMS WITH HEADER LINE,
      return like table of BAPIRET2 with header line,
      COMPANY_CODE  LIKE  BAPI_CJ_HEADER-COMP_CODE,
      CASH_JOURNAL_NUMBER LIKE  BAPI_CJ_HEADER-CAJO_NUMBER,
      FISCAL_YEAR LIKE  BAPI_CJ_KEY-FISC_YEAR,
      CASH_JOURNAL_DOC_NO LIKE  BAPI_CJ_KEY-POSTING_NUMBER.


* define structure for cash journal upload
types: begin of cash_journal,
    comp_code type BUKRS,
    cajo_number type CJNR,
    transact_num type CJTRANSNUMB,
    h_payments type BAPIWRBTR,
    tax_code type MWSKZ,
    gl_account type HKONT,
    bp_name type CJBPNAME,
    alloc_number type DZUONR,
    position_text type CJPOSTEXT,
    posting_date type string,
    doc_number type XBLNR1,
    cost_cntr type KOSTL,
    profit_cntr type PRCTR,
 end of cash_journal.

*administrator error
types: begin of admin_error,
  lights(1) TYPE c,
  error_flag type string,
  msgno type string,
  message(200) type c,
  end of  admin_error.


data errorflag type i.
data l_err type i value 0.

DATA: gs_alv TYPE admin_error,
     gt_alv TYPE TABLE OF admin_error,
     gr_alv TYPE REF TO cl_salv_table,
     gr_columns TYPE REF TO cl_salv_columns_table,
     gr_table   type ref to cl_salv_table..
data: status(1) type c,
      message(100) type c.

data: ad_error type table of admin_error,
      ad_str like line of ad_error.
*data declaration
data: dt_fname type string,
      dt_flen type i.


data: cash_j_tab type table of cash_journal,
      cash_j_line like line of cash_j_tab .

*additional data
data:
      date_conv like sy-datum,
      currency type waers,
      pos_num type i.
SELECTION-SCREEN begin of block b3 with frame title text-002.
parameters:
  company type bukrs DEFAULT '7000'.
SELECTION-SCREEN end of block b3.

SELECTION-SCREEN begin of block b1 with frame title text-000.
parameters:
  fname type ibipparms-path memory id a.

SELECTION-SCREEN end of block b1.

SELECTION-SCREEN begin of block b2 with frame title text-001.
parameters:
  user like sy-uname DEFAULT sy-uname,
  up_date like sy-datum default sy-datum,
  testmod type checkbox1 DEFAULT 'X'.
SELECTION-SCREEN end of block b2.

selection-screen begin of BLOCK b4 with frame title text-003.
parameters:
  template type string default 'MAIN_TEMPLATE'.
SELECTION-SCREEN end of block b4.

* things to get from sap tables
* currency key


*process file upload
at SELECTION-SCREEN on VALUE-REQUEST FOR fname.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      FILE_NAME = fname.

start-of-selection.
  if ( TESTMOD <> 'X' ).
    perform cashJournal.
    else.
      MESSAGE e004(zmsg1) DISPLAY LIKE 'A'.
  endif.

form cashJournal.
  dt_fname = fname.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename            = dt_fname
      filetype            = 'ASC'
      has_field_separator = 'X'
    IMPORTING
      filelength          = dt_flen
    TABLES
      data_tab            = cash_j_tab.

  perform uploadCashJournal.
   CALL METHOD gr_alv->display.
  perform displayalv2 using ad_error[].
endform.

form uploadCashJournal.
*        clear the error log
  clear ad_error[].
*      lets loop through source table and push transaction
  pos_num = 1.
  loop at cash_j_tab into cash_j_line.
*            clear containers to be used for transaction
    clear:
      HEADER, items[], return[].

    perform prepareHeader.
    perform prepareItems.

    pos_num = pos_num + 1.
    CALL FUNCTION 'BAPI_CASHJOURNALDOC_CREATE'
      EXPORTING
        HEADER                    = header
*                 TESTRUN                   =
               IMPORTING
       COMPANY_CODE              = COMPANY_CODE
       CASH_JOURNAL_NUMBER       = CASH_JOURNAL_NUMBER
       FISCAL_YEAR               = FISCAL_YEAR
       CASH_JOURNAL_DOC_NO       = CASH_JOURNAL_DOC_NO
      TABLES
        ITEMS                     = items
*                 TAX_ITEMS                 =
*                 CPD_ITEMS                 =
*                 WTAX_ITEMS                =
*                 EXTENSION_IN              =
        RETURN                    = return
              .

    PERFORM flagerror.
    if ( errorflag = '0' ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                       EXPORTING
*                         WAIT          =
*                       IMPORTING
*                         RETURN        =
                .

    else.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*                         IMPORTING
*                           RETURN        =
                .

    endif.
    CALL METHOD cl_salv_table=>factory
      IMPORTING
        r_salv_table = gr_alv
      CHANGING
        t_table      = ad_error.
    gr_columns = gr_alv->get_columns( ).
    gr_columns->set_exception_column( value = 'LIGHTS' ).

*    delay
    wait up to 1 SECONDS.

  endloop.

endform.

form flagerror.

  loop at return.
*          PREPARE MESSAGE FOR OUTPUT


    clear ERRORFLAG.
    if ( return-TYPE = 'E' ).
      errorflag = 1.
      ad_str-ERROR_FLAG = 'E'.
      ad_str-LIGHTS = '1'.
      ad_str-MSGNO = return-NUMBER.
      ad_str-MESSAGE = return-MESSAGE.
    else.
      ad_str-ERROR_FLAG = 'S'.
      ad_str-LIGHTS = '3'.
      ad_str-MSGNO = return-NUMBER.
      ad_str-MESSAGE = return-MESSAGE.
    endif.
    APPEND AD_STR TO AD_ERROR.
  endloop.
endform.
form displayalv2 using table type standard table.

  try.
      cl_salv_table=>factory(
        importing
          r_salv_table = gr_table
        changing
          t_table      = table ).
    catch cx_salv_msg.                                  "#EC NO_HANDLER
  endtry.
  gr_table->display( ).
endform.

form prepareHeader.
    select single CURRENCY into currency from TCJ_C_JOURNALS ""FETCH THE CURRENCY
      WHERE COMP_CODE = cash_j_line-COMP_CODE
      and CAJO_NUMBER = cash_j_line-CAJO_NUMBER.

      if ( sy-subrc <> 0 ).
          currency = 'NGN'.
        endif.
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        DATE_EXTERNAL                  = cash_j_line-POSTING_DATE
*       ACCEPT_INITIAL_DATE            =
     IMPORTING
       DATE_INTERNAL                  = DATE_CONV
     EXCEPTIONS
       DATE_EXTERNAL_IS_INVALID       = 1
       OTHERS                         = 2
              .
    IF SY-SUBRC <> 0.
      if ( sy-subrc = 1 ).
          MESSAGE e005(zmsg1) with cash_j_line-POSTING_DATE.

        endif.
        ELSE.
          cash_j_line-POSTING_DATE = date_conv.
    ENDIF.


    header-COMP_CODE = cash_j_line-COMP_CODE.
    header-CAJO_NUMBER = cash_j_line-CAJO_NUMBER.
    header-CURRENCY = CURRENCY.
    header-CURRENCY_ISO = CURRENCY.
    header-DOC_DATE = cash_j_line-POSTING_DATE.
    header-PSTNG_DATE = cash_j_line-POSTING_DATE.
    header-BP_NAME = cash_j_line-BP_NAME.




  endform.
  form prepareItems.
    DATA account type HKONT.
    DATA ccenter type KOSTL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT         =  cash_j_line-GL_ACCOUNT
       IMPORTING
         OUTPUT        = account
                .
    if ( sy-subrc = 0 ).
          cash_j_line-GL_ACCOUNT = account.
        endif.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT         =  cash_j_line-COST_CNTR
       IMPORTING
         OUTPUT        = CCENTER
                .

      if ( sy-subrc = 0 ).
          cash_j_line-COST_CNTR = CCENTER.
        endif.

      items-POSITION_NUMBER = POS_NUM.
      items-TRANSACT_NUMBER = cash_j_line-TRANSACT_NUM.
      items-P_PAYMENTS = cash_j_line-H_PAYMENTS.
      items-GL_ACCOUNT = cash_j_line-GL_ACCOUNT.
      items-TAX_CODE = cash_j_line-TAX_CODE.
      items-POSITION_TEXT = cash_j_line-POSITION_TEXT.
      items-COSTCENTER = cash_j_line-COST_CNTR.
      items-PROFIT_CTR = cash_j_line-PROFIT_CNTR.

      append items .

    endform.