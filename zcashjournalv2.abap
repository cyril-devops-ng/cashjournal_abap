*&---------------------------------------------------------------------*
*& Report  ZCASHJOURNAL
*& Catalyst 07066112584 08109255546
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZCASHJOURNAL.

TABLES: TCJ_TRANS_NAMES , TCJ_C_JOURNALS.
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
      pos_num type i,
        tran_num  like tcj_trans_names-transact_number,
        simulation type c value 'Y',
        question type string,
        answer.
*      parameters for FM FCJ_POST_ALL
DATA:
      I_COMP_CODE LIKE  TCJ_C_JOURNALS-COMP_CODE,
      I_CAJO_NUMBER LIKE  TCJ_C_JOURNALS-CAJO_NUMBER,
      I_CURRENCY  LIKE  TCJ_DOCUMENTS-CURRENCY,
      I_TYP TYPE  CJTRANSTYP,
      I_DISPLAY_PERIOD_LO LIKE  SY-DATUM,
      I_DISPLAY_PERIOD_HI LIKE  SY-DATUM,
      E_ERROR_NUMBER  LIKE  TCJ_DOCUMENTS-POSTING_NUMBER,
      P_BEG_BALANCE TYPE  CJAMOUNT,
      P_TOTAL_RECEIPTS  TYPE  CJAMOUNT,
      P_TOTAL_PAYMENTS  TYPE  CJAMOUNT,
      P_TOTAL_CHECKS  TYPE  CJAMOUNT,
      P_RUN_BALANCE TYPE  CJAMOUNT,
      P_RUN_CASH_BALANCE  TYPE  CJAMOUNT,
      P_NUMB_OF_REC TYPE  I,
      P_NUMB_OF_PAYM  TYPE  I,
      P_NUMB_OF_CHECKS  TYPE  I,
      ITCJ_POSTINGS LIKE table of ISCJ_POSTINGS with header line,
      ITCJ_WTAX_ITEMS LIKE table of TCJ_WTAX_ITEMS with header line,
      ITCJ_SPLIT_POSTINGS LIKE  table of ISCJ_POSTINGS with header line,
      ITCJ_CPD  LIKE  table of TCJ_CPD with header line.
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
  testmod type checkbox1 DEFAULT 'X' MEMORY ID chk.
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
    SIMULATION = 'N'.

      QUESTION = 'Are you sure you want to run in update mode?'.

*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
**       TITLEBAR                    = ' '
**       DIAGNOSE_OBJECT             = ' '
*        TEXT_QUESTION               = question
**       TEXT_BUTTON_1               = 'Ja'(001)
**       ICON_BUTTON_1               = ' '
**       TEXT_BUTTON_2               = 'Nein'(002)
**       ICON_BUTTON_2               = ' '
**       DEFAULT_BUTTON              = '1'
**       DISPLAY_CANCEL_BUTTON       = 'X'
**       USERDEFINED_F1_HELP         = ' '
**       START_COLUMN                = 25
**       START_ROW                   = 6
**       POPUP_TYPE                  =
**       IV_QUICKINFO_BUTTON_1       = ' '
**       IV_QUICKINFO_BUTTON_2       = ' '
*     IMPORTING
*       ANSWER                      = answer
**     TABLES
**       PARAMETER                   =
**     EXCEPTIONS
**       TEXT_NOT_FOUND              = 1
**       OTHERS                      = 2
*              .
*    IF SY-SUBRC <> 0.
** Implement suitable error handling here
*    ENDIF.

      perform cashJournal.

    if ( sy-subrc = 0 ).
      if ( answer eq '1' ).

    else.

    endif.
    endif.

  else.
    SIMULATION = 'Y'.
    MESSAGE i004(zmsg1).
    perform cashJournal.
    MESSAGE i008(zmsg1).
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
*  perform displayalv2 using ad_error[].
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
        HEADER              = header
*       TESTRUN             =
      IMPORTING
        COMPANY_CODE        = COMPANY_CODE
        CASH_JOURNAL_NUMBER = CASH_JOURNAL_NUMBER
        FISCAL_YEAR         = FISCAL_YEAR
        CASH_JOURNAL_DOC_NO = CASH_JOURNAL_DOC_NO
      TABLES
        ITEMS               = items
*       TAX_ITEMS           =
*       CPD_ITEMS           =
*       WTAX_ITEMS          =
*       EXTENSION_IN        =
        RETURN              = return.

    PERFORM flagerror.
    if ( errorflag = '0' ).
*      perform postCashJournal. "Added 29/03/2015 by Catalyst
      IF ( SIMULATION = 'N' ).
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
      ENDIF.
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
      DATE_EXTERNAL            = cash_j_line-POSTING_DATE
*     ACCEPT_INITIAL_DATE      =
    IMPORTING
      DATE_INTERNAL            = DATE_CONV
    EXCEPTIONS
      DATE_EXTERNAL_IS_INVALID = 1
      OTHERS                   = 2.
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
      INPUT  = cash_j_line-GL_ACCOUNT
    IMPORTING
      OUTPUT = account.
  if ( sy-subrc = 0 ).
    cash_j_line-GL_ACCOUNT = account.
  endif.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = cash_j_line-COST_CNTR
    IMPORTING
      OUTPUT = CCENTER.

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
form postCashJournal. "29/03/2015
*      form to post cash journal document after creating and saving using BAPI_CASHJOURNALDOC_CREATE
*      call fm FCJ_POST_ALL for this purpose

clear:
  itcj_postings.

refresh:
  itcj_postings.


  I_COMP_CODE = header-COMP_CODE.
  i_cajo_number = header-CAJO_NUMBER.
  i_currency = header-CURRENCY.
  i_typ = 'E'. "Parameter for a payment option R for receipt option
  I_DISPLAY_PERIOD_LO = header-PSTNG_DATE.
  I_DISPLAY_PERIOD_HI = header-PSTNG_DATE.


  itcj_postings-CAJO_NUMBER = header-CAJO_NUMBER.
  itcj_postings-COMP_CODE = header-COMP_CODE.
  itcj_postings-FISC_YEAR = header-PSTNG_DATE+0(4).
  itcj_postings-POSTING_NUMBER = CASH_JOURNAL_DOC_NO.
  itcj_postings-H_PAYMENTS = items-P_PAYMENTS.
  itcj_postings-h_net_amount = items-p_payments.
  itcj_postings-bp_name = header-bp_name.
  itcj_postings-DOCUMENT_DATE = header-DOC_DATE.
  itcj_postings-POSTING_DATE = header-PSTNG_DATE.
  itcj_postings-TAX_CODE = items-TAX_CODE.


CONCATENATE ' ' ITEMS-TRANSACT_NUMBER INTO ITEMS-TRANSACT_NUMBER.

  data trannum type string.

  concatenate space items-transact_number into trannum SEPARATED BY space.

  select single TRANSACT_NAME into itcj_postings-TRANSACT_NAME from TCJ_TRANS_NAMES where COMP_CODE = itcj_postings-COMP_CODE
    and TRANSACT_NUMBER  = trannum and LANGU = 'EN'.

    if ( sy-subrc <> 0 ).
        ad_str-ERROR_FLAG = 'Y'.
        ad_str-LIGHTS = '1'.
        ad_str-MSGNO = 'E000X'.
        ad_str-MESSAGE = 'A Valid Transaction name could not be found!'.

        append ad_str to ad_error .
      endif.


   itcj_postings-POSITION_TEXT = items-POSITION_TEXT.
   itcj_postings-KOKRS = '1000'.

   itcj_postings-KOSTL = items-COSTCENTER.
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = itcj_postings-KOSTL
    IMPORTING
      OUTPUT = itcj_postings-KOSTL.

    itcj_postings-PRCTR = items-PROFIT_CTR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = itcj_postings-PRCTR
    IMPORTING
      OUTPUT = itcj_postings-PRCTR.

    itcj_postings-GL_ACCOUNT = items-GL_ACCOUNT.
    itcj_postings-POSITION_NUMBER = '002'.
    itcj_postings-POSITION_TYPE = 'O'.

    append itcj_postings.

    CALL FUNCTION 'FCJ_POST_ALL'
      EXPORTING
        I_COMP_CODE               =    I_COMP_CODE
        I_CAJO_NUMBER             =    I_CAJO_NUMBER
        I_CURRENCY                =    I_CURRENCY
        I_TYP                     =    I_TYP
        I_DISPLAY_PERIOD_LO       =    I_DISPLAY_PERIOD_LO
        I_DISPLAY_PERIOD_HI       =    I_DISPLAY_PERIOD_HI
     IMPORTING
       E_ERROR_NUMBER            =    E_ERROR_NUMBER
      TABLES
        ITCJ_POSTINGS             =    ITCJ_POSTINGS
        ITCJ_WTAX_ITEMS           =    ITCJ_WTAX_ITEMS
        ITCJ_SPLIT_POSTINGS       =    ITCJ_SPLIT_POSTINGS
        ITCJ_CPD                  =    ITCJ_CPD
      CHANGING
        P_BEG_BALANCE             =    P_BEG_BALANCE
        P_TOTAL_RECEIPTS          =    P_TOTAL_RECEIPTS
        P_TOTAL_PAYMENTS          =    P_TOTAL_PAYMENTS
        P_TOTAL_CHECKS            =    P_TOTAL_CHECKS
        P_RUN_BALANCE             =    P_RUN_BALANCE
        P_RUN_CASH_BALANCE        =    P_RUN_CASH_BALANCE
        P_NUMB_OF_REC             =    P_NUMB_OF_REC
        P_NUMB_OF_PAYM            =    P_NUMB_OF_PAYM
        P_NUMB_OF_CHECKS          =    P_NUMB_OF_CHECKS
              .

    if ( sy-subrc <> 0 ).

      endif.


endform.