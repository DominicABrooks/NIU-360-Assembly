//KC03ABDA JOB ,'DOMINIC BROOKS',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
* CSCI 360-2             PROJECT 1                     FALL 2022 *
*                                                                *
* PROGRAMMER NAME: JACOB DIEP & DOMINIC BROOKS                   *
*        DATE DUE: 10/26/2022                                    *
*     DESCRIPTION: PROJECT TO DEMONSTRATE USE OF DSECTS & TABLES *
*  REGISTER USAGE:                                               *
*    R2   - HOLDS ADDRESS OF BUFFER WHILE STORING VALUES FOR     *
*            CUSTOMER TABLE, ORDER TABLE AND FISCAL REPORT TABLE.*
*         - USED TO SET VARIABLES IN OUTPUT BUFFER TO 0.         *
*         - USED TO HOLD ADDRESS OF THE TABLES TO PARSE VALUES   *
*            INTO THE TABLE OR PULL VALUES FROM THE TABLE.       *
*    R3   - USED TO ADDRESS OF TABLE WHILE PARSING VALUES INTO   *
*            CUSTOMER TABLE, ORDER TABLE AND FISCAL REPORT TABLE.*
*         - USED TO HOLD ADDRESS OF ROWS OF ORDERS WHEN          *
*            GENERATING REPORT FOR EACH CUSTOMER.                *
*    R4   - USED TO HOLD ADDRESS OF VARIABLES: $CID, $CST, $OCID,*
*            AND $OST.                                           *
*         - USED TO HOLD VALUES TO CONVERT INTO OUTPUT.          *
*         - USED TO INCREMENT THE NUMBER OF A VARIABLE SIMILAR   *
*            TO A COUNTER.                                       *
*         - USED TO HOLD VALUES WHEN PERFORMING ARITHMETIC       *
*             WHEN CREATING THE MONTHLY REPORT.                  *
*    R5   - USED TO HOLD VARIABLES TO PERFORM ARITHMETIC WHEN    *
*            GENERATING ORDER OUTPUT.                            *
*         - USED TO HOLD VARIABLE TO PERFORM ARITHMETIC WHEN     *
*            GENERATING FISCAL REPORT OUTPUT.                    *
*         - USED TO HOLD VALUES TO STORE INTO VARIABLE WHEN      *
*            GENERATING FISCAL REPORT OUTPUT.                    *
*         - USED TO HOLD VALUES TO USE IN COMPARISON AND         *
*            BRANCHING WHEN GENERATING FISCAL REPORT OUTPUT      *
*    R6   - USED TO HOLD VALUES WHEN PARSING DATA INTO TABLES.   *
*         - USED TO HOLD VALUES TO PERFORM ARTIHMETIC WHEN       *
*            GENERATING ORDERS FOR EACH CUSTOMER.                *
*         - USED TO INCREMENT VARIABLES WHEN GENERATING ORDERS   *
*            FOR EACH CUSTOMER.                                  *
*         - USED TO PREPARE VALUES FOR DIVISION WHEN CREATING    *
*            FISCAL REPORT OUTPUT.                               *
*    R7   - USED TO HOLD ADDRESS OF VALUES TO PERFORM ARITHMETIC *
*            WHEN GENERATING ORDER REPORTS FOR EACH CUSTOMER.    *
*         - USED TO HOLD VALUES WHEN PERFORMING ARITHMETIC WHEN  *
*            GENERATING ORDER REPORTS FOR EACH CUSTOMER.         *
*         - USED TO HOLD VALUES TO PERFORM ARITHMETIC WHEN       *
*            GENERATING FISCAL REPORT OUTPUT                     *
*    R8   - USED TO HOLD ADDRESS OF END OF A TABLE TO COMPARE    *
*            WITH CURRENT POSITION IN TABLE WHEN GENERATING ALL  *
*            REPORTS.                                            *
*    R9   - USED TO HOLD ADDRESS OF END OF A TABLE TO COMPARE    *
*            WITH CURRENT POSITION IN TABLE WHEN GENERATING      *
*            REPORTS.                                            *
*    R10  - USED TO HOLD VALUES TO CONVERT TO OUTPUT WHEN        *
*            CREATING CUSTOMER REPORT.                           *
*         - USED TO HOLD VALUES TO COMPARE WHEN GENERATING       *
*            ORDER REPORTS FOR EACH CUSTOMER                     *
*    R11  - USED TO HOLD VALUES TO COMPARE WHEN GENERATING       *
*            ORDER REPORTS FOR EACH CUSTOMER.                    *
******************************************************************
*CUST INFO INPUT**************************************************
$CBUFFR  DSECT          INPUT BUFFER FORMAT
$ICID    DS    CL8      CUSTOMER ID
         DS    C
$ICNM    DS    CL20     CUSTOMER NAME
         DS    C
$ICAD    DS    CL30     CUSTOMER ADDRESS
         DS    C
$ICST    DS    CL4      CUSTOMER REGIONAL SALES TAX
******************************************************************
*CUST INFO TABLE**************************************************
$CINFO   DSECT          TABLE STORAGE FORMAT
$CID     DS    2F       CUSTOMER ID
$CNM     DS    CL20     CUSTOMER NAME
$CAD     DS    CL30     CUSTOMER ADDRESS
$CST     DS    F        CUSTOMER REGIONAL SALES TAX
******************************************************************
*ORDER TABLE INPUT************************************************
$ODBUFFR DSECT          INPUT BUFFER FORMAT
$IOID    DS    CL8      ORDER ID
         DS    C
$IOCID   DS    CL8      CUSTOMER ORDER ID
         DS    C
$IOM     DS    CL2      INPUT ORDER MONTH
         DS    C
$IOD     DS    CL2      INPUT ORDER DAY
         DS    C
$IOST    DS    CL6      INPUT ORDER SUBTOTAL
         DS    C
$IOSP    DS    CL4      INPUT ORDER SHIPPING
         DS    C
$IOPM    DS    CL10     INPUT PAYMENT METHOD
         DS    C
$IOS     DS    CL6      INPUT STATUS
******************************************************************
*ORDER TABLE******************************************************
$OINFO   DSECT
$OID     DS    2F       ORDER ID
$OCID    DS    2F       ORDER CUSTOMER ID
$OM      DS    FL2      ORDER MONTH
$OD      DS    FL2      ORDER DAY
$OST     DS    2FL3     ORDER SUBTOTAL
$OSP     DS    F        ORDER SHIPPING
$OPM     DS    CL10     ORDER PAYMENT METHOD
$OS      DS    CL6      ORDER STATUS
******************************************************************
*FISCAL PREDICTION INPUT******************************************
$IFPBF   DSECT          INPUT BUFFER FORMAT
$IFPM    DS    CL2      INPUT MONTH
         DS    C
$IFPP    DS    CL10     INPUT PREDICTED PRICE
         DS    C
$IFPLB   DS    CL10     INPUT LOWER BOUND
         DS    C
$IFPUB   DS    CL10     INPUT UPPER BOUND
         DS    C
******************************************************************
*FISCAL PREDICTION TABLE******************************************
$FP      DSECT
$FPM     DS    FL2      FISCAL PREDICTION MONTH
$FPP     DS    5FL2     FISCAL PREDICTION PREDICTED PRICE
$FPLB    DS    5FL2     FISCAL PREDICTION LOWER BOUND
$FPUB    DS    5FL2     FISCAL PREDICTION UPPER BOUND
******************************************************************
MAIN     CSECT
         USING MAIN,15
******************************************************************
* ADD CUSTOMER INFO TO TABLE *************************************
         USING $CBUFFR,2           ADDR OF DSECT IBFFR
         USING $CINFO,3            ADDR OF DSECT TABLE
         LA    3,CTABL             LA ADDR OF DECLARED TABLE IN STORAGE
         XREAD BUFFER,80           READ LINE OF INPUT
* READ VAL INTO TABLE --------------------------------------------
SCANLOP1 CLI   BUFFER,C'*'         COMPARE TO SEE IF AT END OF LINE
         BE    ENDSCA1             BRANCH TO END OF LOOP IF AT *
         LA    2,BUFFER            LOAD BUFFER ADDRESS
         LA    4,$CID              LOAD CUST ID ADDR INTO REG
         LA    2,$ICID             LOAD INPUTER CUST ID ADDR INTO REG
         XDECI 6,0(2)              CONVERT REG.TAX FOR ARTIH
         ST    6,0(4)              STORE REG.TAX INTO TABLE
         LA    2,BUFFER            LOAD BUFFER ADDRESS
         MVC   $CNM(20),$ICNM      MOVE CUST NAME INTO TABLE
         MVC   $CAD(30),$ICAD      MOVE CUST ADDR INTO TABLE
         LA    4,$CST              LOAD CUST ADDR OF REGIONAL TAX
         LA    2,$ICST             LOAD INPUT REGIONAL TAX ADDR
         XDECI 6,0(2)              CONVERT REG.TAX FOR ARTIH
         ST    6,0(4)              STORE REG.TAX INTO TABLE
*
         LA    3,64(3)             LOAD ADDR OF NEXT ROW
         XREAD BUFFER,80           READ NEXT LINE INTO BUFFER
         B     SCANLOP1            BRANCH TO TOP OF LOOP
ENDSCA1  DS    0H                  END OF SCAN
         DROP  2,3                 DROP USED DSECTS
******************************************************************
*ADD ORDERS INFO TO TABLE ****************************************
         USING $ODBUFFR,2    ADDR OF DSECT IBFFR
         USING $OINFO,3      ADDR OF DSECT TABLE
         LA    3,CTABL       LA ADDR OF DECLARED TABLE IN STORAGE
         A     3,=F'1920'    LOAD RELATIVE ADDRESS OF OTABLE
         XREAD BUFFER,80     READ LINE OF INPUT
*READ VAL INTO TABLE
SCANLOP2 CLI   BUFFER,C'*'   COMPARE TO SEE IF AT END OF LINE
         BE    ENDSCA2       BRANCH TO END OF LOOP IF AT *
         LA    2,BUFFER      LOAD BUFFER ADDR INTO REGISTER 2
         MVC   $OID(8),$IOID MOVE 8 BYTES FROM INPUT TO TABLE
         LA    2,$IOCID      LOAD ADDR OF INPUT ORDER CUST ID INTO R2
         LA    4,$OCID       LOAD ADDR OF ORDER CUST ID INTO R4
         XDECI 6,0(2)        CONVERT R2 TO DECIMAL INTO R6
         ST    6,0(4)        STORE VALUE BACK INTO OCID
         LA    2,BUFFER      LOAD ADDR OF BUFFER BACK INTO R2
         MVC   $OM(2),$IOM   MOVE FROM INPUT INTO TABLE
         MVC   $OD(2),$IOD   MOVE FROM INPUT INTO TABLE
         LA    2,$IOST       LOAD ADDR OF INPUT SUBTOT TO R2
         LA    4,$OST        LOAD ADDR OF SUBTOT TO R4
         XDECI 6,0(2)        CONVERT R2 INTO DECIMAL INTO R6
         ST    6,0(4)        STORE VALUE BACK INTO OST
         LA    2,BUFFER      LOAD BUFFER'S ADDR BACK INTO R2
         MVC   $OSP(4),$IOSP LOAD FROM INPUT INTO TABLE
         MVC   $OPM(10),$IOPM LOAD FROM INPUT INTO TABLE
         MVC   $OS(6),$IOS   LOAD FROM INPUT INTO TABLE
*
         LA    3,48(3)      LOAD ADDR OF NEXT ROW
         XREAD BUFFER,80    READ NEXT LINE
         B     SCANLOP2     LOOP BACK TO TOP OF SCAN LOOP
ENDSCA2  DS    0H           END SCAN LOOP
         DROP  2,3          DROP USED DSECTS
******************************************************************
*ADD FISCAL PREDICTION TO TABLE **********************************
         USING $IFPBF,2            ADDR OF DSECT IBFFR
         USING $FP,3               ADDR OF DSECT TABLE
         LA    3,CTABL             LA ADDR OF DECLARED TABLE IN STORAGE
         A     3,=F'18720'
         XREAD BUFFER,80           READ LINE OF INPUT
*
* READ FISCAL PREDICTION DATE INTO TABLE -------------------------
SCANLOP3 BNZ   ENDSCA3             BRANCH TO END OF LOOP IF AT EOF
         LA    2,BUFFER            LOAD BUFFER ADDRESS
         MVC   $FPM(2),$IFPM       MOVE FROM INPUT INTO TABLE
         MVC   $FPP(10),$IFPP      MOVE FROM INPUT INTO TABLE
         MVC   $FPLB(10),$IFPLB    MOVE FROM INPUT INTO TABLE
         MVC   $FPUB(10),$IFPUB    MOVE FROM INPUT INTO TABLE
*
         LA    3,32(3)             LOAD ADDR OF NEXT ROW
         XREAD BUFFER,80           READ NEXT LINE
         B     SCANLOP3            LOOP BACK TO TOP OF SCAN LOOP
ENDSCA3  DS    0H                  END SCAN LOOP
         DROP  2,3                 DROP USED DSECTS
******************************************************************
* LOAD 0 INTO ACTUAL SALE TOTALS FOR EACH MONTH ------------------
         LA    2,0
         XDECO 2,JANTT
         XDECO 2,FEBTT
         XDECO 2,MARTT
         XDECO 2,APRTT
         XDECO 2,MAYTT
         XDECO 2,JUNTT
         XDECO 2,JULTT
         XDECO 2,AUGTT
         XDECO 2,SEPTT
         XDECO 2,OCTTT
         XDECO 2,NOVTT
         XDECO 2,DECTT
* ----------------------------------------------------------------
* GENERATE CUSTOMER ORDER REPORT ---------------------------------
         USING $CINFO,2      ADDR OF DSECT CUST INFO
         LA    2,CTABL       LOAD IN CTABL INTO R2
* LOAD END OF CTABLE INTO R8
         LA    8,CTABL       LOAD CTABL INTO R8
         A     8,=F'1920'    MOVE PAST CTABL TO OTABL
* LOAD END OF OTABLE INTO R9
         LA    9,CTABL       LOAD CTABL INTO R9
         A     9,=F'18720'   MOVE PAST CTABL TO FTABL
CLOOP    DS    0H            CUSTOMER READ LOOP
*
         L     10,$CID         STORE CONTENTS OF REG 7 INTO PID
         XDECO 10,TEMP
         MVC   PID(8),TEMP+4
         MVC   PNM(20),$CNM
         MVC   PAD(30),$CAD
         L     10,$CST         STORE CONTENTS OF REG 7 INTO PST
         XDECO 10,TEMP
         MVC   PST(4),TEMP+8
*
         XPRNT PRNTCUST,133
         XPRNT PRNTADRS,133
*
         XPRNT PRNTORHD,133
         XPRNT PRNTRPHD,133
         XPRNT PRNTSPRT,133
*
* GENERATING ORDER CONTENTS OF TABLE *****************************
         USING $OINFO,3                LOAD OINFO DSECT
         LA    3,CTABL                 LOAD IN CTABL ADDRESS
         A     3,=F'1920'              MOVE PAST CTABL TO OTABL
         LA    6,0                     LOAD 0 INTO R6
         XDECO 6,PCORD                 CLOSED ORDER COUNT
         XDECO 6,PCOPO                 OPEN ORDER COUNT
         XDECO 6,PRTTO                 TOTAL ORDER COUNT
         XDECO 6,PRETT                 RECEIVING TOTAL
         XDECO 6,PRPTT                 PENDING TOTAL
         XDECO 6,PROAT                 OVERALL AMOUNT
         XDECO 6,PRAOA                 AVERAGE ORDER AMOUNT
OLOOP    DS    0H                      ORDER CONTENTS READ LOOP
         LA    3,48(3)                 LOADS NEXT ORDER
         CR    3,9                     COMPARE TO END OF TABLE
         BE    EOLOOP                  IF AT END GO TO END
         L     10,$OCID                LOAD ORDER CID INTO R10
         L     11,$CID                 LOAD CID       INTO R11
         CR    10,11                   COMPARE THE TWO CID'S
         BNE   OLOOP                   BRANCH TO TOP  IF NOT EQUAL
         MVC   PCRID(8),$OID           MOVE IN ORDER ID
         MVC   PCRM(2),$OM             MOVE IN MONTH FOR DATE
         MVC   PCRD(2),$OD             MOVE IN DAY FOR DATE
         MVC   PCRPM(10),$OPM          LOAD IN PAYMENT METHOD OF ORDER
         L     4,$OST                  LOAD ORDER SUBTOTAL INTO R4
         XDECO 4,TEMP                  MAKE PRINTABLE INTO TEMP
         MVC   PCRST(6),TEMP+6         SHIPPING TAX
         MVC   PCRSP(4),$OSP           LOAD IN SHIPPING VALUE
*CALCULATE TOTAL
         L     7,$CST          LOAD CUSTOMER SALES TAX INTO R7
         M     6,$OST          MULTIPLE BY ORDER SUBTOTAL
         M     6,=F'1'         ROUND UP TO GET RID OF DECIMAL
         D     6,=F'10000'     DIVIDE PRODUCT BY 10000
*
         A     7,$OST          ADD PRODUCT VALUE TO SUBTOTAL
         XDECI 6,$OSP          CONVERT SHIPPING VALUE
         AR    7,6             ADD SHIPPING TO TOTAL
*
******************************************************************
         XDECI 6,PCORD                 LOAD DECIMAL CLOS ORDER INTO R6
         XDECI 4,PCOPO                 LOAD DECIMAL OPEN ORDER INTO R4
         CLI   $OS,C'C'                CHECK IF STATUS IS CLOSED
         BE    CLO                     BRANCH TO CLO IF ORDER IS CLOSED
         LA    4,1(,4)                 ELSE INCREMENT OPEN ORDER
         XDECO 4,PCOPO                 CONVERT OPEN ORDER
         XDECI 5,PRPTT                 CONVERT PENDING TOTAL
         AR    5,7                     ADD SUBTOTAL TO PENDING TOTAL
         XDECO 5,PRPTT                 CONVERT TO OUTPUT PENDING TOTAL
         B     CON                     CONTINUE PASSED CLOSED PORTION
CLO      LA    6,1(,6)                 INCREMENT CLOSED ORDER #
         XDECO 6,PCORD                 CONVERT PEND TOTAL
         XDECI 5,PRETT                 CONVERT RECEIVED TOTAL
         AR    5,7                     ADD TO RECEIVED TOTAL
         XDECO 5,PRETT                 CONVERT TO OUTPUT RECEIVED TOTAL
CON      XDECI 6,PRTTO                 CONVERT TOTAL ORDERS
         LA    6,1(,6)                 INCREMENT TOTAL ORDERS
         XDECO 6,PRTTO                 CONVERT TO OUPUT TOTAL ORDERS
         MVC   PCRSS(6),$OS            MOVE IN ORDER STATUS
         XDECI 5,PROAT                 CONVERT TOTAL AMOUNT
         AR    5,7                     ADD TOTAL OF ORDER TO TOTAL
         XDECO 5,PROAT                 CONVERT TO OUTPUT TOTAL AMOUNT
******************************************************************
         XDECO 7,TEMP       CONVERT TOTAL TO PRINTABLE CHARACTERS
         MVC   PCRTT,TEMP+2 COPY INTO OUTPUT FIELD
         XPRNT PRNTCRTB,133 PRINT FIELDS
* ADD SUBTOTAL TO MONTH'S TOTAL ----------------------------------
         XDECI 5,PCRST               CONVERT SUBTOTAL TO BE ADDED
         CLI   $OM,C'0'              CHECK IF MONTH LEADS WITH 0
         BE    SDMO                  BRANCH TO SINGLE DIGIT MONTHS
*
         CLI   $OM+1,C'0'            BRANCH TO OCT IF MONTH 10
         BE    OCTO
         CLI   $OM+1,C'1'            BRANCH TO NOV IF MONTH 11
         BE    NOVO
         CLI   $OM+1,C'2'            BRANCH TO DEC IF MONTH 12
         BE    DECO
SDMO     CLI   $OM+1,C'1'            BRANCH TO JAN IF MONTH 01
         BE    JANO
         CLI   $OM+1,C'2'            BRANCH TO FEB IF MONTH 02
         BE    FEBO
         CLI   $OM+1,C'3'            BRANCH TO MAR IF MONTH 03
         BE    MARO
         CLI   $OM+1,C'4'            BRANCH TO APR IF MONTH 04
         BE    APRO
         CLI   $OM+1,C'5'            BRANCH TO MAY IF MONTH 05
         BE    MAYO
         CLI   $OM+1,C'6'            BRANCH TO JUN IF MONTH 06
         BE    JUNO
         CLI   $OM+1,C'7'            BRANCH TO JUL IF MONTH 07
         BE    JULO
         CLI   $OM+1,C'8'            BRANCH TO AUG IF MONTH 08
         BE    AUGO
         CLI   $OM+1,C'9'            BRANCH TO SEP IF MONTH 09
         BE    SEPO
*
JANO     XDECI 7,JANTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,JANTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
FEBO     XDECI 7,FEBTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,FEBTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
MARO     XDECI 7,MARTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,MARTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
APRO     XDECI 7,APRTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,APRTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
MAYO     XDECI 7,MAYTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,MAYTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
JUNO     XDECI 7,JUNTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,JUNTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
JULO     XDECI 7,JULTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,JULTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
AUGO     XDECI 7,AUGTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,AUGTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
SEPO     XDECI 7,SEPTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,SEPTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
OCTO     XDECI 7,OCTTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,OCTTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
NOVO     XDECI 7,NOVTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,NOVTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
DECO     XDECI 7,DECTT
         AR    7,5                    ADD SUBTOTAL TO MONTHS TOTAL
         XDECO 7,DECTT
         B     EOMONO                 BRANCH TO END OF MONTH LOGIC
EOMONO   DS    0H
*
         XDECI 7,PROAT                CONVERT OVERALL TT AND PUT IN R7
         XDECI 5,PRTTO                CONVERT TOTAL ORDS AND PUT IN R5
         M     6,=F'1'                SPREAD 6 EVEN-ODD PAIR
         DR    6,5                    DIVIDE OVERALL TT BY TT ORDS
         A     7,=F'1'                ADD 1 TO RESULT
         XDECO 7,PRAOA                LOAD INTO AVR ORDER AMT
         B     OLOOP                  BRANCH TO TOP OF ORDER LOOP
EOLOOP   LA    2,64(2)                LOADS NEXT CUSTOMER
******************************************************************
         XPRNT PRNTSPRT,133
* FOOTER
         MVC   TEMP(12),PCORD      STORE VALUE OF PCORD INTO TEMP
         MVC   PCORD(2),TEMP+10    LAST 2 FROM TEMP TO PCORD
         MVC   PCORD+2(10),TEMP    FILL SPACE TO TAILING 10 BYTES
         XPRNT PRNTRPF1,133        PRNT FIRST FOOTER LINE
         MVC   TEMP(12),PCOPO      STORE VALUE OF PCORD INTO TEMP
         MVC   PCOPO(2),TEMP+10    LAST 2 FROM TEMP TO PCORD
         MVC   PCOPO+2(10),TEMP    FILL SPACE TO TAILING 10 BYTES
         XPRNT PRNTRPF2,133        PRINT SECOND FOOTER LINE
         MVC   TEMP(12),PRTTO      STORE VALUE OF PCORD INTO TEMP
         MVC   PRTTO(2),TEMP+10    LAST 2 FROM TEMP TO PCORD
         MVC   PRTTO+2(10),TEMP    FILL SPACE TO TAILING 10 BYTES
         XPRNT PRNTRPF3,133        PRINTER THIRD FOOTER LINE
         XPRNT PRNTRPF4,133        PRINT FOURTH FOOTER LINE
*
         CR    2,8           IF PAST END OF TABLE
         BE    ECLOOP        GO TO ENDLOOP
         B     CLOOP         OTHERWISE LOAD NEXT CUSTOMER
*
ECLOOP  DS    0H
************************ PART II *********************************
* GENERATING FISCAL REPORT SUMMARY *******************************
         XPRNT PRNTNWLN,133  PRINT NEW LINE
         XPRNT PRNTFRHD,133  PRINT FISCAL REPORT HEADER
         XPRNT PRNTSPRT,133  PRINT SEPERATOR
* GENERATING FISCAL REPORT TABLE ROWS ****************************
         USING $FP,2                   USE FISCAL REPORT DSECT
         LA    2,CTABL                 LOAD IN CTABL ADDRESS
         A     2,=F'18720'             MOVE PAST CTABL TO FTABL
         LR    9,2                     LOAD FTABL ADDR INTO R9
         A     9,=F'384'               MOVE PAST FTABL TO END
FRLOOP   DS    0H                      PRINT CONTENTS OF ROWS
* MONTH ----------------------------------------------------------
         CLI   $FPM,C'0'              CHECK IF MONTH LEADS WITH 0
         BE    SDM                    BRANCH TO SINGLE DIGIT MONTHS
*
         CLI   $FPM+1,C'0'            BRANCH TO OCT IF MONTH 10
         BE    OCT
         CLI   $FPM+1,C'1'            BRANCH TO NOV IF MONTH 11
         BE    NOV
         CLI   $FPM+1,C'2'            BRANCH TO DEC IF MONTH 12
         BE    DEC
SDM      CLI   $FPM+1,C'1'            BRANCH TO JAN IF MONTH 01
         BE    JAN
         CLI   $FPM+1,C'2'            BRANCH TO FEB IF MONTH 02
         BE    FEB
         CLI   $FPM+1,C'3'            BRANCH TO MAR IF MONTH 03
         BE    MAR
         CLI   $FPM+1,C'4'            BRANCH TO APR IF MONTH 04
         BE    APR
         CLI   $FPM+1,C'5'            BRANCH TO MAY IF MONTH 05
         BE    MAY
         CLI   $FPM+1,C'6'            BRANCH TO JUN IF MONTH 06
         BE    JUN
         CLI   $FPM+1,C'7'            BRANCH TO JUL IF MONTH 07
         BE    JUL
         CLI   $FPM+1,C'8'            BRANCH TO AUG IF MONTH 08
         BE    AUG
         CLI   $FPM+1,C'9'            BRANCH TO SEP IF MONTH 09
         BE    SEP
*
JAN      MVC   PFRMO(9),CJAN          LOAD WITH JAN'S NAME
         MVC   PFRAS(12),JANTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
FEB      MVC   PFRMO(9),CFEB          LOAD WITH FEB'S NAME
         MVC   PFRAS(12),FEBTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
MAR      MVC   PFRMO(9),CMAR          LOAD WITH MAR'S NAME
         MVC   PFRAS(12),MARTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
APR      MVC   PFRMO(9),CAPR          LOAD WITH APR'S NAME
         MVC   PFRAS(12),APRTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
MAY      MVC   PFRMO(9),CMAY          LOAD WITH MAY'S NAME
         MVC   PFRAS(12),MAYTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
JUN      MVC   PFRMO(9),CJUN          LOAD WITH JUN'S NAME
         MVC   PFRAS(12),JUNTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
JUL      MVC   PFRMO(9),CJUL          LOAD WITH JUL'S NAME
         MVC   PFRAS(12),JULTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
AUG      MVC   PFRMO(9),CAUG          LOAD WITH AUG'S NAME
         MVC   PFRAS(12),AUGTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
SEP      MVC   PFRMO(9),CSEP          LOAD WITH SEP'S NAME
         MVC   PFRAS(12),SEPTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
OCT      MVC   PFRMO(9),COCT          LOAD WITH OCT'S NAME
         MVC   PFRAS(12),OCTTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
NOV      MVC   PFRMO(9),CNOV          LOAD WITH NOV'S NAME
         MVC   PFRAS(12),NOVTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
DEC      MVC   PFRMO(9),CDEC          LOAD WITH DEC'S NAME
         MVC   PFRAS(12),DECTT
         B     EOMON                  BRANCH TO END OF MONTH LOGIC
EOMON    DS    0H
* PREDICTED SALES -------------------------------------------------
*
         MVC   PFRPS(10),$FPP         LOAD ROW WITH PREDICTED SALES
* DIFF SQRD -------------------------------------------------------
         XDECI 5,PFRPS+2              LOAD PREDICTED SALES INTO R5
         XDECI 4,PFRAS                LOAD ACTUAL    SALES INTO R4
         SR    5,4                    SUBTRACT PREDICTED FROM ACTUAL
         XDECO 5,TEMP                 STORE R5 INTO TEMP FOR DIVERGEN
         MR    4,5                    SQUARE RESULT
         XDECO 5,PFRDS                CONVERT AND PUT IN PRINT VAR
         XDECI 7,PRRSSS               PUT RSS INTO R7
         M     4,=F'1'                SPREAD 4 OVER EVEN-ODD
         D     4,=F'100'              DIVIDE 5 BY 100
         A     5,=F'1'                ADD 1 TO RESULT
         AR    7,5                    ADD RSS TO RESULT
         XDECO 7,PRRSSS               CONVERT AND STORE IN RSS
* DIVERGENCE ------------------------------------------------------
         XDECI 5,PFRAS                STORE ACTUAL SALE INTO R5
         XDECI 4,PFRPS+2              STORE PREDIC SALE INTO R4
         CR    4,5                    CHECK IF NEGATIVE DIFF
         BL    UPPERB                 IF NEGATIVE, BRANCH UPPER
         B     LOWERB                 IF POSSITIVE,BRANCH LOWER
UPPERB   DS    0H                     IF UPPER BOUND CONDITION
         XDECI 4,PFRAS                STORE ACTUAL IN R4
         MVC   TEMP9(9),$FPUB+1       STORE UPPER BOUND IN TEMP
         XDECI 5,TEMP9                CONVERT TEMP INTO R5
         SR    5,4                    SUBTRACT ACTUAL FROM UPPER
         C     5,=F'0'                IF HIGHER THAN 0,
         BH    INBOUNDS               BRANCH INBOUND
         M     4,=F'-1'               MAKE POSITIVE
         XDECO 5,PFRDI                STORE CONTENTS INTO PRINT VAR
         B     EODIV                  BRANCH TO END OF DIV LOGIC
LOWERB   DS    0H                     IF LOWER BOUND CONDITION
         XDECI 4,PFRAS                STORE ACTUAL INTO R4
         MVC   TEMP9(9),$FPLB+1       STORE LOWER BOUND INTO TEMP
         XDECI 5,TEMP9                CONVERT TEMP INTO R5
         SR    5,4                    SUBTRACT ACTUAL FROM LOWER
         C     5,=F'0'                IF LOWER THAN 0,
         BL    INBOUNDS               BRANCH INBOUNDS
         XDECO 5,PFRDI                CONVERT AND STORE INTO PRINT VAR
         B     EODIV                  BRANCH TO END OF DIV LOGIC
INBOUNDS LA    5,0                    STORE 0 IN REG 5
         XDECO 5,PFRDI                LOAD R5 INTO PRINT VAR
EODIV    DS    0H                     END OF DIVERGENCE LOGIC
* END OF ROW CONTENTS ---------------------------------------------
         XPRNT PRNTFRTB,133           PRINT ROW
*
         LA    2,32(2)                LOADS NEXT ORDER
         CR    2,9                    IF ABS ADDRESS = END
         BE    EOFRLOOP               BRANCH TO END OF FISCAL REPORT
         B     FRLOOP                 OTHERWISE BACK TO TOP
EOFRLOOP DS    0H                     END OF FISCAL REPORT CONTENTS
* ----------------------------------------------------------------
*
* GENERATING FISCAL REPORT FOOTER ********************************
         XPRNT PRNTSPRT,133           PRINT SEPERATOR
         XPRNT PRNTFRFT,133           PRINT FOOTER, INCLUDING RSS
*-----------------------------------------------------------------
******************************************************************
*
         BR    14
*
         LTORG
*| Declared storage **********************************************
BUFFER   DS    CL80
TEMP     DS    CL12
TEMP9    DS    CL9
* MONTHS ACTUAL SALES --------------------------------------------
JANTT    DS    CL12
FEBTT    DS    CL12
MARTT    DS    CL12
APRTT    DS    CL12
MAYTT    DS    CL12
JUNTT    DS    CL12
JULTT    DS    CL12
AUGTT    DS    CL12
SEPTT    DS    CL12
OCTTT    DS    CL12
NOVTT    DS    CL12
DECTT    DS    CL12
* MONTHS CONSTANT NAMES ------------------------------------------
CJAN     DC    C'January  '
CFEB     DC    C'February '
CMAR     DC    C'March    '
CAPR     DC    C'April    '
CMAY     DC    C'May      '
CJUN     DC    C'June     '
CJUL     DC    C'July     '
CAUG     DC    C'August   '
CSEP     DC    C'September'
COCT     DC    C'October  '
CNOV     DC    C'November '
CDEC     DC    C'December '
*****************************************************************
* PRINT LINES ***************************************************
* Print CUSTOMER begin ------------------------------------------
PRNTCUST DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTCUST+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    C'Customer: '
PNM      DS    CL20
         DC    C' '
         DC    C'            Customer ID: '
PID      DS    CL8
         DC    C' '
         DC    C'            Regional Sales Tax: '
PST      DS    CL4
         ORG   PRNTCUST+133   JUMP PAST END OF PRINTLN1
*****************************************************************
* Print ADDRESS begin -------------------------------------------
PRNTADRS DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTADRS+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    C'Address: '
PAD      DS    CL30
         ORG   PRNTADRS+133   JUMP PAST END OF PRINTLN1
*****************************************************************
* Print NEWLINE begin -------------------------------------------
PRNTNWLN DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
*****************************************************************
* Print SEPERATOR begin -----------------------------------------
PRNTSPRT DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    68C'='         FILL LINE WITH = SEPERATOR
         DC    64C' '
*****************************************************************
* Print REPORT HEADER begin -------------------------------------
PRNTRPHD DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTRPHD+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    C'ID        '
         DC    C'Date   '
         DC    C'Status  '
         DC    C'Payment Method  '
         DC    C'Subtotal  '
         DC    C'Shipping  '
         DC    C'Total       '
         ORG   PRNTRPHD+133   JUMP PAST END
*****************************************************************
* Print CUSTOMER REPORT TABLE ROW begin -------------------------
PRNTCRTB DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTCRTB+1     GO BACK TO PRINTLN1 START LOC + 1
PCRID    DS    CL8
         DC    C'  '
PCRM     DS    CL2
         DC    C'/'
PCRD     DS    CL2
         DC    C'  '
PCRSS    DS    CL6
         DC    C'  '
PCRPM    DS    CL10
         DC    C'      '
PCRST    DS    CL6
         DC    C'    '
PCRSP    DS    CL4
         DC    C'   '
PCRTT    DS    CL10
         ORG   PRNTCRTB+133   JUMP PAST END
*****************************************************************
* Print ORDER HEADER begin --------------------------------------
PRNTORHD DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTORHD+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    C'Orders: '
         ORG   PRNTORHD+133   JUMP PAST END
*****************************************************************
* Print REPORT FOOTER 1 begin -----------------------------------
PRNTRPF1 DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTRPF1+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    14C' '
         DC    C'Closed Orders: '
PCORD    DS    CL12
         DC    C'Received Total:'
PRETT    DS    CL8
         ORG   PRNTRPF1+133   JUMP PAST END
*****************************************************************
* Print REPORT FOOTER 2 begin -----------------------------------
PRNTRPF2 DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTRPF2+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    16C' '
         DC    C'Open Orders: '
PCOPO    DS    CL12
         DC    C' Pending Total:'
PRPTT    DS    CL8
         ORG   PRNTRPF2+133   JUMP PAST END
*****************************************************************
* Print REPORT FOOTER 3 begin -----------------------------------
PRNTRPF3 DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTRPF3+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    15C' '
         DC    C'Total Orders: '
PRTTO    DS    CL12
         DC    C' Overall Total:'
PROAT    DS    CL8
         ORG   PRNTRPF3+133   JUMP PAST END
*****************************************************************
* Print REPORT FOOTER 4 begin -----------------------------------
PRNTRPF4 DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTRPF4+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    42C' '
         DC    C'Avg Order Amt:'
PRAOA    DS    CL8
         ORG   PRNTRPF4+133   JUMP PAST END
*****************************************************************
* Print FISCAL REPORT HEADER begin ------------------------------
PRNTFRHD DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTFRHD+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    C'Month      '
         DC    C'Predicted Sales  '
         DC    C'Actual Sales  '
         DC    C'DiffSqrd      '
         DC    C'Divergence  '
         ORG   PRNTFRHD+133   JUMP PAST END
*****************************************************************
* Print FISCAL REPORT FOOTER begin ----------------------------
PRNTFRFT DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTFRFT+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    44C' '
         DC    C'RSS: '
PRRSSS   DS    CL8
         ORG   PRNTFRFT+133   JUMP PAST END
*****************************************************************
* Print  FISCAL REPORT TABLE ROW begin -------------------------
PRNTFRTB DC    C' '           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTFRTB+1     GO BACK TO PRINTLN1 START LOC + 1
PFRMO    DS    CL9
         DC    C'  '
PFRPS    DS    CL10
         DC    C'   '
PFRAS    DS    CL12
         DC    C'  '
PFRDS    DS    CL12
         DC    C'      '
PFRDI    DS    CL12
         ORG   PRNTFRTB+133   JUMP PAST END
*****************************************************************
         DS    0F            ENFORCE FULLWORD BOUNDARY
CTABL    DS    30CL64
OTABL    DS    350CL48
         DS    0H
FTABL    DS    12CL32
         DS    0F
*
         END   MAIN
/*
//*
//* IN-STREAM PROGRAM DATA
//FT05F001 DD *
25103262 Vanesa Alonso        4154 W. Madison Dr. Buffalo 14 1000
47947039 Jayda Graham         8012 S. Thompson St. West Farg 0800
14939103 Paddy Loates         9963 Newbridge Road Ridgewood  1100
99706089 Clay Tyson           334 Clinton Ave. Land O Lakes  1700
83817646 Cari Culver          9159 Lafayette St. Dawsonville 0950
71142682 Geo Blanco           833 Wintergreen Ave. East Hart 1850
72363068 Dani Dyal            773 Saxon St. Union City 7087  2000
27945702 Amy Ponting          961 Meadowbrook St. Grand Have 0800
24887287 Angel Hernandez      5426 E. Buckingham Ave. Pleasa 0950
62378290 Nicolasa Cruz        3769 Corona Dr. Mountain View  1850
99143393 Irene Byram          881 Rockwell St. Rockville 208 1000
47068534 Joseph Mileham       4189 Prairie St. Pleasanton 94 1850
43425673 Caroline Hill        3817 Mayfair Avenue West Haven 1500
29636844 Kirk Rundle          6422 West Prairie Ave. Clearwa 1700
71762930 Montie Kettle        9682 West Belmont Drive Lawren 1500
54793504 Dulcie Lane          3946 Queen Ave. Westland 48185 0800
99184972 Garth Voyle          123 Walnut Ave. Hastings 55033 1500
19509311 Leonora Keenum       8010 Atlantic Dr. Northbrook 6 1500
99951450 Patsy Balfour        6258 Shirley Ave. Douglasville 2000
58436555 William Guest        2810 S. Chestnut Lane West Blo 0950
38675976 Juanma Moreno        2707 North Rockland Street Min 1700
90249485 Alice Abarough       5425 Atlantic Dr. Suitland 207 2000
20366910 Aidin Leavitt        2233 W. Garfield Street Readin 0800
80626987 Elektra Kovacevic    5635 Purple Finch Lane Union C 1700
43465338 Sylvie Wolfenden     8899 Newcastle Avenue Green Co 1100
81535346 Finn MacCawley       1272 Kirkland St. Rock Hill 29 2000
82561732 Elisha Bugden        6552 Roehampton Ave. Frederick 1850
61051086 Stacy Bean           4828 Ridgeview Court Englewood 1500
46547823 Melissa Anstey       9070 Edgemont Avenue Mentor 44 0800
05398625 Raphael Poe          387 Kent Road Boca Raton 33428 1100
*
07855347 62378290 08 17 222618 7712 MONEY ORD  CLOSED
82932238 83817646 01 28 182891 1986 MASTERCARD CLOSED
38492198 29636844 06 28 145899 3408 MASTERCARD CLOSED
33625743 71142682 12 13 759397 0172 CHECK      CLOSED
39490450 83817646 06 06 799839 0772 MASTERCARD CLOSED
94441773 47068534 12 07 317915 1603 MONEY ORD  CLOSED
73847233 80626987 09 19 910345 3686 CHECK      CLOSED
41898542 72363068 07 02 862030 3649 MONEY ORD  CLOSED
34090777 99706089 05 15 843182 0679 MONEY ORD  CLOSED
68748522 43465338 07 16 903170 0563 MONEY ORD  CLOSED
11097075 27945702 02 19 337437 0073 MASTERCARD CLOSED
73339488 62378290 10 10 017370 8345 CHECK      CLOSED
13315533 99143393 03 16 713033 4262 VISA       CLOSED
68763759 62378290 11 05 824320 3924 CHECK      CLOSED
66106118 81535346 08 22 223138 0114 CHECK      CLOSED
72943928 47947039 09 23 960611 1225 MASTERCARD CLOSED
33334966 99143393 12 23 546236 2670 VISA       CLOSED
00779227 46547823 07 08 356850 4294 MONEY ORD  CLOSED
81874183 29636844 12 23 105313 5928 MASTERCARD CLOSED
11801350 90249485 08 08 104527 6858 CHECK      CLOSED
59702529 99184972 04 19 583993 0669 VISA       CLOSED
93345468 29636844 01 30 911528 0993 MASTERCARD CLOSED
17201194 47947039 03 23 526119 2125 CHECK      CLOSED
57909439 27945702 06 11 092237 4620 MONEY ORD  CLOSED
31044048 54793504 10 03 167460 6025 VISA       CLOSED
68150603 99143393 05 19 340123 4688 MASTERCARD CLOSED
14093445 99951450 10 28 901806 1685 CHECK      CLOSED
06921921 47068534 03 08 200310 1053 VISA       CLOSED
26287968 38675976 11 27 859227 2287 VISA       CLOSED
47440292 43465338 05 25 772782 0778 VISA       CLOSED
88308206 25103262 11 19 291642 2858 MONEY ORD  CLOSED
41957773 24887287 06 24 035608 3982 CHECK      CLOSED
49562752 80626987 04 07 728641 0535 CHECK      CLOSED
14256936 99706089 06 30 167337 0050 MASTERCARD CLOSED
23499474 43425673 05 18 940024 3490 MONEY ORD  CLOSED
06857893 72363068 10 25 762575 5874 MONEY ORD  CLOSED
05803760 54793504 12 04 726806 1476 MONEY ORD  CLOSED
27819182 19509311 04 04 765386 4622 MASTERCARD CLOSED
96364478 14939103 11 06 580252 0347 MONEY ORD  CLOSED
42561027 38675976 02 19 420562 0788 MASTERCARD CLOSED
16477278 43425673 10 08 323757 8780 VISA       CLOSED
08321338 99184972 10 20 056960 1455 MASTERCARD CLOSED
39635893 43425673 04 06 156320 1650 CHECK      CLOSED
06674512 83817646 10 27 847913 4495 VISA       CLOSED
56037663 82561732 02 17 849272 0423 CHECK      CLOSED
05144694 61051086 05 03 197852 1111 MONEY ORD  CLOSED
65149320 99184972 05 20 618505 0793 VISA       CLOSED
86684409 54793504 12 30 599568 3088 MONEY ORD  CLOSED
40861222 14939103 01 24 137229 0504 MONEY ORD  CLOSED
34939668 82561732 04 12 235033 4455 VISA       CLOSED
91366923 43425673 05 17 710277 4515 MASTERCARD CLOSED
29001792 24887287 06 06 602807 1401 MASTERCARD CLOSED
25782080 25103262 03 07 577358 2521 MASTERCARD CLOSED
87015406 54793504 03 16 897115 5860 MONEY ORD  CLOSED
34606985 81535346 10 28 300094 3187 VISA       CLOSED
38948276 99143393 12 22 235383 4105 MASTERCARD CLOSED
65014727 29636844 11 10 883213 1732 MONEY ORD  CLOSED
38098686 99143393 06 17 535418 1895 MONEY ORD  CLOSED
19361203 90249485 11 13 278471 6348 CHECK      CLOSED
77723292 99143393 08 01 624905 2126 MONEY ORD  CLOSED
04325687 71762930 03 22 357950 0341 CHECK      CLOSED
04637871 54793504 10 30 189101 0431 VISA       CLOSED
92783470 47947039 09 08 048867 4635 VISA       CLOSED
10676283 38675976 11 26 791486 6094 CHECK      CLOSED
32311752 58436555 08 12 045121 1302 CHECK      CLOSED
09641731 29636844 12 02 388656 0774 MONEY ORD  CLOSED
85630853 99951450 09 20 920342 1071 MONEY ORD  CLOSED
86736604 62378290 01 06 196614 0505 VISA       CLOSED
69983752 46547823 05 26 506484 3227 MONEY ORD  CLOSED
92351893 80626987 08 06 291043 2106 MASTERCARD CLOSED
48887558 24887287 08 04 533468 0792 MASTERCARD CLOSED
67264567 25103262 08 12 153253 2026 MASTERCARD CLOSED
54993518 20366910 08 26 489503 7903 MASTERCARD CLOSED
72527813 62378290 07 24 036721 1598 CHECK      OPEN
19710598 80626987 02 06 742446 0916 MASTERCARD CLOSED
30261657 43425673 01 27 710067 3340 MONEY ORD  CLOSED
79577401 47068534 11 15 205246 5790 CHECK      CLOSED
63182723 82561732 06 06 623600 1169 MONEY ORD  OPEN
95621833 19509311 04 22 072507 5054 VISA       CLOSED
22665226 71762930 03 02 725729 4644 VISA       CLOSED
81657307 82561732 05 30 441621 0983 MONEY ORD  CLOSED
01067102 83817646 12 20 313424 1056 VISA       CLOSED
56426714 47947039 03 11 542271 5410 MONEY ORD  CLOSED
72249621 27945702 01 01 494330 1150 VISA       CLOSED
67786355 71142682 11 24 738500 8448 CHECK      CLOSED
70179007 05398625 12 26 764547 0780 MASTERCARD CLOSED
03680549 54793504 03 11 713050 0258 VISA       CLOSED
96501142 99951450 10 22 989553 4708 MASTERCARD CLOSED
03140780 46547823 09 29 705058 6296 MASTERCARD CLOSED
27983613 99706089 07 17 437359 1397 MASTERCARD OPEN
74556268 99951450 03 30 761187 2327 MONEY ORD  CLOSED
94950551 72363068 03 27 863063 1793 CHECK      CLOSED
49475060 05398625 10 30 509354 1848 MASTERCARD CLOSED
53962705 90249485 02 17 397010 6369 MONEY ORD  CLOSED
99018487 27945702 07 01 817149 2464 VISA       OPEN
27981071 43465338 12 03 214556 1302 VISA       CLOSED
93416785 99951450 10 10 207173 1062 CHECK      CLOSED
82497400 29636844 08 25 022869 0448 MONEY ORD  CLOSED
25873624 61051086 09 11 856805 4905 MONEY ORD  CLOSED
55886100 54793504 08 20 656334 0641 CHECK      CLOSED
58099938 24887287 02 06 060848 3286 MASTERCARD CLOSED
56832898 27945702 01 12 968639 2632 MONEY ORD  CLOSED
44602454 99184972 03 06 905585 0497 CHECK      CLOSED
65328555 99706089 01 19 587146 1859 MONEY ORD  CLOSED
18520480 47068534 07 04 199434 4159 CHECK      OPEN
50506140 71762930 03 13 896325 6030 CHECK      CLOSED
57041858 43425673 05 14 227399 1098 VISA       CLOSED
35442566 83817646 08 08 604047 3161 VISA       CLOSED
68965957 99184972 04 27 523576 4696 MASTERCARD CLOSED
02454766 14939103 12 08 113030 0101 CHECK      CLOSED
03621570 19509311 01 16 547660 5049 CHECK      CLOSED
61136812 62378290 02 12 340967 4828 MASTERCARD CLOSED
21505972 90249485 03 22 661697 8137 MONEY ORD  CLOSED
94555674 71762930 07 02 958419 3414 MASTERCARD CLOSED
21149602 46547823 06 11 872119 1826 MASTERCARD CLOSED
65144297 82561732 09 04 895135 2950 VISA       CLOSED
36630247 24887287 07 11 728674 5379 MASTERCARD OPEN
37252367 24887287 02 24 965709 0752 MONEY ORD  CLOSED
25199224 27945702 02 03 644658 1991 MONEY ORD  CLOSED
56355493 82561732 05 26 305082 1643 MASTERCARD CLOSED
29830097 61051086 06 30 748660 0843 VISA       CLOSED
09141843 27945702 12 24 060610 3641 CHECK      CLOSED
17440882 90249485 04 19 643547 0773 CHECK      CLOSED
11899663 99706089 05 13 686973 2426 MASTERCARD CLOSED
66666030 83817646 09 24 969459 4092 MASTERCARD CLOSED
61123090 99706089 06 19 207680 4855 VISA       CLOSED
92646523 62378290 04 02 287799 0250 CHECK      CLOSED
97972181 47068534 09 11 926353 1390 CHECK      CLOSED
58971039 14939103 07 18 394238 0323 MASTERCARD OPEN
64466890 27945702 07 11 515801 4127 MASTERCARD CLOSED
10137538 99706089 08 08 383483 1393 MONEY ORD  CLOSED
89984191 46547823 07 09 080282 2409 CHECK      CLOSED
46741877 43425673 08 10 473166 2139 MASTERCARD CLOSED
36259462 62378290 03 21 977236 1941 VISA       CLOSED
12484124 20366910 06 05 347512 3267 CHECK      OPEN
49793699 54793504 03 05 271958 5200 VISA       CLOSED
17947093 29636844 11 28 067784 4277 MONEY ORD  CLOSED
90539988 24887287 10 08 024295 3529 CHECK      CLOSED
27584890 58436555 01 15 631888 0071 CHECK      CLOSED
66405712 25103262 05 01 658977 4997 VISA       CLOSED
24872635 19509311 10 22 936165 1701 CHECK      CLOSED
65104255 72363068 09 13 785892 0777 MONEY ORD  CLOSED
08879047 29636844 06 24 502837 4379 MONEY ORD  OPEN
56602582 71762930 09 29 344360 2800 MONEY ORD  CLOSED
30550182 27945702 07 12 304947 4990 VISA       CLOSED
15722041 47068534 04 01 122371 2362 VISA       CLOSED
80807508 71762930 01 07 532408 4576 MONEY ORD  CLOSED
47864394 58436555 03 29 185522 5899 MASTERCARD CLOSED
80394410 99706089 04 26 669881 0489 MONEY ORD  CLOSED
28859946 58436555 10 02 335338 6022 VISA       CLOSED
47144984 43425673 09 09 582453 4198 CHECK      CLOSED
03948574 71142682 08 11 818083 1408 MONEY ORD  CLOSED
68062304 20366910 01 04 974606 0351 MONEY ORD  CLOSED
30152742 90249485 04 07 937721 2012 CHECK      CLOSED
67416468 71762930 07 20 232881 0911 MONEY ORD  CLOSED
02221971 47068534 11 07 441174 1801 MONEY ORD  CLOSED
49624650 25103262 11 01 824398 1202 MONEY ORD  CLOSED
54288644 58436555 09 23 906113 2244 MASTERCARD CLOSED
03443334 99143393 04 17 954573 0042 MONEY ORD  CLOSED
51101319 90249485 02 12 391177 0218 MASTERCARD CLOSED
86988355 99143393 03 12 312093 0872 VISA       CLOSED
99171541 71762930 12 26 109121 0725 CHECK      CLOSED
82950997 38675976 08 25 528325 0421 VISA       CLOSED
64348640 71762930 10 30 715749 0656 CHECK      CLOSED
22662323 14939103 08 14 133322 0232 MONEY ORD  CLOSED
34959403 99706089 02 05 766102 1297 VISA       CLOSED
02003157 80626987 11 11 172491 1088 VISA       CLOSED
56892718 54793504 08 27 782714 3331 MONEY ORD  CLOSED
00541070 71762930 10 17 397743 0512 VISA       CLOSED
73546089 71142682 07 19 071254 1127 CHECK      CLOSED
92864804 43425673 12 19 392312 4902 MASTERCARD CLOSED
12275371 46547823 08 30 662795 5564 CHECK      CLOSED
69213871 47068534 10 21 034337 9023 CHECK      CLOSED
29908632 46547823 07 16 487058 0075 MONEY ORD  OPEN
64681845 81535346 03 26 733307 6297 MONEY ORD  CLOSED
48571321 27945702 10 06 367116 2272 MONEY ORD  CLOSED
68013697 54793504 11 24 969864 1416 VISA       CLOSED
06576323 80626987 02 08 948964 3214 MASTERCARD CLOSED
50448670 90249485 03 21 366858 0866 MASTERCARD CLOSED
83231803 29636844 08 01 751469 2210 VISA       CLOSED
10761804 99951450 12 14 045980 7439 VISA       CLOSED
92605962 83817646 12 30 931515 0416 VISA       CLOSED
17937690 29636844 07 19 247404 3537 VISA       OPEN
66244660 54793504 02 10 193999 0136 CHECK      CLOSED
83775811 19509311 05 06 106069 1378 MASTERCARD CLOSED
28384004 99143393 10 18 313684 6005 MASTERCARD CLOSED
59415734 99184972 12 26 213699 0468 VISA       CLOSED
74396219 62378290 10 25 811046 3925 CHECK      CLOSED
68870570 99143393 05 30 973332 1442 MONEY ORD  CLOSED
40631591 38675976 02 09 168463 2790 MONEY ORD  CLOSED
80246922 83817646 06 16 673325 1117 MASTERCARD CLOSED
35406073 38675976 05 16 209679 0132 CHECK      CLOSED
25449141 71762930 04 16 631550 0282 VISA       CLOSED
59686941 29636844 01 15 883309 7080 MASTERCARD CLOSED
69074831 61051086 03 10 730256 4094 CHECK      CLOSED
45546796 20366910 09 05 497664 6255 CHECK      CLOSED
47117589 80626987 03 15 529409 2554 VISA       CLOSED
22479402 61051086 09 08 832084 0273 VISA       CLOSED
58841704 29636844 11 16 214276 2933 MONEY ORD  CLOSED
58097745 61051086 09 05 318244 1692 MONEY ORD  CLOSED
09568477 99184972 01 03 977497 1679 VISA       CLOSED
43399670 58436555 08 13 108085 5063 MONEY ORD  CLOSED
35734296 14939103 10 16 596286 5339 CHECK      CLOSED
57967786 99706089 05 14 671894 0375 CHECK      CLOSED
91455666 20366910 07 10 863322 1299 CHECK      CLOSED
21042890 99951450 10 16 963359 0083 VISA       CLOSED
06752671 27945702 04 14 715840 0060 MASTERCARD CLOSED
64553401 71762930 09 16 066836 0556 VISA       CLOSED
07537237 29636844 03 26 375930 3365 MASTERCARD CLOSED
72530825 80626987 01 03 425909 7333 CHECK      CLOSED
91424346 80626987 06 20 831693 3242 MONEY ORD  OPEN
85559730 43465338 12 06 718428 1549 MASTERCARD CLOSED
07076759 61051086 09 28 472415 1470 CHECK      CLOSED
88029978 99143393 02 03 096626 0027 MONEY ORD  CLOSED
95559243 80626987 03 07 484996 4259 CHECK      CLOSED
70368413 19509311 02 09 990168 3457 MONEY ORD  CLOSED
20636842 47947039 02 19 909500 3891 MONEY ORD  CLOSED
95649692 24887287 11 24 518914 2402 MASTERCARD CLOSED
56371269 29636844 04 02 268823 2719 MASTERCARD CLOSED
00547667 14939103 09 18 931187 2944 CHECK      CLOSED
73280347 61051086 12 13 516604 2473 CHECK      CLOSED
32903625 38675976 07 17 637452 1309 CHECK      CLOSED
20444642 81535346 03 02 814585 5172 MONEY ORD  CLOSED
50854422 47068534 02 23 619437 0654 VISA       CLOSED
74938345 25103262 08 03 035915 7856 CHECK      CLOSED
14978119 82561732 10 27 157193 3735 CHECK      CLOSED
76861349 61051086 12 24 862597 2916 MONEY ORD  CLOSED
31447220 27945702 06 19 334480 0384 CHECK      CLOSED
73420565 71142682 11 18 824553 0590 CHECK      CLOSED
19567743 72363068 02 05 793133 2408 CHECK      CLOSED
53900721 43425673 10 11 861556 3177 VISA       CLOSED
92741547 43425673 08 30 153727 0871 CHECK      CLOSED
54240413 54793504 07 14 446126 3562 MONEY ORD  OPEN
43645014 81535346 01 16 695074 1808 CHECK      CLOSED
88202242 46547823 04 22 012277 0481 CHECK      CLOSED
71765483 71762930 04 28 678964 0542 MASTERCARD CLOSED
56652860 99706089 04 28 989441 7209 MONEY ORD  CLOSED
55246494 20366910 06 25 570763 0579 MONEY ORD  CLOSED
11120210 99184972 11 18 646410 4116 VISA       CLOSED
92910983 47068534 02 15 175264 0233 CHECK      CLOSED
88322931 62378290 07 02 506857 4655 MONEY ORD  CLOSED
20380948 58436555 09 14 784497 6402 MONEY ORD  CLOSED
25745738 38675976 06 06 068312 4440 MONEY ORD  CLOSED
66831465 47068534 03 13 899679 0975 CHECK      CLOSED
11243536 99951450 01 19 860228 0167 MASTERCARD CLOSED
00506179 43425673 02 08 537436 0902 VISA       CLOSED
75612395 72363068 02 17 497186 2018 MONEY ORD  CLOSED
27862384 99706089 01 23 375742 3151 MONEY ORD  CLOSED
36425837 83817646 03 02 764652 5462 VISA       CLOSED
42615547 99143393 07 04 954201 1300 MONEY ORD  OPEN
66184775 24887287 12 15 179887 2130 MASTERCARD CLOSED
32990296 19509311 09 13 677057 4070 MONEY ORD  CLOSED
89818414 24887287 06 05 423283 4487 MASTERCARD OPEN
87135534 38675976 02 16 232474 1251 VISA       CLOSED
14744798 72363068 12 14 336138 7014 MASTERCARD CLOSED
20091469 25103262 12 01 456401 4720 CHECK      CLOSED
21626699 47947039 02 10 258483 9232 VISA       CLOSED
78049609 71762930 05 11 378566 3579 VISA       CLOSED
72604465 99706089 11 12 482781 3665 MASTERCARD CLOSED
94381348 61051086 09 14 283966 3518 CHECK      CLOSED
70837104 81535346 01 23 729219 9223 VISA       CLOSED
60469941 27945702 04 10 432037 2410 CHECK      CLOSED
21412814 46547823 04 12 419881 8959 CHECK      CLOSED
37155603 29636844 01 02 423015 0983 MONEY ORD  CLOSED
57980020 19509311 07 07 099816 0016 MONEY ORD  OPEN
82419390 43465338 03 01 273011 1494 CHECK      CLOSED
28106006 99143393 08 05 560706 3079 CHECK      CLOSED
81642161 62378290 10 21 220856 2015 MONEY ORD  CLOSED
66027612 58436555 08 01 102094 4777 MASTERCARD CLOSED
09988260 61051086 08 20 315013 0688 VISA       CLOSED
23809722 47947039 07 01 991365 4095 VISA       CLOSED
28296706 25103262 02 22 203537 0327 VISA       CLOSED
12382525 05398625 10 19 458758 9122 MONEY ORD  CLOSED
81359343 58436555 10 27 075156 7129 MASTERCARD CLOSED
16922415 25103262 07 07 059754 2207 MONEY ORD  OPEN
88245327 46547823 12 22 808380 4061 MONEY ORD  CLOSED
80614589 27945702 07 13 500098 3846 MONEY ORD  CLOSED
72484917 47947039 04 23 223864 0704 CHECK      CLOSED
40495677 62378290 02 18 014448 0825 MONEY ORD  CLOSED
54990197 14939103 10 15 812796 3807 MONEY ORD  CLOSED
32437397 81535346 08 23 410519 1220 CHECK      CLOSED
17867215 71762930 08 10 388477 0640 MASTERCARD CLOSED
18879656 25103262 11 26 115196 7361 MASTERCARD CLOSED
19713468 47947039 03 20 948200 2560 MASTERCARD CLOSED
76836982 05398625 11 26 319438 7215 MASTERCARD CLOSED
71676830 47068534 08 17 077522 1138 MONEY ORD  CLOSED
75238594 05398625 10 05 272610 3743 VISA       CLOSED
79657431 25103262 10 08 851211 3223 MONEY ORD  CLOSED
99316973 83817646 06 01 013000 0751 CHECK      CLOSED
46333791 62378290 06 21 148184 2237 MONEY ORD  CLOSED
17502313 61051086 10 22 745447 0205 MASTERCARD CLOSED
92623943 83817646 02 09 833096 2980 MASTERCARD CLOSED
10997309 72363068 01 20 956326 5374 CHECK      CLOSED
74396138 72363068 02 14 461049 0069 MASTERCARD CLOSED
66014574 99951450 01 05 846231 2406 CHECK      CLOSED
76984710 19509311 02 08 928688 0124 MASTERCARD CLOSED
89426016 81535346 01 15 237611 3188 MASTERCARD CLOSED
54593386 47947039 09 11 485948 4714 MASTERCARD CLOSED
36463298 99706089 06 12 320313 0782 CHECK      CLOSED
89364721 71142682 07 04 606398 0445 MASTERCARD OPEN
33756671 62378290 07 13 744533 2160 VISA       CLOSED
50241087 61051086 10 16 838922 7615 MASTERCARD CLOSED
45843907 43425673 03 28 060039 1724 MONEY ORD  CLOSED
88867446 62378290 04 19 711757 3358 MASTERCARD CLOSED
19591129 61051086 10 24 661908 5522 MASTERCARD CLOSED
56009287 82561732 05 03 151878 3183 CHECK      CLOSED
56408158 05398625 12 26 732330 0811 VISA       CLOSED
27228603 99706089 01 20 863520 3868 VISA       CLOSED
55199959 83817646 01 07 511781 1648 MONEY ORD  CLOSED
10182892 99143393 11 07 290328 6171 CHECK      CLOSED
81699374 14939103 01 29 599639 4150 MASTERCARD CLOSED
83735884 05398625 03 11 537588 0462 MASTERCARD CLOSED
87844676 81535346 12 05 751243 5033 MONEY ORD  CLOSED
23563604 99184972 02 19 116050 0262 MASTERCARD CLOSED
67741757 27945702 02 15 531388 1878 VISA       CLOSED
94440113 99951450 12 30 477435 3897 CHECK      CLOSED
78758267 82561732 01 13 866274 3701 VISA       CLOSED
66631664 38675976 02 15 972852 0659 CHECK      CLOSED
81143904 24887287 09 18 266048 0765 CHECK      CLOSED
84021420 19509311 09 18 669788 0257 VISA       CLOSED
29610048 58436555 07 13 328302 7684 CHECK      OPEN
55462437 71762930 09 05 577111 3171 CHECK      CLOSED
98759207 05398625 04 19 892894 1093 VISA       CLOSED
77540077 99951450 05 14 231213 7383 CHECK      CLOSED
49031233 29636844 01 28 704461 1483 VISA       CLOSED
47778937 61051086 05 12 895251 0316 CHECK      CLOSED
68850115 05398625 02 16 679006 0503 MONEY ORD  CLOSED
57158190 82561732 08 18 928371 4745 CHECK      CLOSED
93242702 38675976 02 07 583067 3819 CHECK      CLOSED
87978121 19509311 10 04 893305 5338 MONEY ORD  CLOSED
40640249 83817646 05 04 498706 2370 CHECK      CLOSED
86295196 47068534 06 19 753525 3090 MONEY ORD  CLOSED
35573368 29636844 03 22 099654 2289 VISA       CLOSED
78736651 99706089 09 12 329303 4503 CHECK      CLOSED
84854464 43425673 06 21 785150 1115 VISA       CLOSED
95804354 29636844 11 08 457578 0911 CHECK      CLOSED
51968233 61051086 03 20 193672 0835 MONEY ORD  CLOSED
22813532 90249485 04 24 423003 4456 MONEY ORD  CLOSED
23217947 47068534 05 29 913824 7198 VISA       CLOSED
01515953 72363068 08 08 635418 0962 VISA       CLOSED
01727815 27945702 04 13 601459 4348 MONEY ORD  CLOSED
06175828 24887287 09 15 243624 5656 CHECK      CLOSED
12410930 19509311 11 12 735286 0202 MONEY ORD  CLOSED
40588531 25103262 04 09 635596 5157 MASTERCARD CLOSED
08642177 19509311 03 08 866852 1886 MASTERCARD CLOSED
81072490 83817646 05 15 431103 6315 VISA       CLOSED
06858101 83817646 11 27 565245 3253 CHECK      CLOSED
00363298 14939103 03 25 223984 0327 MONEY ORD  CLOSED
15121077 47068534 12 03 933211 1746 MASTERCARD CLOSED
00393290 14939103 04 21 970608 6447 CHECK      CLOSED
*
01 0017755379 0017705570 0017805188
02 0017642145 0017629235 0017655055
03 0019861223 0019739603 0019982842
04 0015350665 0015250921 0015450408
05 0012660722 0012630886 0012690559
06 0010590482 0010574060 0010606905
07 0014358739 0014339568 0014377909
08 0012162453 0012145469 0012179437
09 0017274423 0017260912 0017287934
10 0017580581 0017526081 0017635082
11 0013130073 0013092533 0013167613
12 0013581599 0013562113 0013601084
/*
//