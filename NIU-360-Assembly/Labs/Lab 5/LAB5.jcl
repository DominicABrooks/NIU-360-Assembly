//KC03ABDA JOB ,'DOMINIC BROOKS',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
* CSCI 360-1            LAB EXERCISE 5               SPRING 2022 *
*                                                                *
* NAME:  DOMINIC BROOKS                                          *
* DATE:  11/11/22                                                *
*                                                                *
* INTERNAL SUBROUTINES AND EXTERNAL SUBPROGRAMS                  *
*                                                                *
* REGISTER USAGE:                                                *
* R2 - HOLDS PARAM 1                                             *
* R3 - HOLDS PARAM 2                                             *
* R4 - HOLDS TMP ADDRESS TO STORE IN  FROM PARAMLIST & PARAM 3   *
*                                                                *
******************************************************************
*
$TABLE   DSECT               TABLE TO STORE THE SEQ
$NUM1    DC    PL4'0'        FIRST NUM ROW
$NUM2    DC    PL4'0'        SECOND NUM ROW
$NUM3    DC    PL4'0'        THIRD NUM ROW
MAIN     CSECT
* ==== ENTRY LINKAGE ===========
*      Back up caller's register state
         STM   14,12,12(13)   SAVE REGS IN CALLER'S SAVE AREA
*      Establish local addressability
         LR    12,15          COPY CSECT ADDRESS INTO R12
         USING MAIN,12        ESTABLISH R12 AS THE BASE REG
*      Store backwards, forwards pointers
         LA    14,MAINSAVE    R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    13,4(,14)      STORE ADDRESS OF CALLER'S SAVE AREA
         ST    14,8(,13)      STORE ADDRESS OF THIS CSECT'S SAVE AREA
*      Preemptively point r13 at local save area
         LR    13,14          POINT R13 AT THIS CSECT'S SAVE AREA
*
* EXERCISE 1
         XREAD BUFFER,80            READ NEXT LINE FROM BUFFER
         PACK  Q12SQR(2),BUFFER(2)  PACK 2 BYTES INTO PMULT
         LA    1,=A(Q12SQR,Q1SQRD)   POINT R1 AT PARMLIST
         BAL   11,INSQUARE           BRANCH AND LINK TO SUBRTN
         XDUMP Q12SQR,6
* EXERCISE 2
         XREAD BUFFER,80            READ NEXT LINE FROM BUFFER
         LA    1,=A(BUFFER,Q2PSEQ)  POINT R1 AT PARMLIST
         BAL   11,LOADPSEQ          BRANCH AND LINK TO SUBRTN
         XDUMP Q2PSEQ,64
* EXERCISE 3
         LA    2,Q2PSEQ          LOAD SEQ IN R2
         LA    3,Q2PSEQ          LOAD SEQ IN R3
         LA    3,64(3)           PUT END OF SEQ IN R3
SQRSEQ   DS    0H                START OF SEQ SQR LOOP
         LA    1,=A(Q2PSEQ)      R1 -> PARAMETER
         L     15,=V(EXSQUARE)   LOAD ADDRESS OF SUBPROGRAM INTO R15
         BALR  14,15             BRANCH AND LINK TO SUBPROGRAM
         LA    2,8(2)            LOADS NEXT INT INTO SEQ
         CR    2,3               IF AT END
         BE    EOLOOP            EXIT LOOP
         B     SQRSEQ            ELSE BACK TO TOP OF LOOP
EOLOOP   DS    0H                END OF LOOP
         XDUMP Q2PSEQ,64         DUMP THE SEQ
* EXERCISE 4
         LA    1,=A(BUFFER,TSSTABL) LOAD ADDRESS OF BUFFER AND TABLE
         L     15,=V(LOADDATA)   LOAD ADDRESS OF SUBPROGRAM INTO R15
         BALR  14,15             BRANCH AND LINK TO SUBPROGRAM
         XDUMP TSSTABL,60        DUMP TABLE
* EXERCISE 5
         LA    2,TSSTABL         LOAD BEGINNING OF TABLE
         LA    3,120(2)          LOAD END       OF TABLE
         LA    4,TSS             LOAD TSS
         L     15,=V(TSUMSQRS)   LOAD ADDRESS OF SUBPROGRAM INTO R15
         BALR  14,15             BRANCH AND LINK TO SUBPROGRAM
         XDUMP TSSTABL,60        DUMP TABLE
         XDUMP TSS,8             DUMP TSS
*
* ==== EXIT LINKAGE ============
PRGEXIT  LA    15,0           R15 = RETURN CODE OF 0
         L     13,4(,13)      POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)     RESTORE REGISTER 14
         LM    0,12,20(13)    RESTORE R0 THRU R12
         BR    14             RETURN TO CALLER
* (vv Program storage vv)
*
         LTORG                LITERAL ORGANIZATION
*
BUFFER   DS    CL80           BUFFER
TSS      DC    PL8'0'         SQUARED SUM
Q12SQR   DC    PL2'0'         OUTPUT SEQ
Q1SQRD   DC    PL4'0'         INPUT  SEQ
MAINSAVE DC    18F'-1'
Q2PSEQ   DC    PL8'0,0,0,0,0,0,0,0' EXER 3 TABLE
TSSTABL  DC    10PL12'0'
*
******************************************************************
*                                                                *
* INSQUARE SUBROUTINE                                            *
*                                                                *
* PARAMLIST:                                                     *
*   R2: (PL2) INPUT VAL                                          *
*   R3: (PL4) OUTPUT VAL                                         *
*                                                                *
* REGISTER USAGE:                                                *
*   R2 - STORE INPUT  VALUE                                      *
*   R3 - STORE OUTPUT VALUE                                      *
*                                                                *
******************************************************************
INSQUARE STM   2,3,INSAVE
         LM    2,3,0(1)        LOAD PARAMER ADDRESES INTO R2,3
*
         ZAP   0(4,3),0(2,2)   STORE INPUT INTO OUTPUT VAL
         MP    0(4,3),0(2,2)   MULTIPLY THE TWO TOGETHER
*
         LM    2,3,INSAVE     RESTORE R2,3
         BR    11
         LTORG             LITERAL ORGANIZATION
*
INSAVE   DS    2F
*
******************************************************************
*                                                                *
* LOADPSEQ SUBROUTINE                                            *
*                                                                *
* PARAMLIST:                                                     *
*   R2: (80F) BUFFER                                             *
*   R3: (8PL8) SEQ                                               *
*                                                                *
* REGISTER USAGE:                                                *
*   R2 - HOLDS BUFFER                                            *
*   R3 - HOLDS SEQUENCE                                          *
*                                                                *
******************************************************************
LOADPSEQ STM   2,3,LDSAVE
         LM    2,3,0(1)        LOAD PARAMER ADDRESES INTO R2,3
*
         PACK  0(8,3),0(6,2)          LOAD INT 1
         LA    2,7(2)                 LOAD NEXT INT TO BUFFER
         LA    3,8(3)                 LOAD NEXT INT TO SEQ
         PACK  0(8,3),0(6,2)          LOAD INT 2
         LA    2,7(2)                 LOAD NEXT INT TO BUFFER
         LA    3,8(3)                 LOAD NEXT INT TO SEQ
         PACK  0(8,3),0(6,2)          LOAD INT 3
         LA    2,7(2)                 LOAD NEXT INT TO BUFFER
         LA    3,8(3)                 LOAD NEXT INT TO SEQ
         PACK  0(8,3),0(6,2)          LOAD INT 4
         LA    2,7(2)                 LOAD NEXT INT TO BUFFER
         LA    3,8(3)                 LOAD NEXT INT TO SEQ
         PACK  0(8,3),0(6,2)          LOAD INT 5
         LA    2,7(2)                 LOAD NEXT INT TO BUFFER
         LA    3,8(3)                 LOAD NEXT INT TO SEQ
         PACK  0(8,3),0(6,2)          LOAD INT 6
         LA    2,7(2)                 LOAD NEXT INT TO BUFFER
         LA    3,8(3)                 LOAD NEXT INT TO SEQ
         PACK  0(8,3),0(6,2)          LOAD INT 7
         LA    2,7(2)                 LOAD NEXT INT TO BUFFER
         LA    3,8(3)                 LOAD NEXT INT TO SEQ
         PACK  0(8,3),0(6,2)          LOAD INT 8
         LA    2,7(2)                 LOAD NEXT INT TO BUFFER
         LA    3,8(3)                 LOAD NEXT INT TO SEQ
*
         LM    2,3,LDSAVE             RESTORE R2,3
         BR    11
         LTORG             LITERAL ORGANIZATION
*
LDSAVE   DS    2F
*
******************************************************************
*                                                                *
* EXTERNAL SUBPROGRAM: EXSQUARE                                  *
*                                                                *
* PARAMLIST:                                                     *
*   R2: (PL8) NUM TO BE SQRD                                     *
*                                                                *
* REGISTER USAGE:                                                *
*   R2 - HOLDS NUMBER TO SQUARE                                  *
*                                                                *
******************************************************************
EXSQUARE CSECT
         STM   14,12,12(13)   SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15          COPY CSECT ADDRESS INTO R12
         USING EXSQUARE,12    ESTABLISH R12 AS THE BASE REG
         LA    14,EXSAVE      R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)      STORE ADDRESS OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)      STORE ADDRESS OF CALLER'S SAVE AREA
         LR    13,14          POINT R13 AT THIS CSECT'S SAVE AREA
*
*  DEREFERENCE PARAMETER LIST
*
         ZAP   EXSTMP,0(8,2)
         MP    0(8,2),EXSTMP
*
*  RESTORE ALL OF THE CALLER'S REGISTERS AND RETURN
*
         SR    15,15          R15 = RETURN CODE OF 0
         L     13,4(,13)      POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)     RESTORE REGISTER 14
         LM    0,12,20(13)    RESTORE R0 THRU R12
         BR    14             RETURN TO CALLER
*
         LTORG                LITERAL ORGANIZATION
*
EXSAVE   DC    18F'-1'     SAVE AREA
EXSTMP   DC    PL4'0'      TEMP
*
******************************************************************
*                                                                *
* EXTERNAL SUBPROGRAM: LOADDATA                                  *
*                                                                *
* PARAMLIST:                                                     *
*   R2: (80F) BUFFER                                             *
*   R3: (15PL3) TSSTABLE                                         *
*                                                                *
* REGISTER USAGE:                                                *
*   R2 - HOLD BUFFER                                             *
*   R3 - HOLD TABLE ADDR                                         *
*   R4 - HOLD ADDR TEMP TO PUT IN VARS                           *
*                                                                *
******************************************************************
LOADDATA CSECT
         STM   14,12,12(13)   SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15          COPY CSECT ADDRESS INTO R12
         USING LOADDATA,12    ESTABLISH R12 AS THE BASE REG
         LA    14,ELSAVE      R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)      STORE ADDRESS OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)      STORE ADDRESS OF CALLER'S SAVE AREA
         LR    13,14          POINT R13 AT THIS CSECT'S SAVE AREA
*
*  DEREFERENCE PARAMETER LIST
*
         USING $TABLE,3           LOAD DSECT INTO R3
         LM    2,3,0(1)           LOAD PARAMS INTO R2 AND R3
LOOP     XREAD 0(2),80            READ 80 BYTES
         BNZ   ENDOLOOP           BRANCH IF AT END
*
         LA    4,0(2)             LOAD BUFFER ADDR INTO R4
         ST    4,TAB              STORE IN TRAIL
*
         LA    4,80(2)            LOAD EOBUFFER ADDR INTO R4
         ST    4,EOTAB            STORE IN TAB
*
         LA    1,PARMS            LOAD PARMS INTO R1
         L     15,=V(PARSEDEC)    LOAD ADDRESS OF SUBPROGRAM INTO R15
         BALR  14,15              BRANCH AND LINK TO SUBPROGRAM
*
         ZAP   $NUM1(4),ELSTMP(8) STORE IN TABLE
*
         LA    4,8(2)             LOAD NEXT INTO TRAIL
         ST    4,TAB
*
         LA    4,80(2)            LOAD END INTO TRAIL
         ST    4,EOTAB
*
         LA    1,PARMS            LOAD PARMS INTO R1
         L     15,=V(PARSEDEC)    LOAD ADDRESS OF SUBPROGRAM INTO R15
         BALR  14,15              BRANCH AND LINK TO SUBPROGRAM
*
         ZAP   $NUM2(4),ELSTMP(8) STORE IN 2ND COLUMN
         LA    3,12(3)            MOVE TO NEXT PRT OF TABLE
*
         B     LOOP               LOOP TO TOP
ENDOLOOP DS    0H                 END OF LOOP
*
*
*  RESTORE ALL OF THE CALLER'S REGISTERS AND RETURN
*
         SR    15,15          R15 = RETURN CODE OF 0
         L     13,4(,13)      POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)     RESTORE REGISTER 14
         LM    0,12,20(13)    RESTORE R0 THRU R12
         BR    14             RETURN TO CALLER
*
         LTORG                LITERAL ORGANIZATION
*
ELSAVE   DC    18F'-1'     SAVE AREA
IMPDP    DS    F           IMPLIED DECIMALS
ELSTMP   DC    PL8'0'      TEMPORARY
PARMS    DS    0H          PARAMETERS
TAB      DS    F           TABLE
EOTAB    DS    F           END OF TABLE
         DC    A(ELSTMP) RESULT
         DC    A(TAB)    TRAIL ADDR
         DC    A(IMPDP)  # OF IMPLIED DEC PLACES
*
*
******************************************************************
*                                                                *
* EXTERNAL SUBPROGRAM: TSUMSQRS                                  *
*                                                                *
* PARAMLIST:                                                     *
* REGISTER USAGE:                                                *
******************************************************************
TSUMSQRS CSECT
         STM   14,12,12(13)   SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15          COPY CSECT ADDRESS INTO R12
         USING TSUMSQRS,12    ESTABLISH R12 AS THE BASE REG
         LA    14,TSSAVE      R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)      STORE ADDRESS OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)      STORE ADDRESS OF CALLER'S SAVE AREA
         LR    13,14          POINT R13 AT THIS CSECT'S SAVE AREA
*
*  DEREFERENCE PARAMETER LIST
*
         USING $TABLE,3
         L     3,0(2)
*
*  RESTORE ALL OF THE CALLER'S REGISTERS AND RETURN
*
         SR    15,15          R15 = RETURN CODE OF 0
         L     13,4(,13)      POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)     RESTORE REGISTER 14
         LM    0,12,20(13)    RESTORE R0 THRU R12
         BR    14             RETURN TO CALLER
*
         LTORG                LITERAL ORGANIZATION
*
TSSAVE   DC    18F'-1'     SAVE AREA
TEMP     DC    8F'0'
*
******************************************************************
*                                                                *
* DECIMAL PARSING EXTERNAL SUBPROGRAM                            *
*                                                                *
* TAKES AN EBCDIC REPRESENTATION OF A DECIMAL VALUE AND CONVERTS *
* IT TO A PACKED DECIMAL REPRESENTATION. THE PROVIDED START AND  *
* TRAILING ADDRESSES DETERMINE THE REGION OF STORAGE THAT WILL   *
* BE SCANNED, WITH THE TRAILING ADDRESS 1 POS PAST THE END. SETS *
* THE SIGN DIGIT OF THE RESULTING PACKED DECIMAL IF A PRECEDING  *
* SIGN CHARACTER ACCOMPANIES THE NUMERIC CHARACTERS. IF NO SIGN  *
* CHARACTER IS FOUND ASSUMES A POSITIVE QUANTITY. COUNTS AND     *
* RETURNS THE NUMBER OF IMPLIED DECIMAL PLACES, WHICH WILL BE    *
* BE NON-ZERO IF A DECIMAL CHARACTER IS FOUND.                   *
*                                                                *
* RETURN CODES:                                                  *
*   0 - PARSE WAS SUCCESSFUL                                     *
*   1 - PARSE WAS UNSUCCESSFUL                                   *
*                                                                *
*   NOTE: IF PARSE WAS UNSUCCESSFUL THEN RETURN FIELD WILL NOT   *
*         CONTAIN A VALID PACKED DECIMAL, BUT WILL CONTAIN PREV  *
*         DATA WITH THE LAST BYTE OVERWRITTEN WITH X'00'         *
*         (GUARANTEEING IT WILL NOT CONTAIN A VALID PACKED DEC)  *
*                                                                *
* PARSING WILL FAIL IN THE FOLLOWING CASES:                      *
*   1.) THE SUBPROGRAM ATTEMPTED TO PARSE AN INVALID CHARACTER   *
*       (I.E., NOT C'+', C'-', C'.' OR NUMERIC)                  *
*   2.) NO NUMERIC CHARACTERS WERE FOUND                         *
*   3.) NO TRAILING SPACE CHARACTER WAS FOUND                    *
*   4.) THE NUMBER BEING PARSED CONTAINED MORE THAN 15 DIGITS    *
*                                                                *
* TODO: IMPLEMENT PARSE END CONSTRAINTS (STOP AT BUFFER END,     *
*       > 15 SOURCE DIGITS)                                      *
*                                                                *
* REGISTER USAGE:                                                *
*   2 - (IN) PTR TO BUFFERING LOC BEGIN ADDR (F)                 *
*   3 - (IN) PTR TO BUFFERING LOC TRAILING ADDR (F)              *
*   4 - (OUT) PTR TO PACKED DECIMAL'S RECEIVING FIELD (PL8)      *
*   5 - (OUT) ADDRESS OF FIRST TRAILING SPACE IN BUFFER (F)      *
*   6 - (OUT) IMPLIED DECIMALS (F)                               *
*   7 - BUFFER READ PTR                                          *
*   8 - COPY FIELD WRITE PTR                                     *
*   9 - COUNTER, IMPLIED DECIMAL PLACES                          *
*                                                                *
******************************************************************
PARSEDEC CSECT
* ==== Entry linkage ===========
*      Back up caller's register state
         STM   14,12,12(13)   SAVE REGS IN CALLER'S SAVE AREA
*      Establish local addressability
         LR    12,15          COPY CSECT ADDRESS INTO R12
         USING PARSEDEC,12    ESTABLISH R12 AS THE BASE REG
*      Store backwards, forwards pointers
         LA    14,PARSSAVE    R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    13,4(,14)      STORE ADDRESS OF CALLER'S SAVE AREA
         ST    14,8(,13)      STORE ADDRESS OF THIS CSECT'S SAVE AREA
*      Preemptively point r13 at local save area
         LR    13,14          POINT R13 AT THIS CSECT'S SAVE AREA
*
         LM    2,6,0(1)       R2 -> PARSE START ADDR
*                             R3 -> PARSE END ADDR
*                             R4 -> PACKED RETURN FIELD
*                             R5 -> ADDR IN BUFFER, TRAILING SPACE
*                             R6 -> IMPLIED DECIMAL PLACES
*
         LM    2,6,0(1)       R2 -> PARSE START ADDR
*                             R3 -> PARSE END ADDR
*                             R4 -> PACKED RETURN FIELD
*                             R5 -> ADDR IN BUFFER, TRAILING SPACE
*                             R6 -> IMPLIED DECIMAL PLACES
*
* (RE)INITIALIZE STATE VARS
         MVI   7(4),X'00'     SET LAST BYTE TO ERROR CODE
         MVI   SCODE,X'00'    (RE)SET DEFAULT SIGN CODE
         MVI   DECFLAG,X'00'  (RE)SET DECIMAL FLAG
         LR    7,2            SET SCAN HEAD TO BUFFER START
         LA    9,0            RESET DECIMAL PLACE COUNTER
*
* FAST-FORWARD PAST LEADING SPACES
FFLOOP   CLI   0(7),C' '      TEST FOR LEADING SPACE
         BNE   TESTNEG        NON-SPACE CHAR FOUND, BEGIN PARSE
         LA    2,1(,2)        MOVE START ADDR FORWARD 1 POS
         LA    7,1(,7)        MOVE READ HEAD FORWARD 1 POS
         B     FFLOOP         TEST NEXT POS
*
** TEST FOR LEADING +/-
*
* TEST IF FIRST CHAR IS '-'
TESTNEG  CLI   0(7),C'-'      TEST FOR NEGATIVE SIGN
         BNE   TESTPOS        NO NEGATIVE SIGN, TEST FOR +
         MVI   SCODE,X'FF'    SET SIGN CODE BYTE TO -1
         LA    2,1(,7)        MOVE BUFFER START PAST SIGN
         B     UPDTSCHD       GO TO NEXT CHAR
*
* TEST IF FIRST CHAR IS '+'
TESTPOS  CLI   0(7),C'+'      TEST FOR POSITIVE SIGN
         BNE   SCANLOOP       NO POSITIVE SIGN, START SCANNING
         LA    2,1(,7)        MOVE BUFFER START PAST SIGN
         BE    UPDTSCHD       POS SIGN FOUND, GO TO NEXT CHAR
*
** SCAN FROM LEFT TO RIGHT FOR FIRST TRAILING SPACE **
*
* TEST AND HANDLE SPACE FOUND CASE
SCANLOOP CLI   0(7),C' '      TEST FOR SPACE CHAR
         BNE   TSTDEC         NOT A SPACE, TEST FOR DECIMAL CHAR
         CR    7,2            TEST OFFSET INTO BUFFER
         BNE   COPYPREP       FOUND TRAIL SPC, BEGIN NEXT STEPS
         LA    15,1           OTHERWISE SET R15 = 1 (PARSE ERROR)
         B     PARSEXIT       EXIT IMMEDIATELY
*
* TEST FOR DECIMAL CHARACTER
TSTDEC   CLI   0(7),C'.'      TEST FOR DECIMAL CHAR
         BNE   TSTCXLOW       NOT FOUND, TEST FOR NON-NUMERIC
         CLI   DECFLAG,X'01'  TEST IF FLAG ALREADY SET
         BNE   SETDFLAG       IF NOT THEN SET FLAG BYTE
         LA    15,1           OTHERWISE SET R15 = 1 (PARSE ERROR)
         B     PARSEXIT       EXIT IMMEDIATELY
SETDFLAG MVI   DECFLAG,X'01'  SET DECIMAL FLAG TO 1 (TRUE)
         BCTR  9,0            SET COUNT TO -1 (IGNORES DEC CHAR)
         B     UPDTSCHD       UPDATE AND CONTINUE
*
* TEST FOR NON-NUMERIC CHARACTERS
TSTCXLOW CLI   0(7),X'F0'     TEST IF < X'F0' (CHAR 0)
         BNL   TSTCXHI        IF NOT LOW THEN TEST IF HIGH
         LA    15,1           R15 = 1 (PARSE ERROR)
         B     PARSEXIT       OTHERWISE DO NOTHING AND EXIT
TSTCXHI  CLI   0(7),X'F9'     TEST IF > X'F9' (CHAR 9)
         BNH   UPDTSCHD       IF NOT HIGH THEN UPDATE AND CONTINUE
         LA    15,1           OTHERWISE R15 = 1 (PARSE ERROR)
         B     PARSEXIT       DO NOTHING AND EXIT
*
* UPDATE AND CONTINUE
UPDTSCHD LA    7,1(,7)        INCREMENT READ PTR
         CLI   DECFLAG,X'01'  TEST IF DECIMAL FLAG IS SET
         BNE   SCANLOOP       IF NOT THEN TEST NEXT CHAR
         LA    9,1(,9)        INCREMENT IMPLIED DEC COUNTER
         B     SCANLOOP       TEST NEXT CHAR
*
* PREPARE FOR COPY INTO TEMP FIELD
COPYPREP ST    7,0(,5)         WRITE OUT TRAILING SPACE ADDR
         MVI   ZNTEMP,X'F0'    RESET COPY FIELD TO 0
         MVC   ZNTEMP+1(14),ZNTEMP
         LA    8,ZNTEMP+14     GET ADDR OF LAST CHAR IN COPY FIELD
         BCTR  7,0             MOVE READ PTR BACK TO LAST DIGIT
*
* COPY EBCDIC CODES RIGHT TO LEFT INTO CONVERSION FIELD
COPYLOOP CLI   0(7),C'.'      TEST IF DECIMAL CHARACTER
         BE    UPDTNCPY       SKIP THIS POS IF SO (NO COPY)
         MVC   0(1,8),0(7)    COPY EBCDIC CODE
UPDTCPY  BCTR  8,0            DECREMENT COPY HEAD
UPDTNCPY BCTR  7,0            DECREMENT SCAN HEAD
         CR    7,2            TEST IF BACK AT FIRST DIGIT
         BNL   COPYLOOP       IF NOT THEN CONTINUE
*
* SET RESULTS IN RECEIVER FIELDS
         ST    9,0(,6)            WRITE BACK # IMPLIED DECIMALS
         PACK  0(8,4),ZNTEMP(15)  WRITE BACK PACKED DECIMAL
         CLI   SCODE,X'FF'        SET SIGN DIGIT IF NEGATIVE
         BNE   SETRCODE
         MP    0(8,4),=PL1'-1'
*
* ==== Exit linkage ============
SETRCODE LA    15,0           R15 = RETURN CODE OF 0
PARSEXIT L     13,4(,13)      POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)     RESTORE REGISTER 14
         LM    0,12,20(13)    RESTORE R0 THRU R12
         BR    14             RETURN TO CALLER
*
         LTORG
*
PARSSAVE DS    18F'-1'        PARSE SUBPROGRAM'S SAVE AREA
*
SCODE    DC    X'00'          SIGN CODE BYTE, DEFAULT 0 (POSITIVE)
DECFLAG  DC    X'00'          DECIMAL FOUND FLAG, DEFAULT 0 (FALSE)
ZNTEMP   DS    CL15           CONVERSION COPY FIELD
*
         END   MAIN
/*
//*
//* IN-STREAM PROGRAM DATA
//FT05F001 DD *
50
082770 019928 000192 729900 081992 290019 100203 177290
0199928 0199938
0027819 0025274
1092837 1092861
0092409 0092312
0719822 0720081
/*
//