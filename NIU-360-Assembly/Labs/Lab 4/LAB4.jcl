//KC03ABDA JOB ,'DOMINIC BROOKS',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
* CSCI 360-1            LAB EXERCISE 4               SPRING 2022 *
*                                                                *
* NAME:  DOMINIC BROOKS                                          *
* DATE:  10/31/22                                                *
*                                                                *
* PACKED DECIMALS                                                *
*                                                                *
* REGISTER USAGE:                                                *
* 2 - USAGE GIVEN IN EXERCISE HEADER 7 & 8                       *
*                                                                *
******************************************************************
*
$PARMS   DSECT               TABLE TO STORE THE SEQ OF VAL READ
$NUM1    DC    PL1'0'        FIRST NUM ROW
$NUM2    DC    PL3'0'        SECOND NUM ROW
$NUM3    DC    PL2'0'        THIRD NUM ROW
$NUM4    DC    PL3'0'        FOURTH NUM ROW
$NUM5    DC    PL2'0'        FIFTH NUM ROW
$NUM6    DC    PL2'0'        SIXTH NUM ROW
******************************************************************
MAIN     CSECT
         USING MAIN,15
******************************************************************
*| Exercise 1                                                    |
*| NO REGISTERS USED                                             |
         XREAD BUFFER,80               READ NEXT LINE FROM BUFFER
         PACK  PMULT(2),BUFFER(2)      PACK 2 BYTES INTO PMULT
         XDUMP PMULT,2                 DUMP RESULT
*| Exercise 2                                                    |
*| NO REGISTERS USED                                             |
         XREAD BUFFER,80               READ NEXT LINE FROM BUFFER
*
         PACK  PADDTMP(3),BUFFER(3)    PACK FIRST VAL INTO PADDTMP
         ZAP   PTOTAL(3),PADDTMP(3)    LOAD INTO TOTAL
*
         PACK  PADDTMP(3),BUFFER+4(3)  PACK NEXT INTO TMP
         AP    PTOTAL(3),PADDTMP(3)    ADD TO TOTAL
*
         PACK  PADDTMP(2),BUFFER+8(2)  PACK NEXT INTO TMP
         AP    PTOTAL(3),PADDTMP(2)    ADD TO TOTAL
*
         PACK  PADDTMP(2),BUFFER+11(2) PACK NEXT INTO TMP
         AP    PTOTAL(3),PADDTMP(2)    ADD TO TOTAL
*
         XDUMP PTOTAL,3                DUMP TOTAL RESULT
*| Exercise 3                                                    |
*| NO REGISTERS USED                                             |
         ZAP   PCALCTMP(5),PMULT(2)    PUT MULT IN CALC
         MP    PCALCTMP(5),PTOTAL(3)   MULTIPLY BY TOTAL
         XDUMP PCALCTMP,5              DUMP RESULT
*| Exercise 4                                                    |
*| NO REGISTERS USED                                             |
         SRP   PCALCTMP(5),64-4,5      SHIFT 4 AND ROUND
         XDUMP PCALCTMP,5              DUMP RESULT
*| Exercise 5                                                    |
*| NO REGISTERS USED                                             |
         ZAP   PDIVDTMP(7),PCALCTMP(5)     PREP EXERCISE 4 RESULT
         SRP   PDIVDTMP(7),4,0             PREP FOR DIVISION
         DP    PDIVDTMP(7),=PL2'2.25'      DIV BY 2.25
         SRP   PDIVDTMP(5),64-1,5          SHIFT 1 AND ROUND
         XDUMP PDIVDTMP,5                  DUMP RESULT
*| Exercise 6                                                    |
*| NO REGISTERS USED                                             |
         XDUMP PDIVDTMP,5
         ZAP   POUTPTMP(3),PDIVDTMP(5)          PREP FOR OUTPUT
*                              0 0 0   1 3 C
*                            _ 0 0 0 . 1 3
         MVC   PRNCALC(7),=X'402020214B2020' EDIT PATTERN
         ED    PRNCALC(7),POUTPTMP           CONVERT, STORE IN PRNCALC
         XPRNT PRNCALC,7                     PRINT CALC
*| Exercise 7                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD ADDRESS OF PRINT, USED TO POINT TO WHERE TO INSERT $ |
         LA    2,PRNCALC+3                   ADDRESS @ SIGNIF DIGIT
         MVC   PRNCALC(7),=X'402021204B2020' MOVE SIGNIF DIGIT
         EDMK  PRNCALC(7),POUTPTMP           CONVERT, STORE IN PRNCALC
         XDUMP ,
         BCTR  2,0                           DECREMENT ADDR
         MVI   0(2),C'$'                     ADD $ SIGN
         XPRNT PRNCALC,7                     PRINT CALC
*| Exercise 8                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD ADDRESS OF TABLE TO STORE BUFFER INFO INTO           |
          USING $PARMS,2                     LOAD DSECT INTO R2
          LA    2,PNLPARMS                   LOAD TABLE INTO R2
          XREAD BUFFER,80                    READ NEXT LINE
*
          PACK  PITENSOR(4),BUFFER(2)        READ FIRST INTO ARRAY
          PACK  PITENSOR+4(4),BUFFER+3(4)    READ SECON INTO ARRAY
          PACK  PITENSOR+8(4),BUFFER+8(3)    READ THIRD INTO ARRAY
          PACK  PITENSOR+12(4),BUFFER+12(2)  READ FOURT INTO ARRAY
          PACK  PITENSOR+16(4),BUFFER+15(2)  READ FIFTH INTO ARRAY
          PACK  PITENSOR+20(4),BUFFER+18(1)  READ SIXTH INTO ARRAY
*
          XREAD BUFFER,80
READ      BNZ   EOREAD         IF AT END, GO TO END OF READ (EOREAD)
*
          PACK  $NUM1(1),BUFFER(1)     READ FIRST INTO NUM1
          PACK  $NUM2(3),BUFFER+2(5)   READ SECON INTO NUM2
          PACK  $NUM3(2),BUFFER+8(3)   READ THIRD INTO NUM3
          PACK  $NUM4(3),BUFFER+12(4)  READ FOURT INTO NUM4
          PACK  $NUM5(2),BUFFER+17(3)  READ FIFTH INTO NUM5
          PACK  $NUM6(2),BUFFER+21(2)  READ SIXTH INTO NUM6
*
          LA    2,13(2)                LOAD NEXT LINE FROM TABLE
          XREAD BUFFER,80              READ NEXT LINE
          B     READ                   LOOP BACK TO TOP OF READ LOOP
EOREAD    DS    0H                     END OF READ LOOP
* DUMP READ DATA
          XDUMP PITENSOR,24            DUMP ARRAY
          XDUMP PNLPARMS,52            DUMP TABLE
*
          DROP  2                      DROP DSECT
******************************************************************
         BR    14
*
         LTORG
* Declared Storage ***********************************************
BUFFER   DS    CL80
PMULT    DC    PL2'0'
PADDTMP  DC    PL3'0'
PTOTAL   DC    PL3'0'
PCALCTMP DC    PL5'0'
PDIVDTMP DC    PL7'0'
POUTPTMP DC    PL3'0'
PRNCALC  DS    CL7
PITENSOR DC    PL4'0,0,0,0,0,0'
PNLPARMS DC    4PL13'0'
*
         END   MAIN
/*
//*
//* IN-STREAM PROGRAM DATA
//FT05F001 DD *
25
279 928 27 46
19 2778 191 38 72 8
2 72707 392 1788 928 27
8 26398 293 5601 965 18
0 19826 501 2985 601 08
9 84516 871 9864 168 50
/*
//