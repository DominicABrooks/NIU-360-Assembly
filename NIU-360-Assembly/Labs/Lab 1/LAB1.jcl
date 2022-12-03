//KC03ABDA JOB ,'DOMINIC BROOKS',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
* CSCI 360-1            LAB EXERCISE 1               SPRING 2022 *
*                                                                *
* NAME:  DOMINIC BROOKS                                          *
* DATE:  9/18/22                                                 *
*                                                                *
* SERIES OF BASIC ARITHMETIC, ADDRESSING, AND OUTPUT EXERCISES   *
*                                                                *
* REGISTER USAGE:                                                *
* 2 - USAGE GIVEN IN EXERCISE HEADER                             *
* 3 - USAGE GIVEN IN EXERCISE HEADER                             *
* 4 - USAGE GIVEN IN EXERCISE HEADER                             *
*                                                                *
******************************************************************
*
*| Code body goes here, beginning with CSECT + USING and         |
*| ending with return to caller (i.e., the BCR instruction)      |
*
MAIN     CSECT
         USING MAIN,15      ESTABLISH ADDRESSABILITY ON REG 15
*| Exercise 1                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD ADDEND, 5                                            |
*| 3 - HOLD SUBTRAHEND, 10                                       |
*| 4 - HOLD MINUEND, 200 AND FINAL SUM                           |
         LA    2,5          LOAD 5   INTO REGISTER 2
         LA    3,10         LOAD 10  INTO REGISTER 3
         LA    4,200        LOAD 200 INTO REGISTER 4
         SR    4,3          SUBTRACT REG 3'S CONTENT TO REG 4'S
         AR    4,2          ADD REG 2'S CONTENT TO REG 4'S
         XDUMP ,            DUMP CONTENTS OF ALL 16 REGS
*| Exercise 2                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD MINUEND, 15 AND FINAL SUM                            |
*| 3 - HOLD SUBTRAHEND, 87, AND ADDEND 9                         |
         LA    2,15         LOAD 15  INTO REGISTER 2
         LA    3,87         LOAD 87  INTO REGISTER 3
         SR    2,3          SUBTRACT REG 3'S CONTENT TO REG 2'S
         LA    3,9          LOAD 9   INTO REGISTER 3
         AR    2,3          ADD REG 3'S CONTENT TO REG 2'S
         XDUMP ,            DUMP CONTENTS OF ALL 16 REGS
*| Execrise 3                                                    |
*| REGISTER USAGE:                                               |
*| 2 - STORE SUM OF EXERCISE 2, STORED IN REG 2 INTO FW @ RESA   |
         ST    2,RESA       STORE CONTENTS OF REG 2 INTO RESA
         XDUMP RESA,4       DUMPS CONTENTS AT RESA
*| Execrise 4                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD LEFT  HAND FACTORS AND ADDEND AND SUM                |
*| 3 - HOLD RIGHT HAND FACTORS AND ADDEND                        |
         LA    2,5          LOAD 5 INTO REGISTER 2
         LA    3,4          LOAD 4 INTO REGISTER 3
         A     2,=F'5'      ADD 5 TO REGISTER 2 TO FIND PRODUCT OF 2*5
         A     3,=F'4'      ADD 4 TO REGISTER 3,
         A     3,=F'4'      TWICE TO FIND THE PRODUCT OF 3*4
         AR    2,3          ADD REGISTER 3'S CONTENTS INTO REGISTER 2
         ST    2,RESB       STORE CONTENTS OF REG 2 INTO RESB
         XDUMP RESB,4       DUMPS CONTENTS AS RESB
*| Execrise 5                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD FINAL VALUE OF EXERCISE 2 IN HEX                     |
*| 3 - HOLD FINAL VALUE OF EXERCISE 3 IN HEX                     |
         L     2,RESA       LOAD RESA INTO REGISTER 2
         L     3,RESB       LOAD RESB INTO REGISTER 3
         XDECO 2,ORESA      CONVERT AND STORE IN ORESA
         XDECO 3,ORESB      CONVERT AND STORE IN ORESB
         XPRNT PRNTLINE,133 PRINT STARTING AT ORESA
*| Execrise 6                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD FINAL VALUE OF EXERCISE 2 TO BE LOADED INTO SEQA     |
*| 3 - HOLD FINAL VALUE OF EXERCISE 3 TO BE LOADED INTO SEQA     |
*| 4 - HOLD ADDRESS OF SEQA                                      |
         LA    4,SEQA       LOAD SEQA ADDRESS INTO REGISTER 4
         ST    2,0(4)       STORE RESA INTO SEQA
         ST    3,4(4)       STORE RESB INTO SEQA
         XDUMP SEQA,12      DUMP CONTENTS OF SEQA
*| Execrise 7                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD FINAL VALUE OF EXERCISE 2 TO BE LOADED INTO SEQA     |
*| 3 - HOLD FINAL VALUE OF EXERCISE 3 TO BE LOADED INTO SEQA     |
*| 4 - HOLD ADDRESS OF SEQA                                      |
*| 5 - HOLD ADDRESS OF SEQB                                      |
         LA    5,SEQB       LOAD SEQB ADDRESS INTO REGISTER 5
         L     2,0(4)       LOAD WHAT'S AT SEQA0 INTO REGISTER 2
         L     3,4(4)       LOAD WHAT'S AT SEQA1 INTO REGISTER 3
         ST    2,0(5)       STORE CONTENTS OF REGISTER 2 INTO SEQB0
         ST    3,4(5)       STORE CONTENTS OF REGISTER 2 INTO SEQB1
         XDUMP SEQB,12      DUMP CONTENTS OF SEQB
*
         BCR   B'1111',14   UNCONDITIONAL RETURN TO CALLER (OS)
*
         LTORG              LITERAL ORGANIZATION
*
*| Declared storage follows. Your declarations may go anywhere   |
*| you choose, except beteen the print line declarations.        |
SEQA     DC    F'-1,-1,-1'
SEQB     DC    F'-1,-1,-1'
RESA     DS    F
RESB     DS    F
*
* Print line begin ----------------------------------------------
PRNTLINE DC    C'1'           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    132C' '        WIPE PRINT LINE (SPACES)
         ORG   PRNTLINE+1     GO BACK TO PRINTLN1 START LOC + 1
         DC    C'RESA:'
ORESA    DS    CL12           RESA OUTPUT FIELD
         DC    C'    RESB:'
ORESB    DS    CL12           RESB OUTPUT FIELD
         ORG   PRNTLINE+133   JUMP PAST END OF PRINTLN1
* Print line end ------------------------------------------------
         END   MAIN
/*
//
