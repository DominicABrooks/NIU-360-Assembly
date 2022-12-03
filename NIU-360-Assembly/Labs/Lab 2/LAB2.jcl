//KC03ABDA JOB ,'DOMINIC BROOKS',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
* CSCI 360-1            LAB EXERCISE 2               SPRING 2022 *
*                                                                *
* NAME:  DOMINIC BROOKS                                          *
* DATE:  10/2/22                                                 *
*                                                                *
* MULTIPLICATION, DIVISION, AND BRANCHING                        *
*                                                                *
* REGISTER USAGE:                                                *
* 2 - USAGE GIVEN IN EXERCISE HEADER                             *
* 3 - USAGE GIVEN IN EXERCISE HEADER                             *
* 4 - USAGE GIVEN IN EXERCISE HEADER                             *
*                                                                *
******************************************************************
*
MAIN     CSECT
         USING MAIN,15
*| Exercise 1                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLDS MOST SIGNICAND 32 BITS OF PRODUCT                   |
*| 3 - HOLDS MULTIPLICAND AND LEAST SIGNFICAND 32 BITS OF PRODUCT|
*| 4 - HOLDS MULTIPLER                                           |
         LA    3,10           LOAD 10  INTO REGISTER 3
         LA    4,128          LOAD 128 INTO REGISTER 4
         MR    2,4            MULTIPLY R3 AND R4
         ST    3,PRODA        STORE PRODUCT INTO PRODA
         XDUMP PRODA,4        DUMP CONTENTS OF PRODA
*| Exercise 2                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLDS MOST SIGNICAND 32 BITS OF PRODUCT AND MULTIPLIER    |
*| 3 - HOLDS MULTIPLICAND AND LEAST SIGNFICAND 32 BITS OF PRODUCT|
         LA    3,28           LOAD 28 INTO REGISTER 3
         M     2,=F'-1'       NEGATE CONTENTS OF R3
         LA    2,300          LOAD 300 INTO REGISTER 2
         MR    2,2            MULTIPLY R3 AND R2
         ST    3,PRODB        STORE PRODUCT INTO PRODB
         XDUMP PRODB,4        DUMP CONTENTS OF PRODB
*| Exercise 3                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLDS REMAINDER                                           |
*| 3 - HOLDS QUOTIENT AND DIVIDEND                               |
*| 4 - HOLDS DIVISOR                                             |
         LA    3,200          LOAD 200 INTO REGISTER 3
         LA    4,15           LOAD 15  INTO REGISTER 4
         M     2,=F'1'        ENSURE PAIR IS EXTENDED
         DR    2,4            DIVIDE R3 AND R4
         XDECO 3,OQUOT        CONVERT AND STORE R3 IN OQUOT
         XDECO 2,OREMAIN      CONVERT AND STORE R2 IN OREMAIN
         XPRNT S3PRLINE,133   PRINT STARTING AT OQUOT
*| Exercise 4                                                    |
*| REGISTER USAGE:                                               |
*| 2 - HOLD SEQ ADDR                                             |
*| 3 - HOLD PARSED BUFFER                                        |
         LA    2,SEQ               LOAD ARR INTO R2
         XREAD BUFFER,80           INITIAL READ
READLOOP BNZ   ENDLOOP             LOOP UNTIL EOF
         XDECI 3,BUFFER            CONVERT AND STORE IN R3
         ST    3,0(2)              STORE INTO SEQ
         LA    2,4(2)              LOAD NEXT ADDR INTO R2
         XREAD BUFFER,80           GET NEXT INPUT
         B     READLOOP            BACK TO READLOOP
ENDLOOP  DS    0H                  GO TO AT END LOOP, DO NOTHING
*| Exercise 5                                                    |
*| REGISTER USAGE:                                               |
*| 2 - LOADS SEQ ADDR IN 2                                       |
*| 3 - LOAD END OF SEQUENCE INTO R3                              |
*| 5 - HOLDS SUM OF SEQ                                          |
         LA    2,SEQ               RE-LOAD ARR INTO R2
         LA    3,EOSEQ             LOAD END OF SEQ INTO R3
         LA    5,0                 PUT 0 INTO R5 TO BEGIN ADDING TO IT
SUMLOOP  DS    0H                  LOOP TO GET SUM OF DS
         A     5,0(2)              ADD SEQ ELEMENT TO R5
         LA    2,4(2)              LOAD NEXT FW INTO SEQ
         CR    2,3                 CHECK IF AT END OF SEQ
         BL    SUMLOOP             IF NOT, GO BACK TO START OF LOOP
         ST    5,SEQSUM            STORE SUM INTO SEQSUM
         XDUMP SEQSUM,4            DUMP CONTENTS OF SEQSUM
*| Exercise 6                                                    |
*| REGISTER USAGE:                                               |
*| 4 - HOLDS REMAINDER OF DIVISION                               |
*| 5 - HOLDS DIVIDEND                                            |
         M     4,=F'1'             SPLIT CONTENTS R5 TO EVEN-ODD PAIR
         D     4,=F'2'             DIVIDE R4 AND R5 BY 2
         C     4,=F'0'             CHECK IF REMAINDER IS 0
         BE    EVEN                IF REMAINDER IS 0, BRANCH TO EVEN
         BNE   ODD                 IF REMAINDER IS 1, BRANCH TO ODD
*
EVEN     DS    0H
         XPRNT ISEVEN,133          PRINT THAT SUM IS EVEN
         B     END                 BRANCH TO END
*
ODD      DS    0H
         XPRNT ISODD,133           PRINT THAT SUM IS ODD
         B     END                 BRANCH TO END
*
END      BR    14
*
         LTORG
*
*| Declared storage                                              |
PRODA    DS    F
PRODB    DS    F
SEQSUM   DS    F
*
BUFFER   DS    CL80
SEQ      DC    F'-1,-1,-1,-1,-1,-1,-1,-1,-1'
EOSEQ    DS    0C
*
ISEVEN   DC    C'1'
         DC    C'THE ACCUMULATED RESULT IS EVEN!'
         DC    101C' '
ISODD    DC    C'1'
         DC    C'THE ACCUMULATED RESULT IS ODD!'
         DC    102C' '
* STEP 3 OUTPUT BUFFER
S3PRLINE DC    C'1'           CARRIAGE CONTROL CHAR (NEW PAGE)
         DC    C'STEP 3. QUOTIENT:'
OQUOT    DS    CL12           S3 QUOTIENT OUTPUT FIELD
         DC    C'    REMAINDER:'
OREMAIN  DS    CL12           S3 REMAINDER OUTPUT FIELD
         DC    76C' '         FILL REMAINING BYTES WITH SPACES
*
         END   MAIN
/*
//*
//* IN-STREAM PROGRAM DATA
//FT05F001 DD *
134
96
782
-38
71
922
-277
-72
48
/*
//