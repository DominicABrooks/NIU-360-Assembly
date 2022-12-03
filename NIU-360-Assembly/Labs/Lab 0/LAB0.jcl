//KC03ABDA JOB ,'DOMINIC BROOKS',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
* CSCI 360-2              LAB EXERCISE 0             SPRING 2022 *
*                                                                *
* DATE DUE:     08/28/2022                                       *
* TIME DUE:     11:59 PM                                         *
*                                                                *
******************************************************************
*        COL. 10
*        |     COL. 16
*        |     |
*        v     v
MAIN     CSECT
         USING MAIN,15      ESTABLISH ADDRESSABILITY ON REG 15
*
         LA    4,13         LOAD 13 INTO REG 4
         LA    8,6          LOAD 6 INTO REG 8
         AR    4,8          ADD REG 8'S CONTENTS TO REG 4'S
         XDUMP ,            DUMP CONTENTS OF ALL 16 REGS
         BCR   B'1111',14   UNCONDITIONAL RETURN TO CALLER (OS)
*
         LTORG              LITERAL ORGANIZATION
*
         END   MAIN
/*
//