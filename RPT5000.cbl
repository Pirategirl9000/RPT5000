       IDENTIFICATION DIVISION.                                         00010000
                                                                        00020000
       PROGRAM-ID. RPT5000.                                             00030001
                                                                        00040000
      *   Programmers.: Violet French                                   00050000
      *   Date........: 2026.03.18                                      00060001
      *   Github URL..: https://github.com/Pirategirl9000/RPT5000       00070001
      *   Description.: This program produces a sales report based on   00080000
      *   values acquired from the CUSTMAST dataset and produces        00090000
      *   subtotals and grandtotals for the different branches and      00100001
      *   sales representatives                                         00110001
       ENVIRONMENT DIVISION.                                            00120000
                                                                        00130000
       INPUT-OUTPUT SECTION.                                            00140000
                                                                        00150000
       FILE-CONTROL.                                                    00160000
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00170000
           SELECT ORPT5000 ASSIGN TO RPT5000.                           00180001
                                                                        00190000
       DATA DIVISION.                                                   00200000
                                                                        00210000
       FILE SECTION.                                                    00220000
                                                                        00230000
      **************************************************************    00240000
      * INPUT FILE                                                 *    00250000
      **************************************************************    00260000
       FD  CUSTMAST                                                     00270000
           RECORDING MODE IS F                                          00280000
           LABEL RECORDS ARE STANDARD                                   00290000
           RECORD CONTAINS 130 CHARACTERS                               00300000
           BLOCK CONTAINS 130 CHARACTERS.                               00310000
       01  CUSTOMER-MASTER-RECORD.                                      00320000
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00330000
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00340000
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00350000
           05  CM-CUSTOMER-NAME        PIC X(20).                       00360000
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00370000
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00380000
           05  FILLER                  PIC X(87).                       00390000
                                                                        00400000
      **************************************************************    00410000
      * OUTPUT FILE                                                *    00420000
      **************************************************************    00430000
       FD  ORPT5000                                                     00440001
           RECORDING MODE IS F                                          00450000
           LABEL RECORDS ARE STANDARD                                   00460000
           RECORD CONTAINS 130 CHARACTERS                               00470000
           BLOCK CONTAINS 130 CHARACTERS.                               00480000
       01  PRINT-AREA      PIC X(130).                                  00490000
                                                                        00500000
       WORKING-STORAGE SECTION.                                         00510000
                                                                        00520000
      *------------------------------------------------------------*    00530000
      *                        WORKING FIELDS                      *    00540000
      *============================================================*    00550000
      *     THE FOLLOWING RECORDS ARE USED FOR WORKING WITH DATA   *    00560000
      *              AND ARE NOT USED FOR PROGRAM OUTPUT           *    00570000
      *------------------------------------------------------------*    00580000
                                                                        00590000
      **************************************************************    00600000
      * SWITCHES FOR END OF FILE AND FIRST RECORD                  *    00610007
      **************************************************************    00620000
       01  SWITCHES.                                                    00630000
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00640000
               88  CUSTMAST-EOF                 VALUE "Y".              00650007
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00660000
               88  FIRST-RECORD                 VALUE "Y".              00670007
                                                                        00680000
      **************************************************************    00690000
      * SWITCH FOR END OF FILE                                     *    00700000
      **************************************************************    00710000
       01  CONTROL-FIELDS.                                              00720000
           05  OLD-BRANCH-NUMBER       PIC 99.                          00730000
           05  OLD-SALESREP-NUMBER     PIC 99.                          00740005
                                                                        00750000
      **************************************************************    00760000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00770000
      **************************************************************    00780000
       01  PRINT-FIELDS.                                                00790000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00800000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00810000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00820000
                                                                        00830000
      **************************************************************    00840000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00850000
      **************************************************************    00860000
       01  TOTAL-FIELDS.                                                00870000
           05  BRANCH-TOTAL-THIS-YTD    PIC S9(6)V99   VALUE ZERO.      00880008
           05  BRANCH-TOTAL-LAST-YTD    PIC S9(6)V99   VALUE ZERO.      00890008
           05  SALESREP-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.      00900008
           05  SALESREP-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.      00910008
           05  GRAND-TOTAL-THIS-YTD     PIC S9(7)V99   VALUE ZERO.      00920008
           05  GRAND-TOTAL-LAST-YTD     PIC S9(7)V99   VALUE ZERO.      00930008
                                                                        00940000
      **************************************************************    00950000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    00960000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    00970000
      **************************************************************    00980000
       01  CURRENT-DATE-AND-TIME.                                       00990000
           05  CD-YEAR         PIC 9999.                                01000000
           05  CD-MONTH        PIC 99.                                  01010000
           05  CD-DAY          PIC 99.                                  01020000
           05  CD-HOURS        PIC 99.                                  01030000
           05  CD-MINUTES      PIC 99.                                  01040000
           05  FILLER          PIC X(9).                                01050000
                                                                        01060000
      **************************************************************    01070000
      * STORES VALUES USED FOR CALCULATIONS                       *     01080008
      **************************************************************    01090000
       01  CALCULATED-FIELDS.                                           01100000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            01110000
                                                                        01120000
      *------------------------------------------------------------*    01130000
      *                       OUTPUT FIELDS                        *    01140000
      *============================================================*    01150000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01160000
      *                      THE OUTPUT FILE                       *    01170000
      *------------------------------------------------------------*    01180000
                                                                        01190000
      **************************************************************    01200000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01210000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01220000
      **************************************************************    01230000
       01  HEADING-LINE-1.                                              01240000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01250000
           05  HL1-MONTH       PIC 9(2).                                01260000
           05  FILLER          PIC X(1)    VALUE "/".                   01270000
           05  HL1-DAY         PIC 9(2).                                01280000
           05  FILLER          PIC X(1)    VALUE "/".                   01290000
           05  HL1-YEAR        PIC 9(4).                                01300000
           05  FILLER          PIC X(16)   VALUE SPACE.                 01310000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01320000
           05  FILLER          PIC X(10)   VALUE "EPORT     ".          01330000
           05  FILLER          PIC X(15)   VALUE SPACE.                 01340000
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            01350000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01360000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01370000
                                                                        01380000
      **************************************************************    01390000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01400000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01410000
      **************************************************************    01420000
       01  HEADING-LINE-2.                                              01430000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01440000
           05  HL2-HOURS       PIC 9(2).                                01450000
           05  FILLER          PIC X(1)    VALUE ":".                   01460000
           05  HL2-MINUTES     PIC 9(2).                                01470000
           05  FILLER          PIC X(68)   VALUE SPACE.                 01480000
           05  FILLER          PIC X(10)   VALUE "RPT5000".             01490001
           05  FILLER          PIC X(39)   VALUE SPACE.                 01500000
                                                                        01510000
      **************************************************************    01520000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01530000
      **************************************************************    01540000
       01  HEADING-LINE-3.                                              01550000
           05 FILLER               PIC X(130)   VALUE SPACE.            01560000
                                                                        01570000
      **************************************************************    01580000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01590000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01600000
      * THE NEXT HEADER LINE                                       *    01610000
      **************************************************************    01620000
       01  HEADING-LINE-4.                                              01630000
           05  FILLER      PIC X(7)    VALUE "BRANCH ".                 01640016
           05  FILLER      PIC X(6)    VALUE "SALES ".                  01650016
           05  FILLER      PIC X(20)   VALUE "CUST                ".    01660002
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    01670000
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    01680000
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    01690000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01700000
                                                                        01710000
      **************************************************************    01720000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01730000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01740000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01750000
      **************************************************************    01760000
       01  HEADING-LINE-5.                                              01770000
           05  FILLER      PIC X(8)    VALUE " NUM    ".                01780018
           05  FILLER      PIC X(5)    VALUE "REP  ".                   01790018
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01800000
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01810000
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01820000
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01830000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01840000
                                                                        01850000
      **************************************************************    01860000
      * STORES THE SIXTH HEADER LINE WHICH IS USED FOR SPACING     *    01870000
      **************************************************************    01880000
       01  HEADING-LINE-6.                                              01890000
           05  FILLER      PIC X(130)  VALUE SPACES.                    01900000
                                                                        01910000
      **************************************************************    01920000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    01930000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    01940000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    01950000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    01960000
      * DIFFERENCE IN PERCENT.                                     *    01970000
      **************************************************************    01980000
       01  CUSTOMER-LINE.                                               01990000
           05  FILLER              PIC X(2)     VALUE SPACE.            02000000
           05  CL-BRANCH-NUMBER    PIC X(2).                            02010000
           05  FILLER              PIC X(4)     VALUE SPACE.            02020018
           05  CL-SALESREP-NUMBER  PIC X(2).                            02030012
           05  FILLER              PIC X(3)     VALUE SPACE.            02040015
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            02050000
           05  FILLER              PIC X(2)     VALUE SPACE.            02060000
           05  CL-CUSTOMER-NAME    PIC X(20).                           02070000
           05  FILLER              PIC X(3)     VALUE SPACE.            02080000
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      02090000
           05  FILLER              PIC X(4)     VALUE SPACE.            02100000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      02110000
           05  FILLER              PIC X(4)     VALUE SPACE.            02120000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      02130000
           05  FILLER              PIC X(3)     VALUE SPACE.            02140000
           05  CL-CHANGE-PERCENT   PIC ZZ9.9-.                          02150000
           05  FILLER              PIC X(47)    VALUE SPACE.            02160000
                                                                        02170000
      **************************************************************    02180000
      * STORES THE BRANCH TOTAL LINE                               *    02190000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02200000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02210000
      * USED FOR OUTPUTTING                                        *    02220000
      **************************************************************    02230000
       01  BRANCH-TOTAL-LINE.                                           02240000
           05  FILLER              PIC X(28)    VALUE SPACE.            02250024
           05  FILLER              PIC X(14)    VALUE "BRANCH TOTAL".   02260000
           05  BTL-SALES-THIS-YTD  PIC ZZZ,ZZ9.99-.                     02270000
           05  FILLER              PIC X(3)     VALUE SPACE.            02280000
           05  BTL-SALES-LAST-YTD  PIC ZZZ,ZZ9.99-.                     02290000
           05  FILLER              PIC X(3)     VALUE SPACE.            02300000
           05  BTL-CHANGE-AMOUNT   PIC ZZZ,ZZ9.99-.                     02310000
           05  FILLER              PIC X(3)     VALUE SPACE.            02320000
           05  BTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02330000
           05  FILLER              PIC X(48)    VALUE " **".            02340005
                                                                        02350000
      **************************************************************    02360005
      * STORES THE SALES REP TOTAL LINE                            *    02370005
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02380005
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02390005
      * USED FOR OUTPUTTING                                        *    02400005
      **************************************************************    02410005
       01  SALESREP-TOTAL-LINE.                                         02420011
           05  FILLER              PIC X(28)    VALUE SPACE.            02430024
           05  FILLER              PIC X(14)    VALUE "SALESREP TOTAL". 02440005
           05  STL-SALES-THIS-YTD  PIC ZZZ,ZZ9.99-.                     02450009
           05  FILLER              PIC X(3)     VALUE SPACE.            02460005
           05  STL-SALES-LAST-YTD  PIC ZZZ,ZZ9.99-.                     02470009
           05  FILLER              PIC X(3)     VALUE SPACE.            02480005
           05  STL-CHANGE-AMOUNT   PIC ZZZ,ZZ9.99-.                     02490009
           05  FILLER              PIC X(3)     VALUE SPACE.            02500005
           05  STL-CHANGE-PERCENT  PIC ZZ9.9-.                          02510009
           05  FILLER              PIC X(48)    VALUE " *".             02520005
      **************************************************************    02530000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02540000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02550000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02560000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02570000
      **************************************************************    02580000
       01  GRAND-TOTAL-LINE.                                            02590000
           05  FILLER              PIC X(28)    VALUE SPACE.            02600028
           05  FILLER              PIC X(12)    VALUE "GRAND TOTAL ".   02610000
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   02620000
           05  FILLER              PIC X(1)     VALUE SPACE.            02630000
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   02640000
           05  FILLER              PIC X        VALUE SPACE.            02650000
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   02660000
           05  FILLER              PIC X(3)     VALUE SPACE.            02670000
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02680000
           05  FILLER              PIC X(43)    VALUE " ***".           02690005
                                                                        02700000
       PROCEDURE DIVISION.                                              02710000
                                                                        02720000
      **************************************************************    02730000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    02740000
      * READING AND WRITING TO AND FROM THEM                       *    02750000
      **************************************************************    02760000
       000-PREPARE-SALES-REPORT.                                        02770000
                                                                        02780000
           OPEN INPUT  CUSTMAST                                         02790000
                OUTPUT ORPT5000.                                        02800001
                                                                        02810000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   02820000
           *> THE HEADER LINES                                          02830000
           PERFORM 100-FORMAT-REPORT-HEADING.                           02840000
                                                                        02850000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     02860000
           *> THE END OF THE INPUT FILE                                 02870000
           PERFORM 200-PREPARE-SALES-LINES                              02880000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         02890000
                                                                        02900000
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                02910000
           PERFORM 300-PRINT-GRAND-TOTALS.                              02920000
                                                                        02930000
           CLOSE CUSTMAST                                               02940000
                 ORPT5000.                                              02950001
           STOP RUN.                                                    02960000
                                                                        02970000
      **************************************************************    02980000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    02990000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    03000000
      **************************************************************    03010000
       100-FORMAT-REPORT-HEADING.                                       03020000
                                                                        03030000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03040000
                                                                        03050000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03060000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03070000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03080000
           MOVE CD-MONTH   TO HL1-MONTH.                                03090000
           MOVE CD-DAY     TO HL1-DAY.                                  03100000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03110000
           MOVE CD-HOURS   TO HL2-HOURS.                                03120000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03130000
                                                                        03140000
      **************************************************************    03150000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03160000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03170000
      * TERMINATING LINE OF THE FILE                               *    03180000
      **************************************************************    03190000
       200-PREPARE-SALES-LINES.                                         03200000
                                                                        03210000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03220000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03230000
                                                                        03240000
           *> PERFORMS DUTIES BASED ON THE ENTRY                        03250006
           *>  * IF WE RUN OUT OF DATA PRINT THE SALES AND BRANCH TOTALS03260006
           *>  * IF IT'S THE FIRST RECORD PRINT THE CUSTOMER LINE AND   03270006
           *>    STORE THE CURRENT SALESREP AND BRANCH NUMBER TO THE OLD03280006
           *>  * IF THE BRANCH NUMBER IS GREATER THAN THE CURRENT ONE   03290006
           *>    THEN PRINT THE SALES REP LINE, BRANCH TOTAL LINE, AND  03300006
           *>    THEN THE NEW CUSTOMER'S LINE. AFTER UPDATE THE BRANCH  03310006
           *>    AND SALESREP NUMBERS                                   03320006
           *>  * IF THE SALES REP NUMBER IS GREATER THAN THE CURRENT ONE03330006
           *>    PRINT SALES LINE THEN THE CURRENT CUSTOMER LINE AFTER  03340006
           *>    UPDATE THE SALES REP NUMBER                            03350006
           *>  * IF NOTHING ELSE JUST PRINT THE CUSTOMER RECORD         03360006
           EVALUATE TRUE                                                03370006
               WHEN CUSTMAST-EOF                                        03380006
                   PERFORM 250-PRINT-SALESREP-LINE                      03390006
                   PERFORM 240-PRINT-BRANCH-LINE                        03400006
               WHEN FIRST-RECORD                                        03410006
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03420006
                   MOVE "N" TO FIRST-RECORD-SWITCH                      03430006
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03440006
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03450006
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03460006
                   PERFORM 250-PRINT-SALESREP-LINE                      03470006
                   PERFORM 240-PRINT-BRANCH-LINE                        03480006
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03490006
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03500006
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03510006
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      03520025
                   PERFORM 250-PRINT-SALESREP-LINE                      03530006
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03540006
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03550006
               WHEN OTHER                                               03560006
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03570006
           END-EVALUATE.                                                03580006
                                                                        03590006
      **************************************************************    03600000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03610000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03620000
      **************************************************************    03630000
       210-READ-CUSTOMER-RECORD.                                        03640000
                                                                        03650000
           READ CUSTMAST                                                03660000
               AT END                                                   03670000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03680000
                                                                        03690000
      **************************************************************    03700000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03710000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    03720000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    03730000
      **************************************************************    03740000
       220-PRINT-CUSTOMER-LINE.                                         03750000
                                                                        03760000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     03770000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   03780000
           IF LINE-COUNT >= LINES-ON-PAGE                               03790000
               PERFORM 230-PRINT-HEADING-LINES.                         03800000
                                                                        03810000
           *> PERFROMS DUTIES BASED ON THE ENTRY                        03820012
           *>  * IF IT'S THE FIRST RECORD PRINT THE BRANCH NUMBER       03830012
           *>    AND THE SALESREP NUMBER                                03840012
           *>  * IF IT'S A NEW BRANCH PRINT THE BRANCH NUMBER AND       03850025
           *>    SALES REP NUMBER                                       03860025
           *>  * IF IT'S A NEW SALES REP PRINT THE SALESREP NUMBER      03870012
           *>  * OTHERWISE PRINT SPACES IN THOSE LINES FOR PADDING      03880012
           EVALUATE TRUE                                                03890012
               WHEN FIRST-RECORD                                        03900012
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            03910012
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        03920012
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03930012
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            03940012
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        03950026
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      03960025
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        03970012
                   MOVE SPACES TO CL-BRANCH-NUMBER                      03980027
               WHEN OTHER                                               03990012
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04000012
                   MOVE SPACES TO CL-SALESREP-NUMBER                    04010012
           END-EVALUATE.                                                04020012
                                                                        04030000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         04040000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     04050000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              04060000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                04070000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               04080000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               04090020
                                                                        04100000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    04110000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      04120000
           COMPUTE CHANGE-AMOUNT =                                      04130000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   04140000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      04150000
                                                                        04160000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     04170000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        04180000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        04190000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          04200000
           IF CM-SALES-LAST-YTD = ZERO                                  04210000
               MOVE 999.9 TO CL-CHANGE-PERCENT                          04220000
           ELSE                                                         04230000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04240000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              04250000
                   ON SIZE ERROR                                        04260000
                       MOVE 999.9 TO CL-CHANGE-PERCENT.                 04270000
                                                                        04280000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       04290000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            04300000
           PERFORM 225-WRITE-REPORT-LINE.                               04310000
                                                                        04320000
           *> ADD THIS CUSTOMERS SALES TO THE SALESREP TOTALS           04330021
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD.            04340021
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD.            04350021
                                                                        04360000
      **************************************************************    04370000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04380000
      * FOR EVERY PAGE                                             *    04390000
      **************************************************************    04400000
       225-WRITE-REPORT-LINE.                                           04410000
           WRITE PRINT-AREA.                                            04420000
           ADD 1 TO LINE-COUNT.                                         04430000
                                                                        04440000
      **************************************************************    04450000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04460000
      * FOR EVERY PAGE                                             *    04470000
      **************************************************************    04480000
       230-PRINT-HEADING-LINES.                                         04490000
                                                                        04500000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             04510000
           *> SO WE INCREASE THE PAGE COUNT HERE                        04520000
           ADD 1 TO PAGE-COUNT.                                         04530000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      04540000
                                                                        04550000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 04560000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           04570000
           WRITE PRINT-AREA.                                            04580000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           04590000
           WRITE PRINT-AREA.                                            04600000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           04610000
           WRITE PRINT-AREA.                                            04620000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           04630000
           WRITE PRINT-AREA.                                            04640000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           04650000
           WRITE PRINT-AREA.                                            04660000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           04670000
           WRITE PRINT-AREA.                                            04680000
                                                                        04690000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    04700000
           *> OF A NEW PAGE                                             04710000
           MOVE ZERO TO LINE-COUNT.                                     04720000
                                                                        04730000
      **************************************************************    04740000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    04750000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    04760000
      **************************************************************    04770000
       240-PRINT-BRANCH-LINE.                                           04780000
                                                                        04790000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           04800000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            04810000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            04820000
                                                                        04830000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            04840000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       04850000
           COMPUTE CHANGE-AMOUNT =                                      04860000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           04870000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     04880000
                                                                        04890000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                04900000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        04910000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              04920000
               MOVE 999.9 TO BTL-CHANGE-PERCENT                         04930000
           ELSE                                                         04940000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     04950000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          04960000
                   ON SIZE ERROR                                        04970000
                       MOVE 999.9 TO BTL-CHANGE-PERCENT.                04980000
                                                                        04990000
           *> PRINT BRANCH LINE                                         05000000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        05010000
           PERFORM 225-WRITE-REPORT-LINE.                               05020000
                                                                        05030000
           *> WRITE A BLANK SPACER LINE                                 05040000
           MOVE SPACES TO PRINT-AREA.                                   05050000
           PERFORM 225-WRITE-REPORT-LINE.                               05060000
                                                                        05070000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 05080000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           05090000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           05100000
                                                                        05110000
           *> ZERO OUT THE BRANCH TOTALS                                05120000
           MOVE ZERO TO BRANCH-TOTAL-THIS-YTD.                          05130000
           MOVE ZERO TO BRANCH-TOTAL-LAST-YTD.                          05140000
                                                                        05150000
      **************************************************************    05160006
      * PRINTS THE CURRENT SALESREP'S TOTALS, RAN ONCE FOR EVERY   *    05170006
      * SALESREP. ALSO CALCULATES THE CHANGE BETWEEN YEARS         *    05180006
      **************************************************************    05190006
       250-PRINT-SALESREP-LINE.                                         05200006
                                                                        05210006
           *> MOVE THE SALESREP TOTALS TO THE SALESREP TOTAL LINE       05220010
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          05230010
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          05240010
                                                                        05250006
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05260006
           *> FOR THE CURRENT SALESREP AND ADD IT TO THE TOTAL LINE     05270010
           COMPUTE CHANGE-AMOUNT =                                      05280006
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       05290010
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     05300010
                                                                        05310006
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05320006
           *> THEN MOVE TO THE SALESREP TOTAL LINE                      05330010
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            05340010
               MOVE 999.9 TO STL-CHANGE-PERCENT                         05350010
           ELSE                                                         05360006
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     05370010
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        05380010
                   ON SIZE ERROR                                        05390006
                       MOVE 999.9 TO STL-CHANGE-PERCENT.                05400010
                                                                        05410006
           *> PRINT SALESREP LINE                                       05420010
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      05430010
           PERFORM 225-WRITE-REPORT-LINE.                               05440006
                                                                        05441029
           *> PRINT A SPACER LINE                                       05442029
           MOVE SPACES TO PRINT-AREA.                                   05443029
           PERFORM 225-WRITE-REPORT-LINE.                               05444029
                                                                        05450006
           *> ADD THE SALESREP TOTALS TO THE BRANCH TOTALS              05460010
           *> WHEN A BRANCH IS PRINTED THEN THOSE TOTALS ARE MOVED      05470010
           *> TO THE GRAND TOTALS                                       05480010
           *> CUSTOMER->SALESREP->BRANCH->GRAND-TOTAL                   05490010
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.        05500010
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.        05510010
                                                                        05520006
           *> ZERO OUT THE SALESREP TOTALS                              05530010
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        05540010
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        05550010
      **************************************************************    05560000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    05570000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    05580000
      * BEEN PRINTED                                               *    05590000
      **************************************************************    05600000
       300-PRINT-GRAND-TOTALS.                                          05610000
                                                                        05620000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                05630000
           *> OUTPUT LINE FOR GRAND TOTALS                              05640000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             05650000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             05660000
                                                                        05670000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             05680000
           COMPUTE CHANGE-AMOUNT =                                      05690000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             05700000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     05710000
                                                                        05720000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             05730000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   05740000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           05750000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          05760000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               05770000
               MOVE 999.9 TO GTL-CHANGE-PERCENT                         05780000
           ELSE                                                         05790000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     05800000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           05810000
                   ON SIZE ERROR                                        05820000
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.                05830000
                                                                        05840000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  05850000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         05860000
           PERFORM 225-WRITE-REPORT-LINE.                               05870000
