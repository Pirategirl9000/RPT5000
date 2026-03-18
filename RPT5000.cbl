       IDENTIFICATION DIVISION.                                         00010000
                                                                        00020000
       PROGRAM-ID. RPT5000.                                             00030001
                                                                        00040000
      *   Programmers.: Violet French                                   00050000
      *   Date........: 2026.03.18                                      00060001
      *   Github URL..: https://github.com/Pirategirl9000/RPT5000       00070001
      *   Description.: This program produces a sales report based on   00080000
      *   values acquired from the CUSTMAST dataset and produces        00090000
      *   subtotals and grandtotals for the different branches and      00091001
      *   sales representatives                                         00091101
       ENVIRONMENT DIVISION.                                            00092000
                                                                        00093000
       INPUT-OUTPUT SECTION.                                            00094000
                                                                        00095000
       FILE-CONTROL.                                                    00096000
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00097000
           SELECT ORPT5000 ASSIGN TO RPT5000.                           00098001
                                                                        00099000
       DATA DIVISION.                                                   00100000
                                                                        00110000
       FILE SECTION.                                                    00120000
                                                                        00130000
      **************************************************************    00140000
      * INPUT FILE                                                 *    00150000
      **************************************************************    00160000
       FD  CUSTMAST                                                     00170000
           RECORDING MODE IS F                                          00180000
           LABEL RECORDS ARE STANDARD                                   00190000
           RECORD CONTAINS 130 CHARACTERS                               00200000
           BLOCK CONTAINS 130 CHARACTERS.                               00210000
       01  CUSTOMER-MASTER-RECORD.                                      00220000
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00230000
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00240000
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00250000
           05  CM-CUSTOMER-NAME        PIC X(20).                       00260000
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00270000
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00280000
           05  FILLER                  PIC X(87).                       00290000
                                                                        00300000
      **************************************************************    00310000
      * OUTPUT FILE                                                *    00320000
      **************************************************************    00330000
       FD  ORPT5000                                                     00340001
           RECORDING MODE IS F                                          00350000
           LABEL RECORDS ARE STANDARD                                   00360000
           RECORD CONTAINS 130 CHARACTERS                               00370000
           BLOCK CONTAINS 130 CHARACTERS.                               00380000
       01  PRINT-AREA      PIC X(130).                                  00390000
                                                                        00400000
       WORKING-STORAGE SECTION.                                         00410000
                                                                        00420000
      *------------------------------------------------------------*    00430000
      *                        WORKING FIELDS                      *    00440000
      *============================================================*    00450000
      *     THE FOLLOWING RECORDS ARE USED FOR WORKING WITH DATA   *    00460000
      *              AND ARE NOT USED FOR PROGRAM OUTPUT           *    00470000
      *------------------------------------------------------------*    00480000
                                                                        00490000
      **************************************************************    00500000
      * SWITCH FOR END OF FILE                                     *    00510000
      **************************************************************    00520000
       01  SWITCHES.                                                    00530000
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00540000
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00550000
                                                                        00560000
      **************************************************************    00570000
      * SWITCH FOR END OF FILE                                     *    00580000
      **************************************************************    00590000
       01  CONTROL-FIELDS.                                              00600000
           05  OLD-BRANCH-NUMBER       PIC 99.                          00610000
           05  OLD-SALESREP-NUMBER     PIC 99.                          00611005
                                                                        00620000
      **************************************************************    00630000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00640000
      **************************************************************    00650000
       01  PRINT-FIELDS.                                                00660000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00670000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00680000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00690000
                                                                        00700000
      **************************************************************    00710000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00720000
      **************************************************************    00730000
       01  TOTAL-FIELDS.                                                00740000
           05  BRANCH-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.        00750000
           05  BRANCH-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.        00760000
           05  GRAND-TOTAL-THIS-YTD   PIC S9(7)V99   VALUE ZERO.        00770000
           05  GRAND-TOTAL-LAST-YTD   PIC S9(7)V99   VALUE ZERO.        00780000
                                                                        00790000
      **************************************************************    00800000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    00810000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    00820000
      **************************************************************    00830000
       01  CURRENT-DATE-AND-TIME.                                       00840000
           05  CD-YEAR         PIC 9999.                                00850000
           05  CD-MONTH        PIC 99.                                  00860000
           05  CD-DAY          PIC 99.                                  00870000
           05  CD-HOURS        PIC 99.                                  00880000
           05  CD-MINUTES      PIC 99.                                  00890000
           05  FILLER          PIC X(9).                                00900000
                                                                        00910000
      **************************************************************    00920000
      * STORES FIELDS WITH VALUES CALCULATED PER CUSTOMER         *     00930000
      **************************************************************    00940000
       01  CALCULATED-FIELDS.                                           00950000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            00960000
                                                                        00970000
      *------------------------------------------------------------*    00980000
      *                       OUTPUT FIELDS                        *    00990000
      *============================================================*    01000000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01010000
      *                      THE OUTPUT FILE                       *    01020000
      *------------------------------------------------------------*    01030000
                                                                        01040000
      **************************************************************    01050000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01060000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01070000
      **************************************************************    01080000
       01  HEADING-LINE-1.                                              01090000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01100000
           05  HL1-MONTH       PIC 9(2).                                01110000
           05  FILLER          PIC X(1)    VALUE "/".                   01120000
           05  HL1-DAY         PIC 9(2).                                01130000
           05  FILLER          PIC X(1)    VALUE "/".                   01140000
           05  HL1-YEAR        PIC 9(4).                                01150000
           05  FILLER          PIC X(16)   VALUE SPACE.                 01160000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01170000
           05  FILLER          PIC X(10)   VALUE "EPORT     ".          01180000
           05  FILLER          PIC X(15)   VALUE SPACE.                 01190000
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            01200000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01210000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01220000
                                                                        01230000
      **************************************************************    01240000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01250000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01260000
      **************************************************************    01270000
       01  HEADING-LINE-2.                                              01280000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01290000
           05  HL2-HOURS       PIC 9(2).                                01300000
           05  FILLER          PIC X(1)    VALUE ":".                   01310000
           05  HL2-MINUTES     PIC 9(2).                                01320000
           05  FILLER          PIC X(68)   VALUE SPACE.                 01330000
           05  FILLER          PIC X(10)   VALUE "RPT5000".             01340001
           05  FILLER          PIC X(39)   VALUE SPACE.                 01350000
                                                                        01360000
      **************************************************************    01370000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01380000
      **************************************************************    01390000
       01  HEADING-LINE-3.                                              01400000
           05 FILLER               PIC X(130)   VALUE SPACE.            01410000
                                                                        01420000
      **************************************************************    01430000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01440000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01450000
      * THE NEXT HEADER LINE                                       *    01460000
      **************************************************************    01470000
       01  HEADING-LINE-4.                                              01480000
           05  FILLER      PIC X(8)    VALUE "BRANCH  ".                01490000
           05  FILLER      PIC X(8)    VALUE "SLSREP  ".                01491004
           05  FILLER      PIC X(20)   VALUE "CUST                ".    01500002
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    01510000
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    01520000
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    01530000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01540000
                                                                        01550000
      **************************************************************    01560000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01570000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01580000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01590000
      **************************************************************    01600000
       01  HEADING-LINE-5.                                              01610000
           05  FILLER      PIC X(8)    VALUE " NUM    ".                01620000
           05  FILLER      PIC X(9)    VALUE " NUM     ".               01621004
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01630000
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01640000
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01650000
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01660000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01670000
                                                                        01680000
      **************************************************************    01690000
      * STORES THE SIXTH HEADER LINE WHICH IS USED FOR SPACING     *    01700000
      **************************************************************    01710000
       01  HEADING-LINE-6.                                              01720000
           05  FILLER      PIC X(130)  VALUE SPACES.                    01730000
                                                                        01740000
      **************************************************************    01750000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    01760000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    01770000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    01780000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    01790000
      * DIFFERENCE IN PERCENT.                                     *    01800000
      **************************************************************    01810000
       01  CUSTOMER-LINE.                                               01820000
           05  FILLER              PIC X(2)     VALUE SPACE.            01830000
           05  CL-BRANCH-NUMBER    PIC X(2).                            01840000
           05  FILLER              PIC X(4)     VALUE SPACE.            01850000
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            01860000
           05  FILLER              PIC X(2)     VALUE SPACE.            01870000
           05  CL-CUSTOMER-NAME    PIC X(20).                           01880000
           05  FILLER              PIC X(3)     VALUE SPACE.            01890000
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      01900000
           05  FILLER              PIC X(4)     VALUE SPACE.            01910000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      01920000
           05  FILLER              PIC X(4)     VALUE SPACE.            01930000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      01940000
           05  FILLER              PIC X(3)     VALUE SPACE.            01950000
           05  CL-CHANGE-PERCENT   PIC ZZ9.9-.                          01960000
           05  FILLER              PIC X(47)    VALUE SPACE.            01970000
                                                                        01980000
      **************************************************************    01990000
      * STORES THE BRANCH TOTAL LINE                               *    02000000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02010000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02020000
      * USED FOR OUTPUTTING                                        *    02030000
      **************************************************************    02040000
       01  BRANCH-TOTAL-LINE.                                           02050000
           05  FILLER              PIC X(23)    VALUE SPACE.            02060000
           05  FILLER              PIC X(14)    VALUE "BRANCH TOTAL".   02070000
           05  BTL-SALES-THIS-YTD  PIC ZZZ,ZZ9.99-.                     02080000
           05  FILLER              PIC X(3)     VALUE SPACE.            02090000
           05  BTL-SALES-LAST-YTD  PIC ZZZ,ZZ9.99-.                     02100000
           05  FILLER              PIC X(3)     VALUE SPACE.            02110000
           05  BTL-CHANGE-AMOUNT   PIC ZZZ,ZZ9.99-.                     02120000
           05  FILLER              PIC X(3)     VALUE SPACE.            02130000
           05  BTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02140000
           05  FILLER              PIC X(48)    VALUE " **".            02150005
                                                                        02160000
      **************************************************************    02161005
      * STORES THE SALES REP TOTAL LINE                            *    02162005
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02163005
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02164005
      * USED FOR OUTPUTTING                                        *    02165005
      **************************************************************    02166005
       01  SALESREP-TOTAL-LINE                                          02167005
           05  FILLER              PIC X(23)    VALUE SPACE.            02168005
           05  FILLER              PIC X(14)    VALUE "SALESREP TOTAL". 02169005
           05  BTL-SALES-THIS-YTD  PIC ZZZ,ZZ9.99-.                     02169105
           05  FILLER              PIC X(3)     VALUE SPACE.            02169205
           05  BTL-SALES-LAST-YTD  PIC ZZZ,ZZ9.99-.                     02169305
           05  FILLER              PIC X(3)     VALUE SPACE.            02169405
           05  BTL-CHANGE-AMOUNT   PIC ZZZ,ZZ9.99-.                     02169505
           05  FILLER              PIC X(3)     VALUE SPACE.            02169605
           05  BTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02169705
           05  FILLER              PIC X(48)    VALUE " *".             02169805
      **************************************************************    02170000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02180000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02190000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02200000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02210000
      **************************************************************    02220000
       01  GRAND-TOTAL-LINE.                                            02230000
           05  FILLER              PIC X(23)    VALUE SPACE.            02240000
           05  FILLER              PIC X(12)    VALUE "GRAND TOTAL ".   02250000
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   02260000
           05  FILLER              PIC X(1)     VALUE SPACE.            02270000
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   02280000
           05  FILLER              PIC X        VALUE SPACE.            02290000
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   02300000
           05  FILLER              PIC X(3)     VALUE SPACE.            02310000
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02320000
           05  FILLER              PIC X(43)    VALUE " ***".           02330005
                                                                        02340000
       PROCEDURE DIVISION.                                              02350000
                                                                        02360000
      **************************************************************    02370000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    02380000
      * READING AND WRITING TO AND FROM THEM                       *    02390000
      **************************************************************    02400000
       000-PREPARE-SALES-REPORT.                                        02410000
                                                                        02420000
           OPEN INPUT  CUSTMAST                                         02430000
                OUTPUT ORPT5000.                                        02440001
                                                                        02450000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   02460000
           *> THE HEADER LINES                                          02470000
           PERFORM 100-FORMAT-REPORT-HEADING.                           02480000
                                                                        02490000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     02500000
           *> THE END OF THE INPUT FILE                                 02510000
           PERFORM 200-PREPARE-SALES-LINES                              02520000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         02530000
                                                                        02540000
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                02550000
           PERFORM 300-PRINT-GRAND-TOTALS.                              02560000
                                                                        02570000
           CLOSE CUSTMAST                                               02580000
                 ORPT5000.                                              02590001
           STOP RUN.                                                    02600000
                                                                        02610000
      **************************************************************    02620000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    02630000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    02640000
      **************************************************************    02650000
       100-FORMAT-REPORT-HEADING.                                       02660000
                                                                        02670000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         02680000
                                                                        02690000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          02700000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         02710000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           02720000
           MOVE CD-MONTH   TO HL1-MONTH.                                02730000
           MOVE CD-DAY     TO HL1-DAY.                                  02740000
           MOVE CD-YEAR    TO HL1-YEAR.                                 02750000
           MOVE CD-HOURS   TO HL2-HOURS.                                02760000
           MOVE CD-MINUTES TO HL2-MINUTES.                              02770000
                                                                        02780000
      **************************************************************    02790000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    02800000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    02810000
      * TERMINATING LINE OF THE FILE                               *    02820000
      **************************************************************    02830000
       200-PREPARE-SALES-LINES.                                         02840000
                                                                        02850000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               02860000
           PERFORM 210-READ-CUSTOMER-RECORD.                            02870000
                                                                        02880000
           *> IF THE LINE WE READ WASN'T BLANK THEN                     02890000
           *> WE WILL OUTPUT THAT CUSTOMER'S SALES TO THE OUTPUT        02900000
           *> NOTE: WE DON'T OUTPUT THE LAST LINE BECAUSE IT'S BLANK    02910000
           IF CUSTMAST-EOF-SWITCH = "N"                                 02920000
               IF FIRST-RECORD-SWITCH = "Y"                             02930000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      02940000
                   MOVE "N" TO FIRST-RECORD-SWITCH                      02950000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           02960000
               ELSE                                                     02970000
                   IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER              02980000
                       PERFORM 240-PRINT-BRANCH-LINE                    02990000
                       PERFORM 220-PRINT-CUSTOMER-LINE                  03000000
                       MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER       03010000
                   ELSE                                                 03020000
                       PERFORM 220-PRINT-CUSTOMER-LINE                  03030000
           ELSE                                                         03040000
               PERFORM 240-PRINT-BRANCH-LINE.                           03050000
                                                                        03060000
      **************************************************************    03070000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03080000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03090000
      **************************************************************    03100000
       210-READ-CUSTOMER-RECORD.                                        03110000
                                                                        03120000
           READ CUSTMAST                                                03130000
               AT END                                                   03140000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03150000
                                                                        03160000
      **************************************************************    03170000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03180000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    03190000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    03200000
      **************************************************************    03210000
       220-PRINT-CUSTOMER-LINE.                                         03220000
                                                                        03230000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     03240000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   03250000
           IF LINE-COUNT >= LINES-ON-PAGE                               03260000
               PERFORM 230-PRINT-HEADING-LINES.                         03270000
                                                                        03280000
           *> IF THIS IS THE FIRST RECORD OR THE FIRST RECORD OF THIS   03290000
           *> BRANCH THEN WE MOVE THE BRANCH NUMBER TO BE PRINTED       03300000
           *> OTHERWISE WE MOVE SPACES TO THE BRANCH NUMBER ITEM        03310000
           IF FIRST-RECORD-SWITCH = "Y"                                 03320000
               MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER                03330000
           ELSE                                                         03340000
               IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                  03350000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            03360000
               ELSE                                                     03370000
                   MOVE SPACES TO CL-BRANCH-NUMBER.                     03380000
                                                                        03390000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         03400000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     03410000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              03420000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                03430000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               03440000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               03450000
                                                                        03460000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    03470000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      03480000
           COMPUTE CHANGE-AMOUNT =                                      03490000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   03500000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      03510000
                                                                        03520000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     03530000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        03540000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        03550000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          03560000
           IF CM-SALES-LAST-YTD = ZERO                                  03570000
               MOVE 999.9 TO CL-CHANGE-PERCENT                          03580000
           ELSE                                                         03590000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      03600000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              03610000
                   ON SIZE ERROR                                        03620000
                       MOVE 999.9 TO CL-CHANGE-PERCENT.                 03630000
                                                                        03640000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       03650000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            03660000
           PERFORM 225-WRITE-REPORT-LINE.                               03670000
                                                                        03680000
           *> ADD THIS CUSTOMERS SALES TO THE BRANCH TOTALS             03690000
           ADD CM-SALES-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.              03700000
           ADD CM-SALES-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.              03710000
                                                                        03720000
      **************************************************************    03730000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    03740000
      * FOR EVERY PAGE                                             *    03750000
      **************************************************************    03760000
       225-WRITE-REPORT-LINE.                                           03770000
           WRITE PRINT-AREA.                                            03780000
           ADD 1 TO LINE-COUNT.                                         03790000
                                                                        03800000
      **************************************************************    03810000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    03820000
      * FOR EVERY PAGE                                             *    03830000
      **************************************************************    03840000
       230-PRINT-HEADING-LINES.                                         03850000
                                                                        03860000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             03870000
           *> SO WE INCREASE THE PAGE COUNT HERE                        03880000
           ADD 1 TO PAGE-COUNT.                                         03890000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      03900000
                                                                        03910000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 03920000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           03930000
           WRITE PRINT-AREA.                                            03940000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           03950000
           WRITE PRINT-AREA.                                            03960000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           03970000
           WRITE PRINT-AREA.                                            03980000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           03990000
           WRITE PRINT-AREA.                                            04000000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           04010000
           WRITE PRINT-AREA.                                            04020000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           04030000
           WRITE PRINT-AREA.                                            04040000
                                                                        04050000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    04060000
           *> OF A NEW PAGE                                             04070000
           MOVE ZERO TO LINE-COUNT.                                     04080000
                                                                        04090000
      **************************************************************    04100000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    04110000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    04120000
      **************************************************************    04130000
       240-PRINT-BRANCH-LINE.                                           04140000
                                                                        04150000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           04160000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            04170000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            04180000
                                                                        04190000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            04200000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       04210000
           COMPUTE CHANGE-AMOUNT =                                      04220000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           04230000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     04240000
                                                                        04250000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                04260000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        04270000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              04280000
               MOVE 999.9 TO BTL-CHANGE-PERCENT                         04290000
           ELSE                                                         04300000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     04310000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          04320000
                   ON SIZE ERROR                                        04330000
                       MOVE 999.9 TO BTL-CHANGE-PERCENT.                04340000
                                                                        04350000
           *> PRINT BRANCH LINE                                         04360000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        04370000
           PERFORM 225-WRITE-REPORT-LINE.                               04380000
                                                                        04390000
           *> WRITE A BLANK SPACER LINE                                 04400000
           MOVE SPACES TO PRINT-AREA.                                   04410000
           PERFORM 225-WRITE-REPORT-LINE.                               04420000
                                                                        04430000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 04440000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           04450000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           04460000
                                                                        04470000
           *> ZERO OUT THE BRANCH TOTALS                                04480000
           MOVE ZERO TO BRANCH-TOTAL-THIS-YTD.                          04490000
           MOVE ZERO TO BRANCH-TOTAL-LAST-YTD.                          04500000
                                                                        04510000
      **************************************************************    04520000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    04530000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    04540000
      * BEEN PRINTED                                               *    04550000
      **************************************************************    04560000
       300-PRINT-GRAND-TOTALS.                                          04570000
                                                                        04580000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                04590000
           *> OUTPUT LINE FOR GRAND TOTALS                              04600000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             04610000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             04620000
                                                                        04630000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             04640000
           COMPUTE CHANGE-AMOUNT =                                      04650000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             04660000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     04670000
                                                                        04680000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             04690000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   04700000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           04710000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          04720000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               04730000
               MOVE 999.9 TO GTL-CHANGE-PERCENT                         04740000
           ELSE                                                         04750000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     04760000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           04770000
                   ON SIZE ERROR                                        04780000
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.                04790000
                                                                        04800000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  04810000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         04820000
           PERFORM 225-WRITE-REPORT-LINE.                               04830000
