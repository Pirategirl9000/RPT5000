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
           05  FILLER      PIC X(8)    VALUE "BRANCH  ".                01640000
           05  FILLER      PIC X(8)    VALUE "SLSREP  ".                01650004
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
           05  FILLER      PIC X(8)    VALUE " NUM    ".                01780000
           05  FILLER      PIC X(9)    VALUE " NUM     ".               01790004
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
           05  FILLER              PIC X(4)     VALUE SPACE.            02020000
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            02030000
           05  FILLER              PIC X(2)     VALUE SPACE.            02040000
           05  CL-CUSTOMER-NAME    PIC X(20).                           02050000
           05  FILLER              PIC X(3)     VALUE SPACE.            02060000
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      02070000
           05  FILLER              PIC X(4)     VALUE SPACE.            02080000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      02090000
           05  FILLER              PIC X(4)     VALUE SPACE.            02100000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      02110000
           05  FILLER              PIC X(3)     VALUE SPACE.            02120000
           05  CL-CHANGE-PERCENT   PIC ZZ9.9-.                          02130000
           05  FILLER              PIC X(47)    VALUE SPACE.            02140000
                                                                        02150000
      **************************************************************    02160000
      * STORES THE BRANCH TOTAL LINE                               *    02170000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02180000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02190000
      * USED FOR OUTPUTTING                                        *    02200000
      **************************************************************    02210000
       01  BRANCH-TOTAL-LINE.                                           02220000
           05  FILLER              PIC X(23)    VALUE SPACE.            02230000
           05  FILLER              PIC X(14)    VALUE "BRANCH TOTAL".   02240000
           05  BTL-SALES-THIS-YTD  PIC ZZZ,ZZ9.99-.                     02250000
           05  FILLER              PIC X(3)     VALUE SPACE.            02260000
           05  BTL-SALES-LAST-YTD  PIC ZZZ,ZZ9.99-.                     02270000
           05  FILLER              PIC X(3)     VALUE SPACE.            02280000
           05  BTL-CHANGE-AMOUNT   PIC ZZZ,ZZ9.99-.                     02290000
           05  FILLER              PIC X(3)     VALUE SPACE.            02300000
           05  BTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02310000
           05  FILLER              PIC X(48)    VALUE " **".            02320005
                                                                        02330000
      **************************************************************    02340005
      * STORES THE SALES REP TOTAL LINE                            *    02350005
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02360005
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02370005
      * USED FOR OUTPUTTING                                        *    02380005
      **************************************************************    02390005
       01  SALESREP-TOTAL-LINE.                                         02400011
           05  FILLER              PIC X(23)    VALUE SPACE.            02410005
           05  FILLER              PIC X(14)    VALUE "SALESREP TOTAL". 02420005
           05  STL-SALES-THIS-YTD  PIC ZZZ,ZZ9.99-.                     02430009
           05  FILLER              PIC X(3)     VALUE SPACE.            02440005
           05  STL-SALES-LAST-YTD  PIC ZZZ,ZZ9.99-.                     02450009
           05  FILLER              PIC X(3)     VALUE SPACE.            02460005
           05  STL-CHANGE-AMOUNT   PIC ZZZ,ZZ9.99-.                     02470009
           05  FILLER              PIC X(3)     VALUE SPACE.            02480005
           05  STL-CHANGE-PERCENT  PIC ZZ9.9-.                          02490009
           05  FILLER              PIC X(48)    VALUE " *".             02500005
      **************************************************************    02510000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02520000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02530000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02540000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02550000
      **************************************************************    02560000
       01  GRAND-TOTAL-LINE.                                            02570000
           05  FILLER              PIC X(23)    VALUE SPACE.            02580000
           05  FILLER              PIC X(12)    VALUE "GRAND TOTAL ".   02590000
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   02600000
           05  FILLER              PIC X(1)     VALUE SPACE.            02610000
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   02620000
           05  FILLER              PIC X        VALUE SPACE.            02630000
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   02640000
           05  FILLER              PIC X(3)     VALUE SPACE.            02650000
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02660000
           05  FILLER              PIC X(43)    VALUE " ***".           02670005
                                                                        02680000
       PROCEDURE DIVISION.                                              02690000
                                                                        02700000
      **************************************************************    02710000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    02720000
      * READING AND WRITING TO AND FROM THEM                       *    02730000
      **************************************************************    02740000
       000-PREPARE-SALES-REPORT.                                        02750000
                                                                        02760000
           OPEN INPUT  CUSTMAST                                         02770000
                OUTPUT ORPT5000.                                        02780001
                                                                        02790000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   02800000
           *> THE HEADER LINES                                          02810000
           PERFORM 100-FORMAT-REPORT-HEADING.                           02820000
                                                                        02830000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     02840000
           *> THE END OF THE INPUT FILE                                 02850000
           PERFORM 200-PREPARE-SALES-LINES                              02860000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         02870000
                                                                        02880000
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                02890000
           PERFORM 300-PRINT-GRAND-TOTALS.                              02900000
                                                                        02910000
           CLOSE CUSTMAST                                               02920000
                 ORPT5000.                                              02930001
           STOP RUN.                                                    02940000
                                                                        02950000
      **************************************************************    02960000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    02970000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    02980000
      **************************************************************    02990000
       100-FORMAT-REPORT-HEADING.                                       03000000
                                                                        03010000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03020000
                                                                        03030000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03040000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03050000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03060000
           MOVE CD-MONTH   TO HL1-MONTH.                                03070000
           MOVE CD-DAY     TO HL1-DAY.                                  03080000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03090000
           MOVE CD-HOURS   TO HL2-HOURS.                                03100000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03110000
                                                                        03120000
      **************************************************************    03130000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03140000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03150000
      * TERMINATING LINE OF THE FILE                               *    03160000
      **************************************************************    03170000
       200-PREPARE-SALES-LINES.                                         03180000
                                                                        03190000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03200000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03210000
                                                                        03220000
           *> PERFORMS DUTIES BASED ON THE ENTRY                        03230006
           *>  * IF WE RUN OUT OF DATA PRINT THE SALES AND BRANCH TOTALS03240006
           *>  * IF IT'S THE FIRST RECORD PRINT THE CUSTOMER LINE AND   03250006
           *>    STORE THE CURRENT SALESREP AND BRANCH NUMBER TO THE OLD03260006
           *>  * IF THE BRANCH NUMBER IS GREATER THAN THE CURRENT ONE   03270006
           *>    THEN PRINT THE SALES REP LINE, BRANCH TOTAL LINE, AND  03280006
           *>    THEN THE NEW CUSTOMER'S LINE. AFTER UPDATE THE BRANCH  03290006
           *>    AND SALESREP NUMBERS                                   03300006
           *>  * IF THE SALES REP NUMBER IS GREATER THAN THE CURRENT ONE03310006
           *>    PRINT SALES LINE THEN THE CURRENT CUSTOMER LINE AFTER  03320006
           *>    UPDATE THE SALES REP NUMBER                            03330006
           *>  * IF NOTHING ELSE JUST PRINT THE CUSTOMER RECORD         03340006
           EVALUATE TRUE                                                03350006
               WHEN CUSTMAST-EOF                                        03360006
                   PERFORM 250-PRINT-SALESREP-LINE                      03370006
                   PERFORM 240-PRINT-BRANCH-LINE                        03380006
               WHEN FIRST-RECORD                                        03390006
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03400006
                   MOVE "N" TO FIRST-RECORD-SWITCH                      03410006
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03420006
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03430006
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03440006
                   PERFORM 250-PRINT-SALESREP-LINE                      03450006
                   PERFORM 240-PRINT-BRANCH-LINE                        03460006
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03470006
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03480006
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03490006
               WHEN CM-SALESREP-NUMBER > OLD-SALESREP-NUMBER            03500006
                   PERFORM 250-PRINT-SALESREP-LINE                      03510006
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03520006
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03530006
               WHEN OTHER                                               03540006
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03550006
           END-EVALUATE.                                                03560006
                                                                        03570006
      **************************************************************    03580000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03590000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03600000
      **************************************************************    03610000
       210-READ-CUSTOMER-RECORD.                                        03620000
                                                                        03630000
           READ CUSTMAST                                                03640000
               AT END                                                   03650000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03660000
                                                                        03670000
      **************************************************************    03680000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03690000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    03700000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    03710000
      **************************************************************    03720000
       220-PRINT-CUSTOMER-LINE.                                         03730000
                                                                        03740000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     03750000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   03760000
           IF LINE-COUNT >= LINES-ON-PAGE                               03770000
               PERFORM 230-PRINT-HEADING-LINES.                         03780000
                                                                        03790000
           *> IF THIS IS THE FIRST RECORD OR THE FIRST RECORD OF THIS   03800000
           *> BRANCH THEN WE MOVE THE BRANCH NUMBER TO BE PRINTED       03810000
           *> OTHERWISE WE MOVE SPACES TO THE BRANCH NUMBER ITEM        03820000
           IF FIRST-RECORD-SWITCH = "Y"                                 03830000
               MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER                03840000
           ELSE                                                         03850000
               IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                  03860000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            03870000
               ELSE                                                     03880000
                   MOVE SPACES TO CL-BRANCH-NUMBER.                     03890000
                                                                        03900000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         03910000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     03920000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              03930000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                03940000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               03950000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               03960000
                                                                        03970000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    03980000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      03990000
           COMPUTE CHANGE-AMOUNT =                                      04000000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   04010000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      04020000
                                                                        04030000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     04040000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        04050000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        04060000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          04070000
           IF CM-SALES-LAST-YTD = ZERO                                  04080000
               MOVE 999.9 TO CL-CHANGE-PERCENT                          04090000
           ELSE                                                         04100000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04110000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              04120000
                   ON SIZE ERROR                                        04130000
                       MOVE 999.9 TO CL-CHANGE-PERCENT.                 04140000
                                                                        04150000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       04160000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            04170000
           PERFORM 225-WRITE-REPORT-LINE.                               04180000
                                                                        04190000
           *> ADD THIS CUSTOMERS SALES TO THE BRANCH TOTALS             04200000
           ADD CM-SALES-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.              04210000
           ADD CM-SALES-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.              04220000
                                                                        04230000
      **************************************************************    04240000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04250000
      * FOR EVERY PAGE                                             *    04260000
      **************************************************************    04270000
       225-WRITE-REPORT-LINE.                                           04280000
           WRITE PRINT-AREA.                                            04290000
           ADD 1 TO LINE-COUNT.                                         04300000
                                                                        04310000
      **************************************************************    04320000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04330000
      * FOR EVERY PAGE                                             *    04340000
      **************************************************************    04350000
       230-PRINT-HEADING-LINES.                                         04360000
                                                                        04370000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             04380000
           *> SO WE INCREASE THE PAGE COUNT HERE                        04390000
           ADD 1 TO PAGE-COUNT.                                         04400000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      04410000
                                                                        04420000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 04430000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           04440000
           WRITE PRINT-AREA.                                            04450000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           04460000
           WRITE PRINT-AREA.                                            04470000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           04480000
           WRITE PRINT-AREA.                                            04490000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           04500000
           WRITE PRINT-AREA.                                            04510000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           04520000
           WRITE PRINT-AREA.                                            04530000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           04540000
           WRITE PRINT-AREA.                                            04550000
                                                                        04560000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    04570000
           *> OF A NEW PAGE                                             04580000
           MOVE ZERO TO LINE-COUNT.                                     04590000
                                                                        04600000
      **************************************************************    04610000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    04620000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    04630000
      **************************************************************    04640000
       240-PRINT-BRANCH-LINE.                                           04650000
                                                                        04660000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           04670000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            04680000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            04690000
                                                                        04700000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            04710000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       04720000
           COMPUTE CHANGE-AMOUNT =                                      04730000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           04740000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     04750000
                                                                        04760000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                04770000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        04780000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              04790000
               MOVE 999.9 TO BTL-CHANGE-PERCENT                         04800000
           ELSE                                                         04810000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     04820000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          04830000
                   ON SIZE ERROR                                        04840000
                       MOVE 999.9 TO BTL-CHANGE-PERCENT.                04850000
                                                                        04860000
           *> PRINT BRANCH LINE                                         04870000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        04880000
           PERFORM 225-WRITE-REPORT-LINE.                               04890000
                                                                        04900000
           *> WRITE A BLANK SPACER LINE                                 04910000
           MOVE SPACES TO PRINT-AREA.                                   04920000
           PERFORM 225-WRITE-REPORT-LINE.                               04930000
                                                                        04940000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 04950000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           04960000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           04970000
                                                                        04980000
           *> ZERO OUT THE BRANCH TOTALS                                04990000
           MOVE ZERO TO BRANCH-TOTAL-THIS-YTD.                          05000000
           MOVE ZERO TO BRANCH-TOTAL-LAST-YTD.                          05010000
                                                                        05020000
      **************************************************************    05030006
      * PRINTS THE CURRENT SALESREP'S TOTALS, RAN ONCE FOR EVERY   *    05040006
      * SALESREP. ALSO CALCULATES THE CHANGE BETWEEN YEARS         *    05050006
      **************************************************************    05060006
       250-PRINT-SALESREP-LINE.                                         05070006
                                                                        05080006
           *> MOVE THE SALESREP TOTALS TO THE SALESREP TOTAL LINE       05090010
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          05100010
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          05110010
                                                                        05120006
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05130006
           *> FOR THE CURRENT SALESREP AND ADD IT TO THE TOTAL LINE     05140010
           COMPUTE CHANGE-AMOUNT =                                      05150006
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       05160010
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     05170010
                                                                        05180006
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05190006
           *> THEN MOVE TO THE SALESREP TOTAL LINE                      05200010
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            05210010
               MOVE 999.9 TO STL-CHANGE-PERCENT                         05220010
           ELSE                                                         05230006
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     05240010
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        05250010
                   ON SIZE ERROR                                        05260006
                       MOVE 999.9 TO STL-CHANGE-PERCENT.                05270010
                                                                        05280006
           *> PRINT SALESREP LINE                                       05290010
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      05300010
           PERFORM 225-WRITE-REPORT-LINE.                               05310006
                                                                        05320006
           *> WRITE A BLANK SPACER LINE                                 05330006
           MOVE SPACES TO PRINT-AREA.                                   05340006
           PERFORM 225-WRITE-REPORT-LINE.                               05350006
                                                                        05360006
           *> ADD THE SALESREP TOTALS TO THE BRANCH TOTALS              05370010
           *> WHEN A BRANCH IS PRINTED THEN THOSE TOTALS ARE MOVED      05380010
           *> TO THE GRAND TOTALS                                       05390010
           *> CUSTOMER->SALESREP->BRANCH->GRAND-TOTAL                   05400010
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.        05410010
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.        05420010
                                                                        05430006
           *> ZERO OUT THE SALESREP TOTALS                              05440010
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        05450010
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        05460010
      **************************************************************    05470000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    05480000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    05490000
      * BEEN PRINTED                                               *    05500000
      **************************************************************    05510000
       300-PRINT-GRAND-TOTALS.                                          05520000
                                                                        05530000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                05540000
           *> OUTPUT LINE FOR GRAND TOTALS                              05550000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             05560000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             05570000
                                                                        05580000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             05590000
           COMPUTE CHANGE-AMOUNT =                                      05600000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             05610000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     05620000
                                                                        05630000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             05640000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   05650000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           05660000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          05670000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               05680000
               MOVE 999.9 TO GTL-CHANGE-PERCENT                         05690000
           ELSE                                                         05700000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     05710000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           05720000
                   ON SIZE ERROR                                        05730000
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.                05740000
                                                                        05750000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  05760000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         05770000
           PERFORM 225-WRITE-REPORT-LINE.                               05780000
