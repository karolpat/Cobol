      ******************************************************************
      * Author: KarolPat
      * Date: 21.09.2018
      * Purpose:Recruitment
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Reports.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           SELECT INPUT-RECORD ASSIGN TO WS-INPUT-FILE-NAME
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS INPUT-STATUS.

           SELECT FIRST-REPORT ASSIGN TO "first_rep.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS REP1-STATUS.

           SELECT SECOND-REPORT ASSIGN TO "second_rep.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS REP2-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  INPUT-RECORD.
       01  INPUT-FILE PIC X(40).

       FD  FIRST-REPORT.
       01  FIRST-REPORT-RECORD.
            05 REP1-OUT-NAME   PIC X(20).
            05 FILLER          PIC X(3) VALUE SPACES.
            05 REP1-OUT-AMOU   PIC Z(7).9(2).

       FD  SECOND-REPORT.
       01  SECOND-REPORT-RECORD.
            05 REP2-OUT-AMOU   PIC Z(7).9(2).
            05 FILLER          PIC X(3) VALUE SPACES.
            05 REP2-OUT-CURR   PIC X(3).

       WORKING-STORAGE SECTION.
      ******************************************************************
      *    TABLES
      ******************************************************************
      *    INPUT FILE TABLE
       01  WS-TABLE-INP.
            05 TABLE-INP-ENTRY    OCCURS 100 TIMES
                                 INDEXED BY TABLE-IN-INDEX.
                10 IN-RECORD.
                    15 IN-NAME    PIC X(20).
                    15 IN-AMOUNT  PIC 9(5),9(2).
                    15 IN-CURREN  PIC X(3).
                    15 IN-EXRATE  PIC 9(1),9(3).

      *    TABLE FOR THE FIRST REPORT - NAMES AND AMOUNT IN PLN

       01  WS-TABLE-REP1.
            05 TABLE-REP1-ENTRY   OCCURS 1 TO 100 TIMES
                                   DEPENDING ON IN-NAMES-COUNTER.
                10 REP1-RECORD.
                    15 REP1-NAME    PIC X(20).
                    15 REP1-AMOU    PIC 9(7)V9(2).

      *    TABLE FOR THE SECOND REPORT - CURRENCY AND AMOUNT

       01  WS-TABLE-REP2.
            05 TABLE-REP2-ENTRY   OCCURS 10 TIMES
                                  INDEXED BY TABLE-REP2-INDEX.
                10 REP2-RECORD.
                    15 REP2-CURR  PIC X(3).
                    15 REP2-AMOU  PIC 9(7)V9(4).

       01  WS-IN-EOF             PIC A(1).
      *    COUNTER DEFINING REP2-TABLE SIZE
       01  IN-NAMES-COUNTER      PIC 9(3).

      ******************************************************************
      *    FILES STATUSES
      ******************************************************************

       01  WS-FILE-STATUS.
            05 INPUT-STATUS          PIC X(2).
            05 REP1-STATUS           PIC X(2).
            05 REP2-STATUS           PIC X(2).

       01  SW-FILE-STATUSES          PIC X(2).
            88 RECORD-OK                       VALUE "00".
            88 AT-END                          VALUE "10".
            88 NO-FILE                         VALUE "35" "05".

      ******************************************************************
      *    GLOAL VARIABLES AND TEMPORARY VARIABLES
      ******************************************************************

       01  WS-TEMP-RECORD           PIC X(40).
       01  WS-INPUT-FILE-NAME       PIC X(60).
       01  READ-COUNTER             PIC 9(2).

       01  WS-TEMP-REP1.
            05 WS-TEMP-REP1-NAME    PIC X(20).
            05 WS-TEMP-REP1-AMOU    PIC 9(7)V9(2) COMP-3.
            05 WS-TEMP-REP1-EXRATE  PIC 9(1)V9(3) COMP-3.

       01  WS-TEMP-REP2.
            05 WS-TEMP-REP2-CURR    PIC X(3).
            05 WS-TEMP-REP2-AMOU    PIC 9(7)V9(2) COMP-3.

      *    LOOPS ITERATORS

       01  WS-GL-ITER.
            05  WS-I                PIC 9(3).
            05  WS-J                PIC 9(3).

       01  CT-ERR                   PIC X(1).
            88 CT-ERR-INPUT                  VALUE "I".
            88 CT-ERR-EMPTY                  VALUE "E".
            88 CT-ERR-BOUND                  VALUE "B".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 1000-INIT
           PERFORM 2000-PROCESS
           PERFORM 3000-FINISH.

       1000-INIT.

           INITIALIZE
                       READ-COUNTER
                       WS-TABLE-REP1
                       WS-TABLE-REP2
                       WS-TABLE-INP
                       WS-TEMP-RECORD
                       WS-TEMP-REP1
                       WS-TEMP-REP2
                       IN-NAMES-COUNTER
                       WS-FILE-STATUS
                       WS-INPUT-FILE-NAME
                       INPUT-FILE
                       FIRST-REPORT-RECORD
                       SECOND-REPORT-RECORD.


           PERFORM 1100-RETRIEVE-INPUT-FILE-NAME
           PERFORM 1200-OPEN-FILE.

       1100-RETRIEVE-INPUT-FILE-NAME.

           DISPLAY "ENTER NAME OF THE INPUT FILE"
           ACCEPT WS-INPUT-FILE-NAME
           END-ACCEPT.

       1200-OPEN-FILE.

           SET TABLE-IN-INDEX TO 1
           MOVE ZERO TO READ-COUNTER

           OPEN INPUT INPUT-RECORD.
               MOVE INPUT-STATUS TO SW-FILE-STATUSES
               IF NO-FILE THEN
                 SET CT-ERR-INPUT TO TRUE
                 PERFORM 9999-ABEND
               ELSE
                 PERFORM 2100-READ-FILE UNTIL AT-END
                 IF READ-COUNTER = 0 THEN
                   SET CT-ERR-EMPTY TO TRUE
                   CLOSE INPUT-RECORD
                   PERFORM 9999-ABEND
                 END-IF
           CLOSE INPUT-RECORD.

       2000-PROCESS.

           PERFORM 2200-SORT-TABLE-NAMES
           PERFORM 2300-GET-NAME-N-AMOUNT
           PERFORM 2400-SORT-TABLE-CURRENCY
           PERFORM 2500-GET-CURRENCY-N-AMOUNT
           PERFORM 2600-WRITE-OUTPUT-REPORTS.

       2100-READ-FILE.

           READ INPUT-RECORD INTO TABLE-INP-ENTRY(TABLE-IN-INDEX)
               NOT AT END
      *    ABEND WHEN THERE ARE MORE THAN 100 RECORDS IN INPUT FILE
                 IF TABLE-IN-INDEX = 100 THEN
                   SET CT-ERR-BOUND TO TRUE
                   CLOSE INPUT-RECORD
                   PERFORM 9999-ABEND
                 ELSE
                   SET TABLE-IN-INDEX UP BY 1
                   ADD 1 TO READ-COUNTER
                 END-IF
               AT END SET AT-END TO TRUE
           END-READ.

      *    SORTS TABLE BY NAME FOR THE FIRST REPORT

       2200-SORT-TABLE-NAMES.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > TABLE-IN-INDEX

               PERFORM VARYING WS-J FROM WS-I BY 1
                   UNTIL WS-J > TABLE-IN-INDEX

                   IF(IN-NAME(WS-I) >
                       IN-NAME(WS-J)) THEN
                       MOVE TABLE-INP-ENTRY(WS-I) TO WS-TEMP-RECORD
                       MOVE TABLE-INP-ENTRY(WS-J) TO
                           TABLE-INP-ENTRY(WS-I)
                       MOVE WS-TEMP-RECORD TO TABLE-INP-ENTRY(WS-J)
                   END-IF

               END-PERFORM

           END-PERFORM.

      *    PREPARES DATA FOR THE FIRST REPORT

       2300-GET-NAME-N-AMOUNT.

           MOVE SPACES TO WS-TEMP-REP1-NAME
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > TABLE-IN-INDEX

               IF(IN-NAME(WS-I)NOT EQUAL WS-TEMP-REP1-NAME) THEN
                   ADD 1 TO IN-NAMES-COUNTER
                   MOVE IN-NAME(WS-I) TO WS-TEMP-REP1-NAME
                   MOVE IN-NAME(WS-I) TO REP1-NAME(IN-NAMES-COUNTER)
                   PERFORM 2310-MATCH-NAME-WITH-AMOUNT
               ELSE

                   IF(WS-TEMP-REP1-NAME NOT EQUAL SPACES) THEN
                       PERFORM 2310-MATCH-NAME-WITH-AMOUNT
                   END-IF

               END-IF

           END-PERFORM.

      *    MATCHES AMOUNT WITH NAME. IN CASE IT IS PLN JUST ADD
      *    WHEN OTHER, MULTIPLIES AMOUNT AND EXCHANGE RATE AND ADDS

       2310-MATCH-NAME-WITH-AMOUNT.

           IF(IN-CURREN(WS-I) EQUALS "PLN") THEN
               MOVE IN-AMOUNT(WS-I) TO WS-TEMP-REP1-AMOU
               ADD WS-TEMP-REP1-AMOU TO REP1-AMOU(IN-NAMES-COUNTER)
           ELSE
               MOVE IN-AMOUNT(WS-I) TO WS-TEMP-REP1-AMOU
               MOVE IN-EXRATE(WS-I) TO WS-TEMP-REP1-EXRATE
               MULTIPLY WS-TEMP-REP1-AMOU BY WS-TEMP-REP1-EXRATE
                   GIVING WS-TEMP-REP1-AMOU
               ADD WS-TEMP-REP1-AMOU TO REP1-AMOU(IN-NAMES-COUNTER)
           END-IF.

      *    SORTS TABLE BY CURRENCY FOR THE SECOND REPORT PURPOSE

       2400-SORT-TABLE-CURRENCY.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > TABLE-IN-INDEX

               PERFORM VARYING WS-J FROM WS-I BY 1
                   UNTIL WS-J > TABLE-IN-INDEX

                   IF(IN-CURREN(WS-I) >
                       IN-CURREN(WS-J)) THEN
                       MOVE TABLE-INP-ENTRY(WS-I) TO WS-TEMP-RECORD
                       MOVE TABLE-INP-ENTRY(WS-J) TO
                           TABLE-INP-ENTRY(WS-I)
                       MOVE WS-TEMP-RECORD TO TABLE-INP-ENTRY(WS-J)
                   END-IF

               END-PERFORM

           END-PERFORM.

      *    PREPARES DATA FOR THE SECOND REPORT

       2500-GET-CURRENCY-N-AMOUNT.

           MOVE SPACES TO WS-TEMP-REP2-CURR.
           SET TABLE-REP2-INDEX TO 0
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > TABLE-IN-INDEX

               IF(IN-CURREN(WS-I) NOT EQUAL WS-TEMP-REP2-CURR) THEN
                   SET TABLE-REP2-INDEX UP BY 1
                   MOVE IN-CURREN(WS-I) TO WS-TEMP-REP2-CURR
                   MOVE IN-CURREN(WS-I) TO REP2-CURR(TABLE-REP2-INDEX)
                   MOVE IN-AMOUNT(WS-I) TO REP2-AMOU(TABLE-REP2-INDEX)
               ELSE
                   IF(WS-TEMP-REP2-CURR NOT EQUAL SPACES) THEN
                       MOVE IN-AMOUNT(WS-I) TO WS-TEMP-REP2-AMOU
                       ADD WS-TEMP-REP2-AMOU
                           TO REP2-AMOU(TABLE-REP2-INDEX)
                   END-IF
               END-IF

           END-PERFORM.

       2600-WRITE-OUTPUT-REPORTS.

           PERFORM 2610-WRITE-FIRST-REPORT
           PERFORM 2620-WRITE-SECOND-REPORT.

       2610-WRITE-FIRST-REPORT.

           DISPLAY "WRITING FIRST REPORT"
           OPEN OUTPUT FIRST-REPORT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >IN-NAMES-COUNTER
             MOVE REP1-NAME(WS-I)   TO REP1-OUT-NAME
             MOVE REP1-AMOU(WS-I) TO REP1-OUT-AMOU
             WRITE FIRST-REPORT-RECORD
           END-PERFORM

           CLOSE FIRST-REPORT
           DISPLAY "WRITING FIRST REPORT FINISHED".

       2620-WRITE-SECOND-REPORT.

           DISPLAY "WRITING SECOND REPORT"
           OPEN OUTPUT SECOND-REPORT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >TABLE-REP2-INDEX
             MOVE REP2-CURR(WS-I)   TO REP2-OUT-CURR
             MOVE REP2-AMOU(WS-I) TO REP2-OUT-AMOU
             WRITE SECOND-REPORT-RECORD
           END-PERFORM

           CLOSE SECOND-REPORT
           DISPLAY "WRITING SECOND REPORT FINISHED".

       3000-FINISH.
            STOP RUN.
       9999-ABEND.

           EVALUATE TRUE
             WHEN CT-ERR-INPUT
               DISPLAY "NO SUCH INPUT FILE"
             WHEN CT-ERR-EMPTY
               DISPLAY "EMPTY INPUT FILE"
             WHEN CT-ERR-BOUND
               DISPLAY "TOO MANY RECORDS IN INPUT FILE"
           END-EVALUATE
           STOP RUN.

       END PROGRAM Reports.
