      ******************************************************************
      * Author:KarolPat
      * Date:20.09.2018
      * Purpose:Recruitment
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DateShift.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-IN.
            02 WS-IN-DATE.
                05 WS-IN-YEAR     PIC 9(4).
                05 WS-IN-MONTH    PIC 9(2).
                05 WS-IN-DAY      PIC 9(2).
            02 WS-IN-DAYS         PIC 9(4).

       01  WS-GL.
            02 WS-GL-DATE.
                05 WS-GL-YEAR     PIC 9(4).
                05 WS-GL-MONTH    PIC 9(2).
                05 WS-GL-DAY      PIC 9(2).
            02 WS-GL-DAYS         PIC 9(4) COMP-3.
            02 WS-GL-DAYS-TO-SUBT PIC 9(2) COMP-3.
            02 WS-GL-MON-CAPACITY PIC 9(2) COMP-3.
            02 WS-GL-REMINDER     PIC 9(2) COMP-3.

       01  SW-DAYS-IN-MONTH   PIC 9(2).
            88 31-DAYS VALUES 1,3,5,7,8,10,12.
            88 30-DAYS VALUE  4,6,9,11.

       01  CT.
            02 CT-DIVISOR         PIC 9(1) VALUE 4.
            02 CT-ERR             PIC X(1).
                88 CT-ERR-DAY     VALUE "D".
                88 CT-ERR-MONTH   VALUE "M".

       01  WS-OUT.
            02 WS-OUT-DATE.
                05 WS-OUT-YEAR     PIC 9(4).
                05 FILLER          PIC X(3) VALUE " - ".
                05 WS-OUT-MONTH    PIC 9(2).
                05 FILLER          PIC X(3) VALUE " - ".
                05 WS-OUT-DAY      PIC 9(2).


       PROCEDURE DIVISION.

           PERFORM 1000-INIT
           PERFORM 2000-PROCESS
           PERFORM 3000-FINISH.

       1000-INIT.

           INITIALIZE WS-IN
                      WS-GL
                      WS-OUT.

           PERFORM 1100-RETRIEVE-DATA
           PERFORM 1200-MOVE-DATA-TO-GLOBAL-VARS
           PERFORM 1300-VALIDATE-DATA.

       1100-RETRIEVE-DATA.

            DISPLAY "ENTER DATE (YYYYMMDD)"
            ACCEPT WS-IN-DATE
            DISPLAY "ENTER NUMBER OF DAYS TO SHIFT THE DATE"
            ACCEPT WS-IN-DAYS.

       1200-MOVE-DATA-TO-GLOBAL-VARS.

           MOVE WS-IN-DATE TO WS-GL-DATE
           MOVE WS-IN-DAYS TO WS-GL-DAYS.

       1300-VALIDATE-DATA.

           IF WS-GL-MONTH > 12 THEN
             SET CT-ERR-MONTH TO TRUE
             PERFORM 9999-ABEND
           END-IF

           IF WS-GL-DAY = 0 THEN
             SET CT-ERR-DAY TO TRUE
             PERFORM 9999-ABEND
           END-IF.

       2000-PROCESS.

           PERFORM 2100-SHIFT-DAYS
               UNTIL WS-GL-DAYS = 0

           PERFORM 2200-WRITE-OUTPUT.


       2100-SHIFT-DAYS.

           MOVE WS-GL-MONTH TO SW-DAYS-IN-MONTH
      *    GET THE REMINDER TO CHECK WHETHER YEAR IS A LEAP YEAR
           COMPUTE WS-GL-REMINDER = FUNCTION MOD(WS-GL-YEAR, CT-DIVISOR)

           PERFORM 2110-CHECK-NUMBER-OF-DAYS
           PERFORM 2120-SHIFT-DATE.

       2110-CHECK-NUMBER-OF-DAYS.

           EVALUATE TRUE
               WHEN 31-DAYS
                   MOVE 31 TO WS-GL-MON-CAPACITY
               WHEN 30-DAYS
                   MOVE 30 TO WS-GL-MON-CAPACITY
               WHEN OTHER
                   IF(WS-GL-REMINDER = 0) THEN
                       MOVE 29 TO WS-GL-MON-CAPACITY
                   ELSE
                       MOVE 28 TO WS-GL-MON-CAPACITY
                    END-IF
           END-EVALUATE.

       2120-SHIFT-DATE.

           IF(WS-GL-MON-CAPACITY = WS-GL-DAY) THEN
               PERFORM 2121-CHECK-MONTH
               MOVE 1 TO WS-GL-DAY
               SUBTRACT 1 FROM WS-GL-DAYS
           ELSE IF(WS-GL-MON-CAPACITY<WS-GL-DAY) THEN
               SET CT-ERR-DAY TO TRUE
               PERFORM 9999-ABEND
           ELSE
               SUBTRACT WS-GL-DAY FROM WS-GL-MON-CAPACITY
                   GIVING WS-GL-DAYS-TO-SUBT
               IF(WS-GL-DAYS-TO-SUBT < WS-GL-DAYS) THEN
                  SUBTRACT WS-GL-DAYS-TO-SUBT FROM WS-GL-DAYS
                  ADD WS-GL-DAYS-TO-SUBT TO WS-GL-DAY
                  IF (WS-GL-DAYS>0) THEN
                      SUBTRACT 1 FROM WS-GL-DAYS
                      MOVE 1 TO WS-GL-DAY
                      PERFORM 2121-CHECK-MONTH
                  END-IF
               ELSE IF (WS-GL-DAYS-TO-SUBT = WS-GL-DAYS) THEN
                   MOVE WS-GL-MON-CAPACITY TO WS-GL-DAY
                   MOVE 0 TO WS-GL-DAYS
               ELSE IF (WS-GL-DAYS-TO-SUBT > WS-GL-DAYS) THEN
                   ADD WS-GL-DAYS TO WS-GL-DAY
                   MOVE 0 TO WS-GL-DAYS
               END-IF
           END-IF.

       2121-CHECK-MONTH.

           IF (WS-GL-MONTH = 12) THEN
               ADD 1 TO WS-GL-YEAR
               MOVE 1 TO WS-GL-MONTH
           ELSE IF(WS-GL-MONTH < 12) THEN
               ADD 1 TO WS-GL-MONTH
           END-IF.

       2200-WRITE-OUTPUT.

           MOVE WS-GL-YEAR TO WS-OUT-YEAR
           MOVE WS-GL-MONTH TO WS-OUT-MONTH
           MOVE WS-GL-DAY TO WS-OUT-DAY.

       3000-FINISH.

           DISPLAY "YYYY - MM - DD"
           DISPLAY WS-OUT-DATE
           STOP RUN.

      *    ERRORS HANDLING

       9999-ABEND.

           EVALUATE TRUE
               WHEN CT-ERR-DAY
                   DISPLAY "WRONG INPUT ON DAY " WS-GL-DAY
               WHEN CT-ERR-MONTH
                   DISPLAY "WRONG INPUT ON MONTH " WS-GL-MONTH
           END-EVALUATE
           STOP RUN.

       END PROGRAM DateShift.
