         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY02P1.
            
         ENVIRONMENT DIVISION. 

         INPUT-OUTPUT SECTION. 
         FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO INPFILE
            ORGANIZATION IS LINE SEQUENTIAL.

         DATA DIVISION.
         FILE SECTION. 
         FD INPUT-FILE.
         01 INPUT-RECORD.
            88 INPUT-END            VALUE LOW-VALUE.
            05 INPUT-TEXT           PIC X(80).
         WORKING-STORAGE SECTION.


         01 WS-SUM                PIC 9(10) VALUE 0.

         01 WS-SIMILARITY         PIC 9(10).

         01 WS-NUMBERS.
            05 WS-NUM1            PIC S9(3) VALUE -1.
            05 WS-NUM2            PIC S9(3) VALUE -1.
            05 WS-NUM3            PIC S9(3) VALUE -1.
            05 WS-NUM4            PIC S9(3) VALUE -1.
            05 WS-NUM5            PIC S9(3) VALUE -1.
            05 WS-NUM6            PIC S9(3) VALUE -1.
            05 WS-NUM7            PIC S9(3) VALUE -1.
            05 WS-NUM8            PIC S9(3) VALUE -1.

         01 WS-NUM-TAB REDEFINES WS-NUMBERS.
            05 WS-NUM             PIC S9(3) OCCURS 8.

         01 WS-DIFF               PIC S9(3).

         01 WS-TREND              PIC X.
            88 TREND-INIT          VALUE ' '.
            88 TREND-UNSAFE        VALUE 'U'.
            88 TREND-SAFE          VALUE 'I' 'D'.
            88 TREND-INCREASE      VALUE 'I'.
            88 TREND-DECREASE      VALUE 'D'.

         01 IX                  PIC 9(3).
         01 IX2                 PIC 9(3).
            
         PROCEDURE DIVISION.
            PERFORM INIT

            DISPLAY WS-SUM

            STOP RUN.

         INIT SECTION.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END SET INPUT-END TO TRUE
           END-READ

           PERFORM UNTIL INPUT-END
               MOVE -1 TO WS-NUM1 WS-NUM2 WS-NUM3 WS-NUM4
                          WS-NUM5 WS-NUM6 WS-NUM7 WS-NUM8
               UNSTRING INPUT-TEXT DELIMITED BY ALL SPACES
                   INTO WS-NUM1, WS-NUM2, WS-NUM3, WS-NUM4
                       ,WS-NUM5, WS-NUM6, WS-NUM7, WS-NUM8

               MOVE 1 TO IX
               MOVE 2 TO IX2 

               SET TREND-INIT TO TRUE
               PERFORM UNTIL IX2 > 8 OR WS-NUM(IX2) = -1 OR TREND-UNSAFE
                 COMPUTE WS-DIFF = WS-NUM(IX2) - WS-NUM(IX)


                 IF TREND-INIT THEN
                    IF WS-DIFF > 0
                       SET TREND-INCREASE    TO TRUE
                    ELSE
                       IF WS-DIFF < 0 THEN
                          SET TREND-DECREASE TO TRUE
                       ELSE
                          SET TREND-UNSAFE   TO TRUE
                       END-IF
                    END-IF
                 END-IF

                 IF TREND-INCREASE THEN
                    IF WS-DIFF >= 1 AND WS-DIFF <= 3
                       CONTINUE
                     ELSE
                       SET TREND-UNSAFE      TO TRUE
                     END-IF
                 END-IF
                 IF TREND-DECREASE THEN
                    IF WS-DIFF <= -1 AND WS-DIFF >= -3
                       CONTINUE
                    ELSE
                       SET TREND-UNSAFE      TO TRUE
                     END-IF
                 END-IF
                 ADD 1 TO IX
                 ADD 1 TO IX2
               END-PERFORM
               IF TREND-SAFE
                  ADD 1 TO WS-SUM
               END-IF
               READ INPUT-FILE
                  AT END SET INPUT-END TO TRUE
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE
           .
