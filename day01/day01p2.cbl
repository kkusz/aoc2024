         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY01P2.
            
         ENVIRONMENT DIVISION. 

         INPUT-OUTPUT SECTION. 
         FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO INPFILE
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT NUMBERS-LEFT ASSIGN TO NUMLEFT
            ORGANIZATION IS LINE SEQUENTIAL. 

            SELECT NUMBERS-RIGHT ASSIGN TO NUMRIGHT
            ORGANIZATION IS LINE SEQUENTIAL. 
            
         DATA DIVISION.
         FILE SECTION. 
         FD INPUT-FILE.
         01 INPUT-RECORD.
            88 INPUT-END            VALUE LOW-VALUE.
            05 INPUT-TEXT           PIC X(80).

         FD NUMBERS-LEFT.
         01 LEFT-RECORD.
            88 NUMLEFT-END          VALUE LOW-VALUE.
            05 NUMLEFT-TEXT         PIC 9(5).

         FD NUMBERS-RIGHT.
         01 RIGHT-RECORD.
            88 NUMRIGHT-END         VALUE LOW-VALUE.
            05 NUMRIGHT-TEXT        PIC 9(5).
         WORKING-STORAGE SECTION.


         01 WS-SUM                PIC 9(10) VALUE 0.

         01 WS-SIMILARITY         PIC 9(10).

         01 WS-OCCURS-TABLE.
            05 WS-OCCURS        PIC 9(10) OCCURS 100000 VALUE 0.

         01 IX                  PIC 9(6).
            
         PROCEDURE DIVISION.
            PERFORM INIT

            OPEN INPUT NUMBERS-LEFT

            READ NUMBERS-LEFT
               AT END SET NUMLEFT-END TO TRUE
            END-READ

            PERFORM UNTIL NUMLEFT-END
               COMPUTE WS-SIMILARITY = NUMLEFT-TEXT 
                                   * WS-OCCURS(NUMLEFT-TEXT)
               DISPLAY WS-SIMILARITY
               ADD WS-SIMILARITY TO WS-SUM
               READ NUMBERS-LEFT
                  AT END SET NUMLEFT-END TO TRUE
               END-READ
            END-PERFORM

            CLOSE NUMBERS-LEFT 

            DISPLAY WS-SUM

            STOP RUN.

         INIT SECTION.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT NUMBERS-LEFT
           OPEN OUTPUT NUMBERS-RIGHT
           READ INPUT-FILE
               AT END SET INPUT-END TO TRUE
           END-READ

           PERFORM UNTIL INPUT-END
               UNSTRING INPUT-TEXT DELIMITED BY ALL SPACES
                   INTO NUMLEFT-TEXT, NUMRIGHT-TEXT 
               ADD 1 TO WS-OCCURS(NUMRIGHT-TEXT)
               WRITE LEFT-RECORD
               WRITE RIGHT-RECORD
               READ INPUT-FILE
                  AT END SET INPUT-END TO TRUE
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE
           CLOSE NUMBERS-LEFT 
           CLOSE NUMBERS-RIGHT
           .
