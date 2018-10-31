000100950530     H DATEDIT(*DMY)
000200950711     F* BANKS DIH Employee PAYROLL ANALYSIS
000300950711     FREGLOCL   IF   E           K DISK
000400980519     Fdptloc    IF   E           K DISK
000500950705     FQSYSPRT   O    F  132        PRINTER
000600950530     D*
000700950530     D                SDS
000800950530     D  @PGM             *PROC
000900950530     D  @USER                254    263
001000950530     **
001100950530     C***************************************************************
001200951120     *****       PRINT REPORT REQUESTED                         *****
001300950530     C***************************************************************
001400950711     C     *LIKE         DEFINE    LOCAT         LOLAST
001500950629     C                   EXCEPT    HEADER
001600950629     **
001700950711     C                   READ      ANLREC                                 LR
001800950711     C                   Z-ADD     LOCAT         LOLAST
001900950629     **
002000950705     C     *INLR         DOWNE     *ON
002100950710     ** CHECK DEPARTMENT CHANGE FOR SUB TOTAL
002200950711     C     LOCAT         IFNE      LOLAST
002300950713     C     LOLAST        CHAIN     LOCFMT                             H1
002400950710     C                   EXCEPT    DET
002500950711     C                   Z-ADD     LOCAT         LOLAST
002600950710     C                   END
002700950710     **
002800950824     C                   MOVEL     CHRIST        INITAL            1
002900950629     C                   ADD       1             Y                 2 0
003000950714     C                   ADD       NET           AMTS              9 2
003100950714     C                   ADD       NET           AMTS#             9 2
003200950714     C                   ADD       NET           #TOTS             9 2
003300950710     **
003400950710     C                   EXCEPT    DETAIL
003500950629     ** PRINT A 60 LINE PAGE
003600950629     C     Y             IFEQ      61
003700950629     C                   EXCEPT    HEADER
003800950629     C                   Z-ADD     *ZERO         Y
003900950629     C                   END
004000950705     **
004100950711     C                   READ      ANLREC                                 LR
004200950629     C                   END
004300950713     ** WRITE LAST GROUP READ TOTAL
004400950713     C     LOLAST        CHAIN     LOCFMT                             H1
004500950713     C                   EXCEPT    DET
004600950629     C                   EXCEPT    TOTAL
004700950629     C***************************************************************
004800951120     OQSYSPRT   E            HEADER           01
004900950629     O                       @PGM                10
005000950629     O                                           70 'BANKS  DIH  LIMITED'
005100951120     OQSYSPRT   E            HEADER           02
005200950629     O                       @USER               10
005300950629     O                                           65 'D"AGUIAR"S INDUSTRIES & '
005400950629     O                                         +  1 ' HOLDINGS'
005500951120     OQSYSPRT   E            HEADER           03
005600950711     O                                           68 'SECURITY PAYMENTS REPORT'
005700950629     O                                          120 'RUN Date: '
005800950629     O                       UDATE         Y   +  1
005900950824     OQSYSPRT   E            HEADER      2
006000950824     O                                           28 '***'
006100950824     O                                           41 'NORMAL  TIME'
006200950824     O                                           48 '***'
006300950824     O                                           58 'OVERTIME'
006400950824     O                                           65 '***'
006500950824     O                                           78 '***'
006600950824     O                                           89 'DEDUCTIONS'
006700950824     O                                           93 '***'
006800950824     OQSYSPRT   E            HEADER      1
006900980203     O                                            6 'EMP.'
007000950824     O                                           32 'HOURS'
007100950824     O                                           48 'HOURS'
007200950824     O                                           69 'TRAVEL'
007300950824     O                                          100 'GROSS'
007400950824     O                                          113 'NET'
007500950824     OQSYSPRT   E            HEADER      1
007600980203     O                                            6 'NO.'
007700950824     O                                           20 'NAME'
007800950824     O                                           33 'WORKED'
007900950824     O                                           42 'SALARY'
008000950824     O                                           49 'WORKED'
008100950824     O                                           57 'SALARY'
008200950824     O                                           69 'ALLOW.'
008300950824     O                                           79 'LOANS'
008400950824     O                                           90 'MEDICAL'
008500950824     O                                          101 'SALARY'
008600950824     O                                          114 'SALARY'
008700950824     O                                          126 'SIGNATURE'
008800950629     OQSYSPRT   E            HEADER      1
008900980203     O                                            6 '---'
009000950824     O                                           20 '----'
009100950824     O                                           33 '------'
009200950824     O                                           42 '------'
009300950824     O                                           49 '------'
009400950824     O                                           57 '------'
009500950824     O                                           69 '------'
009600950824     O                                           79 '-----'
009700950824     O                                           90 '-------'
009800950824     O                                          101 '------'
009900950824     O                                          114 '------'
010000950824     O                                          126 '---------'
010100950824     OQSYSPRT   E            DETAIL      2
010200980203     O                       EMP           Z      6
010300980203     O                       SURNAM              20
010400980203     O                       INITAL              21
010500980203     O                                           22 '.'
010600950824     O                       NRHRS         1   +  1
010700950824     O**                       NRATE 1 +  1
010800950824     O                       NTWAGE        1   +  1
010900950824     O                       OTHRS         1   +  1
011000950824     O**                       OTRATE3 +  1
011100950824     O                       OTWAGE        1   +  1
011200950824     O                       COE           1   +  1
011300950824     O                       TOTLON        1   +  1
011400950824     O                       TOTMED        1   +  1
011500950824     O                       TGROS         1   +  1
011600950824     O                       AMTS          LB  +  1
011700950824     O                                         +  1 '.............'
011800950713     OQSYSPRT   E            DET         1  1
011900950711     O                       LTNO                20
012000950711     O                       LTNAME            +  2
012100950710     O                                         +  2 'TOTAL'
012200950710     O                       AMTS#         LB    65
012300950629     OQSYSPRT   E            TOTAL       2
012400950629     O                                           20 'TOTAL ENTRIES'
012500950710     O                       #TOTS         L     65
012600950629     OQSYSPRT   E            TOTAL       2
012700950629     O                                           40 '*** END OF REPORT ***'
