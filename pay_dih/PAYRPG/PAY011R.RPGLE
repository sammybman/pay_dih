000100950530     H DATEDIT(*DMY)
000101980202     Fbrkdwn02  UF   E           K DISK
000200950711     F* BANKS DIH Employee PAYROLL ANALYSIS
000300950823     FREGDIVL   IF   E           K DISK    USROPN
000400950823     FODDDIVL   IF   E           K DISK    USROPN
000500950823     F                                     RENAME(ANLREC:ODDREC)
000600950823     FCASDIVL   IF   E           K DISK    USROPN
000700950823     F                                     RENAME(ANLREC:CASREC)
000800950823     FSPCDIVL   IF   E           K DISK    USROPN
000900950823     F                                     RENAME(ANLREC:SPCREC)
001000950823     FCABDIVL   IF   E           K DISK    USROPN
001100950823     F                                     RENAME(ANLREC:CABREC)
001200950717     FDEPTHRS   IF   E           K DISK
001300950711     FDIV       IF   E           K DISK
001400950705     FQSYSPRT   O    F  132        PRINTER
001500950717     D*
001600950530     D                SDS
001700950530     D  @PGM             *PROC
001800950530     D  @USER                254    263
001808980123     D
001809980123     Dptype            S              1
001810980123     Dnoact1           S              5  0
001811980123     Dnoact2           S             11  2
001812980123     Dact1             S              5  0
001813980123     Dact2             S             11  2
001900950530     I*
002000950717     IDHREC
002100950717     I              DEPT                        DPT
002101980202     C**
002300950823     C     *ENTRY        PLIST
002400950823     C                   PARM                    TYPE              1
002500950823     C     TYPE          COMP      'R'                                    77
002600950823     C     TYPE          COMP      'O'                                    78
002700950823     C     TYPE          COMP      'C'                                    79
002800950823     C     TYPE          COMP      'S'                                    76
002900950823     C     TYPE          COMP      'B'                                    75
003000951205     C  N76TYPE          COMP      'X'                                    76
003100950823     **
003200950823     C     *IN80         IFEQ      '0'
003300950823     C   77              OPEN      REGDIVL
003400950823     C   78              OPEN      ODDDIVL
003500950823     C   79              OPEN      CASDIVL
003600950823     C   76              OPEN      SPCDIVL
003700950823     C   75              OPEN      CABDIVL
003800950823     C                   ENDIF
003900951130     **
004000950823     C                   SETON                                        80
004100951130     **
004200950530     C***************************************************************
004300951130     ***         PRINT REPORT REQUESTED                           ***
004400950530     C***************************************************************
004500950711     C     *LIKE         DEFINE    DIVIS         LASTD
004600950717     C     *LIKE         DEFINE    DEPT          LASTDP
004700950717     C     *LIKE         DEFINE    DNAME         LNAME
004800950629     **
004900950823     C   77              READ      ANLREC                                 LR
005000950823     C   78              READ      ODDREC                                 LR
005100950823     C   79              READ      CASREC                                 LR
005200950823     C   76              READ      SPCREC                                 LR
005300950823     C   75              READ      CABREC                                 LR
005400951125     **
005500950717     C                   Z-ADD     DIVIS         LASTD
005600980127     C                   Z-ADD     DEPT          LASTDP
005700950717     C     DIVIS         CHAIN     DIVFMT                             H1
005800950717     C                   EXCEPT    HEADER
005900950629     **
006000950705     C     *INLR         DOWNE     *ON
006100951125     ******************************************************************
006200951125     *** THIS ROUTINE IS NOT FOR EXECUTIVE STAFF ***
006300951125     ******************************************************************
006400951130     C     STATUS        IFNE      'E'
006500950717     ** CHECK DIVISIONAL CHANGE FOR SUB TOTAL
006600950717     C     DIVIS         IFNE      LASTD
006700950717     **
006800950717     C     LASTDP        CHAIN     DHREC                              H3
006900950717     C                   MOVE      DNAME         LNAME
007000950717     C                   EXCEPT    DETAIL
007100971230     C                   EXCEPT    DET
007200950717     C     DIVIS         CHAIN     DIVFMT                             H1
007300950717     C                   EXCEPT    HEADER
007400950717     C                   Z-ADD     DIVIS         LASTD
007500980127     C                   z-add     DEPT          LASTDP
007600951125     C                   ENDIF
007700950717     **
007800950717     C     DEPT          IFNE      LASTDP
007900950717     C                   ADD       1             Y                 2 0
008000950717     **
008100950717     **
008200950717     C     LASTDP        CHAIN     DHREC                              H3
008300950717     C                   MOVE      DNAME         LNAME
008301971231     C     divis         ifeq      3
008302971231     C                   SELECT
008303971231     C     lastdp        wheneq    6
008304980105     C                   z-add     amts          parm9             9 2
008306971231     C     lastdp        wheneq    36
008307980105     C                   z-add     amts          parm10            9 2
008308971231     C     lastdp        wheneq    38
008309980105     C                   z-add     amts          parm11            9 2
008310971231     C     lastdp        wheneq    39
008311980105     C                   z-add     amts          parm12            9 2
008312971231     C     lastdp        wheneq    45
008313980105     C                   z-add     amts          parm13            9 2
008314971231     C     lastdp        wheneq    48
008315980105     C                   z-add     amts          parm14            9 2
008316971231     C     lastdp        wheneq    64
008317980105     C                   z-add     amts          parm15            9 2
008318971231     C     lastdp        wheneq    47
008319980105     C                   z-add     amts          parm16            9 2
008320971231     C                   endsl
008321971231     C                   endif
008322980102     C************************************************************
008323980102     C     divis         ifeq      2
008324980105     C                   z-add     amts#         parm8            11 2
008325980102     C                   endif
008400980102     **************************************************************
008500950717     C                   EXCEPT    DETAIL
008600980127     C                   Z-ADD     DEPT          LASTDP
008622980102     C**************************************************************
008700950717     ** PRINT A 60 LINE PAGE
008800950717     C     Y             IFEQ      61
008900950717     C                   EXCEPT    HEADER
009000950717     C                   Z-ADD     *ZERO         Y
009100951125     C                   ENDIF
009200950717     **
009300951125     C                   ENDIF
009400950717     **
009500950717     C                   ADD       NET           AMTS              9 2
009600951130     C                   ADD       NET           AMTS#            11 2
009700951130     C                   ADD       NET           #TOT#            11 2
009800950717     **
009900950717     C                   ADD       1             DTOT              5 0
010000950717     C                   ADD       1             DTOT#             5 0
010100950717     C                   ADD       1             #TOT              5 0
010101990216     C
010102990216     C
010104990216     C     divis         ifgt      4
010106990216     C                   add       amts#         diff             11 2
010107990216     C                   endif
010108990216     C
010200951130     **
010201980304     C     divis         ifeq      3
010202980304     C
010203980107     C     lastdp        ifeq      64
010204980107     C                   z-add     amts          parm15            9 2
010205980107     C                   endif
010206980303     C
010207980303     C     lastdp        ifeq      48
010208980303     C                   z-add     amts          parm14            9 2
010209980303     C                   endif
010210980303     C
010211980303     C     lastdp        ifeq      45
010212980303     C                   z-add     amts          parm13            9 2
010213980303     C                   endif
010214980303     C
010215980303     C     lastdp        ifeq      47
010216980303     C                   z-add     amts          parm16            9 2
010217980303     C                   endif
010218980304     C
010219980304     C                   endif
010220980123     C*********************************************************************
010221980123     C     divis         ifeq      4
010222980123     C
010223980123     C                   z-add     dtot#         act1
010224980123     C                   z-add     amts#         act2
010225980123     C                   move      type          ptype
010226980123     C
010227980123     C                   endif
010228980123     C*********************************************************************
010229980102     C     divis         ifeq      2
010230980105     C                   z-add     amts#         parm8            11 2
010231980102     C                   endif
010232980123     C*********************************************************************
010233980123     C*********************************************************************
010234980102     C     divis         ifeq      1
010235980102     c     dtot#         andne     0
010236980126     C                   CALL      'BREAKDWN02'
010237980126     C                   PARM                    type
010238980126     C                   PARM                    dtot
010239980126     C                   PARM                    lastdp
010240980102     C                   z-add     dtot#         parm3             5 0
010241980105     C                   z-add     amts#         parm4            11 2
010242980105     C
010243980123     C                   z-add     dtot#         noact1
010244980123     C                   z-add     amts#         noact2
010245980106     C                   move      type          ptype
010246980105     C
010247980102     C                   endif
010248980123     C*********************************************************************
010300951130     ** FOR XMAS BONUS , INCLUDE C.O.E. PAYSLIPS IN TOTALS
010400951130     C     TYPE          IFEQ      'X'
010500951130     C     COE           ANDGT     *ZEROS
010600951130     C                   ADD       1             DTOT              5 0
010700951130     C                   ADD       1             DTOT#             5 0
010800951130     C                   ADD       1             #TOT              5 0
010900951130     C                   ENDIF
011000951125     ******************************************************************
011100951130     C                   ENDIF
011200951125     ******************************************************************
011300950823     C   77              READ      ANLREC                                 LR
011400950823     C   78              READ      ODDREC                                 LR
011500950823     C   79              READ      CASREC                                 LR
011600950823     C   76              READ      SPCREC                                 LR
011700950823     C   75              READ      CABREC                                 LR
011800951125     **
011900951125     C                   ENDDO
012000951125     **
012100950717     C     LASTDP        CHAIN     DHREC                              H3
012200950717     C                   MOVE      DNAME         LNAME
012201980107     C     divis         ifeq      4
012202980107     C                   z-add     dtot#         parm5             5 0
012203980107     C                   z-add     amts#         parm6            11 2
012204980107     C                   endif
012205980317     C*    divis         ifeq      3
012206980317     C*                  SELECT
012207980317     C*    lastdp        wheneq    6
012208980317     C*                  z-add     amts          parm9             9 2
012209980317     C*    lastdp        wheneq    36
012210980317     C*                  z-add     amts          parm10            9 2
012211980317     C*    lastdp        wheneq    38
012212980317     C*                  z-add     amts          parm11            9 2
012213980317     C*    lastdp        wheneq    39
012214980317     C*                  z-add     amts          parm12            9 2
012215980317     C*    lastdp        wheneq    45
012216980317     C*                  z-add     amts          parm13            9 2
012217980317     C*    lastdp        wheneq    48
012218980317     C*                  z-add     amts          parm14            9 2
012219980317     C*    lastdp        wheneq    64
012220980317     C*                  z-add     amts          parm15            9 2
012221980317     C*    lastdp        wheneq    47
012222980317     C*                  z-add     amts          parm16            9 2
012223980317     C*                  endsl
012224980317     C*                  endif
012300950717     ** PRINT LAST DETAIL GROUP
012303971230     C                   EXCEPT    DETAIL
012500950717     ** PRINT LAST LEVEL BREAK
012600950717     C                   EXCEPT    DET
012700950717     ** PRINT LAST GRAND TOTAL
012800950629     C                   EXCEPT    TOTAL
012801980105     C                   z-add     #tot#         parm7            11 2
012802990216     C     parm7         sub       diff          parm7
012900950629     C***************************************************************
012901980106     C                   CALL      'BALSHEET'
012902980106     C                   PARM                    TYPE
012903980106     C                   PARM                    parm3             5 0
012904980106     C                   PARM                    parm4            11 2
012905980106     C                   PARM                    parm5             5 0
012906980106     C                   PARM                    parm6            11 2
012907980106     C                   PARM                    parm7            11 2
012908980106     C                   PARM                    parm8            11 2
012909980106     C                   PARM                    parm9             9 2
012910980106     C                   PARM                    parm10            9 2
012911980106     C                   PARM                    parm11            9 2
012912980106     C                   PARM                    parm12            9 2
012913980106     C                   PARM                    parm13            9 2
012914980106     C                   PARM                    parm14            9 2
012915980106     C                   PARM                    parm15            9 2
012916980106     C                   PARM                    parm16            9 2
012918980106     C
012919980123     C                   CALL      'BREAKDWN01'
012920980123     C                   PARM                    ptype
012933980123     C                   PARM                    noact1
012934980123     C                   PARM                    noact2
012935980123     C                   PARM                    act1
012936980123     C                   PARM                    act2
012964971231     C                   seton                                        lr
012965971231     C                   return
013000951120     OQSYSPRT   E            HEADER           01
013100950629     O                       @PGM                10
013200950629     O                                           70 'BANKS  DIH  LIMITED'
013300951120     OQSYSPRT   E            HEADER           02
013400950629     O                       @USER               10
013500950629     O                                           65 'D"AGUIAR"S INDUSTRIES & '
013600980202     O                                         +  1 'HOLDINGS'
013700950823     O               77                         120 'REGULAR   '
013800950823     O               78                         120 'ON LEAVE  '
013900950823     O               79                         120 'CASUAL    '
014000950823     O               76                         120 'SPECIAL   '
014100950823     O               75                         120 'CAS. BONUS'
014200951120     OQSYSPRT   E            HEADER           03
014300950711     O                                           68 'SUMMARY OF PAYMENTS'
014400950629     O                                          120 'RUN Date: '
014500950629     O                       UDATE         Y   +  1
014600950629     OQSYSPRT   E            HEADER      2
014700950717     O                                           15 'DIVISION NO.:'
014800950717     O                       DIVIS         Z   +  2
014900950717     O                                         +  5 'NAME:'
015000950717     O                       DTNAME            +  2
015100950717     OQSYSPRT   E            HEADER      2
015200950717     O                                           12 'DEPT NO'
015300950717     O                                           24 'DEPT NAME'
015400950717     O                                           71 'PAY SLIPS'
015500950717     O                                           89 'NETT PAY'
015600950629     OQSYSPRT   E            HEADER      1
015700950717     O                                           12 '-------'
015800950717     O                                           24 '---------'
015900950717     O                                           71 '---------'
016000950717     O                                           89 '--------'
016100980105     OQSYSPRT   E            DETAIL      1
016200950717     O                       LASTDP              10
016300950717     O                       LNAME               55
016400950717     O                       DTOT          ZB    66
016500950717     O                       AMTS          LB    90
016600971230     OQSYSPRT   E            DET         2
016700950710     O                                         +  2 'TOTAL'
016800950717     O                       DTOT#         ZB    66
016900950717     O                       AMTS#         LB    90
017000971230     OQSYSPRT   E            TOTAL       2
017100950629     O                                           20 'TOTAL ENTRIES'
017200950717     O                       #TOT          Z     66
017300950717     O                       #TOT#         L     90
017400971230     OQSYSPRT   E            TOTAL       2
017500950629     O                                           40 '*** END OF REPORT ***'
