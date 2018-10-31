000100950530     H DATEDIT(*DMY)
000200950711     F* BANKS DIH Employee PAYROLL ANALYSIS
000300961127     FWAGDIFL2  IF   E           K DISK    USROPN
000400950717     FDEPTHRS   IF   E           K DISK
000500950711     FDIV       IF   E           K DISK
000600950705     FQSYSPRT   O    F  132        PRINTER
000601980212     Dnoact1           S              5  0
000602980212     Dnoact2           S             11  2
000603980212     Dact1             S              5  0
000604980212     Dact2             S             11  2
000700950717     D*
000800950530     D                SDS
000900950530     D  @PGM             *PROC
001000950530     D  @USER                254    263
001100950530     I*
001200950717     IDHREC
001300950717     I              DEPT                        DPT
001301980212     C                   move      'A'           paytype           1
001302980212     C                   move      'A'           type              1
001400950823     **
001500950823     C     *IN80         IFEQ      '0'
001600961127     C                   OPEN      WAGDIFL2
001700950823     C                   ENDIF
001800950823     C**
001900950823     C                   SETON                                        80
002000950823     C**
002100950530     C***************************************************************
002200950717     **          PRINT REPORT REQUESTED                         ******
002300950530     C***************************************************************
002400950711     C     *LIKE         DEFINE    DIVIS         LASTD
002500950717     C     *LIKE         DEFINE    DEPT          LASTDP
002600950717     C     *LIKE         DEFINE    DNAME         LNAME
002700950629     **
002800951127     C                   READ      WAGREC                                 LR
002900951125     **
003000950717     C                   Z-ADD     DIVIS         LASTD
003100950717     C                   MOVE      DEPT          LASTDP
003200950717     C     DIVIS         CHAIN     DIVFMT                             H1
003300950717     C                   EXCEPT    HEADER
003400950629     **
003500950705     C     *INLR         DOWNE     *ON
003600951125     ******************************************************************
003700951125     *** THIS ROUTINE IS NOT FOR EXECUTIVE STAFF ***
003800951125     ******************************************************************
003900951127     C*          STATUS    IFNE 'E'
004000950717     ** CHECK DIVISIONAL CHANGE FOR SUB TOTAL
004100950717     C     DIVIS         IFNE      LASTD
004200950717     **
004300950717     C     LASTDP        CHAIN     DHREC                              H3
004400950717     C                   MOVE      DNAME         LNAME
004500950717     C                   EXCEPT    DETAIL
004600950710     C                   EXCEPT    DET
004700950717     C     DIVIS         CHAIN     DIVFMT                             H1
004800950717     C                   EXCEPT    HEADER
004802990216     C     divis         ifgt      4
004803990216     C                   add       amts#         diff             11 2
004804990216     C                   endif
004900950717     C                   Z-ADD     DIVIS         LASTD
005000950717     C                   MOVE      DEPT          LASTDP
005100951125     C                   ENDIF
005200950717     **
005300950717     C     DEPT          IFNE      LASTDP
005400950717     C                   ADD       1             Y                 2 0
005500950717     **
005600950717     **
005700950717     C     LASTDP        CHAIN     DHREC                              H3
005800950717     C                   MOVE      DNAME         LNAME
005900950717     **
006000950717     C                   EXCEPT    DETAIL
006100950717     C                   MOVE      DEPT          LASTDP
006200950717     ** PRINT A 60 LINE PAGE
006300950717     C     Y             IFEQ      61
006400950717     C                   EXCEPT    HEADER
006500950717     C                   Z-ADD     *ZERO         Y
006600951125     C                   ENDIF
006700950717     **
006800951125     C                   ENDIF
006900950717     **
007000951127     C                   ADD       AMOUNT        AMTS              9 2
007100951127     C                   ADD       AMOUNT        AMTS#             9 2
007200951127     C                   ADD       AMOUNT        #TOT#             9 2
007300950717     **
007400950717     C                   ADD       1             DTOT              5 0
007500950717     C                   ADD       1             DTOT#             5 0
007600950717     C                   ADD       1             #TOT              5 0
007601980211     C
007602980211     C*************************************************************
007603990216     C
007606990216     C
007607980211     C     divis         ifeq      4
007608980211     C                   z-add     dtot#         parm5             5 0
007609980211     C                   z-add     amts#         parm6            11 2
007610980211     C                   endif
007611990216     C
007612990216     C     divis         ifgt      4
007613990216     C                   add       amts#         diff             11 2
007614990216     C                   endif
007615980211     C
007616980211     C     divis         ifeq      3
007617980211     C                   SELECT
007618980211     C     lastdp        wheneq    6
007619980211     C                   z-add     amts          parm9             9 2
007620980211     C     lastdp        wheneq    36
007621980211     C                   z-add     amts          parm10            9 2
007622980211     C     lastdp        wheneq    38
007623980211     C                   z-add     amts          parm11            9 2
007624980211     C     lastdp        wheneq    39
007625980211     C                   z-add     amts          parm12            9 2
007626980211     C     lastdp        wheneq    45
007627980211     C                   z-add     amts          parm13            9 2
007628980211     C     lastdp        wheneq    48
007629980211     C                   z-add     amts          parm14            9 2
007630980211     C     lastdp        wheneq    64
007631980211     C                   z-add     amts          parm15            9 2
007632980211     C     lastdp        wheneq    47
007633980211     C                   z-add     amts          parm16            9 2
007634980211     C                   endsl
007635980211     C                   endif
007636980211     C*********************************************************************
007637980211     C
007638980211     C     lastdp        ifeq      64
007639980211     C                   z-add     amts          parm15            9 2
007640980211     C                   endif
007641980211     C
007642980211     C*********************************************************************
007643980211     C
007644980211     C     divis         ifeq      4
007645980211     C
007646980211     C                   z-add     dtot#         act1
007647980211     C                   z-add     amts#         act2
007648980211     C
007649980211     C                   endif
007650980211     C
007651980211     C*********************************************************************
007652980211     C
007653980211     C     divis         ifeq      2
007654980211     C                   z-add     amts#         parm8            11 2
007655980211     C                   endif
007656980211     C*********************************************************************
007657980211     C*********************************************************************
007658980211     C     divis         ifeq      1
007659980211     c     dtot#         andne     0
007660980211     C                   CALL      'BREAKDWN02'
007661980211     C                   PARM                    type
007662980211     C                   PARM                    dtot
007663980211     C                   PARM                    lastdp
007664980211     C                   z-add     dtot#         parm3             5 0
007665980211     C                   z-add     amts#         parm4            11 2
007666980211     C
007667980211     C                   z-add     dtot#         noact1
007668980211     C                   z-add     amts#         noact2
007669980211     C
007670980211     C                   endif
007700951125     ******************************************************************
007800951127     C**                   ENDIF
007900951125     ******************************************************************
008000951127     C                   READ      WAGREC                                 LR
008100951125     **
008200951125     C                   ENDDO
008300951125     **
008400950717     C     LASTDP        CHAIN     DHREC                              H3
008500950717     C                   MOVE      DNAME         LNAME
008600950717     ** PRINT LAST DETAIL GROUP
008700950717     C                   EXCEPT    DETAIL
008800950717     ** PRINT LAST LEVEL BREAK
008900950717     C                   EXCEPT    DET
009000950717     ** PRINT LAST GRAND TOTAL
009100950629     C                   EXCEPT    TOTAL
009101980211     C                   z-add     #tot#         parm7            11 2
009103990216     C     parm7         sub       diff          parm7
009104980211     C***************************************************************
009105980211     C                   CALL      'BALSHEET'
009106980211     C                   PARM                    TYPE
009107980211     C                   PARM                    parm3             5 0
009108980211     C                   PARM                    parm4            11 2
009109980211     C                   PARM                    parm5             5 0
009110980211     C                   PARM                    parm6            11 2
009111980211     C                   PARM                    parm7            11 2
009112980211     C                   PARM                    parm8            11 2
009113980211     C                   PARM                    parm9             9 2
009114980211     C                   PARM                    parm10            9 2
009115980211     C                   PARM                    parm11            9 2
009116980211     C                   PARM                    parm12            9 2
009117980211     C                   PARM                    parm13            9 2
009118980211     C                   PARM                    parm14            9 2
009119980211     C                   PARM                    parm15            9 2
009120980211     C                   PARM                    parm16            9 2
009121980211     C
009122980211     C                   CALL      'BREAKDWN01'
009123980212     C                   PARM                    paytype
009124980211     C                   PARM                    noact1
009125980211     C                   PARM                    noact2
009126980211     C                   PARM                    act1
009127980211     C                   PARM                    act2
009200950629     C***************************************************************
009300951120     OQSYSPRT   E            HEADER           01
009400950629     O                       @PGM                10
009500950629     O                                           70 'BANKS  DIH  LIMITED'
009600951120     OQSYSPRT   E            HEADER           02
009700950629     O                       @USER               10
009800950629     O                                           65 'D"AGUIAR"S INDUSTRIES & '
009900950629     O                                         +  1 ' HOLDINGS'
010000951127     O                                          120 'REG.ALLOW.'
010100951120     OQSYSPRT   E            HEADER           03
010200950711     O                                           68 'SUMMARY OF PAYMENTS'
010300950629     O                                          120 'RUN Date: '
010400950629     O                       UDATE         Y   +  1
010500950629     OQSYSPRT   E            HEADER      2
010600950717     O                                           15 'DIVISION NO.:'
010700950717     O                       DIVIS         Z   +  2
010800950717     O                                         +  5 'NAME:'
010900950717     O                       DTNAME            +  2
011000950717     OQSYSPRT   E            HEADER      2
011100950717     O                                           12 'DEPT NO'
011200950717     O                                           24 'DEPT NAME'
011300950717     O                                           71 'PAY SLIPS'
011400950717     O                                           89 'NETT PAY'
011500950629     OQSYSPRT   E            HEADER      1
011600950717     O                                           12 '-------'
011700950717     O                                           24 '---------'
011800950717     O                                           71 '---------'
011900950717     O                                           89 '--------'
012000950629     OQSYSPRT   E            DETAIL      1
012100950717     O                       LASTDP              10
012200950717     O                       LNAME               55
012300950717     O                       DTOT          ZB    66
012400950717     O                       AMTS          LB    90
012500950710     OQSYSPRT   E            DET         1
012600950710     O                                         +  2 'TOTAL'
012700950717     O                       DTOT#         ZB    66
012800950717     O                       AMTS#         LB    90
012900950629     OQSYSPRT   E            TOTAL       2
013000950629     O                                           20 'TOTAL ENTRIES'
013100950717     O                       #TOT          Z     66
013200950717     O                       #TOT#         L     90
013300950629     OQSYSPRT   E            TOTAL       2
013400950629     O                                           40 '*** END OF REPORT ***'
