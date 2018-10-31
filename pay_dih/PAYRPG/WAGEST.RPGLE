000100950406     H DECEDIT('.') DATEDIT(*DMY/)
000200950406     F* CASUALS & WEEKLY EARNINGS & HOURS WORKED COMPARISON
000300950728     F* PROGRAMMER:-MOD. BY SAMUEL BRITTON     **** R.WILLIAMS
000400950406     FREGANLL   IP   E           K DISK
000500950408     FEXCANLL   IS   E           K DISK
000600950406     F                                     RENAME(ANLREC:EXCREC)
000700950408     FCASANLL   IS   E           K DISK
000800950406     F                                     RENAME(ANLREC:CASREC)
000900950406     FDEPTHRS   IF   E           K DISK
001000950406     FREPORT    O    F  132        PRINTER OFLIND(*INOF)
001100951027     D*
001200951027     D                SDS
001300951027     D  @PGM             *PROC
001400951027     D  @USER                254    263
001403961210     D*
001500950406     IANLREC        01
001600950415     I                                          DEPT          L1M1
001700950406     ICASREC        02
001800950415     I                                          DEPT          L1M1
001900950406     IEXCREC        03
002000950415     I                                          DEPT          L1M1
002100951027     **
002200950406     C   L1              Z-ADD     DEPT          DEPTNO            2 0
002300950406     C   L1DEPT          CHAIN     DEPTHRS                            99
002400950406     C     DEST          MULT      DHOURS        TOTHOR            7 2
002500950408     C     *IN01         IFEQ      '1'
002600950408     C     *IN03         OREQ      '1'
002700950408     C     NRHRS         IFEQ      *ZEROS
002800950408     C     DEPT          CHAIN     DEPTHRS                            99
002900950408     C                   ADD       DHOURS        TOTREG
003000950408     C                   ELSE
003100950408     C                   ADD       NRHRS         TOTREG            7 2
003200950408     C                   ENDIF
003300950408     C                   ADD       OTHRS         REGTOT            7 2
003400950408     C                   ADD       1             TOTEMP            4 0
003500950408     C                   ELSE
003600950408     C     *IN02         IFEQ      '1'
003700950408     C     NRHRS         IFEQ      *ZEROS
003800950408     C     DEPT          CHAIN     DEPTHRS                            99
003900950408     C                   ADD       DHOURS        TOTCAS
004000950408     C                   ELSE
004100950408     C                   ADD       NRHRS         TOTCAS            7 2
004200950408     C                   ENDIF
004300950408     C                   ADD       OTHRS         CASTOT            7 2
004400950408     C                   ADD       1             EMPTOT            4 0
004500950408     C                   ENDIF
004600950408     C                   ENDIF
004700950410     C     TOTREG        ADD       TOTCAS        TOTNT             7 2
004800950410     C     REGTOT        ADD       CASTOT        TOTOT             7 2
004900950408     C**         TABHRS    MULT 2         CASHRS  30       DEPT.F/N HOURS
005000950410     C**         TOTCAS    DIV  CASHRS    EMPTOT           CAS.EMP.IN DEPT
005100950406     CL1   TOTEMP        ADD       EMPTOT        TOTWOR            4 0
005200950406     CL1   TOTOT         ADD       TOTNT         OTFIN             7 2
005300950406     CL1   OTFIN         SUB       TOTHOR        BUDDIF            7 2
005400950406     CL1   TOTREG        ADD       FINREG        FINREG            7 2          REGNT FINAL
005500950406     CL1   TOTCAS        ADD       FINCAS        FINCAS            7 2          CASNT FINAL
005600950406     CL1   REGTOT        ADD       FINROT        FINROT            7 2          REGOT FINAL
005700950406     CL1   CASTOT        ADD       FINCOT        FINCOT            7 2          CASOT FINAL
005800950406     CL1   TOTEMP        ADD       FINEMP        FINEMP            4 0          REGS.FINAL TOT.
005900950406     CL1   EMPTOT        ADD       FINCEM        FINCEM            4 0          FINAL TOT FOR C
006000950406     CL1                 ADD       DEST          TOTEST            4 0
006100950406     CL1   TOTHOR        ADD       FINHOR        FINHOR            8 2
006200950406     CL1   TOTNT         ADD       FINNT         FINNT             7 2
006300950406     CL1   TOTOT         ADD       FINOT         FINOT             7 2
006400950406     CL1   TOTWOR        ADD       FINWOR        FINWOR            5 0
006500950406     CL1   FINNT         ADD       FINOT         NTFIN             7 2
006600950406     CL1   BUDDIF        ADD       TOTDIF        TOTDIF            7 2
006700950406     OREPORT    H    1P                     2 01
006800950406     O         OR    OF
006900951027     O                       @PGM                10
007000950406     O                                           77 'B A N K S  D I H  L T D'
007100950406     O          H    1P                  0  2
007200950406     O         OR    OF
007300951027     O                       @USER               10
007400950406     O                                           71 'D"AGUIAR"S   INDUSTRIES'
007500950406     O                                           84 '&  HOLDINGS'
007600950406     O          H    1P                     2
007700950406     O         OR    OF
007800950406     O                                           56 'W A G E S   A N A L Y S'
007900950406     O                                           81 'I S   F O R   F O R T N '
008000950406     O                                          102 'I G H T    E N D I N G'
008100950406     O                       UDATE         Y    112
008200950406     O          H    1P                     1
008300950406     O         OR    OF
008400950406     O                                           23 'D E P A R T M E N T'
008500950406     O                                           48 '           REGULAR EMPLO'
008600950406     O                                           72 'YEES          CASUAL EMP'
008700950406     O                                           96 'LOYEES     FIXED ESTABLI'
008800950406     O                                          119 'SHMENT  TOTAL     TOTAL'
008900950406     O                                          128 'OVER'
009000950406     O          H    1P                     1
009100950406     O         OR    OF
009200950406     O                                           13 'NO.      NAME'
009300950406     O                                           56 'NO.OF  N.T.HRS  O.T.HRS'
009400950406     O                                           82 'NO.OF  N.T.HRS  O.T.HRS'
009500950406     O                                          109 'NO.OF   HOURS       EMPS'
009600950406     O                                          132 'HOURS    UNDER(CR)'
009700950406     O          H    1P                     2
009800950406     O         OR    OF
009900950406     O                                           55 'EMPS.  WORKED   WORKED'
010000950406     O                                           81 'EMPS.  WORKED   WORKED'
010100950406     O                                          108 'EMPS.   WORKED     WORK'
010200950406     O                                          129 'ED    WORKED   BUDGET'
010300950406     O          T    L1                     2
010400950410     O                       DEPT          Z      3
010500950406     O                    N99DNAME               43
010600950406     O                       TOTEMP        ZB    37
010700950406     O                       EMPTOT        ZB    63
010800950406     O                       TOTREG        1B    47
010900950406     O                       REGTOT        1B    56
011000950406     O                       TOTCAS        1B    73
011100950406     O                       CASTOT        1B    82
011200950406     O                       DEST          ZB    88
011300950406     O                       TOTHOR        1B    99
011400950406     O                       TOTWOR        ZB   108
011500950406     O                       OTFIN         1B   119
011600950406     O                       BUDDIF        AB   131
011700950406     O          T    LR                  2       01
011800950406     O                       FINEMP        Z     36
011900950406     O                       FINCEM        Z     62
012000950406     O                       FINREG        1     47
012100950406     O                       FINROT        1     57
012200950406     O                       FINCAS        1     72
012300950406     O                       FINCOT        1     82
012400950406     O                       TOTEST        Z     88
012500950406     O                       FINHOR        1     99
012600950406     O                       FINWOR        Z    108
012700950406     O                       NTFIN         1    119
012800950406     O                       TOTDIF        A    131
012900950406     O                                           37 '*'
013000950406     O                                           63 '*'
013100950406     O                                           48 '*'
013200950406     O                                           58 '*'
013300950406     O                                           73 '*'
013400950406     O                                           83 '*'
013500950406     O                                           89 '*'
013600950406     O                                          100 '*'
013700950406     O                                          109 '*'
013800950406     O                                          120 '*'
013900950406     O                                          132 '*'
