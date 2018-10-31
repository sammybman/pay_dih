000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200950726     F** LISTING PAYROLL YEAR TO DATES
000300950726     F** ADJUSTMENTS BY------T.I.BYNOE  *** R.WILLIAMS   & S.BRITTON
000400950726     FREGYTDL   IF   E           K DISK    USROPN
000500950726     FEXCYTDL   IF   E           K DISK    USROPN
000600950726     F                                     RENAME(EMPYDREC:REGREC)
000700950726     FCASYTDL   IF   E           K DISK    USROPN
000800950726     F                                     RENAME(EMPYDREC:CASREC)
000900950731     FLEFYTDL   IF   E           K DISK    USROPN
001000950731     F                                     RENAME(EMPYDREC:LEFREC)
001100950726     FREPORT    O    F  132        PRINTER OFLIND(*INOF)
001200950930     D*
001300950930     D                SDS
001400950930     D  @PGM             *PROC
001500950930     D  @USER                254    263
001600950930     **
001700950726     C     *ENTRY        PLIST
001800950726     C                   PARM                    TYPE              1
001900950726     C     TYPE          COMP      'E'                                    77
002000950726     C     TYPE          COMP      'R'                                    78
002100950726     C     TYPE          COMP      'C'                                    79
002200950726     C     TYPE          COMP      'S'                                    76
002300950731     C     TYPE          COMP      'L'                                    75
002400950726     C**
002500950726     C     *IN80         IFEQ      '0'
002600950726     C   77              OPEN      EXCYTDL
002700950726     C   78              OPEN      REGYTDL
002800950726     C   79              OPEN      CASYTDL
002900950726     C   76              OPEN      REGYTDL
003000950731     C   75              OPEN      LEFYTDL
003100950726     C                   ENDIF
003200950726     C**
003300950726     C     *IN80         IFEQ      '0'
003400950726     C     *INOF         OREQ      '1'
003500950726     C                   EXCEPT    HEDR
003600950726     C                   ENDIF
003700950726     C**
003800950726     C   77              READ      EXCYTDL                                LR
003900950726     C   78              READ      REGYTDL                                LR
004000950726     C   79              READ      CASYTDL                                LR
004100950726     C   76              READ      REGYTDL                                LR
004200950731     C   75              READ      LEFYTDL                                LR
004300950726     C**
004400950726     C                   SETON                                        80
004500950726     C**
004600950726     C     *INLR         IFEQ      '0'
004700950726     C                   ADD       YTDGRO        TOTGRO           13 2
004800950726     C                   ADD       YTDTAX        TOTTAX           11 2
004900950726     C                   ADD       YTDNTX        TOTNTX           10 2
005000950726     C                   ADD       YTDMED        TOTMED           10 2
005100950726     C                   ADD       YTDSAV        TOTSAV           10 2
005200950726     C                   ENDIF
005300950726     C**
005400950726     OREPORT    E            HEDR           2 04
005500950930     O                       @PGM                10
005600950726     O                                           72 'BANKS DIH LTD'
005700950726     O          E            HEDR           2
005800950930     O                       @USER               10
005900950726     O                                           67 'D"AGUIAR"S  INDUSTRIES'
006000950726     O                                           80 ' & HOLDINGS'
006100950726     O          E            HEDR           2
006200950726     O                     79                    30 'CASUALS'
006300950726     O                     78                    30 'GRADES A TO 5'
006400950726     O                     77                    30 'EXECUTIVES'
006500950726     O                     76                    30 'GRADES A TO 5 SPECIAL'
006600950731     O                     75                    30 'LEFTS '
006700950726     O                                           49 'CURRENT     Y.T.D.'
006800950726     O                                           73 'FIGURES  FOR  PERIOD'
006900950726     O                       UDATE         Y     83
007000950726     O          E            HEDR           2
007100950726     O                                           28 'EMPLOYEE NO    DEPT NO'
007200950726     O                                           57 '  WAGES             TAX'
007300950726     O                                           87 'N.I.S.       MEDICAL'
007400950726     O                                          100 'SAVINGS'
007500950726     O          D   NLR                     1
007600950726     O                       EMP           Z     13
007700950726     O                       DEPT          Z     25
007800950726     O                       YTDGRO        A     43
007900950726     O                       YTDTAX        A     59
008000950726     O                       YTDNTX        A     74
008100950726     O                       YTDMED        A     89
008200950726     O                       YTDSAV        A    102
008300950726     O          T    LR                  1
008400950726     O                       TOTGRO        2     41
008500950726     O                       TOTTAX        2     57
008600950726     O                       TOTNTX        2     72
008700950726     O                       TOTMED        2     87
008800950726     O                       TOTSAV        2    100
