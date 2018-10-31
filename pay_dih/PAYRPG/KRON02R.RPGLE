000100950105     H DATEDIT(*DMY)
000200950429     H******  PROGRAMMER - SAMUEL BRITTON
000300960413     FKRON02D   CF   E             WORKSTN
000400960307     D*
000500950516     D                SDS
000600960416     D  @PGM             *PROC
000700960416     D  @USER                254    263
000800960307     **
000900980206     C     start         tag
001000980206     C                   WRITE     BEGDSP
001100980206     C                   EXFMT     MENUBAR
001200980206     C                   move      *blanks       errmsg
001300960523
001400960523
001500960415     ** CHECK FOR USER TASKS SELECTION
001600960415     C     MNUFLD        IFEQ      01
001700960415     C                   READ      TASK                                   10
001800960419     **
001900960415     C     F1            IFEQ      01
002000960506     C                   CALL      'KRON03R'                                    CREATE REC.
002100960415     C                   ENDIF
002200960419     **
002300960415     C     F1            IFEQ      02
002400960506     C                   CALL      'KRON01R'                                    CREATE INTF
002500980206     C                   parm      *blanks       @msgid            7
002600980209     C                   exsr      get_er_rtn
002700980206     **
002800960415     C                   ENDIF
002900980209     ** Create payroll files from Kronos data
003000960419     C     F1            IFEQ      03
003100980206     C                   CALL      'KRON13C'                                    CREATE P/ROLL
003200980209     C                   parm      *blanks       @msgid
003300980209     **
003400980209     C                   exsr      get_er_rtn
003500980209     **
003600960419     C                   ENDIF
003700980209     ** Refresh payroll files
003800980209     C     F1            IFEQ      04
003900980209     C                   CALL      'KRONREVC'                                   CREATE P/ROLL
004000980209     C                   parm      *blanks       @msgid
004100980209     **
004200980209     C                   exsr      get_er_rtn
004300980209     **
004400980209     C                   ENDIF
004500980209     ** Regular time card entry
004600980209     C     F1            IFEQ      05
004700980209     C                   CALL      'RTIME'                                      CREATE P/ROLL
004800980209     C                   ENDIF
004900980209     ** Casual time card entry
005000980209     C     F1            IFEQ      06
005100980209     C                   CALL      'CTIME'                                      CREATE P/ROLL
005200980209     C                   ENDIF
005300960415     C                   ENDIF
005400960523
005500960523
005600960415     ** CHECK FOR MAINTENANCE SELECTION
005700960415     C     MNUFLD        IFEQ      02
005800960415     C                   READ      EDIT                                   10
005900960415     **
006000960415     C     F1            IFEQ      01
006100960416     C                   CALL      'KRON04R'                                    EDIT CLASSES
006200960415     C                   ENDIF
006300960416     **
006400960416     C     F1            IFEQ      02
006500960416     C                   CALL      'KRON06R'                                    EDIT TERMNAL
006600960416     C                   ENDIF
006700960416     **
006800960416     C     F1            IFEQ      03
006900960416     C                   CALL      'KRON08R'                                    EDIT RSTRCK
007000960416     C                   ENDIF
007100960419     **
007200960419     C     F1            IFEQ      04
007300960502     C                   CALL      'KRON11R'                                    EDIT ACCRUAL
007400960419     C                   ENDIF
007500960502     **
007600960502     C     F1            IFEQ      05
007700960502     C                   CALL      'KRON14R'                                    EDIT SCHDE.
007800960502     C                   ENDIF
007900960415     **
008000960415     C                   ENDIF
008100960523
008200960523
008300960415     ** CHECK FOR VIEW SELECTION
008400960415     C     MNUFLD        IFEQ      03
008500960415     C                   READ      VIEW                                   10
008600960416     **
008700960416     C     F1            IFEQ      01
008800960416     C                   CALL      'KRON05R'                                    DSPLY CLASS
008900960415     C                   ENDIF
009000960416     **
009100960416     C     F1            IFEQ      02
009200960416     C                   CALL      'KRON07R'                                    DSPLY TRMNAL
009300960416     C                   ENDIF
009400960416     **
009500960416     C     F1            IFEQ      03
009600960416     C                   CALL      'KRON09R'                                    DSPLY RSTRCK
009700960416     C                   ENDIF
009800960419     **
009900960419     C     F1            IFEQ      04
010000960502     C                   CALL      'KRON12R'                                    DSPLY ACCRUL
010100960419     C                   ENDIF
010200960502     **
010300960502     C     F1            IFEQ      05
010400960502     C                   CALL      'KRON15R'                                    DSPLY SCHDE.
010500960502     C                   ENDIF
010600960415     **
010700960415     C                   ENDIF
010800980206
010900980206
011000980206     ** CHECK FOR REPORT SELECTION
011100980206     C     MNUFLD        IFEQ      04
011200980206     C                   READ      REPORT                                 10
011300980206     **
011400001206     C                   select
011500001206     C                   when      f1 >= 1 and F1 <= 3
011600980209     C                   CALL      'KRONXHRS'                                   DSPLY CLASS
011700980209     C                   parm                    @msgid
011800001206     c                   parm                    f1
011900001206     C                   exsr      get_er_rtn
012000001206     C                   other
012100001206     C                   endsl
012200980209     **
012300980206     **
012400990420     C                   ENDIF
012500960523
012600960523
012700960416     ** CHECK FOR EXIT SELECTION
012800980206     C     MNUFLD        IFEQ      05
012900960415     C                   READ      EXIT                                   10
013000960415     **
013100960415     C     F1            IFEQ      01
013200960415     C                   SETON                                        LR
013300960415     C                   RETURN
013400960415     C                   ENDIF
013500960415     C                   ENDIF
013600980209     **
013700980209     ** GET ERROR MESSAGE
013800980209     C     get_er_rtn    begsr
013900980209     C     @msgid        ifne      *blanks
014000980209     C                   call      'GETMESS'
014100980209     C                   parm                    @msgid
014200980209     C                   parm      *blanks       errmsg
014300980209     C                   goto      start
014400980209     C                   endif
014500980209     C                   endsr
