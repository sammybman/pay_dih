000100010227     H DECEDIT('.') DATEDIT(*ymd)  copyright('COPYRIGHT: (C) DISERV SYSTEM+
000200091119     H S, 1998-2009')
000300150305     H dftactgrp(*no) bnddir('DISDIR')
000400950821     F**
000500020206     F** Credit Control Application Display (EMPLOYEES)
000600010227     F** Done By Diserv Application Development Team
000700950821     F**
000800021005     F* Control Master File
000900021005     Fempcon    if   e             disk
001000030308     Fdisutl    if   e           k disk
001100020206     Femp000d   cf   e             workstn
001200030308     F                                     sfile(utilsfl:rrn)
001300010902     ** General PROTOTYPES
001400111013      /copy QCPYSRC,genPR
001500970602     **
001600980727     D                SDS
001700980727     D  @PGM             *PROC
001800111013     D  @USER                358    367
001900980727     D  @dsp                 244    253
002000000310     D  @excp_type            40     42
002100000310     D  @excp_num             43     46
002200000310     D  @excp_id              40     46
002300000310     **
002400980727     D arr             s             40    dim(2) ctdata perrcd(1)
002500010227     **
002600010807     D msgid           s              7
002700010807     D msgfile         s             10
002800010807     D msglib          s             10
002900020124     d #date           s              8  0
003000020125     d flag            s              1
003100010902     D curdate         s               d   datfmt(*iso)
003200010902     d nxtdate         s               d   datfmt(*iso)
003300020312     d date1           s              8  0
003400020312     d prno            s              6  0
003500021005     D upd_prno        s                   like(prno)
003600020124     d password1       s                   like(password)
003700020124     d cnt             s              1  0
003800020204     **
003900030308     D cmd             c                   'go '
004000030308     D cmd2            c                   'runqry '
004100020204     D length          s             15  5 inz(15)
004200020204     D command         s             15
004300030308     D application     s             10
004400030308     D origin          s              3    inz('EMT')
004500030308     d rrn             s              2  0
004600020124     **
004700111013     d*esysupdprd#     s                   export like(esysupdprd)
004800111013     d*esysfinmth#     s                   export like(esysfinmth)
004900111013     d*esysdedcyc#     s                   export like(esysdedcyc)
005000111013     d*esysdedcyh#     s                   export like(esysdedcyh)
005100111013     d*esyshis#        s                   export like(esyshis)
005200111013     d*esysceacct#     s                   export like(esysceacct)
005300111013     d*esysinacct#     s                   export like(esysinacct)
005400020124     **
005500020125     d msg1            c                   'The Location already closed'
005600020124     d  in             c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ +
005700020124     d                                     + 1234567890')
005800020124     d  out            c                   const('|@#$%¬&*()_+!¦:"}¢\;>{,./? +
005900020124     d                                     + giahlrbjku')
006000000205     **
006100000131
006200020124     **
006300000131     c
006400010920     **
006500980731     ** get data for heading
006600021005     c                   exsr      aainit
006700030308     ** Load Utilities Menu
006800030308     C                   eval      *in33 = *on
006900030308     C                   eval      rrn = *zeros
007000030308     C                   write     utilctl
007100030308     C                   eval      *in33 = *off
007200030308     **
007300030308     C                   eval      *in55 = *off
007400030308
007500030308     C     utl_key       klist
007600030308     C                   kfld                    origin
007700030308     **
007800030308     c     utl_key       setll     dutlfmt
007900030308     c     utl_key       reade     dutlfmt
008000030308     c                   dow       not(%eof)
008100030308     c                   eval      rrn = rrn + 1
008200030308     c                   eval      utilname = dutldesc
008300030308     c                   eval      utilorg  = dutlorg
008400030308     c                   eval      utilobj  = dutlobj
008500030308     c                   eval      utilobjt = dutlobjt
008600030308     c                   write     utilsfl
008700030308     c     utl_key       reade     dutlfmt
008800030308     c                   enddo
008900030308     C                   eval      *in55 = *on
009000030308     C                   eval      *in20 = rrn > *zeros
009100030308     C                   write     utilctl
009200980731     **
009300020124
009400020124     C
009500980731     **
009600010301     c     start         tag
009700010227     **********************
009800010227     ** Option selection **
009900010227     **********************
010000010227     C                   dow       0 = 0
010100020124     ** write record format to disply with intro text
010200991127     c                   write     fmt2
010300980629     c                   exfmt     menubar
010400991126     C                   clear                   errmsg
010500980629     ** f4 to get command line
010600980629     **
010700010227     c                   if        *in04
010800980629     c                   call      'QUSCMDLN'
010900010227     c                   iter
011000980629     c                   endif
011100010227
011200010227     C                   select
011300020204     ******************
011400020204     **  Daily Ops.  **
011500020204     ******************
011600010227     c                   when      mnufld = 1
011700980629     c                   read      opts                                   55
011800980724     **
011900010301     C                   select
012000980814
012100020206     ** Setup LIability Fixed Deductions
012200980805     **
012300011007     c                   when      f1 = 1
012400091121     C                   call      'EMP001R'                                    CREATE P/ROLL
012500010227     c                   iter
012600020206     ** Set Payroll deductions
012700980916
012800011007     c                   when      f1 = 2
012900091121     C                   call      'EMP002R'                                    CREATE P/ROLL
013000010227     c                   iter
013100020206     ** Employee Liability
013200020206
013300020206     c                   when      f1 = 3
013400091121     C                   call      'EMP003R'                                    CREATE P/ROLL
013500020206     c                   iter
013600020206     **Handling Small Balances
013700020206
013800020206     c                   when      f1 = 4
013900111013     C                   call      'EMP004R'                                    CREATE P/ROLL
014000020206     c                   iter
014100020206     **Authorise Emp Transactions
014200020206
014300050627     c                   when      f1 = 5
014400091121     C                   call      'EMP006R'                                    CREATE P/ROLL
014500020206     c                   iter
014600010426
014700010301     c                   endsl
014800990407
014900010227     *****************
015000980730     **  MAINTENANCE
015100010227     *****************
015200010227     c                   when      mnufld = 2
015300980629     c                   read      maint                                  55
015400010301
015500010301     c                   select
015600010227     ** System Control
015700010301     c                   when      f1 = 1
015800111013     c                   call      'EMPSYSCTLR'
015900010227     c                   iter
016000020206     ** Type Maintenance
016100010301     c                   when      f1 = 2
016200091121     c                   call      'EMPMNT01R'
016300091121     C                   parm      '2'           fld               1
016400010227     c                   iter
016500071018     c                   when      f1 = 3
016600071018     c                   call      'EMPMNT02R'
016700071018     c                   iter
016800010301     c                   endsl
016900980629     **
017000010227     *****************
017100010227     **  Enquiry    **
017200010227     *****************
017300010227     **
017400010425     c                   when      mnufld = 3
017500010227     c                   read      view                                   55
017600010301
017700010301     C                   select
017800020206     ** Employee Enquiry
017900020204     c                   when      f1 = 1
018000020206     c                   call      'EMPPRF'
018100020204     c                   iter
018200010301     C                   endsl
018300010227     *****************
018400010227     **  Reports    **
018500010227     *****************
018600010425     c                   when      mnufld = 4
018700980629     c                   read      report                                 55
018800010301
018900010301     C                   select
019000020204     ** General reports
019100010301     c                   when      f1 = 1
019200111013     c                   call      'EMPRPTSR'
019300010227     c                   iter
019400020204     ** Period end Close
019500010301     c                   when      f1 = 2
019600111013     C**                 eval      upd_prno = get_new_prd(esysupdprd#:1:'A')
019700111013     C**                 if        prno > upd_prno
019800111013     C                   call      'EMPRPTFR'
019900111013     C**                 parm      *blank        ans               3
020000111013     C**                 parm                    upd_prno
020100111013     C**                 endif
020200010227     c                   iter
020300020206     ** Employee Transactions
020400020206     c                   when      f1 = 3
020500091121     c                   call      'EMPRPT01R'
020600020204     c                   iter
020700050627     **Automatic Liability Deductions
020800050627
020900050627     c                   when      f1 = 4
021000091121     C                   call      'EMPRPT02R'                                  CREATE P/ROLL
021100050627     c                   iter
021200010301     C                   endsl
021300030308     ************************
021400030308     **  Utilities         **
021500030308     ************************
021600030308
021700030308     **
021800030308     c                   when      mnufld = 5
021900030308     C                   Readc     utilsfl
022000030308     C                   dow       utilcctl <> 1
022100030308     C                   Readc     utilsfl
022200030308     C                   enddo
022300030308     ** Call selected application
022400030308     c                   eval      application = utilobj
022500030308     c                   select
022600030308     ** Program calls
022700030308     C                   when      utilobjt = 'P'
022800030308     C                   call      application
022900030308     ** Menu calls
023000030308     C                   when      utilobjt = 'M'
023100030308     C                   eval      command = cmd + application
023200030308     C                   call      'QCMDEXC'
023300030308     C                   parm                    command
023400030308     C                   parm                    length
023500030308     ** Query Run
023600030308     C                   when      utilobjt = 'Q'
023700030308     C                   eval      command = cmd2 + application
023800030308     C                   call      'QCMDEXC'
023900030308     C                   parm                    command
024000030308     C                   parm                    length
024100030308     C                   endsl
024200030308     **
024300030308     C                   iter
024400010227     *********
024500980730     ** EXIT
024600010227     *********
024700030308     c                   when      mnufld = 6
024800980629     c                   read      exit                                   55
024900980629     c                   if        F5 = 1
025000010227     c                   eval      *inlr = *on
025100980629     c                   return
025200980629     c                   endif
025300010227     c                   endsl
025400010227
025500010227     c                   enddo
025600000329     *****************************************************************
025700000329     ** Program Exception Routine                                   **
025800000329     *****************************************************************
025900031006     C     *pssr         begsr
026000031006     C                   rolbk
026100031006     C                   select
026200010807     **
026300031006     C                   when      @excp_type = 'DIH'
026400031006     C                   call      'MESSTXT'
026500031006     C                   parm      @excp_id      MSGID
026600031006     C                   parm      'qgpl'        msglib
026700031006     C                   parm      'dihmsgf'     msgfile
026800031006     C                   parm      *BLANKS       ERRMSG
026900010807     **
027000031006     C                   when      @excp_type = 'MSG'
027100031006     C                   call      'MESSTXT'
027200031006     C                   parm      @excp_id      msgid
027300031006     C                   parm      'paylib'      msglib
027400031006     C                   parm      'paymess'     msgfile
027500031006     C                   parm      *BLANKS       errmsg
027600010807     **
027700031006     C                   other
027800031006     C                   call      'MESSAGE'
027900031006     C                   parm      'DIH0129'     MSGID             7
028000031006     C                   parm      *BLANKS       ERRMSG
028100031006     C                   endsl
028200031006     C                   goto      start
028300031006     C                   endsr
028400970916     c
028500980727     *****************************************************************
028600000217     ** Begin subroutine to print header information
028700980727     *****************************************************************
028800980727     C     AAINIT        BEGSR
028900021005     C                   callp     get_sys_date(date1)
029000021005     C                   callp     get_prd_info('*prd':*blanks:prno:date1)
029100020204     C**                 eval      coname = get_coname
029200020204     C**                 eval      @title = arr(1)
029300980727     C                   ENDSR
029400020125
029500020125     *****************************************************************
029600020125     ** Initialize subroutine to read control info
029700020125     *****************************************************************
029800020125     C     *inzsr        begsr
029900020125     ** Read system control info.
030000111013     C*                  read      esyfmt
030100111013     C*                  eval      esysupdprd# = esysupdprd
030200111013     C*                  eval      esysfinmth# = esysfinmth
030300111013     C*                  eval      esysdedcyc# = esysdedcyc
030400111013     C*                  eval      esysdedcyh# = esysdedcyh
030500111013     C*                  eval      esyshis# = esyshis
030600111013     C*                  eval      esysceacct# = esysceacct
030700111013     C*                  eval      esysinacct# = esysinacct
030800020125     C                   endsr
030900000217     *****************************************************************
031000980727**
031100021005       EMPLOYEE CREDIT MANAGEMENT
