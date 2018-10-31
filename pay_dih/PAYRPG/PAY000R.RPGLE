000100010227     H DECEDIT('.') DATEDIT(*ymd)  copyright('COPYRIGHT: (C) DISERV SYSTEM+
000200151009     H S, 2001-2015')
000300151009     H dftactgrp(*no) bnddir('DISDIR')
000400950821     F**
000500010425     F** Payroll Application Display
000600010227     F** Done By Diserv Application Development Team
000700950821     F**
000800011011     Fpay000d   cf   e             workstn
000900020313     F                                     sfile(utilsfl:rrn)
001000020313     Fdisutl    if   e           k disk
001100010902     Fpayrtypl01if   e           k disk
001200010902     ** General PROTOTYPES
001300111002      /copy QCPYSRC,genPR
001400970602     **
001500980727     D                SDS
001600980727     D  @PGM             *PROC
001700010213     D  @USER                254    263
001800980727     D  @dsp                 244    253
001900000310     D  @excp_type            40     42
002000000310     D  @excp_num             43     46
002100000310     D  @excp_id              40     46
002200020626     **
002300020627     D*cmdexc          pr                  extpgm('QCMDEXC')
002400020627     D*cmd                          200    options(*varsize) const
002500020627     D*cmdlen                        15p 5 const
002600020626
002700000310     **
002800980727     D arr             s             40    dim(2) ctdata perrcd(1)
002900030308     c*
003000030308     D cmd             c                   'go '
003100030308     D cmd2            c                   'runqry '
003200020626     D length          s             15  5 inz(25)
003300020927     D command         s             25
003400020313     D application     s             10
003500020313     D origin          s              3    inz('PAY')
003600020313     d rrn             s              2  0
003700010807     D msgid           s              7
003800010807     D msgfile         s             10
003900010807     D msglib          s             10
004000010902     d date            s              8  0
004100010902     dno_days          s              4  0
004200010902     D curdate         s               d   datfmt(*iso)
004300010902     d nxtdate         s               d   datfmt(*iso)
004400010902     D msg1            c                   'Some Payroll(s) need to be processe+
004500010902     D                                     d'
004600000205     **
004700000131
004800000131     c
004900010920     **
005000980731     ** get data for heading
005100980727     c                   exsr      aainit
005200010902     ** Check for available payrolls
005300010902     c     *loval        setll     prtyfmt
005400010902     c                   read      prtyfmt
005500010902     c                   dow       not(%eof)
005600010902     **  Select transactions
005700010902     C                   if        prtyndate > *zeros and prtyprno <> *zeros
005800010902     C                   move      prtyndate     nxtdate
005900010902     C     curdate       subdur    nxtdate       no_days:*d
006000010902     **
006100010902     C                   if        no_days > prtylate
006200010902     c                   eval      errmsg = msg1
006300010902     C                   leave
006400010902     C                   endif
006500010902     **
006600010902     C                   endif
006700010902     c                   read      prtyfmt
006800010902     c                   enddo
006900020313     **
007000020313     ** Load Utilities Menu
007100020313     C                   eval      *in33 = *on
007200020313     C                   eval      rrn = *zeros
007300020313     C                   write     utilctl
007400020313     C                   eval      *in33 = *off
007500020313     **
007600020313     C                   eval      *in55 = *off
007700020313
007800020313     C     utl_key       klist
007900020313     C                   kfld                    origin
008000020313     **
008100020313     c     utl_key       setll     dutlfmt
008200020313     c     utl_key       reade     dutlfmt
008300020313     c                   dow       not(%eof)
008400020313     c                   eval      rrn = rrn + 1
008500020313     c                   eval      utilname = dutldesc
008600020313     c                   eval      utilorg  = dutlorg
008700020313     c                   eval      utilobj  = dutlobj
008800020313     c                   eval      utilobjt = dutlobjt
008900020313     c                   write     utilsfl
009000020313     c     utl_key       reade     dutlfmt
009100020313     c                   enddo
009200020313     C                   eval      *in55 = *on
009300030308     C                   eval      *in20 = rrn > *zeros
009400020313     C                   write     utilctl
009500010902     **
009600980731     ** disply first screen
009700980731     **
009800010227     c***********        exfmt     fmt1
009900980731     **F3 = exit
010000010920     c                   if        *in03
010100010920     c                   eval      *inlr = *on
010200980730     c                   return
010300980730     c                   endif
010400980731     **
010500980731     ** write record format to disply with intro text
010600020315     C     start         tag
010700980731     **
010800010227     **********************
010900010227     ** Option selection **
011000010227     **********************
011100010227     C                   dow       0 = 0
011200991127     c                   write     fmt2
011300980629     c                   exfmt     menubar
011400991126     C                   clear                   errmsg
011500980629     ** f4 to get command line
011600980629     **
011700010227     c                   if        *in04
011800980629     c                   call      'QUSCMDLN'
011900010227     c                   iter
012000980629     c                   endif
012100010227
012200010227     C                   select
012300010227     ***********************
012400010425     **  PAYROLL OPERATIONS
012500010227     ***********************
012600010227     c                   when      mnufld = 1
012700980629     c                   read      opts                                   55
012800980724     **
012900010301     C                   select
013000980814
013100980724     **
013200010425     ** Create Payroll Files
013300980805     **
013400011007     c                   when      f1 = 1
013500091119     C                   call      'PAYR008R'                                   CREATE P/ROLL
013600010227     c                   iter
013700010816     ** Hours Entry
013800980916
013900011007     c                   when      f1 = 2
014000091119     C                   call      'PAYR002R'                                   CREATE P/ROLL
014100010227     c                   iter
014200010612     ** Department Payments setup
014300010425
014400011007     c                   when      f1 = 3
014500091119     c                   call      'PAYR003R'
014600010425     c                   iter
014700010426
014800010426     ** Maintain Employee Timesheets
014900010426
015000011007     c                   when      f1 = 4
015100091119     c                   call      'PAYR006R'
015200010426     c                   iter
015300010426     ** Run Payroll
015400010425
015500011007     c                   when      f1 = 5
015600091119     c                   call      'PAYR000R'
015700010425     c                   iter
015800010816     ** Handling Retro Payments
015900010425
016000011007     c                   when      f1 = 6
016100091119     c                   call      'PAYR004R'
016200010425     c                   iter
016300030315     ** Electronic Payroll Transfer
016400030315
016500030315     c                   when      f1 = 7
016600091119     c                   call      'PAYR010R'
016700101216     c                   eval      *inlr = *on
016800101216     c                   return
016900010301     c                   endsl
017000990407
017100010227     *****************
017200980730     **  MAINTENANCE
017300010227     *****************
017400010227     c                   when      mnufld = 2
017500980629     c                   read      maint                                  55
017600010301
017700010301     c                   select
017800010227     ** System Control
017900010301     c                   when      f1 = 1
018000111002     c                   call      'PAYSYSCTLR'
018100010227     c                   iter
018200011024     ** Suspended Payments
018300010301     c                   when      f1 = 2
018400091119     c                   call      'PAYMNT08R'
018500010227     c                   iter
018600011024     ** Payment Type Maintenance
018700010301     c                   when      f1 = 3
018800091119     c                   call      'PAYMNT01R'
018900010227     c                   iter
019000010425     ** Payroll Type Definitions(GL.)
019100010301     c                   when      f1 = 4
019200091119     c                   call      'PAYMNT02R'
019300010227     c                   iter
019400010425     ** Payroll Cross Reference
019500010425     c                   when      f1 = 5
019600091119     c                   call      'PAYMNT03R'
019700010425     c                   iter
019800010425     ** Payroll Runtype Codes
019900010425     c                   when      f1 = 6
020000091119     c                   call      'PAYMNT05R'
020100010227     c                   iter
020200010425     ** Payroll Runtype Restrictions
020300010425     c                   when      f1 = 7
020400091119     c                   call      'PAYMNT06R'
020500010227     c                   iter
020600010426     ** Payroll Runtype Defaults
020700010426     c                   when      f1 = 8
020800091119     c                   call      'PAYMNT07R'
020900010426     c                   iter
021000010808     ** Holiday file Maintenance
021100010901     c                   when      f1 =  9
021200091119     c                   call      'PAYMNT10R'
021300010227     c                   iter
021400010425     ** Payroll Year to date Maintenance
021500010918     c                   when      f1 = 10
021600091119     c                   call      'PAYMNT11R'
021700010425     c                   iter
021800021115     ** Payroll Payslip Comments
021900021115     c                   when      f1 = 11
022000091119     c                   call      'PAYMNT04R'
022100021115     c                   iter
022200151009     ** Payroll Performance Incentive setup
022300151009     c                   when      f1 = 12
022400151009     c                   call      'PAYMNT12R'
022500151009     c                   iter
022600010301     c                   endsl
022700980629     **
022800010227     *****************
022900010227     **  Enquiry    **
023000010227     *****************
023100010227     **
023200010425     c                   when      mnufld = 3
023300010227     c                   read      view                                   55
023400010301
023500010301     C                   select
023600010227     ** Employee Profile
023700010301     c                   when      f1 = 1
023800010227     c                   call      'EMPPRF'
023900010227     c                   iter
024000010425     ** Employee Leave
024100010425     c                   when      f1 = 2
024200010922     c                   call      'HMRENQ10R'
024300010227     c                   iter
024400010425     ** Employee Leave Payment
024500010425     c                   when      f1 = 3
024600091119     c                   call      'HMRENQ01R'
024700010425     c                   iter
024800010301     C                   endsl
024900010227     *****************
025000010227     **  Reports    **
025100010227     *****************
025200010425     c                   when      mnufld = 4
025300980629     c                   read      report                                 55
025400010301
025500010301     C                   select
025600020129     ** Time sheet Query
025700010301     c                   when      f1 = 1
025800091119     c                   call      'PAYRPT10R'
025900010227     c                   iter
026000020129     ** GL. Defn. report
026100010301     c                   when      f1 = 2
026200091119     c                   call      'PAYRPT11R'
026300010227     c                   iter
026400010905     ** Division Summary
026500010425     c                   when      f1 = 3
026600091119     c                   call      'PAYRPT01R'
026700010425     c                   iter
026800010905     ** Payroll Check Sheet
026900010905     c                   when      f1 = 4
027000091119     c                   call      'PAYRPT03R'
027100010905     c                   iter
027200010908     ** Payroll Department Analysis
027300010908     c                   when      f1 = 5
027400091119     c                   call      'PAYRPT05R'
027500010908     c                   iter
027600010906
027700010906     ** Payroll Denomination Breakdown
027800010906     c                   when      f1 = 6
027900091119     c                   call      'PAYRPT04R'
028000010906     c                   iter
028100010905     ** Monthly NIS
028200010906     c                   when      f1 = 7
028300091119     c                   call      'PAYRPT06R'
028400010425     c                   iter
028500011001     ** Monthly Tax/Union Etc.
028600010906     c                   when      f1 = 8
028700091119     c                   call      'PAYRPT07R'
028800010425     c                   iter
028900011205     ** Payslip Re-Generation report
029000011205     c                   when      f1 = 9
029100091119     c                   call      'PAYRPT09R'
029200011205     c                   iter
029300011003     ** Year-to-date Earnings report
029400011205     c                   when      f1 = 10
029500091119     c                   call      'PAYRPT08R'
029600010425     c                   iter
029700010905     ** Yearend Income tax
029800010906     c                   when      f1 = 11
029900091119     c                   call      'PAYRPT02R'
030000010425     c                   iter
030001151026     ** Performance Incentive
030002151026     c                   when      f1 = 12
030003151026     c                   call      'PAYRPT12R'
030004151026     c                   iter
030100010301     C                   endsl
030200011007     ************************
030300011007     **  Utilities         **
030400011007     ************************
030500011007
030600011007     **
030700011007     c                   when      mnufld = 5
030800020313     C                   Readc     utilsfl
030900020313     C                   dow       utilcctl <> 1
031000020313     C                   Readc     utilsfl
031100020313     C                   enddo
031200020313     ** Call selected application
031300020313     c                   eval      application = utilobj
031400020313     c                   select
031500020313     ** Program calls
031600020313     C                   when      utilobjt = 'P'
031700020315     C                   call      application
031800020313     ** Menu calls
031900020313     C                   when      utilobjt = 'M'
032000020313     C                   eval      command = cmd + application
032100020313     C                   call      'QCMDEXC'
032200020313     C                   parm                    command
032300020313     C                   parm                    length
032400030308     ** Query Run
032500030308     C                   when      utilobjt = 'Q'
032600030308     C                   eval      command = cmd2 + application
032700030308     C                   call      'QCMDEXC'
032800030308     C                   parm                    command
032900030308     C                   parm                    length
033000151011     ** Commands
033100151011     C                   when      utilobjt = 'C'
033200151011     C                   eval      command = '?' + application
033300151011     C                   call      'QCMDEXC'
033400151011     C                   parm                    command
033500151011     C                   parm                    length
033600020313     C                   endsl
033700020313     **
033800020313     C                   iter
033900010227     *********
034000980730     ** EXIT
034100010227     *********
034200011007     c                   when      mnufld = 6
034300980629     c                   read      exit                                   55
034400980629     c                   if        F5 = 1
034500010227     c                   eval      *inlr = *on
034600980629     c                   return
034700980629     c                   endif
034800010227     c                   endsl
034900010227
035000010227     c                   enddo
035100000329     *****************************************************************
035200000329     ** Program Exception Routine                                   **
035300000329     *****************************************************************
035400131101     C**   *pssr         begsr
035500131101     C     apssr         begsr
035600010807     C                   rolbk
035700010807     C                   select
035800010807     **
035900020315     C                   when      @excp_type = 'DIH' or
036000020315     C                               @excp_type = 'PAY'
036100010807     C                   call      'MESSTXT'
036200010807     C                   parm      @excp_id      MSGID
036300100309     C                   parm      'diservsys'   msglib
036400010807     C                   parm      'dihmsgf'     msgfile
036500010807     C                   parm      *BLANKS       ERRMSG
036600010807     **
036700010807     C                   other
036800010807     C                   call      'MESSAGE'
036900010807     C                   parm      'DIH0129'     MSGID             7
037000010807     C                   parm      *BLANKS       ERRMSG
037100010807     C                   endsl
037200020315     C                   goto      start
037300020315     C                   endsr
037400020315     C**                 endsr     '*DETC'
037500970916     c
037600980727     *****************************************************************
037700000217     ** Begin subroutine to print header information
037800980727     *****************************************************************
037900980727     C     AAINIT        BEGSR
038000010227     C*                  MOVEL     ARR(1)        @TITLE
038100010902     C*                  eval      @DATE = get_sys_date(date)
038200010902     C                   callp     get_sys_date(date)
038300010902     C                   time                    @TIME             6 0
038400010902     ** Get system date
038500010902     C                   move      date          curdate
038600980727     C                   ENDSR
038700000217     *****************************************************************
038800980727**
038900010425       Payroll Applications Menu
