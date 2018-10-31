000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200111002     H/define profoundui
000300010427     Fpaymnt06d cf   e             workstn
000400111002     F/IF DEFINED(WEB)
000500111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000600111002     F/ENDIF
000700111002     Fpayrtypl01if   e           k disk    usropn
000800111002     Femptypel02if   e           k disk    usropn
000900111002     Fpayrresl01uf a e           k disk    commit  usropn
001000010723     ** Payroll PROTOTYPES
001100110627      /copy QCPYSRC,genPR
001200110627      /copy QCPYSRC,PAYPR
001300110627      /copy QCPYSRC,autPR
001400971012     **
001500010411     Darr              s             40    dim(1) ctdata perrcd(1)
001600010427     **
001700971012     D                sds
001800971012     D  @pgm             *proc
001900111002     D  @user                358    367
002000111002
002100010427     d cnt             s              2  0
002200010427     d run_list        s              1    dim(20)
002300040621     d msg1            c                   'Invalid Employee Type -'
002400010723     d msg2            c                   const('Cannot Add Employee Type to t+
002500010723     d                                     his Run Type ')
002600010724     d msg3            c                   'Invalid Run Type'
002700010724     d msg4            c                   'This run type cannot be edited'
002800021024     d msg5            c                   'Use F9 to setup Payslip style optio-
002900021024     d                                     ns '
003000021024     d msg6            c                   'Invalid paysheet style selection'
003100040621     d msg7            c                   'Guarantee Percentage cannot be grea-
003200040621     d                                     ter tha 100 % '
003300010228     **
003400010411
003500040207      /free
003600111002         // authority check
003700111002         monitor;
003800111002           authority_check('PAY':@pgm);
003900111002         on-error;
004000111002           show_msg(get_err_msg('DIH0054'));
004100111002           *inlr = *on;
004200111002           return;
004300111002         endmon;
004400040207      /end-free
004500010411     ** Initial Setup
004600010411     C                   exsr      aainit
004700951124     **
004800010411     **************************
004900010411     *** Start of Program   ***
005000010411     **************************
005100010411     C                   dow       0 = 0
005200951124     **
005300010411     C                   exfmt     fmt1
005400010724     ** Clear Error Message
005500010724     C                   clear                   errmsg
005600010411     **
005700010411     C                   if        *in03
005800010411     C                   eval      *inlr = *on
005900010411     C                   return
006000010411     C                   endif
006100010411     **
006200010411     C                   setoff                                       979899
006300010427     ** Display List of Valid Run Types
006400010411     C                   if        *in01
006500010904     C                   call      'PAYENQ02R'
006600010904     C                   parm                    run_type
006700010411     C                   iter
006800010411     C                   endif
006900951207     **
007000010427     C     run_type      chain     prtyfmt                            99
007100010427     ** Invalid Run type entered
007200010411     C                   if        *in99
007300010724     C                   eval      errmsg = msg3
007400010411     C                   iter
007500010411     C                   endif
007600010724     ** Run type cannot be edited
007700010724     C                   if        prtyprno = 0
007800010724     C                   eval      errmsg = msg4
007900010724     C                   iter
008000010724     C                   endif
008100010427     ** Get Run Type name for Screen
008200010427     C                   eval      run_name = prtydesc
008300960127     **
008400971012     *****************************
008500980606     **  Select transactions
008600010427     C     run_type      chain(n)  prrefmt
008700010427     C                   if        %found
008800040622     c                   eval      guar_per = prreguar
008900040622     c                   eval      decl_earn = prredeclr
009000010427     c                   eval      flag = prreflag
009100010427     c                   eval      list = prrelist
009200010918     c                   eval      batch_seq = prreseqcod
009300011126     c                   eval      rate_flag = prrertflag
009400021024     c                   eval      pstyle = prrepstyle
009500021024     c                   eval      ytd_opt = prreytdopt
009600971012     C                   else
009700040622     c                   eval      decl_earn = *zeros
009800040621     c                   eval      guar_per = *zeros
009900010427     c                   eval      flag = *blanks
010000010427     c                   eval      list = *blanks
010100021024     c                   eval      pstyle = *zero
010200021024     c                   eval      ytd_opt = *zero
010300010918     c                   eval      batch_seq = 1
010400971012     C                   endif
010500971012     **
010600010411     C                   leave
010700010411     C                   enddo
010800010411     **************************
010900010411     *** Body of Program    ***
011000010411     **************************
011100010411     C                   dow       0 = 0
011200010427     C                   exfmt     fmt2
011300010427
011400010427     ** Clear Error Message
011500010427     C                   clear                   errmsg
011600010411     **  Go back to start
011700010411     C                   if        *in02
011800010411     C                   leave
011900010411     C                   endif
012000010411     **  End Program
012100010411     C                   if        *in03
012200010411     C                   eval      *inlr = *on
012300010411     C                   return
012400010411     C                   endif
012500010904     ** Display List of Valid Run Types
012600010904     C                   if        *in01
012700010904     C                   call      'HMRENQ08R'
012800010904     C                   iter
012900010904     C                   endif
013000010427     ** Validate List
013100010427     C                   if        list <> *blanks
013200010427     C                   movea     list          run_list
013300010427     C                   eval      cnt  = 1
013400010427     **
013500010427     C                   dow       run_list(cnt) <> *blank
013600010427     C     run_list(cnt) chain     empfmt
013700010427     C                   if        not(%found)
013800010427     C                   eval      errmsg = msg1 + run_list(cnt)
013900010427     C                   leave
014000010427     C                   endif
014100040621     C                   eval      cnt +=  1
014200010427     C                   enddo
014300010427     ** display errmsg
014400010427     C                   if        errmsg <> *blanks
014500010427     C                   iter
014600010427     C                   endif
014700010427     C                   endif
014800010427
014900040621     ** validate other entries
015000040621     C                   if        guar_per > 100
015100040621     c                   eval      errmsg  = msg7
015200040621     C                   iter
015300040621     C                   endif
015400040621
015500010427     ** IF RECORD WAS CHANGED REDISPLAY FORMAT
015600010427     C                   if        *in88
015700010427     C                   iter
015800010427     C                   endif
015900021024     ** More options ......
016000021024     C                   if        *in09
016100021024     C                   exsr      more_options
016200021024     C                   if        *in02
016300021024     C                   iter
016400021024     C                   else
016500021024     C                   leave
016600021024     C                   endif
016700021024     C                   endif
016800010427     **  Confirm
016900010427     C                   if        *in15
017000021024     ** validate other entries
017100021024     C                   if        pstyle = *zero or ytd_opt = *zero
017200021024     c                   eval      errmsg  = msg5
017300021024     C                   iter
017400021024     C                   endif
017500010427     C                   exsr      confirm_rtn
017600010723     ** validate employee type entries
017700010723     C                   if        validate_dft(run_type:*omit)
017800010723     c                   eval      errmsg  = msg2
017900010723     C                   rolbk
018000010723     C                   iter
018100010723     C                   endif
018200010723     C                   commit
018300010427     C                   leave
018400010427     C                   endif
018500951124     **
018600010411     C                   enddo
018700971012     **
018800111002      /free
018900111002
019000111002         //*****************************
019100111002         // Session Init
019200111002         //*****************************
019300111002
019400111002          begsr *inzsr;
019500111006            get_session_info('PAY':errmsg:@pgm);
019600111002            if errmsg <> *blanks;
019700111002              show_msg(errmsg);
019800111002              *inlr = *on;
019900111002              return;
020000111002            endif;
020100111002
020200111002            open(e) payrtypl01;
020300111002            open(e) payrresl01;
020400111002            open(e) emptypel02;
020500111002
020600111002          endsr;
020700111002
020800111002      /end-free
020900010411     ******************************************************************
021000010427     ***  Update Payroll Run Type Restriction File
021100010411     ******************************************************************
021200971012     **
021300010427     C     confirm_rtn   begsr
021400010427
021500010427     C     run_type      chain     prrefmt
021600010427     **
021700010427     c                   eval      prrelist = list
021800040621     c                   eval      prreguar  = guar_per
021900040622     c                   eval      prredeclr = decl_earn
022000010427     c                   eval      prreflag  = flag
022100010918     c                   eval      prreseqcod = batch_seq
022200011126     c                   eval      prrertflag = rate_flag
022300021024     c                   eval      prrepstyle = pstyle
022400021024     c                   eval      prreytdopt = ytd_opt
022500010427     **
022600010427     C                   if        %found
022700010427     C                   update    prrefmt
022800971012     C                   else
022900010427     c                   eval      prreptype = run_type
023000010427     c                   eval      prrests  = 'A'
023100010427     C                   write     prrefmt
023200971012     C                   endif
023300971012     C                   endsr
023400021024     ********************
023500021024     ***  More options **
023600021024     ********************
023700021024     **
023800021024     C     more_options  begsr
023900021024     C                   dow       0 = 0
024000021024
024100021024     C                   exfmt     fmt3
024200021024     C                   eval      errmsg = *blanks
024300021024     ** Go back
024400021024     C                   if        *in02
024500021024     C                   leave
024600021024     C                   endif
024700021024
024800021024     ** validate other entries
024900021024     C                   if        pstyle = *zero or ytd_opt = *zero
025000021024     c                   eval      errmsg  = msg6
025100021024     C                   iter
025200021024     C                   endif
025300021024     **  Confirm
025400021024     C                   if        *in15
025500021024     C                   exsr      confirm_rtn
025600021024     ** validate employee type entries
025700021024     C                   if        validate_dft(run_type:*omit)
025800021024     c                   eval      errmsg  = msg2
025900021024     C                   rolbk
026000021024     C                   iter
026100021024     C                   endif
026200021024     C                   commit
026300021024     C                   leave
026400021024     C                   endif
026500021024     C                   enddo
026600021024
026700021024     C                   endsr
026800010411     ***************************************************
026900010411     ** First initilisation routine for all programs  **
027000010411     ***************************************************
027100010411     C     AAINIT        BEGSR
027200010411     **
027300010411     **
027400010815     C                   eval      CONAME = get_coname
027500010815     C                   eval      @DATE = get_sys_date
027600010411     C                   time                    @TIME             6 0
027700010411     ** select header for program
027800010411     C                   eval      @title = arr(1)
027900010411     C                   eval      *in = *off
028000010411     **
028100010411     C                   ENDSR
028200971012     **
028300010411**  Program Title
028400010427      Payroll Run Type Restrictions
